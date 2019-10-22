! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridCsrv2ndUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridCsrvUTest - Unit tests for conservative Field Regrid methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 conservative Field Regrid unit tests.
!
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    use ESMF_GridUtilMod

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1
    logical :: itrp = .false.
    logical :: csrv = .false.
  
    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE
       ! initialize 
      rc=ESMF_SUCCESS

! This #if surrounds all the tests to enable turning on just one test
#if 1
      !============== 2nd Order  Mesh to Mesh =======================================
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_MeshToMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding Mesh to Mesh."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding Mesh to Mesh.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding Mesh to Mesh.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      !============== 2nd Order  Mesh to Mesh w/ Masks ====================
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_MeshToMeshWMasks(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative Mesh to Mesh with masks."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative Mesh to Mesh with masks", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative Mesh to Mesh with masks.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      !============== 2nd Order  Mesh to Mesh w/ small number of neighbors =======
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_SmallNumNbrs(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative with small number of neighbors."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative with small number of neighbors", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative with small number of neighbors", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !============== 2nd Order Grid to Grid =======================================
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_GridToGrid(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding Grid to Grid."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding Grid to Grid.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding Grid to Grid.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------


      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_MasksAndUserArea(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding on a sphere with user areas"

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding on a sphere with user areas: interpolation error"

      
      ! return result
      call ESMF_Test((itrp .eqv. .true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
       !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding on a sphere with user areas: conservation error"

      
      ! return result
      call ESMF_Test((csrv .eqv. .true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !============== 2nd Order Sph  Mesh to Mesh Concave =======================================
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_RegridSph4ConcaveMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding with sph concave element."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding with sph concave element.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding with sph concave element.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)



      !============== 2nd Order Grid to Grid =======================================
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_CSGridToGrid(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding Cubed Sphere Grid to Grid."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding Cubed Sphere Grid to Grid.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding Cubed Sphere Grid to Grid.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !============== 2nd Order Grid to Grid with masks and user area ============
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_CSGridToGridWMasks(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding Cubed Sphere Grid to Grid with ", &
                      "with masks."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding Cubed Sphere Grid to Grid ", &
                     "with masks interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding Cubed Sphere Grid to Grid ", &
                     "with masks conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !============== 2nd Order  Mesh to Mesh Cartesian =======================================
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_MeshToMeshCart(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding Cart. Mesh to Mesh."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding Cart. Mesh to Mesh.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding Cart. Mesh to Mesh.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)



      !============== 2nd Order Sph  Mesh to Mesh Concave =======================================
      ! initialize 
      rc=ESMF_SUCCESS
      
      call test_RegridCart4ConcaveMesh(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding with Cart. concave element."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding with Cart. concave element.", &
                     " interpolation error"
      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding with Cart. concave element.", &
                     " conservation error"
      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !============== 2nd Order  Grid to Grid Cartesian =======================================
      ! initialize 
      rc=ESMF_SUCCESS

      call test_cartcsrvregridwmasks(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Returned an error"
      write(name, *) "Second-order conservative regridding Cart. Grid to Grid."

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-2"
      write(name, *) "Second-order conservative regridding Cart. Grid to Grid.", &
                     " interpolation error"

      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-12"
      write(name, *) "Second-order conservative regridding Cart. Grid to Grid.", &
                     " conservation error"

      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------

      !EX_UTest
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test per location regrid status output"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_PerLocStatus(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

#endif
#endif

    call ESMF_TestEnd(ESMF_SRCLINE)

contains 

#define ESMF_METHOD "test_MeshToMesh"

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
  integer, pointer :: elemMask(:)
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,1,0,0,0,0,0,0,0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,1/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0,0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0/) 

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
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
         elementCoords=elemCoords, elementMask=elemMask,&
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

subroutine createTestMesh3x3_1(mesh, rc)
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
  integer, pointer :: elemMask(:)
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
                 1.25,1.25, &  ! 6
                 1.75,1.25, &  ! 7
                 3.0,1.0, &  ! 8
                  0.0,2.0, &  ! 9
                 1.25,1.75, &  ! 10
                 1.75,1.75, &  ! 11
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0,0,0,0,0,0,0,0,0/) 

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
                  1.25,1.25 /)  ! 6 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1/) 

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
                  1.25,1.25, &  ! 6
                  1.75,1.25, &  ! 7
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0/) 

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
                  1.25,1.25, &  ! 6
                  1.75,1.25, &  ! 7
                  0.0,2.0, &  ! 9 
                  1.25,1.75, &  ! 10
                  1.75,1.75, &  ! 11
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0,0/) 

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
     nodeCoords=(/1.75,1.25, &  ! 7
                  3.0,1.0, &  ! 8
                  1.75,1.75, &  ! 11
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0/)  

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
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
         elementCoords=elemCoords, elementMask=elemMask,&
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemMask)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)

end subroutine createTestMesh3x3_1





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !                     Mesh Ids
  !
  !   2.8   13 ------ 14 ------- 15 ------- 16
  !         |         |          |  10    / |
  !   2.3   |    7    |    8     |     /    |
  !         |         |          |  /    9  |
  !   1.8   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !   1.5   |    4    |    5     |    6     |
  !         |         |          |          |
  !   1.2   5 ------- 6 -------- 7 -------- 8
  !         |         |          |          |
  !   0.7   |    1    |    2     |    3     |
  !         |         |          |          |
  !   0.2   1 ------- 2 -------- 3 -------- 4
  !            
  !        0.2  0.7  1.2  1.5   1.8  2.3   2.8
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
  !   2.8   2 ------- 2 -------- 3 -------- 3
  !         |         |          |  3    /  |
  !         |    2    |    2     |     /    |
  !         |         |          |  /    3  |
  !   1.8   2 ------- 2 -------- 3 -------- 3
  !         |         |          |          |
  !         |    2    |    2     |    3     |
  !         |         |          |          |
  !   1.2   0 ------- 0 -------- 1 -------- 1
  !         |         |          |          |
  !         |    0    |    1     |    1     |
  !         |         |          |          |
  !   0.2   0 ------- 0 -------- 1 -------- 1
  !
  !        0.2       1.2        1.8        2.8
  !
  !               Node Owners at corners
  !              Element Owners in centers
  ! 

subroutine createTestMesh3x3_2(mesh, rc)
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
     nodeCoords=(/0.2,0.2, & ! 1
                 1.2,0.2, &  ! 2
                 1.8,0.2, &  ! 3
                 2.8,0.2, &  ! 4
                 0.2,1.2, &  ! 5
                 1.2,1.2, &  ! 6
                 1.8,1.2, &  ! 7
                 2.8,1.2, &  ! 8
                  0.2,1.8, &  ! 9
                 1.2,1.8, &  ! 10
                 1.8,1.8, &  ! 11
                 2.8,1.8, &  ! 12
                 0.2,2.8, &  ! 13
                  1.2,2.8, &  ! 14
                 1.8,2.8, &  ! 15
                 2.8,2.8 /)  ! 16
 

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
      elemCoords=(/0.7,0.7, & ! 1
                   1.5,0.7, & ! 2
                   2.3,0.7, & ! 3
                   0.7,1.5, & ! 4
                   1.5,1.5, & ! 5
                   2.3,1.5, & ! 6
                   0.7,2.3, & ! 7
                   1.5,2.3, & ! 8
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
     nodeCoords=(/0.2,0.2, & ! 1
                 1.2,0.2, &  ! 2
                 0.2,1.2, &  ! 5
                  1.2,1.2 /)  ! 6 

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
      elemCoords=(/0.7,0.7/)  ! 1

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
     nodeCoords=(/1.2,0.2, &  ! 2
                  1.8,0.2, &  ! 3
                  2.8,0.2, &  ! 4
                  1.2,1.2, &  ! 6
                  1.8,1.2, &  ! 7
                   2.8,1.2 /)  ! 8

 

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
      elemCoords=(/1.5,0.7, & ! 2
                    2.3,0.7/)  ! 3


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
     nodeCoords=(/0.2,1.2, &  ! 5
                  1.2,1.2, &  ! 6
                  1.8,1.2, &  ! 7
                  0.2,1.8, &  ! 9 
                  1.2,1.8, &  ! 10
                  1.8,1.8, &  ! 11
                  0.2,2.8, &  ! 13
                  1.2,2.8, &  ! 14
                  1.8,2.8/)  ! 15
  

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
      elemCoords=(/0.7,1.5, & ! 4
                    1.5,1.5, & ! 5
                   0.7,2.3, & ! 7
                   1.5,2.3/)  ! 8

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
     nodeCoords=(/1.8,1.2, &  ! 7
                  2.8,1.2, &  ! 8
                  1.8,1.8, &  ! 11
                  2.8,1.8, &  ! 12
                  1.8,2.8, &  ! 15
                   2.8,2.8 /)  ! 16
 

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
      elemCoords=(/2.3,1.5, & ! 6
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
        coordSys=ESMF_COORDSYS_SPH_DEG, &
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

end subroutine createTestMesh3x3_2



 subroutine test_MeshToMesh(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
   type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
   real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
   integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  real(ESMF_KIND_R8) :: lat, lon, phi, theta
  real(ESMF_KIND_R8),parameter :: &
                    DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
 
  real(ESMF_KIND_R8) :: errorTot, errorTotG
   
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
    integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn
  integer :: numOwnedElems
  real(ESMF_KIND_R8), pointer :: ownedElemCoords(:)


  ! result code
  integer :: finalrc

  ! Init to success
   rc=ESMF_SUCCESS
  itrp=.true.
  csrv=.true.

   ! get pet info
   call ESMF_VMGetGlobal(vm, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
 

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!! Setup Source !!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  ! Create Source Mesh
  call createTestMesh3x3(srcMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
   
   ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   ! Create source area field
    srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
    srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! Set interpolated function
  call ESMF_MeshGet(srcMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

    ! Set interpolated function
  call ESMF_MeshGet(srcMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! loop through and set field
  do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)

     ! Set source function
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)

     srcFarrayPtr(i1) = x+y+z
     !srcFarrayPtr(i1) = 1.0
  enddo


   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!! Setup Destination !!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create Destination Mesh
  call createTestMesh3x3_1(dstMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
         name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Set number of points in destination mesh
  call ESMF_MeshGet(dstMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

   ! Set exact destination field
   call ESMF_MeshGet(dstMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   ! loop through and set xfield
   do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)

     ! Set exact dest function
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)

     xdstFarrayPtr(i1) = x+y+z
     ! xdstFarrayPtr(i1) = 1.0
   enddo

   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)

#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
    !      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
           regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
! COMMENT THESE OUT UNTIL THAT PART IS WORKING
!          dstFracField=dstFracField, &
!          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Get areas
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

#if 0
  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)
!      dstmass = dstmass + dstFarrayPtr(i1)
!write(*,*) i1,"::", dstFarrayPtr(i1), dstAreaPtr(i1)


     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     !if (dstFracptr(i1) .lt. 0.999) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

#if 0  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  do i1=clbnd(1),cubnd(1)
!     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
     srcmass(1) = srcmass(1) + srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
    write(*,*) 
    write(*,*) "=== Second Order Conservative Mesh to Mesh ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) " "
  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

    call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! rc, itrp, csrv init to success above

 end subroutine test_MeshToMesh


 subroutine test_MeshToMeshWMasks(itrp, csrv, rc)
#undef ESMF_METHOD 
#define ESMF_METHOD "test_MeshToMeshWMasks"
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
   type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
   real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
   integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  real(ESMF_KIND_R8) :: lat, lon, phi, theta
  real(ESMF_KIND_R8),parameter :: &
                    DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
 
  real(ESMF_KIND_R8) :: errorTot, errorTotG, dstVal
   
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
    integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn
  integer :: numOwnedElems
  real(ESMF_KIND_R8), pointer :: ownedElemCoords(:)


 ! XMRKX

  ! result code
  integer :: finalrc

  ! Init to success
   rc=ESMF_SUCCESS
  itrp=.true.
  csrv=.true.

   ! get pet info
   call ESMF_VMGetGlobal(vm, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
 

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!! Setup Source !!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  ! Create Source Mesh
  call createTestMesh3x3(srcMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
   
   ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   ! Create source area field
    srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
    srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! Set interpolated function
  call ESMF_MeshGet(srcMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

    ! Set interpolated function
  call ESMF_MeshGet(srcMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! loop through and set field
  do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)

     ! Set source function
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)

     srcFarrayPtr(i1) = x+y+z
     !srcFarrayPtr(i1) = 1.0

     ! Set src to bad value to test masking
     ! (elem 3, is the only one with lon >2 and lat <1.0)
     if ((lon > 2.0) .and. (lat < 1.0)) then
        srcFarrayPtr(i1) = 1000.0
     endif
  enddo


   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!! Setup Destination !!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create Destination Mesh
  call createTestMesh3x3_1(dstMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
         name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Set number of points in destination mesh
  call ESMF_MeshGet(dstMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

   ! Set exact destination field
   call ESMF_MeshGet(dstMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   ! loop through and set xfield
   do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)

     ! Set exact dest function
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)

     xdstFarrayPtr(i1) = x+y+z
     ! xdstFarrayPtr(i1) = 1.0

     ! Init destination field to 0.0
     dstFarrayPtr(i1)=0.0
   
     ! Set dst and exact to bad value to test masking
     ! If masking works, they won't change, so they'll still match
     ! (elem 1, is the only one with both < 1.0)
     if ((lon < 1.0) .and. (lat < 1.0)) then
        xdstFarrayPtr(i1) = -1000.00
        dstFarrayPtr(i1) = -1000.00
     endif
   enddo

   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")
   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
    !      regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, &
       zeroregion=ESMF_REGION_SELECT, & ! So bad masked dest doesn't get overwritten
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! Only include in sum, if not masked 
     if (xdstFarrayPtr(i1) .gt. -999.0) then

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)
        !      dstmass = dstmass + dstFarrayPtr(i1)
        !write(*,*) i1,"::", dstFarrayPtr(i1), dstAreaPtr(i1)
     endif


     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     !if (dstFracptr(i1) .lt. 0.999) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     ! Since fraction isn't included in weights in this case, use it to modify dstField value, so 
     ! that it's correct for partial cells
     if (dstFracPtr(i1) .ne. 0.0) then
        dstVal=dstFarrayPtr(i1)/dstFracptr(i1)
     else 
        dstVal=dstFarrayPtr(i1)
     endif

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstVal - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        else
           error=ABS(dstVal - xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif
     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     ! Only include in sum, if not masked 
     ! (src masked values are set to 1000.0)
     if (srcFarrayPtr(i1) .lt. 999.0) then
        srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
     endif
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
    write(*,*) 
    write(*,*) "=== Second Order Conservative Mesh to Mesh with Masks ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) " "
  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

    call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! rc, itrp, csrv init to success above

 end subroutine test_MeshToMeshWMasks

#undef ESMF_METHOD 
#define ESMF_METHOD "test_GridToGrid"

 function calc_lat(i,imax,dy)
   integer :: i, imax
   real(ESMF_KIND_R8) :: calc_lat
   real(ESMF_KIND_R8) :: dy

        if (i .eq. 1) then
           calc_lat = -90.0
        else if (i .eq. imax) then
           calc_lat = 90.0
        else 
           calc_lat = REAL(i-1)*dy - 0.5*dy - 90.0
        endif
 end function calc_lat


 subroutine test_GridToGrid(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
!  Src_nx = 360
!  Src_ny = 180

  Src_nx = 20
  Src_ny = 10

  Src_dx = 360.0/Src_nx
  Src_dy = 180.0/Src_ny

!  Dst_nx = 144
!  Dst_ny = 72

  Dst_nx = 18
  Dst_ny = 12

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

!    write(*,*) "A Corner: x=[",clbnd(1),cubnd(1),"] y=[",clbnd(2),cubnd(2),"]"

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Src_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Src_dy - 90.0
        yp1= REAL(i2-1+1)*Src_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx + 0.5*Src_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! set src data 
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        !farrayPtr(i1,i2) = x+y+z+15.0
        !farrayPtr(i1,i2) = z+15.0

        ! set src data
        !farrayPtr(i1,i2) = 1.
        farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

     enddo
     enddo

  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Dst_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF
 
     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)


        y= REAL(i2-1)*Dst_dy - 90.0
        yp1= REAL(i2-1+1)*Dst_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set exact dst data
        !xfarrayPtr(i1,i2) = x+y+z+15.0
        !xfarrayPtr(i1,i2) = z+15.0

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

     enddo
     enddo


  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
       isSphere=.true., isLatLonDeg=.true., filename="dstGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
!          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  dstmass = 0.
  do lDE=0,localDECount-1

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! temporarily skip checking error close to poles
        lat = farrayPtrYC(i1,i2)
!        if ((lat .gt. 75.0) .or. (lat .lt. -75.0)) cycle

        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif

     enddo
     enddo

  enddo    ! lDE
  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo

  enddo    ! lDE

  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10) csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
    write(*,*) 
    write(*,*) "=== Second Order with Spherical Grids ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) " "
  endif

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="srcGrid", array1=srcArray,  &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
        filename="srcGridCnr", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="dstGrid", array1=dstArray, array2=errorArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
        filename="dstGridCnr", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_GridToGrid


      subroutine test_MasksAndUserArea(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount
  integer :: src_num, dst_num
  integer :: tmp(1), gtmp(1)

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 180
  Src_ny = 100

  Src_dx = 360.0/Src_nx
  Src_dy = 180.0/Src_ny

  Dst_nx = 100
  Dst_ny = 80

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Add Area
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_AREA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_AREA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! (Get memory and set coords for src)
  src_num=0
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Src_dy - 90.0
     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Src_dy - 90.0
        yp1= REAL(i2-1+1)*Src_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx + 0.5*Src_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! set src data 
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
       ! farrayPtr(i1,i2) = 1.
       farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

#if 1
        if ((lat>-45) .and. (lat<45)) then
           srcMask(i1,i2)=1
        else
           srcMask(i1,i2)=0
        endif
#else
           srcMask(i1,i2)=0
#endif      
  
        ! Set user area
        src_num=src_num+1
     enddo
     enddo
  enddo    ! lDE


  ! Sum number of src elements
  tmp(1)=src_num
  call ESMF_VMAllReduce(vm, tmp, gtmp, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
  src_num=gtmp(1)


  ! Set src areas 
  do lDE=0,localDECount-1

     !! Get area pointer
     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
          itemflag=ESMF_GRIDITEM_AREA, farrayPtr=srcAreaPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! Set area, so total area is 1.0
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
           
        ! Set user area
        srcAreaPtr(i1,i2)=1.0/REAL(src_num)
           
     enddo
     enddo
  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Get memory and set coords for dst
  dst_num=0
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Dst_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Dst_dy - 90.0
        yp1= REAL(i2-1+1)*Dst_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
       ! xfarrayPtr(i1,i2) = 1.0


#if 1
        if ((lon>-45) .and. (lon<45)) then
           dstMask(i1,i2)=1
        else 
           dstMask(i1,i2)=0
        endif
#else
           dstMask(i1,i2)=0
#endif

           dst_num=dst_num+1
     enddo
     enddo
  enddo    ! lDE


  ! Sum number of dst elements
  tmp(1)=dst_num
  call ESMF_VMAllReduce(vm, tmp, gtmp, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
  dst_num=gtmp(1)


  ! Set dst areas 
  do lDE=0,localDECount-1

     !! Get dst area
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_AREA, farrayPtr=dstAreaPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

    !! Set dst area so total is 1.0
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        dstAreaPtr(i1,i2)=1.0/REAL(dst_num)
     enddo
     enddo
  enddo    ! lDE

  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField, dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
!          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  dstmass = 0.
  do lDE=0,localDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
         
        ! skip if masked
        if (dstMask(i1,i2) .eq. 1) cycle

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)


        ! If this destination cell isn't covered by a sig. amount of source, then don't compute error on it.
        ! (Note that this is what SCRIP does)
        if (dstFracptr(i1,i2) .lt. 0.999) cycle

        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2)/dstFracPtr(i1,i2)) &
                                       - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2)/dstFracPtr(i1,i2)) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif

     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo

  enddo    ! lDE


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
     write(*,*) 
     write(*,*) "=== Spherical grids with masks and user area ==="
     write(*,*) "Conservation:"
     write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
     write(*,*) "SRC mass = ", srcmassg(1)
     write(*,*) "DST mass = ", dstmassg(1)
     write(*,*) " "
     write(*,*) "Interpolation:"
     write(*,*) "Max Error = ", maxerrorg(1)
     write(*,*) "Min Error = ", minerrorg(1)
     write(*,*) " "
  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_MasksAndUserArea

#undef ESMF_METHOD 
#define ESMF_METHOD "test_CSGridToGrid"
 subroutine test_CSGridToGrid(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, i
  integer ::  srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: src_tile_size
  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  src_tile_size=20

  Dst_nx = 100
  Dst_ny = 80

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! setup source cubed sphere grid
  srcGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srcLocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridGet(dstGrid, localDECount=dstLocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct analytic field
  do lDE=0,srcLocalDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get src coordinates
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        !farrayPtr(i1,i2) = x+y+z+15.0
        !farrayPtr(i1,i2) = z+15.0

        ! set src data
        !farrayPtr(i1,i2) = 1.
        farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)*sin(phi)

     enddo
     enddo

  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstLocalDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = calc_lat(i2,dst_ny+1,dst_dy)

     enddo
     enddo


    !! DO CENTER STAGGER STUFF
 
     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= calc_lat(i2,dst_ny+1,dst_dy)
        yp1= calc_lat(i2+1,dst_ny+1,dst_dy)

        ! Set source coordinates
!        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 1.0*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set exact dst data
        !xfarrayPtr(i1,i2) = x+y+z+15.0
        !xfarrayPtr(i1,i2) = z+15.0

        ! set exact dst data
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)*sin(phi)

     enddo
     enddo


  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
        filename="srcGridCnrb4", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#endif


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
!          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          ignoreDegenerate=.true., &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  dstmass = 0.
  do lDE=0, dstLocalDECount-1

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! temporarily skip checking error close to poles
        lat = farrayPtrYC(i1,i2)
!        if ((lat .gt. 75.0) .or. (lat .lt. -75.0)) cycle

        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif

!        write(*,*) i1,i2,xfarrayPtr(i1,i2),farrayPtr(i1,i2)

     enddo
     enddo

  enddo    ! lDE

  ! Compute src mask
  srcmass(1) = 0.
  do lDE=0, srclocalDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcAreaptr(i1,i2)*farrayPtr(i1,i2)
!write(*,*) i1,i2,srcAreaptr(i1,i2),farrayPtr(i1,i2)

     enddo
     enddo

  enddo    ! lDE

  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10) csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
     write(*,*) 
     write(*,*) "=== Second Order with Cubed Sphere Grids ==="
     write(*,*) "Conservation:"
     write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
     write(*,*) "SRC mass = ", srcmassg(1)
     write(*,*) "DST mass = ", dstmassg(1)
     write(*,*) " "
     write(*,*) "Interpolation:"
     write(*,*) "Max Error = ", maxerrorg(1)
     write(*,*) "Min Error = ", minerrorg(1)
     write(*,*) " "
  endif

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="srcGrid", array1=srcArray,  &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
        filename="srcGridCnr", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="dstGrid", array1=dstArray, array2=errorArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
        filename="dstGridCnr", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_CSGridToGrid

#undef ESMF_METHOD 
#define ESMF_METHOD "test_CSGridToGridWMasks"
 subroutine test_CSGridToGridWMasks(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, i
  integer ::  srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: src_tile_size
  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  src_tile_size=20

  Dst_nx = 100
  Dst_ny = 80

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! setup source cubed sphere grid
  srcGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srcLocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridGet(dstGrid, localDECount=dstLocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct analytic field
  do lDE=0,srcLocalDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get src coordinates
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        !farrayPtr(i1,i2) = x+y+z+15.0
        !farrayPtr(i1,i2) = z+15.0

        ! set src data
        !farrayPtr(i1,i2) = 1.
        farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)*sin(phi)

     enddo
     enddo

  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstLocalDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = calc_lat(i2,dst_ny+1,dst_dy)

     enddo
     enddo


    !! DO CENTER STAGGER STUFF
 
     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= calc_lat(i2,dst_ny+1,dst_dy)
        yp1= calc_lat(i2+1,dst_ny+1,dst_dy)

        ! Set source coordinates
!        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 1.0*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set exact dst data
        !xfarrayPtr(i1,i2) = x+y+z+15.0
        !xfarrayPtr(i1,i2) = z+15.0

        ! set exact dst data
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)*sin(phi)

     enddo
     enddo


  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
        filename="srcGridCnrb4", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#endif


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
!          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          ignoreDegenerate=.true., &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  dstmass = 0.
  do lDE=0, dstLocalDECount-1

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! temporarily skip checking error close to poles
        lat = farrayPtrYC(i1,i2)
!        if ((lat .gt. 75.0) .or. (lat .lt. -75.0)) cycle

        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif

!        write(*,*) i1,i2,xfarrayPtr(i1,i2),farrayPtr(i1,i2)

     enddo
     enddo

  enddo    ! lDE

  ! Compute src mask
  srcmass(1) = 0.
  do lDE=0, srclocalDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcAreaptr(i1,i2)*farrayPtr(i1,i2)
!write(*,*) i1,i2,srcAreaptr(i1,i2),farrayPtr(i1,i2)

     enddo
     enddo

  enddo    ! lDE

  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10) csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
     write(*,*) 
     write(*,*) "=== Second Order with Cubed Sphere Grids ==="
     write(*,*) "Conservation:"
     write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
     write(*,*) "SRC mass = ", srcmassg(1)
     write(*,*) "DST mass = ", dstmassg(1)
     write(*,*) " "
     write(*,*) "Interpolation:"
     write(*,*) "Max Error = ", maxerrorg(1)
     write(*,*) "Min Error = ", minerrorg(1)
     write(*,*) " "
  endif

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="srcGrid", array1=srcArray,  &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
        filename="srcGridCnr", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="dstGrid", array1=dstArray, array2=errorArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CORNER, &
        filename="dstGridCnr", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

 end subroutine test_CSGridToGridWMasks



 subroutine test_PerLocStatus(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField, regridStatusField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer(ESMF_KIND_I4), pointer :: statusPtr(:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  real(ESMF_KIND_R8) :: dx,dy
   
  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

  ! Setup Src Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
      nodeIds=(/1,2,3,4,5,6,7,8,9/) 

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
                   1.0, 2.1, & ! node id 8
                   2.1, 2.1 /) ! node id 9

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


     ! Allocate and fill the node mask array.
     ! Mask out elem 5
     allocate(elemMask(numTotElems))
     elemMask=(/0, & ! elem id 1
                0, & ! elem id 2
                0, & ! elem id 3
                0, & ! elem id 4
                1/)  ! elem id 5


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


       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
       numTriElems=0
       numTotElems=numQuadElems+numTriElems
 
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the elem mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0/)  ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
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

        ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
       numTotElems=numQuadElems+numTriElems
 
       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the elem mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0,  & ! elem id 2
                  0/)   ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
         ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      1.0,2.1 /) ! node id 8

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                      2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
         allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the elem mask array.
         allocate(elemMask(numTotElems))
        elemMask=(/0/)  ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 
 
        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 8
                     2.1,2.1 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems
   
        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the elem mask array.
        allocate(elemMask(numTotElems))
        elemMask=(/1/)  ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
         elemConn=(/1,2,4,3/) ! elem id 5
        endif
    endif

    ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementMask=elemMask, &
         elementIds=elemIds, elementTypes=elemTypes, &
         elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

   ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        meshLoc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)
    deallocate(elemMask)


  ! Create Dest Mesh
  if (petCount .eq. 1) then
      ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/-0.5,-0.5, & ! node id 1  Put outside the src grid
                   1.0,0.0, & ! node id 2
                   2.0,0.0, & ! node id 3
                   0.0,1.0, & ! node id 4
                   1.2,1.2, & ! node id 5
                   2.0,1.1, & ! node id 6
                   0.0,2.0, & ! node id 7
                    1.1,2.0, & ! node id 8
                   2.0,2.0 /) ! node id 9

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

     ! Allocate and fill the node mask array.
     ! Mask out node 9
      allocate(elemMask(numTotElems))
     elemMask=(/0, & ! elem id 1
                0, & ! elem id 2
                2, & ! elem id 3
                0, & ! elem id 4
                0/)  ! elem id 5

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
        nodeCoords=(/-0.5,-0.5, & ! node id 1 Put outside src grid
                     1.0, 0.0, & ! node id 2
                     0.0, 1.0, & ! node id 4
                     1.2, 1.2 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5


       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
        numTriElems=0
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the elem mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0/)  ! elem id 1

       ! Allocate and fill the element connection type array.
         ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
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
       nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.2,1.2, & ! node id 5
                    2.0,1.1 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6


       ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
       numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the elem mask array.
       allocate(elemMask(numTotElems))
       elemMask=(/0,  & ! elem id 2
                  2/)   ! elem id 3

        ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
         nodeCoords=(/0.0,1.0, & ! node id 4
                     1.2,1.2, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.1,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
         allocate(elemIds(numTotElems))
        elemIds=(/4/) 

         ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the elem mask array.
        allocate(elemMask(numTotElems))
        elemMask=(/0/)  ! elem id 4

         ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
         ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.2,1.2, &  ! node id 5
                     2.0,1.1, &  ! node id 6
                     1.1,2.0, &  ! node id 8
                      2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  
 
        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the elem mask array.
        allocate(elemMask(numTotElems))
         elemMask=(/0/)  ! elem id 5

         ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
   endif


  ! Create Mesh structure in 1 step
  dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementMask=elemMask, &
         elementIds=elemIds, elementTypes=elemTypes, &
         elementConn=elemConn, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
     endif


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemMask)
   deallocate(elemConn)


  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
        meshLoc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create regrid status field
  regridStatusField=ESMF_FieldCreate(dstMesh, ESMF_TYPEKIND_I4, &
                                     meshLoc=ESMF_MESHLOC_ELEMENT, &
                                     name="regrid status", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
    return
  endif


  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          dstMaskValues=(/2/), &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
          dstStatusField=regridStatusField, &
          unmappedAction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(regridStatusField, 0, statusPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  !  write(*,*) localPet," Status Field=",statusPtr

  ! Check status
  correct=.true.
  if (PetCount .eq. 1) then
     if (statusPtr(1) .ne. ESMF_REGRIDSTATUS_SMSK_OUT_MP) correct=.false.
     if (statusPtr(2) .ne. ESMF_REGRIDSTATUS_SMSK_MP) correct=.false.
     if (statusPtr(3) .ne. ESMF_REGRIDSTATUS_DSTMASKED) correct=.false.
     if (statusPtr(4) .ne. ESMF_REGRIDSTATUS_SMSK_MP) correct=.false.
     if (statusPtr(5) .ne. ESMF_REGRIDSTATUS_SRCMASKED) correct=.false.
 else if (petCount .eq. 4) then
    if (localPET .eq. 0) then !!! This part only for PET 0

       ! Check status for  elemIds=(/1/) 
       if (statusPtr(1) .ne. ESMF_REGRIDSTATUS_SMSK_OUT_MP) correct=.false.

    else if (localPET .eq. 1) then !!! This part only for PET 1

       ! Check status for elemIds=(/2,3/) 
       if (statusPtr(1) .ne. ESMF_REGRIDSTATUS_SMSK_MP) correct=.false.
       if (statusPtr(2) .ne. ESMF_REGRIDSTATUS_DSTMASKED) correct=.false.

    else if (localPET .eq. 2) then !!! This part only for PET 2

       ! Check status for elemIds=(/4/) 
       if (statusPtr(1) .ne. ESMF_REGRIDSTATUS_SMSK_MP) correct=.false.

    else if (localPET .eq. 3) then !!! This part only for PET 3

       ! Check status for elemIds=(/5/) 
       if (statusPtr(1) .ne. ESMF_REGRIDSTATUS_SRCMASKED) correct=.false.
        
    endif
 endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(regridStatusField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the Meshes
  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_PerLocStatus 


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !                     Mesh Ids
  !
  !   2.8   13 ------ 14 ------- 15 ------- 16
  !         |         |          |  10    / |
  !   2.3   |    7    |    8M    |     /    |
  !         |         |          |  /    9  |
  !   1.8   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !   1.5   |    4M   |    5M    |    6     |
  !         |         |          |          |
  !   1.2   5 ------- 6 -------- 7 -------- 8
  !         |         |          |          |
  !   0.7   |    1    |    2     |    3     |
  !         |         |          |          |
  !   0.2   1 ------- 2 -------- 3 -------- 4
  !            
  !        0.2  0.7  1.2  1.5   1.8  2.3   2.8
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
  !   2.8   2 ------- 2 -------- 3 -------- 3
  !         |         |          |  3    /  |
  !         |    2    |    2     |     /    |
  !         |         |          |  /    3  |
  !   1.8   2 ------- 2 -------- 3 -------- 3
  !         |         |          |          |
  !         |    2    |    2     |    3     |
  !         |         |          |          |
  !   1.2   0 ------- 0 -------- 1 -------- 1
  !         |         |          |          |
  !         |    0    |    1     |    1     |
  !         |         |          |          |
  !   0.2   0 ------- 0 -------- 1 -------- 1
  !
  !        0.2       1.2        1.8        2.8
  !
  !               Node Owners at corners
  !              Element Owners in centers
  ! 

subroutine createTestMesh3x3_snn_1(mesh, rc)
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
  integer, pointer :: elemMask(:)
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
     nodeCoords=(/0.2,0.2, & ! 1
                 1.2,0.2, &  ! 2
                 1.8,0.2, &  ! 3
                 2.8,0.2, &  ! 4
                 0.2,1.2, &  ! 5
                 1.2,1.2, &  ! 6
                 1.8,1.2, &  ! 7
                 2.8,1.2, &  ! 8
                  0.2,1.8, &  ! 9
                 1.2,1.8, &  ! 10
                 1.8,1.8, &  ! 11
                 2.8,1.8, &  ! 12
                 0.2,2.8, &  ! 13
                  1.2,2.8, &  ! 14
                 1.8,2.8, &  ! 15
                 2.8,2.8 /)  ! 16
 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0,1,1,0,0,1,0,0/) 

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
      elemCoords=(/0.7,0.7, & ! 1
                   1.5,0.7, & ! 2
                   2.3,0.7, & ! 3
                   0.7,1.5, & ! 4
                   1.5,1.5, & ! 5
                   2.3,1.5, & ! 6
                   0.7,2.3, & ! 7
                   1.5,2.3, & ! 8
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
     nodeCoords=(/0.2,0.2, & ! 1
                 1.2,0.2, &  ! 2
                 0.2,1.2, &  ! 5
                  1.2,1.2 /)  ! 6 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0/) 


      !! elem types
      allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! 1

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.7,0.7/)  ! 1

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
     nodeCoords=(/1.2,0.2, &  ! 2
                  1.8,0.2, &  ! 3
                  2.8,0.2, &  ! 4
                  1.2,1.2, &  ! 6
                  1.8,1.2, &  ! 7
                   2.8,1.2 /)  ! 8

 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 2
                  ESMF_MESHELEMTYPE_QUAD/)  ! 3

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/1.5,0.7, & ! 2
                    2.3,0.7/)  ! 3


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
     nodeCoords=(/0.2,1.2, &  ! 5
                  1.2,1.2, &  ! 6
                  1.8,1.2, &  ! 7
                  0.2,1.8, &  ! 9 
                  1.2,1.8, &  ! 10
                  1.8,1.8, &  ! 11
                  0.2,2.8, &  ! 13
                  1.2,2.8, &  ! 14
                  1.8,2.8/)  ! 15
  

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,0,1/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 4
                  ESMF_MESHELEMTYPE_QUAD, & ! 5
                  ESMF_MESHELEMTYPE_QUAD, & ! 7
                  ESMF_MESHELEMTYPE_QUAD/)  ! 8
 

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/0.7,1.5, & ! 4
                    1.5,1.5, & ! 5
                   0.7,2.3, & ! 7
                   1.5,2.3/)  ! 8

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
     nodeCoords=(/1.8,1.2, &  ! 7
                  2.8,1.2, &  ! 8
                  1.8,1.8, &  ! 11
                  2.8,1.8, &  ! 12
                  1.8,2.8, &  ! 15
                   2.8,2.8 /)  ! 16
 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0/) 

      !! elem types
      allocate(elemTypes(numElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! 6
                  ESMF_MESHELEMTYPE_TRI,  & ! 9
                  ESMF_MESHELEMTYPE_TRI/)   ! 10

      !! elem coords
      allocate(elemCoords(2*numElems))
      elemCoords=(/2.3,1.5, & ! 6
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
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
         elementCoords=elemCoords,  elementMask=elemMask,&
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
   deallocate(elemMask)

end subroutine createTestMesh3x3_snn_1

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
  !   2.5   |    7    |    8M    |     /    |
  !         |         |          |  /    9  |
  !   2.0   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !   1.5   |    4M   |    5M    |    6     |
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

subroutine createTestMesh3x3_snn_2(mesh, rc)
           
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
  integer, pointer :: elemMask(:)
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0,1,1,0,0,1,0,0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,1,0,1/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0/) 

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
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
         elementCoords=elemCoords, elementMask=elemMask,&
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
   deallocate(elemMask)

end subroutine createTestMesh3x3_snn_2

 subroutine test_SmallNumNbrs(itrp, csrv, rc)
#undef ESMF_METHOD 
#define ESMF_METHOD "test_MeshToMeshWMasks"
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
   type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
   real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
   integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  real(ESMF_KIND_R8) :: lat, lon, phi, theta
  real(ESMF_KIND_R8),parameter :: &
                    DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
 
  real(ESMF_KIND_R8) :: errorTot, errorTotG, dstVal

  integer :: num_errorTot
  real(ESMF_KIND_R8) :: l_errorTot(1),g_errorTot(1)
  integer :: l_num_errorTot(1), g_num_errorTot(1)
   
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
    integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn
  integer :: numOwnedElems
  real(ESMF_KIND_R8), pointer :: ownedElemCoords(:)


 ! XMRKX

  ! result code
  integer :: finalrc

  ! Init to success
   rc=ESMF_SUCCESS
  itrp=.true.
  csrv=.true.

   ! get pet info
   call ESMF_VMGetGlobal(vm, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
 

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!! Setup Source !!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  ! Create Source Mesh
  call createTestMesh3x3_snn_1(srcMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
   
   ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   ! Create source area field
    srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
    srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! Set interpolated function
  call ESMF_MeshGet(srcMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

    ! Set interpolated function
  call ESMF_MeshGet(srcMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! loop through and set field
  do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)

     ! Set source function
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)

     srcFarrayPtr(i1) = x+y+z
     !srcFarrayPtr(i1) = 1.0

     ! Set src to bad value to test masking
     ! (elem 3, is the only one with lon >2 and lat <1.0)
!     if ((lon > 2.0) .and. (lat < 1.0)) then
!        srcFarrayPtr(i1) = 1000.0
!     endif
  enddo


   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!! Setup Destination !!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create Destination Mesh
  call createTestMesh3x3_snn_2(dstMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
         name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Set number of points in destination mesh
  call ESMF_MeshGet(dstMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

   ! Set exact destination field
   call ESMF_MeshGet(dstMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   ! loop through and set xfield
   do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)

     ! Set exact dest function
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)

     xdstFarrayPtr(i1) = x+y+z
     ! xdstFarrayPtr(i1) = 1.0

     ! Init destination field to 0.0
     dstFarrayPtr(i1)=0.0
   
     ! Set dst and exact to bad value to test masking
     ! If masking works, they won't change, so they'll still match
     ! (elem 1, is the only one with both < 1.0)
 !    if ((lon < 1.0) .and. (lat < 1.0)) then
 !       xdstFarrayPtr(i1) = -1000.00
 !       dstFarrayPtr(i1) = -1000.00
 !    endif
   enddo

   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")
   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  num_errorTot=0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)


     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then don't compute error on it.
     if (dstFracPtr(i1) .lt. 0.1) cycle

     ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     ! Since fraction isn't included in weights in this case, use it to modify dstField value, so 
     ! that it's correct for partial cells
     if (dstFracPtr(i1) .ne. 0.0) then
        dstVal=dstFarrayPtr(i1)/dstFracptr(i1)
     else 
        dstVal=dstFarrayPtr(i1)
     endif

     if (xdstFarrayPtr(i1) .ne. 0.0) then
        error=ABS(dstVal - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
     else
        error=ABS(dstVal - xdstFarrayPtr(i1))
     endif

     ! total error 
     errorTot=errorTot+error
     num_errorTot=num_errorTot+1           

     ! min max error
     if (error > maxerror(1)) then
        maxerror(1) = error
     endif
     if (error < minerror(1)) then
        minerror(1) = error
     endif

  enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
        srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
  
  l_errorTot(1)=errorTot
  call ESMF_VMAllReduce(vm, l_errorTot, g_errorTot, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  l_num_errorTot(1)=num_errorTot
  call ESMF_VMAllReduce(vm, l_num_errorTot, g_num_errorTot, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
    write(*,*) 
    write(*,*) "=== Second Order Conservative Mesh to Mesh with Masks ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", g_errorTot(1)/g_num_errorTot(1)
    write(*,*) " "
  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

    call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! rc, itrp, csrv init to success above

 end subroutine test_SmallNumNbrs


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

subroutine createTestMesh3x3Cart_1(mesh, rc)
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
  integer, pointer :: elemMask(:)
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,1,0,0,0,0,0,0,0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,1/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0,0/) 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0/) 

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
         elementCoords=elemCoords, elementMask=elemMask,&
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

end subroutine createTestMesh3x3Cart_1



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

subroutine createTestMesh3x3Cart_2(mesh, rc)
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
  integer, pointer :: elemMask(:)
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
                 1.25,1.25, &  ! 6
                 1.75,1.25, &  ! 7
                 3.0,1.0, &  ! 8
                  0.0,2.0, &  ! 9
                 1.25,1.75, &  ! 10
                 1.75,1.75, &  ! 11
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1,0,0,0,0,0,0,0,0,0/) 

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
                  1.25,1.25 /)  ! 6 

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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/1/) 

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
                  1.25,1.25, &  ! 6
                  1.75,1.25, &  ! 7
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0/) 

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
                  1.25,1.25, &  ! 6
                  1.75,1.25, &  ! 7
                  0.0,2.0, &  ! 9 
                  1.25,1.75, &  ! 10
                  1.75,1.75, &  ! 11
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0,0/) 

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
     nodeCoords=(/1.75,1.25, &  ! 7
                  3.0,1.0, &  ! 8
                  1.75,1.75, &  ! 11
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

      !! elem mask
      allocate(elemMask(numElems))
      elemMask=(/0,0,0/)  

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
         elementCoords=elemCoords, elementMask=elemMask,&
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemMask)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemConn)

end subroutine createTestMesh3x3Cart_2


 subroutine test_MeshToMeshCart(itrp, csrv, rc)
#undef ESMF_METHOD 
#define ESMF_METHOD "test_MeshToMeshCart"
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
   type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
   real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
   integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  real(ESMF_KIND_R8) :: lat, lon, phi, theta
  real(ESMF_KIND_R8),parameter :: &
                    DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
 
  real(ESMF_KIND_R8) :: errorTot, errorTotG
   
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
    integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn
  integer :: numOwnedElems
  real(ESMF_KIND_R8), pointer :: ownedElemCoords(:)


  ! result code
  integer :: finalrc

  ! Init to success
   rc=ESMF_SUCCESS
  itrp=.true.
  csrv=.true.

   ! get pet info
   call ESMF_VMGetGlobal(vm, rc=localrc)
         if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
 

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!! Setup Source !!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  ! Create Source Mesh
  call createTestMesh3x3Cart_1(srcMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
   
   ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   ! Create source area field
    srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
    srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! Set interpolated function
  call ESMF_MeshGet(srcMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

    ! Set interpolated function
  call ESMF_MeshGet(srcMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! loop through and set field
  do i1=1,numOwnedElems

      ! Get coords
     x=ownedElemCoords(2*i1-1)
     y=ownedElemCoords(2*i1)

     srcFarrayPtr(i1) = x+y+2.0
  enddo


   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!! Setup Destination !!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create Destination Mesh
  call createTestMesh3x3Cart_2(dstMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
         name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Set number of points in destination mesh
  call ESMF_MeshGet(dstMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

   ! Set exact destination field
   call ESMF_MeshGet(dstMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   ! loop through and set xfield
   do i1=1,numOwnedElems

      ! Get coords
     x=ownedElemCoords(2*i1-1)
     y=ownedElemCoords(2*i1)

     xdstFarrayPtr(i1) = x+y+2.0
   enddo

   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)

#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")
   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
!           regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
           regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
! COMMENT THESE OUT UNTIL THAT PART IS WORKING
!          dstFracField=dstFracField, &
!          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

#if 0
  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  ! destination grid
  !! check relative error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)
!      dstmass = dstmass + dstFarrayPtr(i1)
!write(*,*) i1,"::", dstFarrayPtr(i1), dstAreaPtr(i1)


     ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
     ! (Note that this is what SCRIP does)
     !if (dstFracptr(i1) .lt. 0.999) cycle

    ! write(*,*) i1,"::",dstFarrayPtr(i1),xdstFarrayPtr(i1)

     if (xdstFarrayPtr(i1) .ne. 0.0) then
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        else
           error=ABS(dstFarrayPtr(i1) - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
           errorTot=errorTot+error
           if (error > maxerror(1)) then
             maxerror(1) = error
           endif
           if (error < minerror(1)) then
             minerror(1) = error
           endif
        endif

  !   write(*,*) i1,":: error=",error," r=",dstFarrayPtr(i1)," e=",xdstFarrayPtr(i1)

     enddo


  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

#if 0  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  do i1=clbnd(1),cubnd(1)
!     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
     srcmass(1) = srcmass(1) + srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
    write(*,*) 
    write(*,*) "=== Second Order Conservative Cart. Mesh to Mesh ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) " "
  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

    call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! rc, itrp, csrv init to success above

 end subroutine test_MeshToMeshCart


 subroutine test_RegridSph4ConcaveMesh(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
  integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn
  real(ESMF_KIND_R8) :: dstVal

  ! result code
  integer :: finalrc

  ! Init to success
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

 ! XMRKX

!!!! Setup source mesh !!!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   20   7 ------- 8 
  !         |         |\
  !   13    |    3    |  9 
  !         |         |4    \
  !   10   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       10  13     20 
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
  !   20  2 ------- 2 
  !        |         |\
  !   12   |    2    |  3 
  !        |         |3   \
  !   10   0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0       10  12      20 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

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
                 12.0,12.0 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
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
                    10.0,0.0, &
                    0.0,10.0, &
                    10.0,10.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/10.0,10.0, &
                    20.0,10.0, &
                    10.0,20.0, &
                    12.0,12.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif


   ! Create Mesh structure in 1 step
  srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        rc=rc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
   srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
   srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1)
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))

     ! Set source function
     srcFarrayPtr(i1) = 20.0+x+y
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

  !!!!!!!!!!!!!!! Setup Destination Mesh !!!!!!!!!!!!!!!!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   20.0   7 ------- 8 
  !         |         |\
  !   14.0  |    3    |  9 
  !         |         |4    \
  !   10.0  4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0      10.0 14.0   20.0 
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
  !   20.0 2 ------- 2 
  !        |         |\
  !   14.0 |    2    |  3 
  !        |         |3   \
  !   10.0 0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0      10.0 14.0    20.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

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
                 14.0,14.0 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
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
                    10.0,0.0, &
                    0.0,10.0, &
                    10.0,10.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,0,0,0/) ! everything on proc 0

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 1) then
        ! Fill in node data
        numNodes=4

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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)
     else if (localPet .eq. 2) then
        ! Fill in node data
        numNodes=4

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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     else 
        ! Fill in node data
        numNodes=4

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/5,6,8,9/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/10.0,10.0, &
                    20.0,10.0, &
                    10.0,20.0, &
                    14.0,14.0/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif


   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     xdstFarrayPtr(i1) = 20.0+x+y
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif



  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! destination grid
  !! compute error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then don't compute error on it.
     ! (Note that this is what SCRIP does)
     if (dstFracptr(i1) .lt. 0.1) cycle

     ! Compute normalized destination value
     if (dstFracptr(i1) .ne. 0.0) then
        dstVal=dstFarrayPtr(i1)/dstFracPtr(i1)
     else
        dstVal=0.0
     endif     

     ! Compute error
     if (xdstFarrayPtr(i1) .ne. 0.0) then
        error=ABS(dstVal - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
     else
        error=ABS(dstVal - xdstFarrayPtr(i1))
     endif

     ! Compute error statistics
     errorTot=errorTot+error
     if (error > maxerror(1)) then
        maxerror(1) = error
     endif
     if (error < minerror(1)) then
        minerror(1) = error
     endif
     
  enddo

  ! Init src mass 
  srcmass(1) = 0.


  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== 2nd order with sph concave quads ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) " "
  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return success if we've gotten this far
    rc=ESMF_SUCCESS

 end subroutine test_RegridSph4ConcaveMesh


 subroutine test_RegridCart4ConcaveMesh(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: srcAreaField, dstAreaField
  type(ESMF_Field) :: srcFracField, dstFracField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: srcFarrayPtr(:), dstFarrayPtr(:), xdstFarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: srcAreaPtr(:), dstAreaPtr(:)
 real(ESMF_KIND_R8), pointer :: srcFracPtr(:), dstFracPtr(:)
  integer :: clbnd(1),cubnd(1)
  integer :: i1,i2,i3
  real(ESMF_KIND_R8) :: x,y,z
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  real(ESMF_KIND_R8) :: errorTot, errorTotG
  
  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:),elemMask(:)
  integer :: numNodes
  integer :: iconn,inode
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn
  real(ESMF_KIND_R8) :: dstVal

  ! result code
  integer :: finalrc

  ! Init to success
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

 ! XMRKX

!!!! Setup source mesh !!!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   2.0   7 ------- 8 
  !         |         |\
  !   1.3   |    3    |  9 
  !         |         |4    \
  !   1.0   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       1.0 1.3   2.0 
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
  !   2.0  2 ------- 2 
  !        |         |\
  !   1.3  |    2    |  3 
  !        |         |3   \
  !   1.0  0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0       1.0 1.3    2.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

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
                 1.3,1.3 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
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
                    1.3,1.3/)
 
       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif


   ! Create Mesh structure in 1 step
  srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
       coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        rc=rc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Array spec for fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source area field
    srcAreaField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_area", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source frac field
   srcFracField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source_frac", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1)
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))

     ! Set source function
     srcFarrayPtr(i1) = 20.0+x+y
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)

  !!!!!!!!!!!!!!! Setup Destination Mesh !!!!!!!!!!!!!!!!!

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   2.0   7 ------- 8 
  !         |         |\
  !   1.4   |    3    |  9 
  !         |         |4    \
  !   1.0   4 ------- 5 -------- 6
  !         |         |          |
  !         |    1    |    2     |
  !         |         |          |
  !   0.0   1 ------- 2 -------- 3
  !
  !        0.0       1.0 1.4   2.0 
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
  !   2.0  2 ------- 2 
  !        |         |\
  !   1.4  |    2    |  3 
  !        |         |3   \
  !   1.0  0 ------- 0 -------- 1
  !        |         |          |
  !        |    0    |    1     |
  !        |         |          |
  !   0.0  0 ------- 0 -------- 1
  !
  !       0.0       1.0 1.4    2.0 
  !
  !      Node Owners at corners
  !      Element Owners in centers
  ! 

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
                 1.4,1.4 /)

      !! node owners
      allocate(nodeOwners(numNodes))
      nodeOwners=0 ! everything on proc 0


      ! Fill in elem data
      numTotElems=4

      !! elem ids
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,3,4/) 

      !! elem types
      allocate(elemTypes(numTotElems))
      elemTypes=ESMF_MESHELEMTYPE_QUAD

      !! elem conn
      allocate(elemConn(numTotElems*4))
      elemConn=(/1,2,5,4, & 
                 2,3,6,5, & 
                 4,5,8,7, & 
                 5,6,9,8/)

   else if (petCount .eq. 4) then
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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/2/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
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
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/3/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
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
                    1.4,1.4/)

       !! node owners
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0,1,2,3/) 

       ! Fill in elem data
       numTotElems=1

       !! elem ids
       allocate(elemIds(numTotElems))
       elemIds=(/4/) 

       !! elem type
       allocate(elemTypes(numTotElems))
       elemTypes=ESMF_MESHELEMTYPE_QUAD

       !! elem conn
       allocate(elemConn(numTotElems*4))
       elemConn=(/1,2,4,3/)  
     endif
   endif


   ! Create Mesh structure in 1 step
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Array spec
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
   
   
   ! Create dest. field
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Create dest. area field
   dstAreaField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_area", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Create dest. frac field
   dstFracField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="dest_frac", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   

   ! Create exact dest. field
   xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
        name="xdest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   ! Init destination field to 0.0
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, dstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
   
   ! Init destination field to 0.0
   dstFarrayPtr=0.0
   
   
   ! Init exact destination field
   ! Should only be 1 localDE
   call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! set interpolated function
  iconn=1
  do i1=1,numTotElems

     ! Loop through nodes in elem
     ! to compute point in center
     x=0.0
     y=0.0
     do i2=1,elemTypes(i1) 
        inode=2*(elemConn(iconn)-1)
        x=x+nodeCoords(inode+1)
        y=y+nodeCoords(inode+2)
        
        iconn=iconn+1
     enddo
     x=x*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))
     y=y*(1.0/REAL(elemTypes(i1),ESMF_KIND_R8))


     ! Set source function
     xdstFarrayPtr(i1) = 20.0+x+y
  enddo

 
   ! For now, Easy set interpolated function
   !xdstFarrayPtr=1.0


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif



  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstAreaField, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  errorTot=0.0
  dstmass = 0.

  ! get dst Field
  call ESMF_FieldGet(dstField, 0, dstFarrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get exact destination Field
  call ESMF_FieldGet(xdstField, 0, xdstFarrayPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! get dst area Field
  call ESMF_FieldGet(dstAreaField, 0, dstAreaPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! get frac Field
  call ESMF_FieldGet(dstFracField, 0, dstFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! destination grid
  !! compute error
  do i1=clbnd(1),cubnd(1)

     ! This is WRONG, shouldn't include Frac
     ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1)*fptr(i1)
     
     ! Instead do this
     dstmass = dstmass + dstAreaptr(i1)*dstFarrayPtr(i1)

     ! If this destination cell isn't covered by a sig. amount of source, then don't compute error on it.
     ! (Note that this is what SCRIP does)
     if (dstFracptr(i1) .lt. 0.1) cycle

     ! Compute normalized destination value
     if (dstFracptr(i1) .ne. 0.0) then
        dstVal=dstFarrayPtr(i1)/dstFracPtr(i1)
     else
        dstVal=0.0
     endif     

     ! Compute error
     if (xdstFarrayPtr(i1) .ne. 0.0) then
        error=ABS(dstVal - xdstFarrayPtr(i1))/ABS(xdstFarrayPtr(i1))
     else
        error=ABS(dstVal - xdstFarrayPtr(i1))
     endif

     ! Compute error statistics
     errorTot=errorTot+error
     if (error > maxerror(1)) then
        maxerror(1) = error
     endif
     if (error < minerror(1)) then
        minerror(1) = error
     endif
     
  enddo

  ! Init src mass 
  srcmass(1) = 0.

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, computationalLBound=clbnd, &
       computationalUBound=cubnd,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

     ! get src Field
  call ESMF_FieldGet(srcAreaField, 0, srcAreaptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  do i1=clbnd(1),cubnd(1)
     srcmass(1) = srcmass(1) + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== 2nd order with Cart. concave quads ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) " "
  endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstAreaField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the meshes
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! return success if we've gotten this far
    rc=ESMF_SUCCESS

 end subroutine test_RegridCart4ConcaveMesh

      subroutine test_cartcsrvregridwmasks(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),xfptr(:,:),errorfptr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: x,y
  real(ESMF_KIND_R8) :: cnr_x,cnr_xp1,cnr_y,cnr_yp1
  real(ESMF_KIND_R8) :: Src_dx, Src_dy
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
  real(ESMF_KIND_R8) :: Src_minx, Src_miny
  real(ESMF_KIND_R8) :: Src_maxx, Src_maxy
  real(ESMF_KIND_R8) :: Dst_minx, Dst_miny
  real(ESMF_KIND_R8) :: Dst_maxx, Dst_maxy
   real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
   integer :: finalrc

  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 30
  Src_ny = 30

  Src_minx=0.0
  Src_miny=0.0

  Src_maxx=10.0
  Src_maxy=10.0

  Dst_nx = 20
  Dst_ny = 20

  Dst_minx=0.0
  Dst_miny=0.0

  Dst_maxx=10.0
  Dst_maxy=10.0


  ! setup source grid
  srcGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_CART, &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              coordSys=ESMF_COORDSYS_CART, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
 
   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
     return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
     return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
 

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! SET CORNER STAGGER COORDS
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates
        fptrXC(i1,i2) = ((Src_maxx-Src_minx)*REAL(i1-1)/REAL(Src_nx-1))+Src_minx
        fptrYC(i1,i2) = ((Src_maxy-Src_miny)*REAL(i2-1)/REAL(Src_ny-1))+Src_miny
     enddo
     enddo


     !! SET CENTER STAGGER COORDS, FUNC, ETC. 
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

 
     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        !!! compute corner coordinates surrounding center
        cnr_x = ((Src_maxx-Src_minx)*REAL(i1-1)/REAL(Src_nx-1))+Src_minx
        cnr_xp1 = ((Src_maxx-Src_minx)*REAL(i1-1+1)/REAL(Src_nx-1))+Src_minx

        cnr_y = ((Src_maxy-Src_miny)*REAL(i2-1)/REAL(Src_ny-1))+Src_miny
        cnr_yp1 = ((Src_maxy-Src_miny)*REAL(i2-1+1)/REAL(Src_ny-1))+Src_miny

         ! Calc Center coordinates as average of corner coords
        x = (cnr_x+cnr_xp1)/2.0
        y = (cnr_y+cnr_yp1)/2.0

        ! Set source value
        fptr(i1,i2)=x+y+20.0

        ! Set Center coordinates
        fptrXC(i1,i2) = x
        fptrYC(i1,i2) = y


        ! Set Mask
        if ((x > 4.0) .and. (x < 6.0)) then
           srcMask(i1,i2)=1
        else
           srcMask(i1,i2)=0
        endif
        
     enddo
     enddo
  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
      endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        fptrXC(i1,i2) = ((Dst_maxx-Dst_minx)*REAL(i1-1)/REAL(Dst_nx-1))+Dst_minx
        fptrYC(i1,i2) = ((Dst_maxy-Dst_miny)*REAL(i2-1)/REAL(Dst_ny-1))+Dst_miny

     enddo
     enddo

 
     !! DO CENTER STAGGER STUFF
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfptr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
         return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        !!! compute corner coordinates surrounding center
        cnr_x = ((Dst_maxx-Dst_minx)*REAL(i1-1)/REAL(Dst_nx-1))+Dst_minx
        cnr_xp1 = ((Dst_maxx-Dst_minx)*REAL(i1-1+1)/REAL(Dst_nx-1))+Dst_minx

        cnr_y = ((Dst_maxy-Dst_miny)*REAL(i2-1)/REAL(Dst_ny-1))+Dst_miny
        cnr_yp1 = ((Dst_maxy-Dst_miny)*REAL(i2-1+1)/REAL(Dst_ny-1))+Dst_miny

        ! Calc Center coordinates as average of corner coords
        x = (cnr_x+cnr_xp1)/2.0
        y = (cnr_y+cnr_yp1)/2.0

         ! Set Center coordinates
        fptrXC(i1,i2) = x
        fptrYC(i1,i2) = y
     
        ! Init dest
        fptr(i1,i2)=0.0

        ! Init exact destination value
        xfptr(i1,i2)=x+y+20.0

        ! Set mask
        if ((y > 4.0) .and. (y < 6.0)) then
           dstMask(i1,i2)=1
        else
           dstMask(i1,i2)=0
        endif
   
     enddo
     enddo

  enddo    ! lDE


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField, dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Init
  minerror(1) = 100000.
  maxerror(1) = 0.
  error = 0.
  dstmass = 0.
 
  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
      endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! skip if masked
        if (dstMask(i1,i2) .eq. 1) cycle

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*fptr(i1,i2)

        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1,i2)*fptr(i1,i2)

        ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
        ! (Note that this is what SCRIP does)
        if (dstFracptr(i1,i2) .lt. 0.999) cycle

        if (xfptr(i1,i2) .ne. 0.0) then
           errorfptr(i1,i2)=ABS(fptr(i1,i2) - xfptr(i1,i2))/ABS(xfptr(i1,i2))
           error = error + errorfptr(i1,i2)
           if (errorfptr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfptr(i1,i2)
           endif
           if (errorfptr(i1,i2) < minerror(1)) then
             minerror(1) = errorfptr(i1,i2)
           endif
        else
           errorfptr(i1,i2)=ABS(fptr(i1,i2) - xfptr(i1,i2))
           error = error + errorfptr(i1,i2)
           if (errorfptr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfptr(i1,i2)
           endif
           if (errorfptr(i1,i2) < minerror(1)) then
             minerror(1) = errorfptr(i1,i2)
           endif
        endif

     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
      call ESMF_FieldGet(srcField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*fptr(i1,i2)
     enddo
     enddo

  enddo    ! lDE


  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_REDUCE_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_REDUCE_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_REDUCE_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif
 
  ! return answer based on correct flags
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-10)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-2) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Cartesian Grid to Grid  with masks ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_cartcsrvregridwmasks


end program ESMF_FieldRegridCsrv2ndUTest
