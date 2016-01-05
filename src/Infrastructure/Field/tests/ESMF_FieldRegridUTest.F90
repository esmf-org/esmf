! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridUTest - Unit tests for Field Regrid methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Regrid unit tests.
!
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
    use ESMF_GridUtilMod


    implicit none

    integer :: virtMemPet, physMemPet

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0
 
       ! individual test result code
    integer :: rc = 1

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
#ifdef ESMF_TESTEXHAUSTIVE

! This #if surrounds all the tests to enable turning on just one test
#if 1
     !------------------------------------------------------------------------
        !EX_UTest
      ! Test regrid between -180-180 sphere and a 360 sphere
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid between a 0 to 360 sphere and a -180 to 180 sphere"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
       call test_regrid180vs360(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!      call ESMF_VMGetMemInfo(virtMemPet, physMemPet, rc)
!      call ESMF_VMLogMemInfo("blahblahblah",rc)


     !------------------------------------------------------------------------
      !EX_UTest
      ! Test regridding with a field with extra dimensions
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid between two Fields with ungridded dimensions"

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_regridExtraFieldDim(rc)

      ! return result
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 

     !------------------------------------------------------------------------
      !EX_UTest
      ! Test regridding on Grids with indices switched
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid between two Fields on Grids with indices switched"

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      ! This test does not work
      !call test_regridSwitchedIndicesII(rc)
      ! this test does work
      call test_regridSwitchedIndices(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test 3D regrid
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "3D Regrid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regrid3D(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with destination masks"

      ! initialize 
      rc=ESMF_SUCCESS
      
       ! do test
      call test_regridDstMask(rc)
 
      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with source masks"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSrcMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid sphere with source masks"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphSrcMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#if 0
!the following test, when included in a run of the full test suite, will occassionally 
!fail on at least one processor on yellowstone...ie the masked values will unexpectedly 
!change...i was unable to reproduce the error by running this test all by itself...
!appears to be a memory error, but its unclear whether the error is in the test itself, 
!or if it exposes a memory problem elsewhere...the problem was reproduced in a code 
!sandbox that did not include any of the PointList code (mvr)

      !------------------------------------------------------------------------
       !EX_OFF_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid sphere with destination masks"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test

      call test_regridSphDstMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with R4 coordinates"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_regridR4(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with corner stagger"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridCnr(rc)

       ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with edge stagger"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridEdge(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Grid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshToGrid(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Spherical Mesh to Grid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshSph3x3ToGrid(rc)
 
      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

 
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Grid to Mesh"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridGridToMesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Mesh"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshToMesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Grid 3D"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
       call test_regridMeshToGrid3D(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


#if 0
      !------------------------------------------------------------------------

      !EX_OFF_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Tetrahedral Mesh to 3D Grid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridTetMeshToGrid3D(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif

      !------------------------------------------------------------------------

      !EX_UTest
       ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from 3D spherical Mesh to 3D spherical Grid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshToGridSph3D(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------

      !EX_UTest
      ! Test regrid with masks
       write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid between two 3D Spherical Global Grids"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridGridToGridSph3D(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding Sphere with ESMF_POLEMETHOD_NONE"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphPoleNone(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding Sphere with ESMF_POLEMETHOD_ALLAVG"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphPoleAllAvg(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
 
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding Sphere with ESMF_POLEMETHOD_NPNTAVG"

       ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphPoleNpntAvg(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding Sphere with ESMF_POLEMETHOD_TEETH"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphPoleTeeth(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with distgrid specified sphere
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding from distgrid connections"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridDGSph(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
 

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test using matrix (factorList, factorIndexList) " // &
                     "from ESMF_FieldRegridStore() in ESMF_FieldSMMStore()"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMatrixFactor(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------



      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test using DEPRECATED matrix arguments (weights, indices) " // &
                     "from ESMF_FieldRegridStore() in ESMF_FieldSMMStore()"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_regridMatrix(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding on a grid with indexflag=ESMF_INDEX_DELOCAL"

      ! initialize 
       rc=ESMF_SUCCESS
      
      ! do test
      call test_regridDELOCAL(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------



      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding on a grid with an irregular distribution"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridIrreg(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding on a spherical grids with NEAREST_STOD regridding"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphNearest(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
       !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Mesh with NEAREST_STOD interp."

      ! initialize 
      rc=ESMF_SUCCESS
      
       ! do test
      call test_regridNearestMeshToMesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------




      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding on a spherical grids with NEAREST_DTOS regridding"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphNearestDTOS(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------




      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test unmapped destination point list"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_unmappedDstList(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#if 0
      !------------------------------------------------------------------------
      !EX_disable_UTest
      ! Test regrid with 2 tile distgrid"
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding from a 2 tile distgrid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regrid2TileDG(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid between Fields where srcGrid has holes in index space"
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding with holes in srcGrid"

       ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSrcHoles(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
       write(name, *) "Test Mesh masking during regrid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshToMeshMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test really coarse regrid
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding with sphere and gc bilinear"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphGC(50, 50, 80, 80, 0.1_ESMF_KIND_R8, rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test really coarse regrid
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding with coarse sphere and gc bilinear"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphGC(5, 5, 80, 80, 0.4_ESMF_KIND_R8, rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
 
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Mesh with Pentagon and Hexagon"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
       call test_regridMeshToMeshPH(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Mesh On Cell Centers"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshToMeshCenter(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid Mesh with collapsed quads"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridCollapsedQuads(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid using Location Streams
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from LocStream to LocStream with Nearest Neighbor interp."

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridNearestLocStreamToLocStream(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid locstream with clustering to mesh using nearest neighbor
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from LocStream with clustering to Mesh using nearest neighbor"

      ! initialize
      rc=ESMF_SUCCESS

      ! do test
      call test_regridNearestLocStream_wClusterToMesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid locstream to grid using nearest neighbor
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from LocStream to Grid using nearest neighbor"

      ! initialize
      rc=ESMF_SUCCESS

       ! do test
      call test_regridNearestLocStreamToGrid(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid grid to a locstream with regular distribution
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Grid to a LocStream with regular distribution, bilinear and patch"

      ! initialize
      rc=ESMF_SUCCESS

      ! do test
      call test_regridGridToLocStreamRegDist(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_UtilIOUnitFlush (6)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid grid to a locstream with a local count
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Grid to a LocStream with a local count"

      ! initialize
      rc=ESMF_SUCCESS

      ! do test
      call test_regridGridToLocStreamLocCnt(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid 3d grid to a 3d locstream
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from 3d Grid to a 3d LocStream"

       ! initialize
      rc=ESMF_SUCCESS

      ! do test
      call test_regridGridToLocStream3d(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_UtilIOUnitFlush (6)


       !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid mesh to locstream with mask using bilinear
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to LocStream with mask using Bilinear"

      ! initialize
      rc=ESMF_SUCCESS

      ! do test
      call test_regridMeshToLocStreamMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid grid to grid/mesh/locstream (GML) with identical points
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Grid to Grid/Mesh/LocStream (GML) with identical points"

      ! initialize
      rc=ESMF_SUCCESS

      ! do test
      call test_regridGridToGML(rc)

     ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!      call ESMF_LogFlush()
!      call ESMF_UtilIOUnitFlush (6)
 

       !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh containing pentagons and hexagons to Grid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridPHMeshToGrid(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with periodic and non-periodic uniform grid creates"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_regrid_gridufrm(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"

      subroutine test_regrid180vs360(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: grid360
  type(ESMF_Grid) :: grid180
  type(ESMF_Field) :: srcField360
  type(ESMF_Field) :: dstField360
  type(ESMF_Field) :: field180
  type(ESMF_Field) :: errorField
  type(ESMF_Array) :: array180
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: lonArray360
  type(ESMF_Array) :: srcArray360, dstArray360
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandle1
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:),errorfarrayPtr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
  real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: rangle, xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: RAD2DEG

  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer :: srcTermProcessing, pipeLineDepth
  

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

  ! Establish the resolution of the grids
  src_nx = 100
  src_ny = 50

  dst_nx = 90
  dst_ny = 40

  ! setup source grid
  grid360=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                  coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
!                                  poleType=(/ESMF_POLETYPE_MONOPOLE,ESMF_POLETYPE_BIPOLE/), & 
                                  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  grid180=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField360 = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField360 = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   field180 = ESMF_FieldCreate(grid180, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(grid360, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(grid180, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(grid360, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! array180
  call ESMF_FieldGet(field180, array=array180, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArray360
  call ESMF_FieldGet(srcField360, array=srcArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! dstArray360
  call ESMF_FieldGet(dstField360, array=dstArray360, rc=localrc)
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



  !! get longitude array
  call ESMF_GridGetCoord(grid360, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=lonArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Write results to a mesh
  num_arrays = 1

! Test interpolation on the sphere
! Set the source grid coordinates to be a 0 to 360 grid

  src_dx = 360./src_nx
  src_dy = 180./src_ny

  DEG2RAD = 3.14159265/180.0
  RAD2DEG = 1./DEG2RAD

  ! Get memory and set coords for src
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcField360, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field pointer
     call ESMF_FieldGet(dstField360, lDE, farrayPtr2,   rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        ! (something relatively smooth, that varies everywhere)
        farrayPtr(i1,i2) = x+y+z+15.0

         ! initialize src destination field
         farrayPtr2(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  dst_dx = 360./dst_nx
  dst_dy = 180./dst_ny

  rangle = DEG2RAD*20.

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(field180, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error dst clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error dst clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error dst cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error dst cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set destination coordinates as -180 to 180
        farrayPtrXC(i1,i2) = -180. + (REAL(i1-1)*dst_dx)
        farrayPtrYC(i1,i2) = -90.  + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! init destination mesh to 0
        farrayPtr(i1,i2) = 0.
     
     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the 0 to 360 grid to the -180 to 180 grid
  ! Regrid store
  ! make sure these work
  srcTermProcessing=0
  pipeLineDepth=localPet
  call ESMF_FieldRegridStore(srcField360, dstField=field180, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          srcTermProcessing=srcTermProcessing, &
          pipeLineDepth=pipeLineDepth, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField360, field180, routeHandle, &
       termorderflag=ESMF_TERMORDER_FREE ,rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  !!!!!!!! Regrid back from the -180 to 180 grid to the 0 to 360 grid
  ! Regrid store
  call ESMF_FieldRegridStore(field180, dstField=dstField360, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(field180, dstField360, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get src Field
     call ESMF_FieldGet(srcField360, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field
     call ESMF_FieldGet(dstField360, lDE, farrayPtr2,  rc=localrc)
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


     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (farrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2) - farrayPtr2(i1,i2))/farrayPtr(i1,i2))
        else
           errorfarrayPtr(i1,i2)=(farrayPtr(i1,i2) - farrayPtr2(i1,i2))
        endif
        if (ABS(errorfarrayPtr(i1,i2)) .gt. 0.01) then
            correct=.false. 
        endif

     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, grid360, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray360, dstArray360, errorArray, lonArray360, rc=localrc, &
               spherical=spherical_grid)
write(*,*) "LOCALRC=",localrc
  call ESMF_MeshIO(vm, grid180, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", array180, rc=localrc, &
               spherical=spherical_grid)
write(*,*) "LOCALRC=",localrc
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField360, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField360, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(field180, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(grid360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(grid180, rc=localrc)
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

      end subroutine test_regrid180vs360

      subroutine test_regrid3D(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: dstFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: errorField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA, dstArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandle1
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:),farrayPtr2(:,:,:),errorfarrayPtr(:,:,:)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(3)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(3)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, A_nz, B_nx, B_ny, B_nz
  integer num_arrays

  real(ESMF_KIND_R8) :: A_minx,A_miny,A_minz
  real(ESMF_KIND_R8) :: A_maxx,A_maxy,A_maxz
  real(ESMF_KIND_R8) :: B_minx,B_miny,B_minz
  real(ESMF_KIND_R8) :: B_maxx,B_maxy,B_maxz
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  B_nx = 20
  B_ny = 20
  B_nz = 20

  A_nx = 10
  A_ny = 10
  A_nz = 10

  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  B_minz = 0.0

  B_maxx = 10.0
  B_maxy = 10.0
  B_maxz = 10.0

  A_minx = 0.0
  A_miny = 0.0
  A_minz = 0.0

  A_maxx = 10.0
  A_maxy = 10.0
  A_maxz = 10.0

  ! setup source grid
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/A_nx,A_ny,A_nz/),regDecomp=(/petCount,1,1/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/B_nx,B_ny,B_nz/),regDecomp=(/1,1,petCount/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! dstArrayA
  call ESMF_FieldGet(dstFieldA, array=dstArrayA, rc=localrc)
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


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field pointer
     call ESMF_FieldGet(dstFieldA, lDE, farrayPtr2,   rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        ! Set source coordinates
        farrayPtrXC(i1,i2,i3) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2,i3) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny
        farrayPtrZC(i1,i2,i3) = ((A_maxz-A_minz)*REAL(i3-1)/REAL(A_nz-1))+A_minz

        ! set src data
        ! (something  smooth, that varies everywhere)
        farrayPtr(i1,i2,i3) = farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+farrayPtrZC(i1,i2,i3)+15.0

        ! initialize src destination field
        farrayPtr2(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        ! Set source coordinates
        farrayPtrXC(i1,i2,i3) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2,i3) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny
        farrayPtrZC(i1,i2,i3) = ((B_maxz-B_minz)*REAL(i3-1)/REAL(B_nz-1))+B_minz

        ! initialize destination field
        farrayPtr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcFieldA, dstField=fieldB, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  !!!!!!!! Regrid back from the B grid to the A grid
  ! Regrid store
  call ESMF_FieldRegridStore(fieldB, dstField=dstFieldA, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(fieldB, dstFieldA, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get src Field
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field
     call ESMF_FieldGet(dstFieldA, lDE, farrayPtr2,  rc=localrc)
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


     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        if (farrayPtr(i1,i2,i3) .ne. 0.0) then
           errorfarrayPtr(i1,i2,i3)=ABS((farrayPtr(i1,i2,i3) - farrayPtr2(i1,i2,i3))/farrayPtr(i1,i2,i3))
        else
           errorfarrayPtr(i1,i2,i3)=(farrayPtr(i1,i2,i3) - farrayPtr2(i1,i2,i3))
        endif
        if (ABS(errorfarrayPtr(i1,i2,i3)) .gt. 0.001) then
            correct=.false. 
        endif

     enddo
     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
  spherical_grid = 0
!  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
!               "srcmesh", srcArrayA, dstArrayA, errorArray, rc=localrc, &
!               spherical=spherical_grid)
!  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
!               "dstmesh", arrayB, rc=localrc, &
!               spherical=spherical_grid)


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

      end subroutine test_regrid3D


      subroutine test_regridDstMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  real(ESMF_KIND_R8) :: B_minx,B_miny
  real(ESMF_KIND_R8) :: B_maxx,B_maxy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  B_nx = 20
  B_ny = 20

  A_nx = 10
  A_ny = 10


  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  
  B_maxx = 10.0
  B_maxy = 10.0
  
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 10.0
  A_maxy = 10.0
  
  ! setup source grid
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_CART,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate Masks
  call ESMF_GridAddItem(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

        ! set src data
        farrayPtr(i1,i2) = 20.0

	! set mask
	dx=farrayPtrXC(i1,i2)-((A_maxx+A_minx)/2.0)
	dy=farrayPtrYC(i1,i2)-((A_maxy+A_miny)/2.0)
	if (sqrt(dx*dx+dy*dy) < 1.0) then
           maskA(i1,i2) = 2
	else
           maskA(i1,i2) = 0
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
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskB, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

	! set mask
	dx=farrayPtrXC(i1,i2)-((B_maxx+B_minx)/2.0)
	dy=farrayPtrYC(i1,i2)-((B_maxy+B_miny)/2.0)
	if (sqrt(dx*dx+dy*dy) < 2.0) then
           maskB(i1,i2) = 3
	else
           maskB(i1,i2) = 0
	endif

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, dstMaskValues=(/1,2,3,4/), &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if we missed the correct spots
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskB, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! make sure we used the mask
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	if (maskB(i1,i2) .ne. 0) then	
	   ! if masked out should be 0.0 (but give ourselves room for imprecision)
	   if (farrayPtr(i1,i2) > 2.0) then
             correct=.false.
           endif
	else
	   ! If not masked out should be 20 (but give ourselves room for imprecision)
	   if (farrayPtr(i1,i2) < 18.0) then
             correct=.false.
	    endif
        endif

     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridDstMask



      subroutine test_regridSrcMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  real(ESMF_KIND_R8) :: B_minx,B_miny
  real(ESMF_KIND_R8) :: B_maxx,B_maxy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  B_nx = 20
  B_ny = 20

  A_nx = 11
  A_ny = 11


  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  
  B_maxx = 10.0
  B_maxy = 10.0
  
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 10.0
  A_maxy = 10.0
  
  ! setup source grid
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate Masks
  call ESMF_GridAddItem(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! arrayBPatch
  call ESMF_FieldGet(fieldBPatch, array=arrayBPatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! set mask (circle of radius 2 around center)
        ! and source data based on mask
	dx=farrayPtrXC(i1,i2)-((A_maxx+A_minx)/2.0)
	dy=farrayPtrYC(i1,i2)-((A_maxy+A_miny)/2.0)
	if (sqrt(dx*dx+dy*dy) < 2.0) then
           maskA(i1,i2) = 2
           farrayPtr(i1,i2) = -1000.0 
	else
           maskA(i1,i2) = 0
           farrayPtr(i1,i2) = 20.0 
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
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination fields
        farrayPtr(i1,i2)=0.0
        farrayPtrPatch(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

#ifdef ESMF_LAPACK

  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldBPatch, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandlePatch, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldBPatch, routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif


  ! Check if we're using any of the bad source points
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working should always be >= 0.0 
        if (farrayPtr(i1,i2) < 0.0) then
           correct=.false.
        endif

#ifdef ESMF_LAPACK
        ! if working should always be >= 0.0 
        if (farrayPtrPatch(i1,i2) < 0.0) then
           correct=.false.
        endif
#endif

     enddo
     enddo

  enddo    ! lDE

  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)

#ifdef ESMF_LAPACK
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, arrayBPatch, rc=localrc, &
               spherical=spherical_grid)
#else
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif



#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldBPatch, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridSrcMask


      subroutine test_regridSphSrcMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate Masks
  call ESMF_GridAddItem(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! arrayBPatch
  call ESMF_FieldGet(fieldBPatch, array=arrayBPatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

	! set mask region around 180
        ! and source data based on mask
	dx=farrayPtrXC(i1,i2)-180.0
	if (abs(dx) < 45.0) then
           maskA(i1,i2) = 2
           farrayPtr(i1,i2) = -1000.0 
	else
           maskA(i1,i2) = 0
           farrayPtr(i1,i2) = 20.0 
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
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0
        farrayPtrPatch(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

#ifdef ESMF_LAPACK
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldBPatch, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandlePatch, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldBPatch, routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif


  ! Check if we're using any of the bad source points
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working should always be >= 0.0 
        if (farrayPtr(i1,i2) < 0.0) then
           correct=.false.
        endif

#ifdef ESMF_LAPACK
        ! if working should always be >= 0.0 
        if (farrayPtrPatch(i1,i2) < 0.0) then
           correct=.false.
        endif
#endif


     enddo
     enddo

  enddo    ! lDE

  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)

#ifdef ESMF_LAPACK
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, arrayBPatch, rc=localrc, &
               spherical=spherical_grid)
#else
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif



#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(fieldBPatch, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridSphSrcMask


 subroutine test_regridSphDstMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny


  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate Masks
  call ESMF_GridAddItem(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! arrayBPatch
  call ESMF_FieldGet(fieldBPatch, array=arrayBPatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Set source data
        farrayPtr(i1,i2) = 20.0 

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetItem(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskB, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	! set mask region around 180
        ! and source data based on mask
	dx=farrayPtrXC(i1,i2)-180.0
	if (abs(dx) < 45.0) then
           maskB(i1,i2) = 2
           farrayPtr(i1,i2) = -1000.0 
	else
           maskB(i1,i2) = 0
           farrayPtr(i1,i2) = 0.0 
	endif

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0
        farrayPtrPatch(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcFieldA, &
          dstField=fieldB, dstMaskValues=(/2/), &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

#ifdef ESMF_LAPACK
  ! Regrid store
  call ESMF_FieldRegridStore(srcFieldA, &
          dstField=fieldBPatch, dstMaskValues=(/2/), &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandlePatch, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldBPatch, routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif


  ! Check if we're using any of the bad source points
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working mask values should be unchanged
        if (maskB(i1,i2) == 2) then
          if (farrayPtr(i1,i2) /= -1000) then
            correct=.false.
          endif
        endif

#ifdef ESMF_LAPACK
        ! if working should be unchanged
        if (maskB(i1,i2) == 2) then
          if (farrayPtrPatch(i1,i2) /= -1000) then
             correct=.false.
          endif
        endif
#endif


     enddo
     enddo

  enddo    ! lDE

  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)

#ifdef ESMF_LAPACK
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, arrayBPatch, rc=localrc, &
               spherical=spherical_grid)
#else
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif



#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(fieldBPatch, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridSphDstMask


      subroutine test_regridCnr(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  real(ESMF_KIND_R8) :: B_minx,B_miny
  real(ESMF_KIND_R8) :: B_maxx,B_maxy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  B_nx = 30
  B_ny = 30

  A_nx = 20
  A_ny = 20


  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  
  B_maxx = 10.0
  B_maxy = 10.0
  
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 10.0
  A_maxy = 10.0
  
  ! setup source grid
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_CART,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CORNER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! func to interpolate
        farrayPtr(i1,i2) = 20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2)

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2)-(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))) > 0.0001) then
           correct=.false.	
	endif	
     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CORNER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridCnr



      subroutine test_regridEdge(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  real(ESMF_KIND_R8) :: B_minx,B_miny
  real(ESMF_KIND_R8) :: B_maxx,B_maxy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  B_nx = 20
  B_ny = 20

  A_nx = 10
  A_ny = 10


  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  
  B_maxx = 10.0
  B_maxy = 10.0
  
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 10.0
  A_maxy = 10.0
  
  ! setup source grid
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_EDGE1, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_EDGE1, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_EDGE1, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_EDGE1, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! func to interpolate
        farrayPtr(i1,i2) = 20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2)

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2)-(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))) > 0.0001) then
           correct=.false.	
	endif	
     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridEdge


 subroutine test_regridMeshToGrid(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: dst_minx,dst_miny
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy

  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

   integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
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

  ! Establish the resolution of the grids
  dst_nx = 10
  dst_ny = 10

  ! Establish the coordinates of the grids
  dst_minx = 0.1
  dst_miny = 0.1
  
  dst_maxx = 1.9
  dst_maxy = 1.9

  ! setup source Mesh
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
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
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
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

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
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

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
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                      1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

    ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y

        ! Advance to next owner
        i2=i2+1
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


  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/2,2/), &
                                  coordSys=ESMF_COORDSYS_CART,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

    dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
      do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

    
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2)-(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))) > 0.0001) then
           correct=.false.	
	endif	
     enddo
     enddo

  enddo    ! lDE

  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, srcMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

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


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridMeshToGrid


 subroutine test_regridGridToMesh(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: src_minx,src_miny
  real(ESMF_KIND_R8) :: src_maxx,src_maxy

  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
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

  ! Establish the resolution of the grids
  src_nx = 10
  src_ny = 10

  ! Establish the coordinates of the grids
  src_minx = -0.1
  src_miny = -0.1
  
  src_maxx = 2.1
  src_maxy = 2.1  


  ! setup src grid
  srcGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/2,2/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((src_maxx-src_minx)*REAL(i1-1)/REAL(src_nx-1))+src_minx
        farrayPtrYC(i1,i2) = ((src_maxy-src_miny)*REAL(i2-1)/REAL(src_ny-1))+src_miny

        ! initialize destination field
        farrayPtr(i1,i2)=farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2)+20.0

     enddo
     enddo

  enddo    ! lDE



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
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
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
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

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
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

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
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
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

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
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

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

	!! if error is too big report an error
	if ( abs( farrayPtr1D(i2)-(x+y+20.0) ) > 0.0001) then
           correct=.false.	
	endif	

        ! Advance to next owner
        i2=i2+1
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



  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, dstMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

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


  ! Free the grids
  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(srcGrid, rc=localrc)
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

 end subroutine test_regridGridToMesh



 subroutine test_regridMeshToMesh(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
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
     nodeIds=(/100,20,30,40,50,60,70,80,90/) 

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
       nodeIds=(/100,20,40,50/) 

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

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/20,30,50,60/) 

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

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/40,50,70,80/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      1.0,2.1 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/50,60,80,90/) 

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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y

        ! Advance to next owner
        i2=i2+1
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
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
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
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

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
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

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
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
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

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

	!! if error is too big report an error
	if ( abs( farrayPtr1D(i2)-(x+y+20.0) ) > 0.0001) then
           correct=.false.	
	endif	

        ! Advance to next owner
        i2=i2+1
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



  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, dstMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

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


  ! Free the grids
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

 end subroutine test_regridMeshToMesh


 subroutine test_regridMeshToGrid3D(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:),farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:),farrayPtr2(:,:,:)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(3)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(3)
  character(len=ESMF_MAXSTR) :: string
  integer dst_nx,dst_ny,dst_nz
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy,dz
  
  real(ESMF_KIND_R8) :: dst_minx,dst_miny,dst_minz
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy,dst_maxz

  real(ESMF_KIND_R8) :: x,y,z
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems

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


  ! Establish the resolution of the grids
  dst_nx = 10
  dst_ny = 10
  dst_nz = 5

  ! Establish the coordinates of the grids
  dst_minx = 0.1
  dst_miny = 0.1
  dst_minz = 0.1
  
  dst_maxx = 1.9
  dst_maxy = 1.9
  dst_maxz = 0.9
  
  ! setup source Mesh
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
     nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                  1.0,0.0,0.0, & ! node id 2
                  2.0,0.0,0.0, & ! node id 3
                  0.0,1.0,0.0, & ! node id 4
                  1.0,1.0,0.0, & ! node id 5
                  2.0,1.0,0.0, & ! node id 6
                  0.0,2.0,0.0, & ! node id 7
                  1.0,2.0,0.0, & ! node id 8
                  2.0,2.0,0.0, & ! node id 9
                  0.0,0.0,1.0, & ! node id 10
                  1.0,0.0,1.0, & ! node id 11
                  2.0,0.0,1.0, & ! node id 12
                  0.0,1.0,1.0, & ! node id 13
                  1.0,1.0,1.0, & ! node id 14
                  2.0,1.0,1.0, & ! node id 15
                  0.0,2.0,1.0, & ! node id 16
                  1.0,2.0,1.0, & ! node id 17
                  2.0,2.0,1.0 /) ! node id 18

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
       nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                    1.0,0.0,0.0, & ! node id 2
                    0.0,1.0,0.0, & ! node id 4
                    1.0,1.0,0.0, & ! node id 5
                    0.0,0.0,1.0, & ! node id 10
                    1.0,0.0,1.0, & ! node id 11
                    0.0,1.0,1.0, & ! node id 13
                    1.0,1.0,1.0 /) ! node id 14


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
       nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                    2.0,0.0,0.0, & ! node id 3
                    1.0,1.0,0.0, & ! node id 5
                    2.0,1.0,0.0, & ! node id 6
                    1.0,0.0,1.0, & ! node id 11
                    2.0,0.0,1.0, & ! node id 12
                    1.0,1.0,1.0, & ! node id 14
                    2.0,1.0,1.0/) ! node id 15

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
        nodeCoords=(/0.0,1.0,0.0, & ! node id 4
                     1.0,1.0,0.0, & ! node id 5
                     0.0,2.0,0.0, & ! node id 7
                     1.0,2.0,0.0, & ! node id 8
                     0.0,1.0,1.0, & ! node id 13
                     1.0,1.0,1.0, & ! node id 14
                     0.0,2.0,1.0, & ! node id 16
                     1.0,2.0,1.0/) ! node id 17


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
        nodeCoords=(/1.0,1.0,0.0, &  ! node id 5
                     2.0,1.0,0.0, &  ! node id 6
                     1.0,2.0,0.0, &  ! node id 8
                     2.0,2.0,0.0, &  ! node id 9
                     1.0,1.0,1.0, &  ! node id 14
                     2.0,1.0,1.0, &  ! node id 15
                     1.0,2.0,1.0, &  ! node id 17
                     2.0,2.0,1.0/)  ! node id 18

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

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=3,spatialDim=3, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(3*i1-2)
        y=nodeCoords(3*i1-1)
        z=nodeCoords(3*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y+z

        ! Advance to next owner
        i2=i2+1
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



  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/dst_nx,dst_ny,dst_nz/), &
              coordSys=ESMF_COORDSYS_CART, regDecomp=(/2,2,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Set source coordinates
        farrayPtrXC(i1,i2,i3) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2,i3) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny
        farrayPtrZC(i1,i2,i3) = ((dst_maxz-dst_minz)*REAL(i3-1)/REAL(dst_nz-1))+dst_minz

        ! initialize destination field
        farrayPtr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif
   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2,i3)-(20.0+farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+ &
                                     farrayPtrZC(i1,i2,i3))) > 0.0001) then
           correct=.false.	
	endif	
     enddo
     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, srcMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

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


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridMeshToGrid3D


      subroutine test_regridSphPoleNone(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  !! THESE RESOLUTIONS ARE SETUP SO THE DEST GRID (GRID B) HAS
  !! ONE ROW ABOVE THE TOP ROW OF THE SOURCE GRID (GRID A) AND
  !! ONE ROW BELOW THE BOTTOM ROW OF THE SOURCE GRID.
  !! THIS ALLOWS THE POLE INFO TO BE MORE EASILY CHECKED.

  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=ESMF_POLEMETHOD_NONE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working the top and bottom dest rows should be close to 0.0
        if ((i2 .eq. 1) .or. (i2 .eq. B_ny)) then
           if (farrayPtr(i1,i2) .gt. 0.000001) then
              correct=.false.
           endif
        else ! otherwise they should be close to 20
           if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
              correct=.false.
           endif
	endif
     enddo
     enddo

  enddo    ! lDE


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridSphPoleNone


      subroutine test_regridSphPoleAllAvg(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  !! THESE RESOLUTIONS ARE SETUP SO THE DEST GRID (GRID B) HAS
  !! ONE ROW ABOVE THE TOP ROW OF THE SOURCE GRID (GRID A) AND
  !! ONE ROW BELOW THE BOTTOM ROW OF THE SOURCE GRID.
  !! THIS ALLOWS THE POLE INFO TO BE MORE EASILY CHECKED.

  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=ESMF_POLEMETHOD_ALLAVG, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridSphPoleAllAvg



      subroutine test_regridSphPoleNpntAvg(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  !! THESE RESOLUTIONS ARE SETUP SO THE DEST GRID (GRID B) HAS
  !! ONE ROW ABOVE THE TOP ROW OF THE SOURCE GRID (GRID A) AND
  !! ONE ROW BELOW THE BOTTOM ROW OF THE SOURCE GRID.
  !! THIS ALLOWS THE POLE INFO TO BE MORE EASILY CHECKED.

  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=ESMF_POLEMETHOD_NPNTAVG, &
          regridPoleNPnts=4, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridSphPoleNpntAvg



      subroutine test_regridSphPoleTeeth(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  !! THESE RESOLUTIONS ARE SETUP SO THE DEST GRID (GRID B) HAS
  !! ONE ROW ABOVE THE TOP ROW OF THE SOURCE GRID (GRID A) AND
  !! ONE ROW BELOW THE BOTTOM ROW OF THE SOURCE GRID.
  !! THIS ALLOWS THE POLE INFO TO BE MORE EASILY CHECKED.

  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=ESMF_POLEMETHOD_TEETH, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
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

 end subroutine test_regridSphPoleTeeth



 subroutine test_regridDGSph(rc)
   integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: srcDistgrid
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD
  
  integer :: spherical_grid

  integer :: localPet, petCount

  type(ESMF_DistGridConnection) :: connectionList(3) ! 3 connections


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

  ! Establish the resolution of the grids
  src_nx = 20
  src_ny = 20

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 80
  dst_ny = 80

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! Create connectionList
  ! periodicity
  call ESMF_DistgridConnectionSet(connection=connectionList(1), &
        tileIndexA=1,tileIndexB=1, &
        positionVector=(/src_nx,0/), &
        orientationVector=(/1,2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_DistgridConnectionSet(connection=connectionList(2), &
        tileIndexA=1,tileIndexB=1, &
        positionVector=(/src_nx+1,2*src_ny+1/), &
        orientationVector=(/-1,-2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_DistgridConnectionSet(connection=connectionList(3), &
        tileIndexA=1,tileIndexB=1, &
        positionVector=(/src_nx/2,1/), &
        orientationVector=(/1,-2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source distgrid
  srcDistgrid=ESMF_DistgridCreate(minIndex=(/1,1/), maxIndex=(/src_nx,src_ny/), regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              connectionList=connectionList,                 &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup source grid
  srcGrid=ESMF_GridCreate(distgrid=srcDistgrid, indexflag=ESMF_INDEX_GLOBAL, &
                          coordSys=ESMF_COORDSYS_SPH_DEG, &
                          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
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

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact src data
        !farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        farrayPtr(i1,i2) = 1.0
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)
  
        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        xfarrayPtr(i1,i2) = 1.0

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.false., isLatLonDeg=.true., filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check results
  do lDE=0,localDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2)) .gt. 0.001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.false., isLatLonDeg=.true., filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.true., isLatLonDeg=.true., filename="dstGrid", array1=dstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


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


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#if 0
  ! Free the srcDistgrid
  call ESMF_DistgridDestroy(srcDistgrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridDGSph



      subroutine test_regridMatrixFactor(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer(ESMF_KIND_I4), pointer:: indices(:,:)
  real(ESMF_KIND_R8), pointer :: weights(:)
  
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

  ! Establish the resolution of the grids
  A_nx = 21
  A_ny = 21

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 10
  B_ny = 10

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          factorIndexList=indices, factorList=weights, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do SMM
  call ESMF_FieldSMMStore(srcFieldA, fieldB, routeHandle=routeHandle, &
         factorList=weights, factorIndexList=indices, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldSMM(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldSMMRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! get rid of matrix
  deallocate(weights)
  deallocate(indices)


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridMatrixFactor


      subroutine test_regridMatrix(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer(ESMF_KIND_I4), pointer:: indices(:,:)
  real(ESMF_KIND_R8), pointer :: weights(:)
  
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

  ! Establish the resolution of the grids
  A_nx = 21
  A_ny = 21

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 10
  B_ny = 10

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  ! NOTE THAT THE FOLLOWING METHOD USES ARGUMENTS THAT ARE DEPRECATED. 
  ! Please use (factorList, factorIndexList) instead of (weights, indices)  
  ! See test_RegridMatrixFactor() for an example of their use. 
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          weights=weights, indices=indices,          & ! DEPRECATED
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR,   &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do SMM
  call ESMF_FieldSMMStore(srcFieldA, fieldB, routeHandle=routeHandle, &
         factorList=weights, factorIndexList=indices, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldSMM(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldSMMRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! get rid of matrix
  deallocate(weights)
  deallocate(indices)


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridMatrix



      subroutine test_regridDELOCAL(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: tmpArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)

  real(ESMF_KIND_R8), pointer :: x_coord(:,:)
  real(ESMF_KIND_R8), pointer :: y_coord(:,:)
  real(ESMF_KIND_R8), pointer :: data(:,:)
  real(ESMF_KIND_R8), pointer :: xdata(:,:)

  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  real(ESMF_KIND_R8) :: DEG2RAD, lat, lon, theta, phi
  real(ESMF_KIND_R8) :: rel_error  
  real(ESMF_KIND_R8) :: max_rel_error  

  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  A_nx = 183
  A_ny = 95

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 10
  B_ny = 10

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny


  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  
  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_DELOCAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_DELOCAL, &
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


  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! Fill src Coords
  allocate(x_coord(A_nx,A_ny))
  allocate(y_coord(A_nx,A_ny))
  allocate(data(A_nx,A_ny))

  ! set coords, interpolated function
  do i1=1,A_nx
     do i2=1,A_ny
        
        ! Set source coordinates as 0 to 360
        x_coord(i1,i2) = REAL(i1-1)*A_dx
        y_coord(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! set src data 
        lon = x_coord(i1,i2)
        lat = y_coord(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
        data(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
  enddo


  ! Scatter X coords
  call ESMF_GridGetCoord(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=x_coord, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! Scatter Y coords
  call ESMF_GridGetCoord(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                         array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=y_coord, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! Scatter data
  call ESMF_FieldGet(srcField, array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=data, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! Deallocate tmp arrays
  deallocate(x_coord)
  deallocate(y_coord)
  deallocate(data)


  ! Fill dst Coords
  allocate(x_coord(B_nx,B_ny))
  allocate(y_coord(B_nx,B_ny))
  allocate(data(B_nx,B_ny))
  allocate(xdata(B_nx,B_ny))

  ! set coords, interpolated function
  do i1=1,B_nx
     do i2=1,B_ny
        
        ! Set source coordinates as 0 to 360
        x_coord(i1,i2) = REAL(i1-1)*B_dx
        y_coord(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)

        ! set src data 
        lon = x_coord(i1,i2)
        lat = y_coord(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! Set exact value
        xdata(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! Init dst data
        data(i1,i2) = 0.0
     enddo
  enddo


  ! Scatter X coords
  call ESMF_GridGetCoord(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=x_coord, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! Scatter Y coords
  call ESMF_GridGetCoord(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                         array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=y_coord, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! Scatter xdata
  call ESMF_FieldGet(xdstField, array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=xdata, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! Scatter data
  call ESMF_FieldGet(dstField, array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=data, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! Deallocate tmp arrays
  deallocate(x_coord)
  deallocate(y_coord)
  deallocate(data)
  deallocate(xdata)


#if 0
  ! Output Mesh
  call ESMF_GridWriteVTK(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, filename="srcGrid", &
                         rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Output Mesh
  call ESMF_GridWriteVTK(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, filename="dstGrid", &
                         rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routehandle=routehandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check interpolation
  max_rel_error=0.0
  do lDE=0,localDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           rel_error=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2)
        else
           rel_error=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        if (rel_error > max_rel_error) max_rel_error=rel_error

        if (rel_error .gt. 0.001) then
           write(*,*) i1,i2," ",farrayPtr(i1,i2),xfarrayPtr(i1,i2)
           correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


! For Debugging
!  write(*,*) "Max_rel_error=",max_rel_error


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


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridDELOCAL


 subroutine test_regridIrreg(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD
  
  integer :: spherical_grid

  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  src_nx = 25
  src_ny = 30

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 80
  dst_ny = 80

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! setup dest. grid
  srcGrid=ESMF_GridCreate1PeriDim(countsPerDeDim1=(/10,15/), countsPerDeDim2=(/14,16/), &
                                  indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                  indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
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

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx

        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
!        farrayPtrYC(i1,i2) = -90. + REAL(i2-1)*src_dy 

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact src data
        !farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        farrayPtr(i1,i2) = 1.0
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)
  
        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        xfarrayPtr(i1,i2) = 1.0

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check results
  do lDE=0,dstlocalDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to exact answer
        if (abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2)) .gt. 0.001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.false., isLatLonDeg=.true., filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.true., isLatLonDeg=.true., filename="dstGrid", array1=dstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


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


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#if 0
  ! Free the srcDistgrid
  call ESMF_DistgridDestroy(srcDistgrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridIrreg



 subroutine test_regridTetMeshToGrid3D(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:),farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:),farrayPtr2(:,:,:)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(3)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(3)
  character(len=ESMF_MAXSTR) :: string
  integer dst_nx,dst_ny,dst_nz
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy,dz
  
  real(ESMF_KIND_R8) :: dst_minx,dst_miny,dst_minz
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy,dst_maxz

  real(ESMF_KIND_R8) :: x,y,z
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  logical :: at_least_one_interp

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

  
  ! setup source Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=10

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9, &
               10/)

     ! Allocate and fill node coordinate array.
     ! Since this is a 3D Mesh the size is 3x the
     ! number of nodes.
     allocate(nodeCoords(3*numNodes))
     nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                  1.0,0.0,0.0, & ! node id 2
                  2.0,0.0,0.0, & ! node id 3
                  0.5,1.0,0.0, & ! node id 4
                  1.5,1.0,0.0, & ! node id 5
                  1.0,2.0,0.0, & ! node id 6
                  0.5,0.5,1.0, & ! node id 7
                  1.0,0.5,1.0, & ! node id 8
                  1.5,0.5,1.0, & ! node id 9
                  1.0,1.5,1.0/)  ! node id 10

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
     elemTypes=ESMF_MESHELEMTYPE_TETRA
                 

     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numElems))
     elemConn=(/1,2,7,4, &  ! elem id 1
                2,3,9,5, &  ! elem id 2
                2,5,8,4, &  ! elem id 3
                4,5,10,6/)  ! elem id 4

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,7/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes)) 
       nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                    1.0,0.0,0.0, & ! node id 2
                    0.5,1.0,0.0, & ! node id 4
                    0.5,0.5,1.0/) ! node id 7
 


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 7

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,9/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 3D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                    2.0,0.0,0.0, & ! node id 3
                    1.5,1.0,0.0, & ! node id 5
                    1.5,0.5,1.0/) ! node id 9


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    2, & ! node id 5
                    1/)  ! node id 9

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 2

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 2

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/2,4,5,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                  0.5,1.0,0.0, & ! node id 4
                  1.5,1.0,0.0, & ! node id 5
                  1.0,0.5,1.0/) ! node id 8



        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 2
                     0, & ! node id 4
                     2, & ! node id 5
                     2/)  ! node id 8

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 3

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,3,4,2/) ! elem id 3

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,6,10/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/0.5,1.0,0.0, & ! node id 4
                     1.5,1.0,0.0, & ! node id 5
                     1.0,2.0,0.0, & ! node id 6
                     1.0,1.5,1.0/)  ! node id 10



        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     2, & ! node id 5
                     3, & ! node id 6
                     3/)  ! node id 10
 
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 4

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 4

       endif
    endif


   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=3,spatialDim=3, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, computationalLBound=clbnd(1:1), computationalUBound=cubnd(1:1), &
                     farrayPtr=farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

!  write(*,*) localPet,"[",clbnd(1:1),cubnd(1:1),"]"

!  write(*,*) localPet, "0: fptr=",farrayPtr1D

  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(3*i1-2)
        y=nodeCoords(3*i1-1)
        z=nodeCoords(3*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y+z

!        write(*,*) localPet,"i2",i2,"i1",i1,"id:",nodeIds(i1)," Own:", &
!                   nodeOwners(i1), farrayPtr1D(i2)

        ! Advance to next owner
        i2=i2+1
     endif
  enddo

 ! write(*,*) localPet, "1: fptr=",farrayPtr1D


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


 !  call ESMF_MeshWrite(srcMesh,"srcMesh",rc=localrc)


  ! Establish the resolution of the dst grids
  dst_nx = 10
  dst_ny = 10
  dst_nz = 5

  ! Establish the coordinates of the dst grids
  dst_minx = 0.0
  dst_miny = 0.0
  dst_minz = 0.0
  
  dst_maxx = 2.0
  dst_maxy = 2.0
  dst_maxz = 1.0

  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/dst_nx,dst_ny,dst_nz/), &
!              coordSys=ESMF_COORDSYS_CART, regDecomp=(/petCount,1,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
       coordSys=ESMF_COORDSYS_CART, regDecomp=(/2,2,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
! DOESN'T WORK WITH 4 PETS? coordSys=ESMF_COORDSYS_CART, regDecomp=(/2,2,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)

  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Set source coordinates
        farrayPtrXC(i1,i2,i3) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2,i3) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny
        farrayPtrZC(i1,i2,i3) = ((dst_maxz-dst_minz)*REAL(i3-1)/REAL(dst_nz-1))+dst_minz

        ! initialize destination field
        farrayPtr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


!  write(*,*) localPet, "2: fptr=",farrayPtr1D

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  ! Check error
  at_least_one_interp=.false.
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif
   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        !! skip if hasn't been interpolated to 
        !! (All interpolated values should be 20.0 or above)
        if (farrayPtr(i1,i2,i3) <1.0) cycle

        !! mark that at least one point has been interpolated
        at_least_one_interp=.true.

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2,i3)-(20.0+farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+ &
                                     farrayPtrZC(i1,i2,i3))) > 0.0001) then

!           write(*,*) localPet," error",abs(farrayPtr(i1,i2,i3)), &
!                    abs(20.0+farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+farrayPtrZC(i1,i2,i3))
           correct=.false.	
	endif	
     enddo
     enddo
     enddo

  enddo    ! lDE

  ! If we haven't interpolated at least one point, return an error
  ! (This protects against no interpolation happening resulting in a PASS)
  if (.not. at_least_one_interp) then
     correct=.false.
!     write(*,*) "Nothing interpolated"
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


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

  end subroutine test_regridTetMeshToGrid3D

  subroutine test_regridExtraFieldDim(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: srcXtraField
  type(ESMF_Field) :: dstXtraField
  type(ESMF_Field) :: xdstXtraField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: srcXtraPtr(:,:,:),dstXtraPtr(:,:,:)
  real(ESMF_KIND_R8), pointer :: xdstXtraPtr(:,:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer :: num_extra

  real(ESMF_KIND_R8) :: theta, phi
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
    ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount

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



  ! Establish the resolution of the grids
  src_nx = 27
  src_ny = 27

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 20
  dst_ny = 20

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! extra dimensions for field
  num_extra=5

  
  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
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


   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields with extra dimensions
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   srcXtraField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", &
                         ungriddedLBound=(/1/), ungriddedUBound=(/num_extra/), &
                         gridToFieldMap=(/2,3/), &
                         rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   dstXtraField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", &
                         ungriddedLBound=(/1/), ungriddedUBound=(/num_extra/),&
                         gridToFieldMap=(/2,3/), &
                         rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  xdstXtraField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", &
                         ungriddedLBound=(/1/), ungriddedUBound=(/num_extra/), &
                         gridToFieldMap=(/2,3/), &
                         rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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
  ! arrayB
  call ESMF_FieldGet(dstField, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Construct 2D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
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

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

     enddo
     enddo

     ! get src pointer
     call ESMF_FieldGet(srcXtraField, lDE, srcXtraPtr,  &
          computationalLBound=fclbnd, computationalUBound=fcubnd, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set interpolated function
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)
     do i3=fclbnd(3),fcubnd(3)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i2,i3))
        phi = DEG2RAD*(90.-farrayPtrYC(i2,i3))

        srcXtraPtr(i1,i2,i3) = 10.0*REAL(i1)*(2. + cos(theta)**2.*cos(2.*phi))

     enddo
     enddo
     enddo


  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

     enddo
     enddo


     call ESMF_FieldGet(dstXtraField, lDE, dstXtraPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstXtraField, lDE, xdstXtraPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)
     do i3=fclbnd(3),fcubnd(3)

        ! initialize destination field
        dstXtraPtr(i1,i2,i3)=0.0

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i2,i3))
        phi = DEG2RAD*(90.-farrayPtrYC(i2,i3))

        xdstXtraPtr(i1,i2,i3) = 10.0*REAL(i1)*(2. + cos(theta)**2.*cos(2.*phi))

     enddo
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
  call ESMF_FieldRegrid(srcXtraField, dstXtraField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check results
  do lDE=0,localDECount-1

     ! Get interpolated dst field
     call ESMF_FieldGet(dstXtraField, lDE, dstXtraPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstXtraField, lDE, xdstXtraPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)
     do i3=fclbnd(3),fcubnd(3)

        if (xdstXtraPtr(i1,i2,i3) .ne. 0.0) then
           if (abs(dstXtraPtr(i1,i2,i3)-xdstXtraPtr(i1,i2,i3))/abs(dstXtraPtr(i1,i2,i3)) &
                .gt. 0.05) then
              correct=.false.
!              write(*,*) i1,i2,i3,"::",(dstXtraPtr(i1,i2,i3)-xdstXtraPtr(i1,i2,i3))/xdstXtraPtr(i1,i2,i3)
           endif
        else
           if (abs(dstXtraPtr(i1,i2,i3)-xdstXtraPtr(i1,i2,i3)) &
                .gt. 0.05) then
              correct=.false.
           endif
        endif
     enddo
     enddo
     enddo

  enddo    ! lDE


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

   call ESMF_FieldDestroy(srcXtraField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstXtraField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstXtraField, rc=localrc)
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


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridExtraFieldDim

 subroutine test_regridSwitchedIndices(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: grid360
  type(ESMF_Grid) :: grid180
  type(ESMF_Field) :: srcField360
  type(ESMF_Field) :: dstField360
  type(ESMF_Field) :: field180
  type(ESMF_Field) :: errorField
  type(ESMF_Array) :: array180
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: lonArray360
  type(ESMF_Array) :: srcArray360, dstArray360
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandle1
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:),errorfarrayPtr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
  real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: rangle, xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: RAD2DEG

  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids

  dst_nx = 90
  dst_ny = 50

  src_nx = 90
  src_ny = 50

  ! setup source grid
  grid360=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                  coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                                  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  grid180=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField360 = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", &
                         gridToFieldMap=(/1,2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField360 = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", &
                         gridToFieldMap=(/1,2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", &
                         gridToFieldMap=(/1,2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   field180 = ESMF_FieldCreate(grid180, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", &
                         gridToFieldMap=(/2,1/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(grid360, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(grid180, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(grid360, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! array180
  call ESMF_FieldGet(field180, array=array180, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArray360
  call ESMF_FieldGet(srcField360, array=srcArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! dstArray360
  call ESMF_FieldGet(dstField360, array=dstArray360, rc=localrc)
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



  !! get longitude array
  call ESMF_GridGetCoord(grid360, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=lonArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Write results to a mesh
  num_arrays = 1

! Test interpolation on the sphere
! Set the source grid coordinates to be a 0 to 360 grid

  src_dx = 360./src_nx
  src_dy = 180./src_ny

  DEG2RAD = 3.14159265/180.0
  RAD2DEG = 1./DEG2RAD

  ! Get memory and set coords for src
  do lDE=0,localDECount-1

     !! get coord 1
     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcField360, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field pointer
     call ESMF_FieldGet(dstField360, lDE, farrayPtr2,   rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        ! (something relatively smooth, that varies everywhere)
        farrayPtr(i1,i2) = x+y+z+15.0

         ! initialize src destination field
         farrayPtr2(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  dst_dx = 360./dst_nx
  dst_dy = 180./dst_ny

  rangle = DEG2RAD*20.

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1

     !! get coord 1
     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(field180, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set destination coordinates as -180 to 180
        farrayPtrXC(i1,i2) = -180. + (REAL(i1-1)*dst_dx)
        farrayPtrYC(i1,i2) = -90.  + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! init destination mesh to 0
        farrayPtr(i2,i1) = 0.

     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the 0 to 360 grid to the -180 to 180 grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcField360, dstField=field180, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcField360, field180, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  !!!!!!!! Regrid back from the -180 to 180 grid to the 0 to 360 grid
  ! Regrid store
  call ESMF_FieldRegridStore(field180, dstField=dstField360, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(field180, dstField360, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get src Field
     call ESMF_FieldGet(srcField360, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field
     call ESMF_FieldGet(dstField360, lDE, farrayPtr2,  rc=localrc)
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


     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (farrayPtr(i1,i2) .gt. 1e-12) then
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2) - farrayPtr2(i1,i2))/farrayPtr(i1,i2))
        else
           errorfarrayPtr(i1,i2)=(farrayPtr(i1,i2) - farrayPtr2(i1,i2))
        endif
        if (ABS(errorfarrayPtr(i1,i2)) .gt. 0.01) then
		  write(*,*) i1,i2,"::", &
                     errorfarrayPtr(i1,i2), &
                     "::", farrayPtr(i1,i2),"::",farrayPtr2(i1,i2)
          correct=.false. 
        endif

     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, grid360, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray360, dstArray360, errorArray, lonArray360, rc=localrc, &
               spherical=spherical_grid)
write(*,*) "LOCALRC=",localrc
  call ESMF_MeshIO(vm, grid180, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", array180, rc=localrc, &
               spherical=spherical_grid)
write(*,*) "LOCALRC=",localrc
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField360, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField360, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(field180, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(grid360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(grid180, rc=localrc)
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

  end subroutine test_regridSwitchedIndices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! THIS TEST ONLY WORKS WITH A CONSTANT FUNCTION !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine test_regridSwitchedIndicesII(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: srcPtr(:,:),dstPtr(:,:), xdstPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny

  real(ESMF_KIND_R8) :: theta, phi, x, y, z
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
    ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount

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



  ! Establish the resolution of the grids
  src_nx = 27
  src_ny = 27

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 20
  dst_ny = 20

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny


  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", &
                         gridToFieldMap=(/1,2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", &
                         gridToFieldMap=(/2,1/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", &
                         gridToFieldMap=(/2,1/), rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! Construct 2D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1

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

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

     enddo
     enddo

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, srcPtr,  &
          computationalLBound=fclbnd, computationalUBound=fcubnd, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set interpolated function
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)

       ! Set the source to be a function of the x,y coordinate
        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

		x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        !srcPtr(i1,i2) = 10.0*REAL(i1)*(2. + cos(theta)**2.*cos(2.*phi))
        !srcPtr(i1,i2) = 10.0*(2. + cos(theta)**2.*cos(2.*phi))
        srcPtr(i1,i2) = 15.0 + x + y + z
        !srcPtr(i1,i2) = 10.0

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1

     !! get coords
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

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

     enddo
     enddo


     call ESMF_FieldGet(dstField, lDE, dstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xdstPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)

        ! initialize destination field
        dstPtr(i1,i2)=0.0

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

		x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        !xdstPtr(i1,i2) = 10.0*REAL(i1)*(2. + cos(theta)**2.*cos(2.*phi))
        !xdstPtr(i1,i2) = 10.0*(2. + cos(theta)**2.*cos(2.*phi))
        xdstPtr(i1,i2) = 15.0 + x + y + z
        !xdstPtr(i1,i2) = 10.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
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


  ! Check results
  do lDE=0,localDECount-1

     ! Get interpolated dst field
     call ESMF_FieldGet(dstField, lDE, dstPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xdstPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)

        if (xdstPtr(i1,i2) .ne. 0.0) then
           if (abs(dstPtr(i1,i2)-xdstPtr(i1,i2))/abs(dstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
              write(*,*) i1,i2,"::",abs(dstPtr(i1,i2)-xdstPtr(i1,i2))/abs(xdstPtr(i1,i2))
           endif
        else
           if (abs(dstPtr(i1,i2)-xdstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
              write(*,*) i1,i2,"::",abs(dstPtr(i1,i2)-xdstPtr(i1,i2))
           endif
        endif
     enddo
     enddo

  enddo    ! lDE


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

   call ESMF_FieldDestroy(xdstField, rc=localrc)
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


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridSwitchedIndicesII

  subroutine test_regridSphNearest(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: srcPtr(:,:),dstPtr(:,:)
  real(ESMF_KIND_R8), pointer :: xdstPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny

  real(ESMF_KIND_R8) :: theta, phi
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
    ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init flags
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



  ! Establish the resolution of the grids
  ! Make the same resolution, so src and dst 
  ! fall on top of each other
  src_nx = 20
  src_ny = 20

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 20
  dst_ny = 20

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny


  
  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
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



  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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
  ! arrayB
  call ESMF_FieldGet(dstField, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Construct 2D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
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
     call ESMF_FieldGet(srcField, lDE, srcPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        srcPtr(i1,i2) = (2. + cos(theta)**2.*cos(2.*phi))
        !srcPtr(i1,i2) = 20.0

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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

     call ESMF_FieldGet(dstField, lDE, dstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xdstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dst coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! initialize destination field
        dstPtr(i1,i2)=0.0

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        ! After calculating field shift coords slighlty to be close, but not exact
        ! to make test more interesting
        farrayPtrXC(i1,i2) = farrayPtrXC(i1,i2) + 2.0

        xdstPtr(i1,i2) = (2. + cos(theta)**2.*cos(2.*phi))
        ! xdstPtr(i1,i2) = 20.0
     enddo
     enddo
  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
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


  ! Check results
  do lDE=0,localDECount-1

     ! Get interpolated dst field
     call ESMF_FieldGet(dstField, lDE, dstPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xdstPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)

        if (xdstPtr(i1,i2) .ne. 0.0) then
           if (abs((dstPtr(i1,i2)-xdstPtr(i1,i2))/xdstPtr(i1,i2)) .gt. 0.05) then
              correct=.false.
              write(*,*) i1,i2,"::",dstPtr(i1,i2),xdstPtr(i1,i2), &
                 abs((dstPtr(i1,i2)-xdstPtr(i1,i2))/xdstPtr(i1,i2))
           endif
        else
           if (abs(dstPtr(i1,i2)-xdstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
           endif
        endif
     enddo
     enddo

  enddo    ! lDE


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

   call ESMF_FieldDestroy(xdstField, rc=localrc)
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


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridSphNearest


  subroutine test_regridSphNearestDToS(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: srcPtr(:,:),dstPtr(:,:)
  real(ESMF_KIND_R8), pointer :: xdstPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny

  real(ESMF_KIND_R8) :: theta, phi
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
    ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init flags
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



  ! Establish the resolution of the grids
  ! Make the same resolution, so src and dst 
  ! fall on top of each other
  src_nx = 20
  src_ny = 20

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 20
  dst_ny = 20

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny


  
  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
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



  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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
  ! arrayB
  call ESMF_FieldGet(dstField, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Construct 2D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
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
     call ESMF_FieldGet(srcField, lDE, srcPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        srcPtr(i1,i2) = (2. + cos(theta)**2.*cos(2.*phi))
        !srcPtr(i1,i2) = 20.0

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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

     call ESMF_FieldGet(dstField, lDE, dstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xdstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! initialize destination field
        dstPtr(i1,i2)=0.0

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        ! After calculating field shift coords slighlty to be close, but not exact
        ! to make test more interesting
        farrayPtrXC(i1,i2) = farrayPtrXC(i1,i2) + 2.0

        xdstPtr(i1,i2) = (2. + cos(theta)**2.*cos(2.*phi))
        ! xdstPtr(i1,i2) = 20.0
     enddo
     enddo
  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_DTOS, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
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


  ! Check results
  do lDE=0,localDECount-1

     ! Get interpolated dst field
     call ESMF_FieldGet(dstField, lDE, dstPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xdstPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)

        if (xdstPtr(i1,i2) .ne. 0.0) then
           if (abs(dstPtr(i1,i2)-xdstPtr(i1,i2))/abs(dstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
              write(*,*) i1,i2,"::",dstPtr(i1,i2),xdstPtr(i1,i2),(dstPtr(i1,i2)-xdstPtr(i1,i2))/xdstPtr(i1,i2)
           endif
        else
           if (abs(dstPtr(i1,i2)-xdstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
           endif
        endif
     enddo
     enddo

  enddo    ! lDE


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

   call ESMF_FieldDestroy(xdstField, rc=localrc)
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


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridSphNearestDToS




  subroutine test_unmappedDstList(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: srcPtr(:,:),dstPtr(:,:)
  real(ESMF_KIND_R8), pointer :: xdstPtr(:,:)
  integer(ESMF_KIND_I4), pointer :: unmappedDstList(:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  logical, pointer :: isVerified(:)

  real(ESMF_KIND_R8) :: theta, phi, dx
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
    ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount

  ! result code
  integer :: finalrc

  integer :: num_unmappedDstList
  integer :: seqInd,i
  logical :: found
  
  ! init flags
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



  ! Establish the resolution of the grids
  ! Make the same resolution, so src and dst 
  ! fall on top of each other
  src_nx = 27
  src_ny = 27

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 20
  dst_ny = 20

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny


  
  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
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



  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


 ! Allocate Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
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
  ! arrayB
  call ESMF_FieldGet(dstField, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Construct 2D Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
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


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, srcPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        srcPtr(i1,i2) = 200.0

	! set mask region around 180
	dx=farrayPtrXC(i1,i2)-180.0
	if (abs(dx) < 45.0) then
           srcMask(i1,i2) = 1
	else
           srcMask(i1,i2) = 0
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

     call ESMF_FieldGet(dstField, lDE, dstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xdstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! initialize destination field
        dstPtr(i1,i2)=0.0

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        xdstPtr(i1,i2) = (2. + cos(theta)**2.*cos(2.*phi))
        ! xdstPtr(i1,i2) = 20.0
     enddo
     enddo
  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif




  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, srcMaskValues=(/1/), &
          dstField=dstField, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          unmappedDstList=unmappedDstList, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
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

   !!!!!  Check results !!!!!!!!!

   ! Allocate and init. verify list
   num_unmappedDstList=size(unmappedDstList)
   allocate(isVerified(num_unmappedDstList))
   isVerified=.false.

  do lDE=0,localDECount-1

     ! Get interpolated dst field
     call ESMF_FieldGet(dstField, lDE, dstPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, &
          rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Loop through and make sure the unmapped dst points are in list
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        if (dstPtr(i1,i2) <1.0) then
           seqInd=i1+(i2-1)*dst_nx

           found=.false.
           do i=1,num_unmappedDstList
              if (seqInd .eq. unmappedDstList(i)) then
                 isVerified(i)=.true.
                 found=.true.
                 exit
              endif
           enddo
           if (.not. found) then
              correct=.false.
           endif
        endif
     enddo
     enddo

  enddo    ! lDE

  ! Check if any haven't been verified
  found=.false.
  do i=1,num_unmappedDstList
     if (.not. isVerified(i)) then
        found=.true.
        exit
     endif
  enddo
  if (found) then
     correct=.false.
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

   call ESMF_FieldDestroy(xdstField, rc=localrc)
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

  ! Free isVerified
  deallocate(isVerified)


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_unmappedDstList


 subroutine test_regridNearestMeshToMesh(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
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
     nodeCoords=(/-0.0,-0.0, & ! node id 1
                   1.0,-0.0, & ! node id 2
                   2.0,-0.0, & ! node id 3
                  -0.0, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.0, 1.0, & ! node id 6
                  -0.0, 2.0, & ! node id 7
                   1.0, 2.0, & ! node id 8
                   2.0, 2.0 /) ! node id 9

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
       nodeCoords=(/-0.0, -0.0, & ! node id 1
                     1.0, -0.0, & ! node id 2
                    -0.0,  1.0, & ! node id 4
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
       nodeCoords=(/1.0,-0.0, & ! node id 2
                    2.0,-0.0, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.0, 1.0 /) ! node id 6

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
        nodeCoords=(/-0.0,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.0,2.0, & ! node id 7
                      1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y

        ! Advance to next owner
        i2=i2+1
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
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
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
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

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
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

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
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
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

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

	!! if error is too big report an error
	if ( abs( farrayPtr1D(i2)-(x+y+20.0) ) > 0.0001) then
           correct=.false.	
           write(*,*) localPet,nodeIds(i1),"::",farrayPtr1D(i2),(x+y+20.0)
	endif	

        ! Advance to next owner
        i2=i2+1
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




#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

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


  ! Free the grids
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

 end subroutine test_regridNearestMeshToMesh

 subroutine test_regrid2TileDG(rc)
   integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: srcDistgrid
  type(ESMF_DELayout) :: delayout
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: src_nx(2), src_ny(2)
  integer :: src_minx(2), src_miny(2)
  integer :: src_maxx(2), src_maxy(2)

  integer :: dst_nx, dst_ny
  integer :: dst_minx, dst_miny
  integer :: dst_maxx, dst_maxy


  integer :: tile
  integer :: tile_nx, tile_ny
  integer :: tile_minx, tile_miny
  integer :: tile_maxx, tile_maxy


  integer :: num_arrays

  real(ESMF_KIND_R8) :: dx,dy
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD
  
  integer :: spherical_grid

  integer :: localPet, petCount

  type(ESMF_DistGridConnection) :: connectionList(2)
  integer :: minIndex(2,2), maxIndex(2,2)
  integer :: regDecomp(2,2)
  type(ESMF_Decomp_Flag) :: decomp(2,2)

  integer :: deCount
  integer, allocatable :: localDEtoDEMap(:), deToTileMap(:)


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

  ! Establish the resolution of the grids
  ! tile 1
  src_nx(1) = 20
  src_ny(1) = 20

  src_minx(1) = 0.0
  src_miny(1) = 0.0
  
  src_maxx(1) = 10.0
  src_maxy(1) = 10.0

  ! tile 2
  src_nx(2) = 20
  src_ny(2) = 20

  src_minx(2) = 11.0
  src_miny(2) = 11.0
  
  src_maxx(2) = 21.0
  src_maxy(2) = 21.0


  dst_nx = 20
  dst_ny = 20

  dst_minx = 0.0
  dst_miny = 0.0
  
  dst_maxx = 21.0
  dst_maxy = 21.0




  ! Create connectionList
  ! periodicity
  call ESMF_DistgridConnectionSet(connection=connectionList(1), &
        tileIndexA=1,tileIndexB=2, &
        positionVector=(/src_nx(1),0/), &
        rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_DistgridConnectionSet(connection=connectionList(2), &
        tileIndexA=2,tileIndexB=1, &
        positionVector=(/-src_nx(1),0/), &
        rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


  ! Setup index space
  minIndex(:,1)=(/1,1/)
  maxIndex(:,1)=(/src_nx(1),src_ny(1)/)
  regDecomp(:,1)=(/petCount,1/)


  minIndex(:,2)=(/1,1/)
  maxIndex(:,2)=(/src_nx(2),src_ny(2)/)
  regDecomp(:,2)=(/petCount,1/)

  decomp(:,:)=ESMF_DECOMP_BALANCED

  ! Create source distgrid
  srcDistgrid=ESMF_DistgridCreate(minIndexPTile=minIndex, maxIndexPTile=maxIndex, regDecompPTile=regDecomp, &
                              decompflagPTile=decomp,indexflag=ESMF_INDEX_GLOBAL, &
                              connectionList=connectionList,                 &
                              rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


  ! setup source grid
  srcGrid=ESMF_GridCreate(distgrid=srcDistgrid, indexflag=ESMF_INDEX_GLOBAL, &
                          coordSys=ESMF_COORDSYS_CART, &
                          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return




  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get information for going from localDE to Tile
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_DistgridGet(srcDistgrid, delayout=delayout, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  allocate(localDEtoDEMap(srclocalDECount))  

  call ESMF_DELayoutGet(delayout, deCount=deCount, localDeToDeMap=localDeToDEMap, &
                        rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  allocate(deToTileMap(deCount))  


  call ESMF_DistgridGet(srcDistgrid, deToTileMap=detoTileMap, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


     !! Get Tile from localDE


     !! Set values based on tile
     tile_nx=src_nx(tile)
     tile_ny=src_ny(tile)

     tile_minx=src_minx(tile)
     tile_maxx=src_maxx(tile)

     tile_miny=src_miny(tile)
     tile_maxy=src_maxy(tile)


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((tile_maxx-tile_minx)*REAL(i1-1)/REAL(tile_nx-1))+tile_minx
        farrayPtrYC(i1,i2) = ((tile_maxy-tile_miny)*REAL(i2-1)/REAL(tile_ny-1))+tile_miny

        ! set src data
        farrayPtr(i1,i2) = farrayPtrXC(i1,i2) + farrayPtrYC(i1,i2) + 20.0

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! set source coordinates
        farrayPtrXC(i1,i2) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny

        ! set expected result field
        xfarrayPtr(i1,i2) = farrayPtrXC(i1,i2) + farrayPtrYC(i1,i2) + 20.0

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="srcGrid", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
        filename="dstGrid", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#endif

! LEAVE HERE AND JUST WRITE OUT THE GRIDS AS A TEST FOR NOW

  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

return 


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


  ! Check results
  do lDE=0,dstlocalDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2)) .gt. 0.001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE



#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.false., isLatLonDeg=.true., filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.true., isLatLonDeg=.true., filename="dstGrid", array1=dstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


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


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#if 0
  ! Free the srcDistgrid
  call ESMF_DistgridDestroy(srcDistgrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regrid2TileDG


 subroutine test_regridMeshToMeshMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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

  integer, pointer :: nodeIds(:),nodeOwners(:),nodeMask(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
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

     ! Allocate and fill the node mask array.
     ! Mask out node 9
     allocate(nodeMask(numNodes))
     nodeMask=(/0,0,0,0,0,0,0,0,1/) 


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

       ! Allocate and fill the node mask array.
       allocate(nodeMask(numNodes))
       nodeMask=(/0, & ! node id 1
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

       ! Allocate and fill the node mask array.
       allocate(nodeMask(numNodes))
       nodeMask=(/0, & ! node id 2
                  0, & ! node id 3
                  0, & ! node id 5
                  0/)  ! node id 6

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

        ! Allocate and fill the node mask array.
        allocate(nodeMask(numNodes))
        nodeMask=(/0, & ! node id 4
                   0, & ! node id 5
                   0, & ! node id 7
                   0/)  ! node id 8

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

        ! Allocate and fill the node mask array.
        allocate(nodeMask(numNodes))
        nodeMask=(/0, & ! node id 5
                   0, & ! node id 6
                   0, & ! node id 8
                   1/)  ! node id 9
 
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, nodeMask=nodeMask, &
         elementIds=elemIds, elementTypes=elemTypes, &
         elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y

        ! Set Node 9 to big value, so that it can be detected
        if (nodeIds(i1) .eq. 9) then
           farrayPtr1D(i2) = 10000.0
        endif

        ! Advance to next owner
        i2=i2+1
     endif
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   deallocate(nodeMask)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


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
                   1.0,1.0, & ! node id 5
                   2.0,1.0, & ! node id 6
                   0.0,2.0, & ! node id 7
                   1.0,2.0, & ! node id 8
                   2.0,2.0 /) ! node id 9

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Allocate and fill the node mask array.
     ! (Mask point sticking out of src grid and point
     !  uncovered by masked src point)
     allocate(nodeMask(numNodes))
     nodeMask=(/2,0,0,0,0,0,0,0,2/) 

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
                     1.0, 1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Allocate and fill the node Mask array.
       allocate(nodeMask(numNodes))
       nodeMask=(/2, & ! node id 1
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
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Allocate and fill the node mask array.
       allocate(nodeMask(numNodes))
       nodeMask=(/0, & ! node id 2
                  0, & ! node id 3
                  0, & ! node id 5
                  0/)  ! node id 6

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
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Allocate and fill the node mask array.
        allocate(nodeMask(numNodes))
        nodeMask=(/0, & ! node id 4
                   0, & ! node id 5
                   0, & ! node id 7
                   0/)  ! node id 8

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
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9

        ! Allocate and fill the node Mask array.
        allocate(nodeMask(numNodes))
        nodeMask=(/0, & ! node id 5
                   0, & ! node id 6
                   0, & ! node id 8
                   2/)  ! node id 9  (Mask out point uncovered by masked src)


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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
   endif


  ! Create Mesh structure in 1 step
  dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
       coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, nodeMask=nodeMask, &
         elementIds=elemIds, elementTypes=elemTypes, &
         elementConn=elemConn, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
     endif

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          dstMaskValues=(/2/), &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          ! NO IGNORE, BECAUSE WANT TO DETECT DST MASK
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  i2=1
  do i1=1,numNodes

     ! if local
     if (nodeOwners(i1) .eq. localPet) then

        ! not masked
        if (nodeMask(i1) .eq. 0) then  

           ! Get coordinates
           x=nodeCoords(2*i1-1)
           y=nodeCoords(2*i1)

           !! if error is too big report an error
           !! (10000.0 will trigger error here)
           if ( abs( farrayPtr1D(i2)-(x+y+20.0) ) > 0.0001) then
              write(*,*) "ERROR:",nodeIds(i1),farrayPtr1D(i2),(x+y+20.0)
              correct=.false.	
           endif
        endif

        ! Advance to next owner
        i2=i2+1
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


  ! Free the grids
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

 end subroutine test_regridMeshToMeshMask


#define FILE "ESMF_FieldRegridUTest.F90"

  subroutine test_regridSrcHoles(rc)
    integer, intent(out)  :: rc
    
    integer,  parameter           :: iMax = 200
    integer,  parameter           :: jMax = 100
    real(ESMF_KIND_R8), parameter :: lonMinS = 0.d0, lonMaxS = 210.d0
    real(ESMF_KIND_R8), parameter :: latMinS = -40.d0, latMaxS = 50.d0
    real(ESMF_KIND_R8), parameter :: lonMinD = 10.d0, lonMaxD = 200.d0
    real(ESMF_KIND_R8), parameter :: latMinD = -30.d0, latMaxD = 40.d0
    
    type(ESMF_VM)         :: vm
    integer               :: petCount, localPet
    integer               :: i, j
    integer, allocatable  :: deBlockList(:,:,:)
    type(ESMF_DistGrid)   :: srcDistGrid, dstDistGrid
    type(ESMF_Grid)       :: srcGrid, dstGrid
    type(ESMF_Field)      :: srcField, dstField
    real(ESMF_KIND_R8), pointer :: fptr(:,:)
    
    type(ESMF_RouteHandle):: rh
    
    rc = ESMF_SUCCESS
    
    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
    
    call ESMF_VMGet(vm, petCount=petCount, localPet=localPet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
      
    ! --- set up the source side ---

    allocate(deBlockList(2,2,0:petCount-1)) ! dimCount, 2, deCount
    
    ! Set up deBlockList that covers the (1...iMax) x (1...jMax) index space
    ! by a row decomposition along j.
    do i=0, petCount-1
      deBlockList(:,1,i) = (/1,i*jMax/petCount+1/)        ! minIndex DE i
      if (i == petCount-1) then
        deBlockList(:,2,i) = (/iMax,jMax/)                ! maxIndex DE i
      else
        deBlockList(:,2,i) = (/iMax,(i+1)*jMax/petCount/) ! maxIndex DE i
      endif
    enddo
    
#if 0
    ! Modify the deBlockList to have holes in the index space coverage.
    do i=0, petCount-1
      deBlockList(1,1,i) = deBlockList(1,1,i) + 1 ! shift the lower bound 1 up
      deBlockList(2,1,i) = deBlockList(2,1,i) + 2 ! shift the lower bound 2 up
      deBlockList(1,2,i) = deBlockList(1,2,i) - 3 ! shift the upper bound 3 dn
      deBlockList(2,2,i) = deBlockList(2,2,i) - 4 ! shift the lower bound 4 dn
    enddo
#endif

    if (localPet==0) then
      do i=0, petCount-1
        print *, i, "deBlockList:", deBlockList(:,1,i), deBlockList(:,2,i)
      enddo
    endif

    ! Create the srcDistGrid.
    srcDistGrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/iMax,jMax/),&
      deBlockList=deBlockList, indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out

    ! Create the srcGrid.
    srcGrid = ESMF_GridCreate(srcDistGrid, coordSys=ESMF_COORDSYS_SPH_DEG, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out

    ! Add coordinates to the srcGrid.
    call ESMF_GridAddCoord(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out

    ! Access the longitude coordinate pointer in srcGrid and fill.
    call ESMF_GridGetCoord(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
    do j=lbound(fptr,2), ubound(fptr,2)
    do i=lbound(fptr,1), ubound(fptr,1)
      fptr(i,j) = (lonMaxS-lonMinS)/real(iMax) * (i-1) + lonMinS
    enddo
    enddo

    ! Access the latitude coordinate pointer in srcGrid and fill.
    call ESMF_GridGetCoord(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
    do j=lbound(fptr,2), ubound(fptr,2)
    do i=lbound(fptr,1), ubound(fptr,1)
      fptr(i,j) = (latMaxS-latMinS)/real(jMax) * (j-1) + latMinS
    enddo
    enddo
    
    ! Create the srcField.
    srcField =  ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
    
    ! --- set up the destination side ---
    
    ! Create the dstDistGrid with default decomposition (no holes!).
    dstDistGrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/iMax,jMax/),&
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out

    ! Create the dstGrid.
    dstGrid = ESMF_GridCreate(dstDistGrid, coordSys=ESMF_COORDSYS_SPH_DEG, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out

    ! Add coordinates to the dstGrid.
    call ESMF_GridAddCoord(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out

    ! Access the longitude coordinate pointer in dstGrid and fill.
    call ESMF_GridGetCoord(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=1, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
    do j=lbound(fptr,2), ubound(fptr,2)
    do i=lbound(fptr,1), ubound(fptr,1)
      fptr(i,j) = (lonMaxD-lonMinD)/real(iMax) * (i-1) + lonMinD
    enddo
    enddo

    ! Access the latitude coordinate pointer in dstGrid and fill.
    call ESMF_GridGetCoord(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      coordDim=2, farrayPtr=fptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
    do j=lbound(fptr,2), ubound(fptr,2)
    do i=lbound(fptr,1), ubound(fptr,1)
      fptr(i,j) = (latMaxD-latMinD)/real(jMax) * (j-1) + latMinD
    enddo
    enddo
    
    ! Create the dstField.
    dstField =  ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
      indexflag=ESMF_INDEX_GLOBAL, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
    
    ! --- Regridding ---
    
    ! Pre-compute the regrid RouteHandle.
    call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
      routehandle=rh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=FILE)) return ! bail out
    
    !TODO: execute the Regrid and validate the result.
    !TODO: right now it doesn't even make it that far for srcDistGrid w/ holes


  end subroutine test_regridSrcHoles


 subroutine test_regridSphGC(src_nx, src_ny, dst_nx, dst_ny, errTol, rc)
  integer, intent(in) ::  src_nx, src_ny, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: errTol
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: errArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:), errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srcLocalDECount, dstLocalDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD
  real(ESMF_KIND_R8) :: err, relErr,maxRelErr  

  integer :: spherical_grid

  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
!  src_nx = 5
!  src_ny = 5

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny


!  dst_nx = 80
!  dst_ny = 80

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup dest. grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/1,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  errField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="err", rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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

  ! dstArray
  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
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


  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0, srclocalDECount-1
 
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

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact src data
       farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
    !    farrayPtr(i1,i2) = 1.0
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0, dstlocalDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)
  
        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
    !    xfarrayPtr(i1,i2) = 1.0

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.false., isLatLonDeg=.true., filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
	  unmappedaction=ESMF_UNMAPPEDACTION_ERROR, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          lineType=ESMF_LINETYPE_GREAT_CIRCLE, &
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


  ! Check results
  maxRelErr=0.0
  do lDE=0, dstlocalDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! compute relative error
        err=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
          relErr=err/xfarrayPtr(i1,i2)
        else
          relErr=err
        endif
        
        ! Calc Max
        if (relErr > maxRelErr) maxRelErr=relErr 

        ! Set error in field
        errfarrayPtr(i1,i2)=relErr 
        
        ! Return error if relative error too big
        if (relErr > errTol) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE

!  write(*,*) "Max. Rel. Error= ",maxRelErr


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


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

  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridSphGC


 subroutine test_regridMeshToMeshPH(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems
  integer :: numPentElems,numHexElems,numTotElems
  integer :: numElemConn

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

!             Src Mesh
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


  ! Setup Src Mesh
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(numElemConn))
        elemConn=(/1,2,6,5,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y

        ! Advance to next owner
        i2=i2+1
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


!              Dest Mesh
!
!  2.0   7 ------- 8 ------- 9
!        |         |         |
!        |    4    |    5    |
!        |         |         |
!  1.0   4 ------- 5 ------- 6
!        |         |  \   3  |
!        |    1    |    \    |
!        |         |  2   \  |
!  0.0   1 ------- 2 ------- 3
!
!       0.0       1.0        2.0 
! 
!        Node Id labels at corners
!       Element Id labels in centers

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
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
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
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

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
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

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
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
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

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

	!! if error is too big report an error
	if ( abs( farrayPtr1D(i2)-(x+y+20.0) ) > 0.0001) then
           correct=.false.	
	endif	

        ! Advance to next owner
        i2=i2+1
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



  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, dstMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

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


  ! Free the grids
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

 end subroutine test_regridMeshToMeshPH



 subroutine test_regridMeshToMeshCenter(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr1D(:)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: relErr
  character(len=ESMF_MAXSTR) :: string
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer, pointer :: elemMasks(:)
  integer :: numNodes, numElems
  integer :: numElemConns, numTriElems, numQuadElems

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

   !!!! Create Src Mesh !!!!
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
  !         |    7    |    8     |     /    |
  !         |         |          |  /    9  |
  !   2.0   9 ------- 10 ------- 11 ------- 12
  !         |         |          |          |
  !         |    4    |    5     |    6     |
  !         |         |          |          |
  !   1.0   5 ------- 6 -------- 7 -------- 8
  !         |         |          |          |
  !         |    1    |    2     |    3     |
  !         |         |          |          |
  !   0.0   1 ------- 2 -------- 3 -------- 4
  !
  !        0.0       1.0        2.0        3.0
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

      !! Masking
      allocate(elemMasks(numElems))
      elemMasks=(/1,0,0,0,0,0,0,0,0,0/) 

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

      !! elem masking
      allocate(elemMasks(numElems))
      elemMasks=(/1/) 


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

      !! elem Masks
      allocate(elemMasks(numElems))
      elemMasks=(/0,0/)

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

      !! elem masks
      allocate(elemMasks(numElems))
      elemMasks=(/0,0,0,0/)

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

      !! elem Masks
      allocate(elemMasks(numElems))
      elemMasks=(/0,0,0/) 

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
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, elementMask=elemMasks, &
        rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif



  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  do i1=1,numElems
     ! Get coordinates
     x=elemCoords(2*i1-1)
     y=elemCoords(2*i1)
     
     ! Set source function
     farrayPtr1D(i1) = 20.0+x+y
     !write(*,*) localPet,":: Src=",farrayPtr1D(i1)

     ! if masked make a bad value
     if (elemMasks(i1) .eq. 1) then
        farrayPtr1D(i1) = 10000.0
     endif
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemMasks)
   deallocate(elemConn)


   !!!! Create Dest Mesh !!!!
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

      !! Masking
      allocate(elemMasks(numElems))
      elemMasks=(/1,0,0,0,0,0,0,0,0,0/) 

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

      !! elem masks
      allocate(elemMasks(numElems))
      elemMasks=(/1/) 

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

      !! elem masking
      allocate(elemMasks(numElems))
      elemMasks=(/0,0/)

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

      !! elem masks
      allocate(elemMasks(numElems))
      elemMasks=(/0,0,0,0/)

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

      !! elem ids
      allocate(elemMasks(numElems))
      elemMasks=(/0,0,0/) 

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
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, elementMask=elemMasks, &
        rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="dst", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0

  ! Exact dest field
  xdstField = ESMF_FieldCreate(dstMesh, arrayspec, meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="dst", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Should only be 1 localDE
  call ESMF_FieldGet(xdstField, 0, xfarrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  do i1=1,numElems
     ! Get coordinates
     x=elemCoords(2*i1-1)
     y=elemCoords(2*i1)
     
     ! Set source function
     xfarrayPtr1D(i1) = 20.0+x+y
     !write(*,*) localPet,":: XDst=",xfarrayPtr1D(i1)

     ! If masked then will remain 0.0
     if (elemMasks(i1) .eq. 1) then
        xfarrayPtr1D(i1) = 0.0
     endif
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)
   
   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemCoords)
   deallocate(elemMasks)
   deallocate(elemConn)



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          srcMaskValues=(/1/), &
          dstField=dstField, &
          dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! Should only be 1 localDE
  call ESMF_FieldGet(xdstField, 0, xfarrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  do i1=1,numElems
     !! Calculate error
     if (xfarrayPtr1D(i1) .ne. 0.0) then
        relErr=abs(farrayPtr1D(i1)-xfarrayPtr1D(i1))/xfarrayPtr1D(i1)
     else
        relErr=abs(farrayPtr1D(i1)-xfarrayPtr1D(i1))
     endif

     !! if error is too big report an error
     if (relErr > 0.0001) then
        correct=.false.	
        !write(*,*) localPet,"::",i1,farrayPtr1D(i1),xfarrayPtr1D(i1)
     endif
  enddo



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


  ! Free the grids
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

 end subroutine test_regridMeshToMeshCenter


 subroutine test_regridMeshSph3x3ToGrid(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: dst_minx,dst_miny
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy

  real(ESMF_KIND_R8) :: x,y

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD
  real(ESMF_KIND_R8) :: err, relErr
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numElemConns,numQuadElems,numTriElems, numTotElems
  real(ESMF_KIND_R8), pointer :: elemCoords(:)

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

  src_nx = 3
  src_ny = 3

  ! Establish the resolution of the grids
  dst_nx = 15
  dst_ny = 15

  ! Establish the coordinates of the grids
  dst_minx = 0.1
  dst_miny = 0.1
  
  dst_maxx = 2.9
  dst_maxy = 2.9

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


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
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, &
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        rc=localrc)

   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        lon=nodeCoords(2*i1-1)
        lat=nodeCoords(2*i1)

        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact src data
        farrayPtr1D(i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! Advance to next owner
        i2=i2+1
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


  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/2,2/), &
                                  coordSys=ESMF_COORDSYS_SPH_DEG,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
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
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

     ! Set destination coordinates
        farrayPtrXC(i1,i2) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny

        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYc(i1,i2)

        theta = DEG2RAD*(lon)
	phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

       ! compute relative error
        err=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
          relErr=err/xfarrayPtr(i1,i2)
        else
          relErr=err
        endif

        ! Return error if relative error too big
        if (relErr > .001) then
            correct=.false.
        endif

     enddo
     enddo

  enddo    ! lDE

  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, srcMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

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

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridMeshSph3x3ToGrid


 subroutine test_regridMeshToGridSph3D(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:),farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:),farrayPtr2(:,:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:,:)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(3)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(3)
  character(len=ESMF_MAXSTR) :: string
  integer dst_nx,dst_ny,dst_nz
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy,dz
  
  real(ESMF_KIND_R8) :: dst_minx,dst_miny,dst_minz
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy,dst_maxz

  real(ESMF_KIND_R8) :: x,y,z
  real(ESMF_KIND_R8) :: lat_rad, lon_rad

  real(ESMF_KIND_R8) :: err, relErr, maxRelErr

  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems

  ! result code
  integer :: finalrc
  
  ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: &
     DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

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
    rc=ESMF_SUCCESS
    return
  endif


 ! XMRKX

  ! Establish the resolution of the grids
  dst_nx = 10
  dst_ny = 10
  dst_nz = 5

  ! Establish the coordinates of the grids
  dst_minx = 1.1
  dst_miny = 1.1
  dst_minz = 1.1
  
  dst_maxx = 19.9
  dst_maxy = 19.9
  dst_maxz = 1.9
  
  ! setup source Mesh
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
     nodeCoords=(/1.0,1.0,1.0, & ! node id 1
                  10.0,1.0,1.0, & ! node id 2
                  20.0,1.0,1.0, & ! node id 3
                  1.0,10.0,1.0, & ! node id 4
                  10.0,10.0,1.0, & ! node id 5
                  20.0,10.0,1.0, & ! node id 6
                  1.0,20.0,1.0, & ! node id 7
                  10.0,20.0,1.0, & ! node id 8
                  20.0,20.0,1.0, & ! node id 9
                  1.0,1.0,2.0, & ! node id 10
                  10.0,1.0,2.0, & ! node id 11
                  20.0,1.0,2.0, & ! node id 12
                  1.0,10.0,2.0, & ! node id 13
                  10.0,10.0,2.0, & ! node id 14
                  20.0,10.0,2.0, & ! node id 15
                  1.0,20.0,2.0, & ! node id 16
                  10.0,20.0,2.0, & ! node id 17
                  20.0,20.0,2.0 /) ! node id 18

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
       nodeCoords=(/1.0,1.0,1.0, & ! node id 1
                    10.0,1.0,1.0, & ! node id 2
                    1.0,10.0,1.0, & ! node id 4
                    10.0,10.0,1.0, & ! node id 5
                    1.0,1.0,2.0, & ! node id 10
                    10.0,1.0,2.0, & ! node id 11
                    1.0,10.0,2.0, & ! node id 13
                    10.0,10.0,2.0 /) ! node id 14


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
       nodeCoords=(/10.0,1.0,1.0, & ! node id 2
                    20.0,1.0,1.0, & ! node id 3
                    10.0,10.0,1.0, & ! node id 5
                    20.0,10.0,1.0, & ! node id 6
                    10.0,1.0,2.0, & ! node id 11
                    20.0,1.0,2.0, & ! node id 12
                    10.0,10.0,2.0, & ! node id 14
                    20.0,10.0,2.0/) ! node id 15

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
        nodeCoords=(/1.0,10.0,1.0, & ! node id 4
                     10.0,10.0,1.0, & ! node id 5
                     1.0,20.0,1.0, & ! node id 7
                     10.0,20.0,1.0, & ! node id 8
                     1.0,10.0,2.0, & ! node id 13
                     10.0,10.0,2.0, & ! node id 14
                     1.0,20.0,2.0, & ! node id 16
                     10.0,20.0,2.0/) ! node id 17


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
        nodeCoords=(/10.0,10.0,1.0, &  ! node id 5
                     20.0,10.0,1.0, &  ! node id 6
                     10.0,20.0,1.0, &  ! node id 8
                     20.0,20.0,1.0, &  ! node id 9
                     10.0,10.0,2.0, &  ! node id 14
                     20.0,10.0,2.0, &  ! node id 15
                     10.0,20.0,2.0, &  ! node id 17
                     20.0,20.0,2.0/)  ! node id 18

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

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=3,spatialDim=3, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         coordSys=ESMF_COORDSYS_SPH_DEG, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

      if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(3*i1-2)
        y=nodeCoords(3*i1-1)
        z=nodeCoords(3*i1)

        ! Compute Lat/Lon in rad
        lon_rad = DEG2RAD*(x)
        lat_rad = DEG2RAD*(90.-y)

        ! Set source function
        !farrayPtr1D(i2) = 20.0+x+y+z
        !farrayPtr1D(i2) = z*(2. + cos(lon_rad)**2.*cos(2.*lat_rad))    
        farrayPtr1D(i2) = z*5.0+2.0+ cos(lon_rad)**2.*cos(2.*lat_rad) 
        !farrayPtr1D(i2) = 1.0+z*0.01+ cos(lat_rad)**2.*cos(2.*lon_rad) 
        !farrayPtr1D(i2) = z

        ! Advance to next owner
        i2=i2+1
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


 ! XMRKX

  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/dst_nx,dst_ny,dst_nz/), &
              coordSys=ESMF_COORDSYS_SPH_DEG, regDecomp=(/2,2,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
    return
  endif

  ! Exact destination field
  xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
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

  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif




   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Set source coordinates
        farrayPtrXC(i1,i2,i3) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2,i3) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny
        farrayPtrZC(i1,i2,i3) = ((dst_maxz-dst_minz)*REAL(i3-1)/REAL(dst_nz-1))+dst_minz
 
        ! Put in more convenient form
        x=farrayPtrXC(i1,i2,i3)
        y=farrayPtrYC(i1,i2,i3)
        z=farrayPtrZC(i1,i2,i3)

        ! Compute Lat/Lon in rad
        lon_rad = DEG2RAD*(x)
        lat_rad = DEG2RAD*(90.-y)

        ! initialize exact destination field
        !xfarrayPtr(i1,i2,i3)=z*(2. + cos(lon_rad)**2.*cos(2.*lat_rad))
        xfarrayPtr(i1,i2,i3)= z*5.0+2.0+ cos(lon_rad)**2.*cos(2.*lat_rad) 
        !xfarrayPtr(i1,i2,i3)= 1.0+z*0.01+ cos(lat_rad)**2.*cos(2.*lon_rad) 
        !xfarrayPtr(i1,i2,i3)=z
        ! Initialize destination field
        farrayPtr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          lineType=ESMF_LINETYPE_GREAT_CIRCLE, &
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

  ! Init max
  maxRelErr=0.0

  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif
   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Calculate error
        err=abs(farrayPtr(i1,i2,i3)-xfarrayPtr(i1,i2,i3))

        ! Calculate relative error
        if (xfarrayPtr(i1,i2,i3) .ne. 0.0) then
           relErr=err/xfarrayPtr(i1,i2,i3)
        else
           relErr=err
        endif

        !! Calculate max
        if (relErr > maxRelErr) maxRelErr=relErr

	!! if error is too big report an error
	if (relErr > 0.009) then
           correct=.false.	
	endif	

     enddo
     enddo
     enddo

  enddo    ! lDE

!  write(*,*) "Max Rel. Error= ",maxRelErr

#if 0
  call ESMF_MeshWrite(srcMesh,"srcMesh",rc=localrc)

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=xdstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
      return
  endif
#endif

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


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridMeshToGridSph3D



 subroutine test_regridGridToGridSph3D(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
   type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:),farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:),farrayPtr2(:,:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:,:)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(3)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(3)
  character(len=ESMF_MAXSTR) :: string
  integer dst_nx,dst_ny,dst_nz
  integer src_nx,src_ny,src_nz
  integer num_arrays
  real(ESMF_KIND_R8) :: dst_dx,dst_dy
  real(ESMF_KIND_R8) :: dst_minr, dst_maxr
  real(ESMF_KIND_R8) :: src_dx,src_dy
  real(ESMF_KIND_R8) :: src_minr, src_maxr
  
  real(ESMF_KIND_R8) :: dst_minx,dst_miny,dst_minz
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy,dst_maxz

  real(ESMF_KIND_R8) :: x,y,z
   real(ESMF_KIND_R8) :: lat_rad, lon_rad, r

  real(ESMF_KIND_R8) :: err, relErr, maxRelErr
  
  integer :: spherical_grid

  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: beg_time, end_time  

  ! result code
  integer :: finalrc
  
  ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: &
     DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! init success flag
  correct=.true.

  rc=ESMF_SUCCESS

 ! XMRKX

  ! Init Grid resolutions
  src_nx=60
  src_ny=60
  src_nz=14
  src_minr=0.9
  src_maxr=2.1

   dst_nx=50
  dst_ny=50
  dst_nz=11
  dst_minr=1.0
  dst_maxr=2.0


  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return


  ! Setup Src Grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1,1/),maxIndex=(/src_nx,src_ny,src_nz/), &
              coordSys=ESMF_COORDSYS_SPH_DEG, regDecomp=(/petCount,1,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
   endif
  
  ! Add center stagger
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Src grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Init some stuff for convenience
  src_dx = 360./src_nx
     src_dy = 180./src_ny

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
  
     !! get coords
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

      call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(srcField, lDE, farrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Set coordinates
        farrayPtrXC(i1,i2,i3) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2,i3) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
        farrayPtrZC(i1,i2,i3) = ((src_maxr-src_minr)*REAL(i3-1)/REAL(src_nz-1))+src_minr

        ! Compute Lat/Lon in rad
        lon_rad = DEG2RAD*farrayPtrXC(i1,i2,i3)
        lat_rad = DEG2RAD*(90.-farrayPtrYC(i1,i2,i3))
         r=farrayPtrZC(i1,i2,i3)

        ! initialize source field
        farrayPtr(i1,i2,i3)= r*5.0+2.0+ cos(lon_rad)**2.*cos(2.*lat_rad) 
        !farrayPtr(i1,i2,i3)= 2.0+r*5.0+ cos(lat_rad)**2.*cos(2.*lon_rad) 
        !farrayPtr(i1,i2,i3)= 1.0
     enddo
     enddo
     enddo

  enddo    ! lDE



  ! Setup Dst Grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1,1/),maxIndex=(/dst_nx,dst_ny,dst_nz/), &
              coordSys=ESMF_COORDSYS_SPH_DEG, regDecomp=(/1,petCount,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
   endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Exact destination field
  xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
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

  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Init some stuff for convenience
  dst_dx = 360./dst_nx
  dst_dy = 180./dst_ny

  ! Get number of local DEs
   call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
    return
  endif


  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            farrayPtr=farrayPtrZC, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Set coordinates
        farrayPtrXC(i1,i2,i3) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2,i3) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)
        farrayPtrZC(i1,i2,i3) = ((dst_maxr-dst_minr)*REAL(i3-1)/REAL(dst_nz-1))+dst_minr
 
        ! Compute Lat/Lon in rad
        lon_rad = DEG2RAD*farrayPtrXC(i1,i2,i3)
        lat_rad = DEG2RAD*(90.-farrayPtrYC(i1,i2,i3))
        r=farrayPtrZC(i1,i2,i3)

        ! initialize exact destination field
        !xfarrayPtr(i1,i2,i3)=r*(2. + cos(lon_rad)**2.*cos(2.*lat_rad))
        !xfarrayPtr(i1,i2,i3)=r*5.0+2. + cos(lon_rad)**2.*cos(2.*lat_rad)
        !xfarrayPtr(i1,i2,i3)= 1.0+r*0.01+ cos(lat_rad)**2.*cos(2.*lon_rad) 
        xfarrayPtr(i1,i2,i3)= r*5.0+2.0+ cos(lon_rad)**2.*cos(2.*lat_rad) 
         !xfarrayPtr(i1,i2,i3)= 1.0
        !xfarrayPtr(i1,i2,i3)=r

        ! Initialize destination field
        farrayPtr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE

  ! Get start time
  ! call ESMF_VMWtime(beg_time)

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
 	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          lineType=ESMF_LINETYPE_GREAT_CIRCLE, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get end time
  !call ESMF_VMWtime(end_time)

  ! output time
  ! write(*,*) "Store time = ",end_time-beg_time

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

  ! Init max
  maxRelErr=0.0

   ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


      call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif
   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Calculate error
        err=abs(farrayPtr(i1,i2,i3)-xfarrayPtr(i1,i2,i3))

        ! Calculate relative error
        if (xfarrayPtr(i1,i2,i3) .ne. 0.0) then
           relErr=err/xfarrayPtr(i1,i2,i3)
        else
           relErr=err
        endif
 
        !! Calculate max
        if (relErr > maxRelErr) maxRelErr=relErr

	!! if error is too big report an error
	if (relErr > 0.002) then
           correct=.false.	
	endif	
     enddo
     enddo
     enddo

  enddo    ! lDE

  if (.not. correct) then
     write(*,*) "Test not correct. Max Rel. Error= ",maxRelErr
  endif

!   write(*,*) "Max Rel. Error= ",maxRelErr

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", &
       rc=localrc)

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=xdstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

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


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridGridToGridSph3D


 subroutine test_regridNearestLocStreamToLocStream(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc

  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm

  real(ESMF_KIND_R8), pointer :: lon(:),lat(:)
  real(ESMF_KIND_R8), pointer :: temperature(:)
  real(ESMF_KIND_R8), pointer :: farrayPtr1D(:)
  integer :: numLocationsOnThisPet,i
  type(ESMF_LocStream) :: srcLocStream,dstLocStream

  integer :: localPet, petCount

  ! init success flag
  correct=.true.

  rc=ESMF_SUCCESS

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif

  ! Setup Src LocStream
  numLocationsOnThisPet=6

  !-------------------------------------------------------------------
  ! Allocate and set example Field data
  !-------------------------------------------------------------------
  allocate(temperature(numLocationsOnThisPet))

  do i=1,numLocationsOnThisPet
    temperature(i)=80.0+i
  enddo


  !-------------------------------------------------------------------
  ! Create the LocStream:  Allocate space for the LocStream object,
  ! define the number and distribution of the locations.
  !-------------------------------------------------------------------
  srcLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=numLocationsOnThisPet, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(srcLocStream,                 &
                            keyName="ESMF:Lat",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Degrees",           &
                            keyLongName="Latitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Lat'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(srcLocStream,                 &
                            keyName="ESMF:Lon",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Degrees",           &
                            keyLongName="Longitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Lon'
    rc=ESMF_FAILURE
    return
  endif
  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  call ESMF_LocStreamGetKey(srcLocStream,                 &
                            localDE=0,                    &
                            keyName="ESMF:Lat",           &
                            farray=lat,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Latitude'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(srcLocStream,                 &
                            localDE=0,                    &
                            keyName="ESMF:Lon",           &
                            farray=lon,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Longitude'
    rc=ESMF_FAILURE
    return
  endif

  !-------------------------------------------------------------------
  ! Set key data.
  !-------------------------------------------------------------------
  do i=1,numLocationsOnThisPet
     lon(i)=(i-1)*360.0/numLocationsOnThisPet
     lat(i)=0.0
  enddo


  !-------------------------------------------------------------------
  ! Create a Field on the Location Stream. In this case the
  ! Field is created from a user array, but any of the other
  ! Field create methods (e.g. from ArraySpec) would also apply.
  !-------------------------------------------------------------------
  srcField=ESMF_FieldCreate(srcLocStream,   &
                                     temperature, &
                                     name="temperature", &
                                     rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif

  ! setup Dst locStream

  !-------------------------------------------------------------------
  ! Create the LocStream:  Allocate space for the LocStream object,
  ! define the number and distribution of the locations.
  !-------------------------------------------------------------------
  dstLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=numLocationsOnThisPet, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(dstLocStream,                 &
                            keyName="ESMF:Lat",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Degrees",           &
                            keyLongName="Latitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Lat'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(dstLocStream,                 &
                            keyName="ESMF:Lon",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Degrees",           &
                            keyLongName="Longitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Lon'
    rc=ESMF_FAILURE
    return
  endif



  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  call ESMF_LocStreamGetKey(dstLocStream,                 &
                            localDE=0,                    &
                            keyName="ESMF:Lat",           &
                            farray=lat,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Latitude'
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_LocStreamGetKey(dstLocStream,                 &
                            localDE=0,                    &
                            keyName="ESMF:Lon",           &
                            farray=lon,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Longitude'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Set key data.
  !-------------------------------------------------------------------
  !setting the coordinates with offset just under limit before nearest neighbor
  !regridding would shift to next point
  do i=1,numLocationsOnThisPet
     lon(i)=(i-1)*360.0/numLocationsOnThisPet+29.999
     lat(i)=0.0
  enddo


  !-------------------------------------------------------------------
  ! Create a Field on the Location Stream. In this case the
  ! Field is created from a user array, but any of the other
  ! Field create methods (e.g. from ArraySpec) would also apply.
  !-------------------------------------------------------------------


  ! Create dest field
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
                        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   ! clear destination Field
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
   endif

   farrayPtr1D=0.0


  rc=ESMF_SUCCESS


  !!! Regrid forward from one locstream to another
  ! Regrid store
  call ESMF_FieldRegridStore( &
 	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
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


   !check error
   call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
   endif

  ! loop through nodes and make sure interpolated values are reasonable
  ! interpolated point should always map to same index
  do i=1,numLocationsOnThisPet
    if ( abs( farrayPtr1D(i)-temperature(i) ) > 0.0001) then
      correct=.false.
      write(*,*) localPet,i,"::",farrayPtr1D(i),temperature(i)
    endif
  enddo

  deallocate(temperature)

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

  !destroy locStream objects
  call ESMF_LocStreamDestroy(srcLocStream, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying location stream'
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_LocStreamDestroy(dstLocStream, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying location stream'
      rc=ESMF_FAILURE
      return
   endif

  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif


 end subroutine test_regridNearestLocStreamToLocStream


 subroutine test_regridNearestLocStream_wClusterToMesh(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: x,y
  
  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, allocatable :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
  integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

  ! result code
  integer :: finalrc

  type(ESMF_LocStream) :: srcLocStream
  real(ESMF_KIND_R8), pointer :: temperature(:)
  integer :: numLocationsOnThisPet,i
  real(ESMF_KIND_R8), pointer :: Xarray(:),Yarray(:)

  
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


  if (petCount .eq. 1) then

    numLocationsOnThisPet=10
    allocate(temperature(numLocationsOnThisPet))

    temperature(1)=20
    temperature(2)=21
    temperature(3)=22
    temperature(4)=21
    temperature(5)=22
    temperature(6)=23
    temperature(7)=22
    temperature(8)=23
    temperature(9)=24
    temperature(10)=99

  else
    if (localpet .eq. 0) then
      numLocationsOnThisPet=2
      allocate(temperature(numLocationsOnThisPet))

      temperature(1)=20
      temperature(2)=21

    else if (localpet .eq. 1) then
      numLocationsOnThisPet=2
      allocate(temperature(numLocationsOnThisPet))

      temperature(1)=22
      temperature(2)=21

    else if (localpet .eq. 2) then
      numLocationsOnThisPet=3
      allocate(temperature(numLocationsOnThisPet))

      temperature(1)=22
      temperature(2)=23
      temperature(3)=22

    else if (localpet .eq. 3) then
      numLocationsOnThisPet=3
      allocate(temperature(numLocationsOnThisPet))

      temperature(1)=23
      temperature(2)=24
      temperature(3)=99
    endif
  endif
  
  !-------------------------------------------------------------------
  ! Create the LocStream:  Allocate space for the LocStream object,
  ! define the number and distribution of the locations.
  !-------------------------------------------------------------------
  srcLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=numLocationsOnThisPet, &
                                   coordSys=ESMF_COORDSYS_CART, &
                                   rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(srcLocStream,                    &
                            keyName="ESMF:Y",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Ydimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Y'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(srcLocStream,                    &
                            keyName="ESMF:X",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Xdimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for X'
    rc=ESMF_FAILURE
    return
  endif
  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  call ESMF_LocStreamGetKey(srcLocStream,                    &
                            localDE=0,                    &
                            keyName="ESMF:Y",                &
                            farray=Yarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Y coordinate'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(srcLocStream,                    &
                            localDE=0,                    &
                            keyName="ESMF:X",                &
                            farray=Xarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for X coordinate'
    rc=ESMF_FAILURE
    return
  endif

  !-------------------------------------------------------------------
  ! Set key data.
  !-------------------------------------------------------------------

  if (petCount .eq. 1) then
    !cluster the data by adding additional point with identical coordinates as another point
    Xarray(1)=0.0
    Xarray(2)=1.0
    Xarray(3)=2.0
    Xarray(4)=0.0
    Xarray(5)=0.99
    Xarray(6)=2.0
    Xarray(7)=0.0
    Xarray(8)=1.0
    Xarray(9)=2.0
    Xarray(10)=0.99
    Yarray(1)=0.0
    Yarray(2)=0.0
    Yarray(3)=0.0
    Yarray(4)=1.0
    Yarray(5)=0.99
    Yarray(6)=1.0
    Yarray(7)=2.0
    Yarray(8)=2.0
    Yarray(9)=2.0
    Yarray(10)=0.99
  else
    if (localPet .eq. 0) then
      Xarray(1)=0.0
      Xarray(2)=1.0
      Yarray(1)=0.0
      Yarray(2)=0.0
    else if (localPet .eq. 1) then
      Xarray(1)=2.0
      Xarray(2)=0.0
      Yarray(1)=0.0
      Yarray(2)=1.0
    else if (localPet .eq. 2) then
      Xarray(1)=0.99
      Xarray(2)=2.0
      Xarray(3)=0.0
      Yarray(1)=0.99
      Yarray(2)=1.0
      Yarray(3)=2.0
    else if (localPet .eq. 3) then
      !cluster the data by adding additional point with identical coordinates as another point
      Xarray(1)=1.0
      Xarray(2)=2.0
      Xarray(3)=0.99
      Yarray(1)=2.0
      Yarray(2)=2.0
      Yarray(3)=0.99
    endif
  endif


  !-------------------------------------------------------------------
  ! Create a Field on the Location Stream. In this case the
  ! Field is created from a user array, but any of the other
  ! Field create methods (e.g. from ArraySpec) would also apply.
  !-------------------------------------------------------------------
  srcField=ESMF_FieldCreate(srcLocStream,   &
                                     temperature, &
                                     name="temperature", &
                                     rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif



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
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
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
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

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
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

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
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
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

  
   ! Create dest field
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="dest", rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  
   ! clear destination Field
   ! Should only be 1 localDE
   call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
   endif

   farrayPtr1D=0.0

  !!! Regrid forward from the src LocStream to the dst Mesh
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

	!! if error is too big report an error
	if ( abs( farrayPtr1D(i2)-(x+y+20.0) ) > 0.0001) then
           correct=.false.	
           write(*,*) localPet,nodeIds(i1),"::",farrayPtr1D(i2),(x+y+20.0)
	endif	

        ! Advance to next owner
        i2=i2+1
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

   deallocate(temperature)


#if 0
   call ESMF_MeshWrite(srcMesh,"srcMesh")

   call ESMF_MeshWrite(dstMesh,"dstMesh")
#endif

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


  ! Free the grids
  call ESMF_LocStreamDestroy(srcLocStream, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_MeshDestroy(dstMesh, rc=localrc)
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

 end subroutine test_regridNearestLocStream_wClusterToMesh



 subroutine test_regridNearestLocStreamToGrid(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec1,arrayspec2
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: srcPtr(:),dstPtr(:,:)
  real(ESMF_KIND_R8), pointer :: xdstPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  integer :: cl,cu,cc
  integer :: clb,cub,mcc,elb,eub,ec
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer :: numLocationsOnThisPet,idx
  type(ESMF_LocStream) :: srcLocStream
  real(ESMF_KIND_R8), pointer :: Xarray(:),Yarray(:)

  real(ESMF_KIND_R8) :: theta, phi
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
    ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init flags
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

  src_nx = 20
  src_ny = 20
  dst_nx = 20
  dst_ny = 20

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny
  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny


  srcLocStream=ESMF_LocStreamCreate(minIndex=1, maxIndex=src_nx*src_ny, regDecomp=petCount, &
                                    indexflag=ESMF_INDEX_GLOBAL, &
                                    coordSys=ESMF_COORDSYS_SPH_DEG, &
                                    rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif

  call  ESMF_LocStreamGet(srcLocStream, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble with locStreamGet'
    rc=ESMF_FAILURE
    return
  endif

  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(srcLocStream,                 &
                            keyName="ESMF:Lat",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="degrees",           &
                            keyLongName="Latitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for latitude'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(srcLocStream,                 &
                            keyName="ESMF:Lon",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="degrees",           &
                            keyLongName="Longitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for longitude'
    rc=ESMF_FAILURE
    return
  endif

  ! Create src field
  call ESMF_ArraySpecSet(arrayspec1, 1, ESMF_TYPEKIND_R8, rc=rc)

  srcField = ESMF_FieldCreate(srcLocStream, arrayspec1, &
                              name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

!  if (petCount .eq. 1) then
    do lDE=0,localDECount-1
      call  ESMF_LocStreamGetBounds(srcLocStream, localDE=lDE, &
                              computationalLBound=cl, computationalUBound=cu, &
                              computationalCount=cc, &
                              rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
        print*,'ERROR:  trouble with locStreamGet'
        rc=ESMF_FAILURE
        return
      endif


      !-------------------------------------------------------------------
      ! Get key data.
      !-------------------------------------------------------------------
      call ESMF_LocStreamGetKey(srcLocStream,                 &
                                localDE=lDE,                    &
                                keyName="ESMF:Lat",           &
                                farray=Yarray,                &
                                rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
        print*,'ERROR:  trouble getting LocStream key for latitude'
        rc=ESMF_FAILURE
        return
      endif
      call ESMF_LocStreamGetKey(srcLocStream,                 &
                                localDE=lDE,                    &
                                keyName="ESMF:Lon",           &
                                farray=Xarray,                &
                                rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
        print*,'ERROR:  trouble getting LocStream key for longitude'
        rc=ESMF_FAILURE
        return
      endif


      ! get src pointer
      call ESMF_FieldGet(srcField, lDE, srcPtr, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
      endif

      do idx=cl,cu
        i1=(idx-1)/src_nx + 1
        i2=mod((idx-1),src_nx) + 1

        ! Set source coordinates as 0 to 360
        Xarray(idx) = REAL(i2-1)*src_dx
        Yarray(idx) = -90. + (REAL(i1-1)*src_dy + 0.5*src_dy)

        ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(Xarray(idx))
        phi = DEG2RAD*(90.-Yarray(idx))

        srcPtr(idx) = (2. + cos(theta)**2.*cos(2.*phi))
      enddo
    enddo
!  endif



  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_ArraySpecSet(arrayspec2, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif




  dstField = ESMF_FieldCreate(dstGrid, arrayspec2, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  xdstField = ESMF_FieldCreate(dstGrid, arrayspec2, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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

     call ESMF_FieldGet(dstField, lDE, dstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xdstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! initialize destination field
        dstPtr(i1,i2)=0.0

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        ! After calculating field shift coords slighlty to be close, but not exact
        ! to make test more interesting
        farrayPtrXC(i1,i2) = farrayPtrXC(i1,i2) + 2.0

        xdstPtr(i1,i2) = (2. + cos(theta)**2.*cos(2.*phi))

     enddo
     enddo

  enddo    ! lDE


  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_DTOS, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid on fields with extra dimension
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


  ! Check results
  do lDE=0,localDECount-1

     ! Get interpolated dst field
     call ESMF_FieldGet(dstField, lDE, dstPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xdstPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)

        if (xdstPtr(i1,i2) .ne. 0.0) then
           if (abs(dstPtr(i1,i2)-xdstPtr(i1,i2))/abs(dstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
              write(*,*) i1,i2,"::",dstPtr(i1,i2),xdstPtr(i1,i2),(dstPtr(i1,i2)-xdstPtr(i1,i2))/xdstPtr(i1,i2)
           endif
        else
           if (abs(dstPtr(i1,i2)-xdstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
           endif
        endif
     enddo
     enddo

  enddo    ! lDE


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

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_LocStreamDestroy(srcLocStream, rc=localrc)
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

 end subroutine test_regridNearestLocStreamToGrid


 subroutine test_regridGridToLocStreamRegDist(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_LocStream) :: dstLocStream
  type(ESMF_Field) :: srcField,dstField,dstFieldPatch
  type(ESMF_RouteHandle) :: routeHandle,routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: latArray(:),lonArray(:)
  real(ESMF_KIND_R8) :: src_dx, src_dy, dst_dx, dst_dy
  real(ESMF_KIND_R8) :: RAD2DEG,DEG2RAD,theta,phi
  real(ESMF_KIND_R8) :: lat,lon
  real(ESMF_KIND_R8) :: x,y,z,expected
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: dclbnd(1),dcubnd(1)
  integer :: i1,i2
  integer :: lDE, localDECount
  integer :: src_nx, src_ny, dst_nx, dst_ny
  integer :: cl,cu,idx
  integer :: localPet, petCount

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

  ! If we don't have 1 or 4 PETS then exit with failure
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif


  ! Establish the resolution of the grids
  src_nx = 100
  src_ny = 50
  src_dx = 360./src_nx
  src_dy = 180./src_ny
  DEG2RAD = 3.14159265/180.0
  RAD2DEG = 1./DEG2RAD

  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                  coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                                  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

  srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                              staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get memory and set coords for src
  do lDE=0,localDECount-1

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
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        ! (something relatively smooth, that varies everywhere)
        farrayPtr(i1,i2) = x+y+z+15.0

     enddo
     enddo

  enddo    ! lDE




  ! Setup Dst LocStream
  dst_nx = 90
  dst_ny = 40
  dst_dx = 360./dst_nx
  dst_dy = 180./dst_ny

  dstLocStream=ESMF_LocStreamCreate(minIndex=1, maxIndex=dst_nx*dst_ny, regDecomp=petCount, &
                                    indexflag=ESMF_INDEX_GLOBAL, &
                                    coordSys=ESMF_COORDSYS_SPH_DEG, &
                                    rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif

  call  ESMF_LocStreamGet(dstLocStream, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble with locStreamGet'
    rc=ESMF_FAILURE
    return
  endif

  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(dstLocStream,                 &
                            keyName="ESMF:Lat",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="degrees",           &
                            keyLongName="Latitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for latitude'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(dstLocStream,                 &
                            keyName="ESMF:Lon",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="degrees",           &
                            keyLongName="Longitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for longitude'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  do lDE=0,localDECount-1
    call  ESMF_LocStreamGetBounds(dstLocStream, localDE=lDE, &
                            computationalLBound=cl, computationalUBound=cu, &
                            rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble with LocStreamGet'
      rc=ESMF_FAILURE
      return
    endif

    !-------------------------------------------------------------------
    ! Get key data.
    !-------------------------------------------------------------------
    call ESMF_LocStreamGetKey(dstLocStream,                 &
                              localDE=lDE,                    &
                              keyName="ESMF:Lat",           &
                              farray=latArray,                &
                              rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble getting LocStream key for latitude'
      rc=ESMF_FAILURE
      return
    endif
    call ESMF_LocStreamGetKey(dstLocStream,                 &
                              localDE=lDE,                    &
                              keyName="ESMF:Lon",           &
                              farray=lonArray,                &
                              rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble getting LocStream key for longitude'
      rc=ESMF_FAILURE
      return
    endif

    do idx=cl,cu
      i1=(idx-1)/dst_nx + 1
      i2=mod((idx-1),dst_nx) + 1

      ! Set source coordinates as 0 to 360
      lonArray(idx) = REAL(i2-1)*dst_dx
      latArray(idx) = -90. + (REAL(i1-1)*dst_dy + 0.5*dst_dy)
    enddo
  enddo

  ! Create dest fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble calling ArraySpecSet'
    rc=ESMF_FAILURE
    return
  endif

  dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
                        name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif

  dstFieldPatch = ESMF_FieldCreate(dstLocStream, arrayspec, &
                        name="destPatch", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif

  ! clear destination Fields
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0

  call ESMF_FieldGet(dstFieldPatch, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0

  !!! Regrid forward from the grid to the LocStream
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  !!! Regrid forward from the Src grid to the LocStream - this time with PATCH
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstFieldPatch, &
          routeHandle=routeHandlePatch, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstFieldPatch, routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check results
  do lDE=0,localDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr1D, computationalLBound=dclbnd, &
                             computationalUBound=dcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=dclbnd(1),dcubnd(1)
        ! Get coordinates
        lon=lonArray(i1)
        lat=latArray(i1)

        ! get the x,y,z coordinates
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! determine validation data
        expected = x+y+z+15.0

	!! if error is too big report an error
	if ( abs( farrayPtr1D(i1)-(expected) )/expected > 0.001) then
           print*,'ERROR: larger than expected difference, expected ',expected, &
                  '  got ',farrayPtr1D(i1),'  diff= ',abs(farrayPtr1D(i1)-expected)
           correct=.false.	
	endif	
     enddo

     ! now for patch
     call ESMF_FieldGet(dstFieldPatch, lDE, farrayPtr1D, computationalLBound=dclbnd, &
                             computationalUBound=dcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=dclbnd(1),dcubnd(1)
        ! Get coordinates
        lon=lonArray(i1)
        lat=latArray(i1)

        ! get the x,y,z coordinates
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! determine validation data
        expected = x+y+z+15.0

	!! if error is too big report an error
	if ( abs( farrayPtr1D(i1)-(expected) )/expected > 0.001) then
           print*,'ERROR: larger than expected difference, expected ',expected, &
                  '  got ',farrayPtr1D(i1),'  diff= ',abs(farrayPtr1D(i1)-expected)
           correct=.false.	
	endif	
     enddo

  enddo    ! lDE

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

   call ESMF_FieldDestroy(dstFieldPatch, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_LocStreamDestroy(dstLocStream, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(srcGrid, rc=localrc)
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

 end subroutine test_regridGridToLocStreamRegDist

 subroutine test_regridGridToLocStreamLocCnt(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_LocStream) :: dstLocStream
  type(ESMF_Field) :: srcField,dstField
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: latArray(:),lonArray(:)
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: RAD2DEG,DEG2RAD,theta,phi
  real(ESMF_KIND_R8) :: lat,lon
  real(ESMF_KIND_R8) :: x,y,z,expected
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2
  integer :: lDE, localDECount
  integer :: src_nx, src_ny
  integer :: localPet, petCount, numLocationsOnThisPet

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

  ! If we don't have 1 or 4 PETS then exit with failure
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    print*,'ERROR:  test must be run using exactly 1 or 4 PETS - detected ',petCount
    rc=ESMF_FAILURE
    return
  endif


  ! Establish the resolution of the grids
  src_nx = 100
  src_ny = 50
  src_dx = 360./src_nx
  src_dy = 180./src_ny
  DEG2RAD = 3.14159265/180.0
  RAD2DEG = 1./DEG2RAD

  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(maxIndex=(/src_nx,src_ny/), &
                                  coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                                  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                              staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get memory and set coords for src
  do lDE=0,localDECount-1

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        ! (something relatively smooth, that varies everywhere)
        farrayPtr(i1,i2) = x+y+z+15.0

     enddo
     enddo

  enddo    ! lDE




  ! Setup Dst LocStream
  if (petCount .eq. 1) then
    numLocationsOnThisPet=7
  else
    if (localpet .eq. 0) then
      numLocationsOnThisPet=2
    else if (localpet .eq. 1) then
      numLocationsOnThisPet=2
    else if (localpet .eq. 2) then
      numLocationsOnThisPet=2
    else if (localpet .eq. 3) then
      numLocationsOnThisPet=1
    endif
  endif

  !-------------------------------------------------------------------
  ! Create the LocStream:  Allocate space for the LocStream object,
  ! define the number and distribution of the locations.
  !-------------------------------------------------------------------
  dstLocStream=ESMF_LocStreamCreate(name="Global Temperatures", &
                                   localCount=numLocationsOnThisPet, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif



  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(dstLocStream,                 &
                            keyName="ESMF:Lat",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="degrees",           &
                            keyLongName="Latitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for latitude'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(dstLocStream,                 &
                            keyName="ESMF:Lon",           &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="degrees",           &
                            keyLongName="Longitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for longitude'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            localDE=0,                    &
                            keyName="ESMF:Lat",                &
                            farray=latArray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for latitude'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            localDE=0,                    &
                            keyName="ESMF:Lon",                &
                            farray=lonArray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for longitude'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Set key data.
  !-------------------------------------------------------------------
  if (petCount .eq. 1) then
    latArray = (/-87.75, -56.25, -26.5, 0.0, 26.5, 56.25, 87.75 /)
    lonArray = (/51.4, 102.8, 154.2, 205.6, 257.0, 308.4, 359.8 /)
  else
    if (localpet .eq. 0) then
      latArray = (/ -87.75, -56.25 /)
      lonArray = (/ 51.4, 102.8 /)
    else if (localpet .eq.1) then
      latArray = (/ -26.5, 0.0 /)
      lonArray = (/ 154.2, 205.6 /)
    else if (localpet .eq.2) then
      latArray = (/ 26.5, 56.25 /)
      lonArray = (/ 257.0, 308.4 /)
    else if (localpet .eq.3) then
      latArray = (/ 87.75 /)
      lonArray = (/ 359.8 /)
    endif
  endif

  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble calling ArraySpecSet'
    rc=ESMF_FAILURE
    return
  endif

  dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
                        name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif

  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0

  !!! Regrid forward from the grid to the LocStream
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! loop through nodes and make sure interpolated values are reasonable
  do i1=1,numLocationsOnThisPet
    lon=lonArray(i1)
    lat=latArray(i1)

    ! get the x,y,z coordinates
    theta = DEG2RAD*(lon)
    phi = DEG2RAD*(90.-lat)
    x = cos(theta)*sin(phi)
    y = sin(theta)*sin(phi)
    z = cos(phi)

    ! determine validation data
    expected = x+y+z+15.0

    ! if error is too big report an error
    if ( abs( farrayPtr1D(i1)-(expected) )/expected > 0.001) then
      print*,'ERROR: larger than expected difference, expected ',expected, &
             '  got ',farrayPtr1D(i1),'  diff= ',abs(farrayPtr1D(i1)-expected), &
             '  rel diff= ',abs(farrayPtr1D(i1)-expected)/expected
      correct=.false.	
    endif	
  enddo


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

  ! Free the grids
  call ESMF_LocStreamDestroy(dstLocStream, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(srcGrid, rc=localrc)
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

 end subroutine test_regridGridToLocStreamLocCnt

 subroutine test_regridGridToLocStream3d(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  integer src_nx, src_ny, src_nz
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: src_minx,src_miny,src_minz
  real(ESMF_KIND_R8) :: src_maxx,src_maxy,src_maxz

  real(ESMF_KIND_R8) :: x,y,z,expected
  
  integer :: localPet, petCount

  ! result code
  
  type(ESMF_LocStream) :: dstLocStream
  integer :: numLocationsOnThisPet,i
  real(ESMF_KIND_R8), pointer :: Xarray(:),Yarray(:),Zarray(:)


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

  ! Establish the resolution of the grids
  src_nx = 10
  src_ny = 10
  src_nz = 10

  ! Establish the coordinates of the grids
  src_minx = -0.1
  src_miny = -0.1
  src_minz = -0.1
  
  src_maxx = 2.1
  src_maxy = 2.1 
  src_maxz = 2.1 


  ! setup src grid
  srcGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/src_nx,src_ny,src_nz/), &
                                regDecomp=(/petCount,1,1/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                                rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(srcField, lDE, farrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Set source coordinates
        farrayPtrXC(i1,i2,i3) = ((src_maxx-src_minx)*REAL(i1-1)/REAL(src_nx-1))+src_minx
        farrayPtrYC(i1,i2,i3) = ((src_maxy-src_miny)*REAL(i2-1)/REAL(src_ny-1))+src_miny
        farrayPtrZC(i1,i2,i3) = ((src_maxz-src_minz)*REAL(i3-1)/REAL(src_nz-1))+src_minz

        ! initialize source field
        farrayPtr(i1,i2,i3) = farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+farrayPtrZC(i1,i2,i3)+20.0

     enddo
     enddo
     enddo

  enddo    ! lDE



  ! Setup Dst LocStream
  if (petCount .eq. 1) then
    numLocationsOnThisPet=10
  else
    if (localpet .eq. 0) then
      numLocationsOnThisPet=3
    else if (localpet .eq. 1) then
      numLocationsOnThisPet=3
    else if (localpet .eq. 2) then
      numLocationsOnThisPet=2
    else if (localpet .eq. 3) then
      numLocationsOnThisPet=2
    endif
  endif

  !-------------------------------------------------------------------
  ! Create the LocStream:  Allocate space for the LocStream object,
  ! define the number and distribution of the locations.
  !-------------------------------------------------------------------
  dstLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=numLocationsOnThisPet, &
                                   coordSys=ESMF_COORDSYS_CART, &
                                   rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(dstLocStream,                    &
                            keyName="ESMF:Y",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Ydimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Y'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(dstLocStream,                    &
                            keyName="ESMF:X",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Xdimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for X'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(dstLocStream,                    &
                            keyName="ESMF:Z",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Zdimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Z'
    rc=ESMF_FAILURE
    return
  endif
  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            localDE=0,                    &
                            keyName="ESMF:Y",                &
                            farray=Yarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Y coordinate'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            localDE=0,                    &
                            keyName="ESMF:X",                &
                            farray=Xarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for X coordinate'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            localDE=0,                    &
                            keyName="ESMF:Z",                &
                            farray=Zarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Z coordinate'
    rc=ESMF_FAILURE
    return
  endif

  !-------------------------------------------------------------------
  ! Set key data.
  !-------------------------------------------------------------------
  if (petCount .eq. 1) then
    !test multiple points at same location
    Xarray(1)=0.0
    Xarray(2)=1.0
    Xarray(3)=2.0
    Xarray(4)=0.0
    Xarray(5)=1.0
    Xarray(6)=2.0
    Xarray(7)=0.0
    Xarray(8)=1.0
    Xarray(9)=2.0
    Xarray(10)=1.0
    Yarray(1)=0.0
    Yarray(2)=0.0
    Yarray(3)=0.0
    Yarray(4)=1.0
    Yarray(5)=1.0
    Yarray(6)=1.0
    Yarray(7)=2.0
    Yarray(8)=2.0
    Yarray(9)=2.0
    Yarray(10)=1.0
    Zarray(1)=0.0
    Zarray(2)=0.0
    Zarray(3)=0.0
    Zarray(4)=1.0
    Zarray(5)=1.0
    Zarray(6)=1.0
    Zarray(7)=2.0
    Zarray(8)=2.0
    Zarray(9)=2.0
    Zarray(10)=1.0
  else
    if (localpet .eq. 0) then
      Xarray(1)=0.0
      Xarray(2)=1.0
      Xarray(3)=2.0
      Yarray(1)=0.0
      Yarray(2)=0.0
      Yarray(3)=0.0
      Zarray(1)=0.0
      Zarray(2)=0.0
      Zarray(3)=0.0
    else if (localpet .eq. 1) then
      Xarray(1)=0.0
      Xarray(2)=1.0
      Xarray(3)=2.0
      Yarray(1)=1.0
      Yarray(2)=1.0
      Yarray(3)=1.0
      Zarray(1)=1.0
      Zarray(2)=1.0
      Zarray(3)=1.0
    else if (localpet .eq. 2) then
      Xarray(1)=0.0
      Xarray(2)=1.0
      Yarray(1)=2.0
      Yarray(2)=2.0
      Zarray(1)=2.0
      Zarray(2)=2.0
    else if (localpet .eq. 3) then
      !test multiple points at same location
      Xarray(1)=2.0
      Xarray(2)=1.0
      Yarray(1)=2.0
      Yarray(2)=1.0
      Zarray(1)=2.0
      Zarray(2)=1.0
    endif
  endif


  ! Create dest fields
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble calling ArraySpecSet'
    rc=ESMF_FAILURE
    return
  endif

  dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
                        name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif

  ! clear destination Fields
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0

  !!! Regrid forward from the Src grid to the LocStream
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  do i1=1,numLocationsOnThisPet

        ! Get coordinates
        x=Xarray(i1)
        y=Yarray(i1)
        z=Zarray(i1)

        expected = x+y+z+20.0

	!! if error is too big report an error
	if ( abs( farrayPtr1D(i1)-expected )/expected > 0.001) then
           print*,'ERROR: larger than expected error, expected ',expected, &
                  '  got ',farrayPtr1D(i1)
           correct=.false.	
	endif	

  enddo

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

  ! Free the grids
  call ESMF_LocStreamDestroy(dstLocStream, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(srcGrid, rc=localrc)
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

 end subroutine test_regridGridToLocStream3d


 subroutine test_regridMeshToLocStreamMask(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  integer :: cl,cu
  
  real(ESMF_KIND_R8) :: x,y

  integer :: localPet, petCount

  integer, allocatable :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
  integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

  type(ESMF_LocStream) :: dstLocStream
  integer :: numLocationsOnThisPet
  real(ESMF_KIND_R8), pointer :: Xarray(:),Yarray(:)
  integer(ESMF_KIND_I4) :: maskValues(2)
  integer(ESMF_KIND_I4), pointer :: maskArray(:)
  
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
     nodeIds=(/100,20,30,40,50,60,70,80,90/) 

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
       nodeIds=(/100,20,40,50/) 

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

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/20,30,50,60/) 

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

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/40,50,70,80/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      1.0,2.1 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/50,60,80,90/) 

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

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y

        ! Advance to next owner
        i2=i2+1
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


  ! Setup Dst LocStream
  if (petCount .eq. 1) then
    numLocationsOnThisPet=11
    cl=1
    cu=11
  else
    if (localpet .eq. 0) then
      numLocationsOnThisPet=3
      cl=1
      cu=3
    else if (localpet .eq. 1) then
      numLocationsOnThisPet=3
      cl=4
      cu=6
    else if (localpet .eq. 2) then
      numLocationsOnThisPet=5
      cl=7
      cu=11
    else if (localpet .eq. 3) then
      numLocationsOnThisPet=0
      cl=12
      cu=11
    endif
  endif

  !-------------------------------------------------------------------
  ! Create the LocStream:  Allocate space for the LocStream object,
  ! define the number and distribution of the locations.
  !-------------------------------------------------------------------
  dstLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=numLocationsOnThisPet, &
                                   coordSys=ESMF_COORDSYS_CART, &
                                   indexflag=ESMF_INDEX_GLOBAL, &
                                   rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(dstLocStream,                    &
                            keyName="ESMF:Y",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Ydimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Y'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(dstLocStream,                    &
                            keyName="ESMF:X",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Xdimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for X'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(dstLocStream,                    &
                            keyName="ESMF:Mask",                &
                            KeyTypeKind=ESMF_TYPEKIND_I4, &
                            keyUnits="none",           &
                            keyLongName="mask values", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Mask'
    rc=ESMF_FAILURE
    return
  endif
  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            keyName="ESMF:Y",                &
                            farray=Yarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Y coordinate'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            keyName="ESMF:X",                &
                            farray=Xarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for X coordinate'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            keyName="ESMF:Mask",                &
                            farray=maskArray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Mask'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Set key data.
  !-------------------------------------------------------------------
  if (petCount .eq. 1) then
    Xarray(1)=0.0
    Xarray(2)=0.5
    Xarray(3)=1.0
    Xarray(4)=2.0
    Xarray(5)=0.0
    Xarray(6)=1.0
    Xarray(7)=2.0
    Xarray(8)=0.0
    Xarray(9)=1.0
    Xarray(10)=1.5
    Xarray(11)=2.0

    Yarray(1)=0.0
    Yarray(2)=0.5
    Yarray(3)=0.0
    Yarray(4)=0.0
    Yarray(5)=1.0
    Yarray(6)=1.0
    Yarray(7)=1.0
    Yarray(8)=2.0
    Yarray(9)=2.0
    Yarray(10)=1.5
    Yarray(11)=2.0

    maskArray(1)=0
    maskArray(2)=1
    maskArray(3)=0
    maskArray(4)=0
    maskArray(5)=0
    maskArray(6)=0
    maskArray(7)=0
    maskArray(8)=0
    maskArray(9)=0
    maskArray(10)=2
    maskArray(11)=0
  else
    if (localpet .eq. 0) then
      Xarray(1)=0.0
      Xarray(2)=0.5
      Xarray(3)=1.0

      Yarray(1)=0.0
      Yarray(2)=0.5
      Yarray(3)=0.0

      maskArray(1)=0
      maskArray(2)=1
      maskArray(3)=0
    else if (localpet .eq. 1) then
      Xarray(4)=2.0
      Xarray(5)=0.0
      Xarray(6)=1.0

      Yarray(4)=0.0
      Yarray(5)=1.0
      Yarray(6)=1.0

      maskArray(4)=0
      maskArray(5)=0
      maskArray(6)=0
    else if (localpet .eq. 2) then
      Xarray(7)=2.0
      Xarray(8)=0.0
      Xarray(9)=1.0
      Xarray(10)=1.5
      Xarray(11)=2.0

      Yarray(7)=1.0
      Yarray(8)=2.0
      Yarray(9)=2.0
      Yarray(10)=1.5
      Yarray(11)=2.0

      maskArray(7)=0
      maskArray(8)=0
      maskArray(9)=0
      maskArray(10)=2
      maskArray(11)=0
!    else if (localpet .eq. 3) then
    endif
  endif


  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  dstField = ESMF_FieldCreate(dstLocStream, arrayspec, &
                        name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0



  !!! Regrid forward from the mesh to the locstream
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, dstMaskValues=(/1,2/), &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  do i1=cl,cu

        if (maskArray(i1) .gt. 0) then
	  if ( abs( farrayPtr1D(i1) ) > 0.0001) then
            correct=.false.
          endif
        else
          ! Get coordinates
          x=Xarray(i1)
          y=Yarray(i1)

          if ( abs( farrayPtr1D(i1)-(x+y+20.0) ) > 0.0001) then
             print*,'ERROR: expecting ',x+y+20.0,'  got ',abs(farrayPtr1d(i1))
             correct=.false.
          endif
        endif
  enddo


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


  ! Free the grids
  call ESMF_LocStreamDestroy(dstLocStream, rc=localrc)
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

 end subroutine test_regridMeshToLocStreamMask

  subroutine test_regridGridToGML(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField, lsdstField, mdstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec, arrayspec2, arrayspec3
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: srcPtr(:,:),dstPtr(:,:),lsdstPtr(:),mdstPtr(:)
  real(ESMF_KIND_R8), pointer :: xdstPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount,localDECountDst
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer cl,cu,numLocationsOnThisPet
  type(ESMF_LocStream) :: dstLocStream
  real(ESMF_KIND_R8), pointer :: Xarray(:),Yarray(:)
  real(ESMF_KIND_R8) :: x,y
  integer :: decompX,decompY

  integer, allocatable :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
  integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

  real(ESMF_KIND_R8) :: theta, phi
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
    ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init flags
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


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Establish the resolution of the grids
  ! Make the same resolution, so src and dst 
  ! fall on top of each other
  src_nx = 17
  src_ny = 17

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/), &
                                  regDecomp=(/petCount,1/), &
                                  coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
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

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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

  ! Construct 2D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, srcPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        srcPtr(i1,i2) = (2. + cos(theta)**2.*cos(2.*phi))
        !srcPtr(i1,i2) = 20.0

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  dst_nx = 4
  dst_ny = 4

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! setup dest. grid
  if (petCount .eq. 4) then
    decompX=2
    decompY=2
  else
    decompX=1
    decompY=1
  endif  
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/), &
                                  regDecomp=(/decompX,decompY/), &
                                  coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                                  rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs for dest
  call ESMF_GridGet(dstGrid, localDECount=localDECountDst, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get memory and set coords for dst
  do lDE=0,localDECountDst-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                            coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(dstField, lDE, dstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xdstPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dst coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        !small shift to x coord
        farrayPtrXC(i1,i2) = farrayPtrXC(i1,i2) + 2.0

        ! initialize destination field
        dstPtr(i1,i2)=0.0

        ! Set the expected to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i1,i2))
        phi = DEG2RAD*(90.-farrayPtrYC(i1,i2))

        xdstPtr(i1,i2) = (2. + cos(theta)**2.*cos(2.*phi))
     enddo
     enddo
  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination LocStream
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (petCount .eq. 1) then
    numLocationsOnThisPet=16
  else
    if (localpet .eq. 0) then
      numLocationsOnThisPet=4
    else if (localpet .eq. 1) then
      numLocationsOnThisPet=4
    else if (localpet .eq. 2) then
      numLocationsOnThisPet=4
    else if (localpet .eq. 3) then
      numLocationsOnThisPet=4
    endif
  endif

  !-------------------------------------------------------------------
  ! Create the LocStream:  Allocate space for the LocStream object,
  ! define the number and distribution of the locations.
  !-------------------------------------------------------------------
  dstLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=numLocationsOnThisPet, &
                                   indexflag=ESMF_INDEX_DELOCAL, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(dstLocStream,                    &
                            keyName="ESMF:Lon",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Longitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Longitude'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(dstLocStream,                    &
                            keyName="ESMF:Lat",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Latitude", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Latitude'
    rc=ESMF_FAILURE
    return
  endif
  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            keyName="ESMF:Lon",                &
                            farray=Xarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Lon coordinate'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(dstLocStream,                    &
                            keyName="ESMF:Lat",                &
                            farray=Yarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Lat coordinate'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Set key data.
  !-------------------------------------------------------------------
  if (petCount .eq. 1) then
    Xarray(1)=2.0
    Xarray(2)=92.0
    Xarray(3)=182.0
    Xarray(4)=272.0
    Xarray(5)=2.0
    Xarray(6)=92.0
    Xarray(7)=182.0
    Xarray(8)=272.0
    Xarray(9)=2.0
    Xarray(10)=92.0
    Xarray(11)=182.0
    Xarray(12)=272.0
    Xarray(13)=2.0
    Xarray(14)=92.0
    Xarray(15)=182.0
    Xarray(16)=272.0

    Yarray(1)=-67.5
    Yarray(2)=-67.5
    Yarray(3)=-67.5
    Yarray(4)=-67.5
    Yarray(5)=-22.5
    Yarray(6)=-22.5
    Yarray(7)=-22.5
    Yarray(8)=-22.5
    Yarray(9)=22.5
    Yarray(10)=22.5
    Yarray(11)=22.5
    Yarray(12)=22.5
    Yarray(13)=67.5
    Yarray(14)=67.5
    Yarray(15)=67.5
    Yarray(16)=67.5
  else
    if (localpet .eq. 0) then
      Xarray(1)=2.0
      Xarray(2)=92.0
      Xarray(3)=2.0
      Xarray(4)=92.0

      Yarray(1)=-67.5
      Yarray(2)=-67.5
      Yarray(3)=-22.5
      Yarray(4)=-22.5
    else if (localpet .eq. 1) then
      Xarray(1)=182.0
      Xarray(2)=272.0
      Xarray(3)=182.0
      Xarray(4)=272.0

      Yarray(1)=-67.5
      Yarray(2)=-67.5
      Yarray(3)=-22.5
      Yarray(4)=-22.5
    else if (localpet .eq. 2) then
      Xarray(1)=2.0
      Xarray(2)=92.0
      Xarray(3)=2.0
      Xarray(4)=92.0

      Yarray(1)=22.5
      Yarray(2)=22.5
      Yarray(3)=67.5
      Yarray(4)=67.5
    else if (localpet .eq. 3) then
      Xarray(1)=182.0
      Xarray(2)=272.0
      Xarray(3)=182.0
      Xarray(4)=272.0

      Yarray(1)=22.5
      Yarray(2)=22.5
      Yarray(3)=67.5
      Yarray(4)=67.5
    endif
  endif


  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec2, 1, ESMF_TYPEKIND_R8, rc=rc)

  lsdstField = ESMF_FieldCreate(dstLocStream, arrayspec2, &
                        name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif

  ! clear destination Field
  call ESMF_FieldGet(lsdstField, 0, lsdstPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  lsdstPtr=0.0


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination Mesh
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (petCount .eq. 1) then
    ! Set number of nodes
    numNodes=16

    ! Allocate and fill the node id array.
    allocate(nodeIds(numNodes))
    nodeIds=(/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/) 

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
    allocate(nodeCoords(2*numNodes))
    nodeCoords=(/2.0,-67.5, &    ! node id 1
                 92.0,-67.5, &   ! node id 2
                 182.0,-67.5, &    ! node id 3
                 272.0,-67.5, &   ! node id 4
                 2.0,-22.5, &  ! node id 5
                 92.0,-22.5, &  ! node id 6
                 182.0,-22.5, &  ! node id 7
                 272.0,-22.5, &  ! node id 8
                 2.0,22.5, &     ! node id 9
                 92.0,22.5, &    ! node id 10
                 182.0,22.5, &     ! node id 11
                 272.0,22.5, &    ! node id 12
                 2.0,67.5, &   ! node id 13
                 92.0,67.5, &   ! node id 14
                 182.0,67.5, &   ! node id 15
                 272.0,67.5 /)   ! node id 16

    ! Allocate and fill the node owner array.
    ! Since this Mesh is all on PET 0, it's just set to all 0.
    allocate(nodeOwners(numNodes))
    nodeOwners=0 ! everything on PET 0

    ! Set the number of each type of element, plus the total number.
    numQuadElems=9
    numTriElems=0
    numTotElems=numQuadElems+numTriElems

    ! Allocate and fill the element id array.
    allocate(elemIds(numTotElems))
    elemIds=(/1,2,3,4,5,6,7,8,9/) 


    ! Allocate and fill the element topology type array.
    allocate(elemTypes(numTotElems))
    elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                ESMF_MESHELEMTYPE_QUAD, & ! elem id 2
                ESMF_MESHELEMTYPE_QUAD, & ! elem id 3
                ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                ESMF_MESHELEMTYPE_QUAD, & ! elem id 5
                ESMF_MESHELEMTYPE_QUAD, & ! elem id 6
                ESMF_MESHELEMTYPE_QUAD, & ! elem id 7
                ESMF_MESHELEMTYPE_QUAD, & ! elem id 8
                ESMF_MESHELEMTYPE_QUAD/)  ! elem id 9


    ! Allocate and fill the element connection type array.
    ! Note that entries in this array refer to the 
    ! positions in the nodeIds, etc. arrays and that
    ! the order and number of entries for each element
    ! reflects that given in the Mesh options 
    ! section for the corresponding entry
    ! in the elemTypes array.
    allocate(elemConn(4*numQuadElems+3*numTriElems))
    elemConn=(/1,2,4,3, &      ! elem id 1
               2,5,7,4, &      ! elem id 2
               5,6,8,7, &      ! elem id 3
               3,4,10,9, &     ! elem id 4
               4,7,13,10, &    ! elem id 5
               7,8,14,13, &    ! elem id 6
               9,10,12,11, &   ! elem id 7
               10,13,15,12, &  ! elem id 8
               13,14,16,15/)   ! elem id 9


  else if (petCount .eq. 4) then
    ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
      ! Set number of nodes
      numNodes=9

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))
      nodeIds=(/1,2,3,5,6,7,9,10,11/) 

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/2.0,-67.5, &    ! node id 1
                   92.0,-67.5, &   ! node id 2
                   182.0,-67.5, &  ! node id 3
                   2.0,-22.5, &    ! node id 5
                   92.0,-22.5, &   ! node id 6
                   182.0,-22.5, &  ! node id 7
                   2.0,22.5, &     ! node id 9
                   92.0,22.5, &    ! node id 10
                   182.0,22.5 /)   ! node id 11

      ! Allocate and fill the node owner array.
      allocate(nodeOwners(numNodes))
      nodeOwners=(/0, & ! node id 1
                   0, & ! node id 2
                   1, & ! node id 3
                   0, & ! node id 5
                   0, & ! node id 6
                   1, & ! node id 7
                   2, & ! node id 9
                   2, & ! node id 10
                   3/)  ! node id 11

      ! Set the number of each type of element, plus the total number.
      numQuadElems=4
      numTriElems=0
      numTotElems=numQuadElems+numTriElems

      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/1,2,4,5/) 

      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                  ESMF_MESHELEMTYPE_QUAD, & ! elem id 2
                  ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                  ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5

      ! Allocate and fill the element connection type array.
      ! Note that entry are local indices
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,5,4, &      ! elem id 1
                 2,3,6,5, &      ! elem id 2
                 4,5,8,7, &     ! elem id 4
                 5,6,9,8 /)    ! elem id 5

    else if (localPET .eq. 1) then !!! This part only for PET 1
      ! Set number of nodes
      numNodes=6

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))
      nodeIds=(/3,4,7,8,11,12/) 

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/182.0,-67.5, &  ! node id 3
                   272.0,-67.5, &  ! node id 4
                   182.0,-22.5, &  ! node id 7
                   272.0,-22.5, &  ! node id 8
                   182.0,22.5, &   ! node id 11
                   272.0,22.5 /)   ! node id 12

      ! Allocate and fill the node owner array.
      allocate(nodeOwners(numNodes))
      nodeOwners=(/1, & ! node id 3
                   1, & ! node id 4
                   1, & ! node id 7
                   1, & ! node id 8
                   3, & ! node id 11
                   3/)  ! node id 12

      ! Set the number of each type of element, plus the total number.
      numQuadElems=2
      numTriElems=0
      numTotElems=numQuadElems+numTriElems

      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/3,6/) 

      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 3
                  ESMF_MESHELEMTYPE_QUAD/)  ! elem id 6

      ! Allocate and fill the element connection type array.
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,4,3, &   ! elem id 3
                 3,4,6,5/)  ! elem id 6

    else if (localPET .eq. 2) then !!! This part only for PET 2
      ! Set number of nodes
      numNodes=6

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))
      nodeIds=(/9,10,11,13,14,15/) 

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/2.0,22.5, &     ! node id 9
                   92.0,22.5, &    ! node id 10
                   182.0,22.5, &   ! node id 11
                   2.0,67.5, &     ! node id 13
                   92.0,67.5, &    ! node id 14
                   182.0,67.5 /)   ! node id 15

      ! Allocate and fill the node owner array.
      ! Since this Mesh is all on PET 0, it's just set to all 0.
      allocate(nodeOwners(numNodes))
      nodeOwners=(/2, & ! node id 9
                   2, & ! node id 10
                   3, & ! node id 11
                   2, & ! node id 13
                   2, & ! node id 14
                   3/)  ! node id 15

      ! Set the number of each type of element, plus the total number.
      numQuadElems=2
      numTriElems=0
      numTotElems=numQuadElems+numTriElems

      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/7,8/) 

      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 7
                  ESMF_MESHELEMTYPE_QUAD/)  ! elem id 8

      ! Allocate and fill the element connection type array.
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,5,4, &   ! elem id 7
                 2,3,6,5 /)  ! elem id 8

    else if (localPET .eq. 3) then !!! This part only for PET 3
      ! Set number of nodes
      numNodes=4

      ! Allocate and fill the node id array.
      allocate(nodeIds(numNodes))
      nodeIds=(/11,12,15,16/) 

      ! Allocate and fill node coordinate array.
      ! Since this is a 2D Mesh the size is 2x the
      ! number of nodes.
      allocate(nodeCoords(2*numNodes))
      nodeCoords=(/182.0,22.5, &   ! node id 11
                   272.0,22.5, &   ! node id 12
                   182.0,67.5, &   ! node id 15
                   272.0,67.5 /)   ! node id 16

      ! Allocate and fill the node owner array.
      allocate(nodeOwners(numNodes))
      nodeOwners=(/3, & ! node id 11
                   3, & ! node id 12
                   3, & ! node id 15
                   3/)  ! node id 16
 
      ! Set the number of each type of element, plus the total number.
      numQuadElems=1
      numTriElems=0
      numTotElems=numQuadElems+numTriElems

      ! Allocate and fill the element id array.
      allocate(elemIds(numTotElems))
      elemIds=(/9/)  

      ! Allocate and fill the element topology type array.
      allocate(elemTypes(numTotElems))
      elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 9

      ! Allocate and fill the element connection type array.
      allocate(elemConn(4*numQuadElems+3*numTriElems))
      elemConn=(/1,2,4,3/) ! elem id 9
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
  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec3, 1, ESMF_TYPEKIND_R8, rc=rc)

  mdstField = ESMF_FieldCreate(dstMesh, arrayspec3, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(mdstField, 0, mdstPtr,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  mdstPtr=0.0


  !dstgrid first
  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
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


  ! now for locstream
  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=lsdstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
  call ESMF_FieldRegrid(srcField, lsdstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! now for mesh
  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=mdstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
  call ESMF_FieldRegrid(srcField, mdstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check results
  do lDE=0,localDECount-1

     ! Get interpolated dst field
     call ESMF_FieldGet(dstField, lDE, dstPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xdstPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)

        if (xdstPtr(i1,i2) .ne. 0.0) then
           if (abs((dstPtr(i1,i2)-xdstPtr(i1,i2))/xdstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
              write(*,*) "dst Grid and expected differ",i1,i2,"::",dstPtr(i1,i2), &
                         xdstPtr(i1,i2),abs((dstPtr(i1,i2)-xdstPtr(i1,i2))/xdstPtr(i1,i2))
           endif
        else
           if (abs(dstPtr(i1,i2)-xdstPtr(i1,i2)) &
                .gt. 0.05) then
              correct=.false.
           endif
        endif
     enddo
     enddo

  enddo    ! lDE


  !verify destination grid,mesh,locstream are essentially identical with strict tolerance
  i3=0
  do i2=fclbnd(2),fcubnd(2)
  do i1=fclbnd(1),fcubnd(1)
    i3=i3+1
    if (abs(dstPtr(i1,i2)-lsdstPtr(i3)) .gt. 0.000000000001) then
      correct=.false.
      write(*,*) "dst Grid and LocStream differ",i1,i2,"::",dstPtr(i1,i2),lsdstPtr(i3)
    endif
    if (abs(dstPtr(i1,i2)-mdstPtr(i3)) .gt. 0.000000000001) then
      correct=.false.
      write(*,*) "dst Grid and Mesh differ",i1,i2,"::",dstPtr(i1,i2),mdstPtr(i3)
    endif
!    write(*,*) "dst Grid and Mesh ",i1,i2,"::",dstPtr(i1,i2),mdstPtr(i3)
!    write(*,*) "dst Grid and LocStream ",i1,i2,"::",dstPtr(i1,i2),lsdstPtr(i3)
    
  enddo
  enddo




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

   call ESMF_FieldDestroy(lsdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(mdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying Grid'
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying Grid'
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_LocStreamDestroy(dstLocStream, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying LocStream'
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      print*,'ERROR:  trouble destroying Mesh'
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridGridToGML


 subroutine test_regridCollapsedQuads(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: x,y

 real(ESMF_KIND_R8) :: err,maxErr
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
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
     rc=ESMF_FAILURE
     return
  endif

 ! XMRKX

  ! Setup Src Mesh

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   2.0            8,9
  !               /   |  \
  !             /  3  | 4  \    
  !           /       |       \   
  !   1.0  7,4 ------- 5 -------- 6,3
  !          \        |        /  
  !            \   1  | 2   /    
  !              \    |  /      
  !   0.0           1,2 
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
  !   2.0            2,3
  !               /   |  \
  !             /  2  | 3  \    
  !           /       |       \   
  !   1.0  2,0 ------ 5 -------- 1,1
  !          \        |        /  
  !            \   0  | 1   /    
  !              \    |  /      
  !   0.0           0,0 
  !
  !        0.0       1.0        2.0 
  !
  !      Node Ids at corners
  !      Element Ids in centers
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
     nodeCoords=(/1.0,0.0, & ! 1
                 1.0,0.0, &  ! 2
                 2.0,1.0, &  ! 3
                 0.0,1.0, &  ! 4
                 1.0,1.0, &  ! 5
                 2.0,1.0, &  ! 6
                 0.0,1.0, &  ! 7
                 1.0,2.0, &  ! 8
                 1.0,2.0 /)  ! 9

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

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
        allocate(nodeCoords(numNodes*2))
       nodeCoords=(/1.0,0.0, &
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
                    2.0,1.0, &
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
                    0.0,1.0, &
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
                    1.0,2.0/)

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
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         coordSys=ESMF_COORDSYS_SPH_DEG, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
       return
   endif


  !   call ESMF_MeshWrite(srcMesh, "srcMesh")

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
   do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        farrayPtr1D(i2) = x+y

        ! Advance to next owner
        i2=i2+1
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

 
  ! Setup Dst Mesh

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! 
  ! Creates the following mesh on 
  ! 1 or 4 PETs. Returns an error 
  ! if run on other than 1 or 4 PETs
  ! 
  !              Mesh Ids
  !
  !   2.0            8,9
  !               /   |  \
  !             /  3  | 4  \    
  !           /       |       \   
  !   1.0  7,4 ------- 5 -------- 6,3
  !          \        |        /  
  !            \   1  | 2   /    
  !              \    |  /      
  !   0.0           1,2 
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
  !   2.0            2,3
  !               /   |  \
  !             /  2  | 3  \    
  !           /       |       \   
  !   1.0  2,0 ------ 5 -------- 1,1
  !          \        |        /  
  !            \   0  | 1   /    
  !              \    |  /      
  !   0.0           0,0 
  !
  !        0.0       1.0        2.0 
  !
  !      Node Ids at corners
  !      Element Ids in centers

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
     nodeCoords=(/0.95,0.1, & ! 1
                 1.05,0.1, &  ! 2
                 1.9,0.95, &  ! 3
                 0.1,0.95, &  ! 4
                 1.0,1.0, &  ! 5
                 1.9,1.05, &  ! 6
                 0.1,1.05, &  ! 7
                 0.95,1.9, &  ! 8
                 1.05,1.9 /)  ! 9

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

       !! node ids
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       !! node Coords
       allocate(nodeCoords(numNodes*2))
       nodeCoords=(/0.95,0.1, & ! 1
                    1.05,0.1, & ! 2
                     0.1,0.95, & ! 4
                    1.0,1.0/)   ! 5

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
       nodeCoords=(/ 1.05,0.1, & ! 2
                    1.9,0.95, &  ! 3
                    1.0,1.0, &   ! 5
                    1.9,1.05/)   ! 6

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
       nodeCoords=(/0.1,0.95, & ! 4
                    1.0,1.0, & ! 5
                    0.1,1.05, &! 7
                    0.95,1.9/) ! 8

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
       nodeCoords=(/1.0,1.0, &   ! 5
                    1.9,1.05, &  ! 6
                    0.95,1.9, &  ! 8
                    1.05,1.9 /)  ! 9

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
  dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         coordSys=ESMF_COORDSYS_SPH_DEG, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
     endif


  !   call ESMF_MeshWrite(dstMesh, "dstMesh")
  
  ! Create dest field
   call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)
  
   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
           dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  maxErr=0.0
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
         !! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        !! Error
        err= abs(farrayPtr1D(i2) - (x+y))/(x+y)

        ! write(*,*) nodeIds(i1), "::",farrayPtr1D(i2), (x+y), " relErr=",err

        !! Set max
        if (err > maxErr) maxErr=err

	!! if error is too big report an error
	if (err > 0.1) then
            correct=.false.	
	endif	
 
        ! Advance to next owner
        i2=i2+1
     endif
  enddo

  ! write(*,*) "Max rel error= ",maxErr

   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


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

  
   ! Free the grids
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

 end subroutine test_regridCollapsedQuads


 subroutine test_regridR4(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R4), pointer :: farrayPtrXC_R4(:,:)
  real(ESMF_KIND_R4), pointer :: farrayPtrYC_R4(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
   integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD, relErr
  
  integer :: spherical_grid

  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  src_nx = 80
  src_ny = 80

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 70
  dst_ny = 70

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! setup dest. grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/), maxIndex=(/src_nx, src_ny/), &
                                  indexflag=ESMF_INDEX_GLOBAL, coordTypeKind=ESMF_TYPEKIND_R4, &
                                  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                  indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  errField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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

  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
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


  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC_R4, &
                            rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC_R4, &
                            rc=localrc)
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

        ! Set source coordinates as 0 to 360
        farrayPtrXC_R4(i1,i2) = REAL(i1-1)*src_dx

        farrayPtrYC_R4(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        ! init exact answer
        lon = farrayPtrXC_R4(i1,i2)
        lat = farrayPtrYC_R4(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact src data
        farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)
  
        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        !xfarrayPtr(i1,i2) = 1.0

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check results
  do lDE=0,dstlocalDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.005) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
	endif

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo

  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
      return
  endif
#endif


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

  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridR4

 subroutine test_regridPHMeshToGrid(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
   real(ESMF_KIND_R8) :: dst_minx,dst_miny
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy

  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

   integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:), elemCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems, numHexElems, numPentElems
  integer :: numQuadElems,numTriElems, numTotElems, numElemConn

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

  ! Establish the resolution of the grids
  dst_nx = 10
  dst_ny = 10

  ! Establish the coordinates of the grids
  dst_minx = 0.1
  dst_miny = 0.1
  
  dst_maxx = 1.9
  dst_maxy = 1.9

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
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
        nodeIds=nodeIds, nodeCoords=nodeCoords, &
        nodeOwners=nodeOwners, elementIds=elemIds,&
        elementTypes=elemTypes, elementConn=elemConn, &
        elementCoords=elemCoords, &
        rc=rc)
   if (rc /= ESMF_SUCCESS) return

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        meshloc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  do i1=1,numTotElems
     ! Get coordinates
     x=elemCoords(2*i1-1)
     y=elemCoords(2*i1)
     
     ! Set source function
     farrayPtr1D(i1) = 20.0+x+y
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)
   deallocate(elemCoords)


  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/2,2/), &
                                  coordSys=ESMF_COORDSYS_CART,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

    dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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
 

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
      do i1=clbnd(1),cubnd(1)
      do i2=clbnd(2),cubnd(2)

         ! Set source coordinates
        farrayPtrXC(i1,i2) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny
 
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE
 ! XMRKX
  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          unmappedAction=ESMF_UNMAPPEDACTION_IGNORE, &
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


  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

    
      !! check error
     do i1=clbnd(1),cubnd(1)
      do i2=clbnd(2),cubnd(2)
 
          ! Skip unmapped points 
         if (farrayPtr(i1,i2) < 1.0) cycle

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2)-(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))) > 0.0001) then
           correct=.false.	
	endif	
     enddo
     enddo

  enddo    ! lDE


#if 0
  ! Output Grid
  call ESMF_GridWriteVTK(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, filename="dstGrid", &
                         array1=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif

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


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridPHMeshToGrid


 subroutine test_regrid_gridufrm(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD, relErr
  real(ESMF_KIND_R8) :: coords(2)  
  integer :: localPet, petCount

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

  ! Establish the resolution of the grids
  src_nx = 80
  src_ny = 80

  dst_nx = 70
  dst_ny = 70

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! Create Src Grid
  srcGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/src_nx,src_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-80.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,80.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create Dst Grid
  dstGrid=ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/-50.0_ESMF_KIND_R8,-50.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/50.0_ESMF_KIND_R8,50.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  errField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
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

  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
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


  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
        endif

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact src data
        farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
        endif

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        !xfarrayPtr(i1,i2) = 1.0

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check results
  do lDE=0,dstlocalDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.005) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
	endif

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo

  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
      return
  endif
#endif


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

  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regrid_gridufrm


end program ESMF_FieldRegridUTest


