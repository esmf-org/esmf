! $Id: ESMF_GridCreateUTest.F90,v 1.41 2007/01/23 21:16:06 svasquez Exp $
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
      program ESMF_GridCreateUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_GridUTest - These tests verifies Grid functionalities.
!
! !DESCRIPTION:
!
! The code in this file specializes on the various ways of creating 
! {\tt Grid}s, including 3D and others.
! The companion test file {\tt ESMF\_GridUTest.F90} contains general
!  grid tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
    
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_GridCreateUTest.F90,v 1.41 2007/01/23 21:16:06 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name, gName, Rgname

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! local variables needed to pass into function/subroutine calls
      !character(ESMF_MAXSTR) :: validate_options
      !character(ESMF_MAXSTR) :: print_options
      !type(ESMF_GridConfig) :: config_set
      !type(ESMF_GridConfig) :: config_get

      integer :: i, counts(2)
      integer :: nDE_i, nDE_j
      type(ESMF_GridHorzStagger) :: horz_stagger, Rhorz_stagger
      type(ESMF_GridVertStagger) :: vert_stagger
      integer :: status
      integer, dimension (4) :: DEDim1
      integer, dimension (1) :: DEDimX
      integer, dimension (10000) :: DEDim2
      real(ESMF_KIND_R8) :: delta(15), grid_min(3), grid_max(3)
      real(ESMF_KIND_R8) :: coord1(21), coord2(16)
      real(ESMF_KIND_R8) :: Rgrid_min(3), Rgrid_max(3)
      type(ESMF_Grid) :: grid, grid1, grid2, grid3
      type(ESMF_DELayout) :: layout
      type(ESMF_VM) :: vm


!------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)


      !------------------------------------------------------------------------
      !NEX_UTest
      call ESMF_VMGetGlobal(vm, status)
      write(name, *) "ESMF_VMGetGlobal Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! set up values to be used below
      counts(1) = 10
      counts(2) = 12
      DEDim1(1) = 1
      DEDim1(2) = 5
      DEDim1(3) = 2
      DEDim1(4) = 2
      DEDim2(1) = 12
      nDE_i = 2
      nDE_j = 2
      horz_stagger      = ESMF_GRID_HORZ_STAGGER_A
      vert_stagger      = ESMF_GRID_VERT_STAGGER_CENTER
      grid_min(1) = -90.0
      grid_max(1) =  90.0
      grid_min(2) =   0.0
      grid_max(2) = 180.0
      grid_min(3) =  90.0
      grid_max(3) = 100.0
      delta(1:15) = 6.6667
      gName = "test grid 1"


      !------------------------------------------------------------------------
      !NEX_UTest
      layout = ESMF_DELayoutCreate(vm, rc=rc)
      write(name, *) "Creating a DELayout Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Create a HorzXYUni Grid Test.
      !NEX_UTest
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a LogRectUniform Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Add Vert Height Test
      !NEX_UTest
      call ESMF_GridAddVertHeight(grid, delta, vertstagger=vert_stagger, &
                                  rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Add Vert Height Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Grid Distribute Test
      !NEX_UTest
      call ESMF_GridDistribute(grid, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Grid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the Grid test
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the Grid Test"
      call ESMF_GridDestroy(grid, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      ! Create a HorzLatLon Grid Test.
      !NEX_UTest
      coord1(1) = grid_min(1)
      do i = 2, size(coord1)
        coord1(i) = coord1(i-1) + 0.50d0
      enddo
      coord2(1) = grid_min(2)
      do i = 2, size(coord2)
        coord2(i) = coord2(i-1) + 0.40d0
      enddo
      grid1 = ESMF_GridCreateHorzLatLon(coord1=coord1, coord2=coord2, &
                                        horzstagger=horz_stagger, &
                                        name=gName, rc=status)

      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a HorzLatLon Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Grid Distribute Test
      !NEX_UTest
      call ESMF_GridDistribute(grid1, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Grid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the Grid test
      call ESMF_GridDestroy(grid1, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      ! Create a HorzLatLon Grid Uni Test.
      !NEX_UTest
      grid2 = ESMF_GridCreateHorzLatLonUni(counts=counts, &
			      minGlobalCoordPerDim=grid_min, &
                              deltaPerDim=(/0.60d0, 0.55d0/), &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)

      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a HorzLatLon Uni Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Grid Distribute Test
      !NEX_UTest
      call ESMF_GridDistribute(grid2, delayout=layout, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Grid Distribute Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the Grid test
      call ESMF_GridDestroy(grid2, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
#ifdef ESMF_EXHAUSTIVE


      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a destroyed Grid
      call ESMF_GridDestroy(grid2, rc=rc)
      write(failMsg, *) "Did not returned ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed Grid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)
                              
      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy a Non-created Grid
      call ESMF_GridDestroy(grid2, rc=rc)
      write(failMsg, *) "Did not returned ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a non-created Grid Test"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid horzStagger from a non-created Grid
      call ESMF_GridGet(grid3, horzstagger=Rhorz_stagger, rc=rc)
      write(name, *) "Get the Grid horzStagger from non-created Grid Test"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED "
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      ! Create a Grid Test.
      !EX_UTest
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzstagger=horz_stagger, &
                              name=gName, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a LogRectUniform Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Grid Add Vert Height Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Add Grid Vert Height Test"
      call ESMF_GridAddVertHeight(grid, delta, vertstagger=vert_stagger, &
                                  rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Grid Distribute Test.
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Grid Distribute Test"
      call ESMF_GridDistribute(grid, delayout=layout, countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDim2, rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a Grid
      !EX_UTest
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid horzStagger
      call ESMF_GridGet(grid, horzstagger=Rhorz_stagger, rc=rc)
      write(name, *) "Get the Grid horzStagger Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the horzStagger is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rhorz_Stagger.eq.horz_stagger), &
                      name, failMsg, result, ESMF_SRCLINE)
  
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid minGlobalCoordPerDim
      call ESMF_GridGet(grid, minGlobalCoordPerDim=Rgrid_min, rc=rc)
      write(name, *) "Get the Grid minGlobalCoordPerDim Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the minGlobalCoordPerDim is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rgrid_min(1).eq.-90.0 .and. Rgrid_min(2).eq.0.0), &
                        name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid maxGlobalCoordPerDim
      call ESMF_GridGet(grid, maxGlobalCoordPerDim=Rgrid_max, rc=rc)
      write(name, *) "Get the Grid maxGlobalCoordPerDim Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the maxGlobalCoordPerDim is not correct"
      write(name, *) "Verify the Grid maxGlobalCoordPerDim Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rgrid_max(1).eq.90.0 .and. Rgrid_max(2).eq.180.0), &
                        name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid name
      call ESMF_GridGet(grid, name=RgName, rc=rc)
      write(name, *) "Get the Grid name Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the name is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. trim(RgName).eq."test grid 1"), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate the Grid test
      call ESMF_GridValidate(grid, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Validate the Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Distribute a Grid test
      call ESMF_GridDistribute(grid, layout, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Distribute the Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy the Grid test
      call ESMF_GridDestroy(grid, rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Create an Empty Grid Test.
      gName = "test grid 2"
      grid1 = ESMF_GridCreate(name=gName, rc=status)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating an empty Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid name
      call ESMF_GridGet(grid1, name=RgName, rc=rc)
      write(name, *) "Get the Grid name Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the name is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. trim(RgName).eq."test grid 2"), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Rgname = ", trim(RgName)
      print *, " rc = ", rc


      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      ! Create a Grid Test.
      !EX_UTest
      grid = ESMF_GridCreateHorzXY(coord1=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   coord2=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   horzstagger=ESMF_GRID_HORZ_STAGGER_C_SW, &
                                   name="coordinate grid", rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a grid with explicit coordinates Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Grid Distribute Test - should fail.
      !EX_UTest
      DEDim1(:) = (/ 2, 3, 3, 3 /)
      DEDimX(1) = 11
      write(failMsg, *) "Returned ESMF_SUCCESS when expecting failure"
      write(name, *) "Grid Distribute Test"
      call ESMF_GridDistribute(grid, delayout=layout, &
                               countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDimX, rc=status)
      call ESMF_Test((status.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Create a Grid Test.
      !EX_UTest
      grid = ESMF_GridCreateHorzXY(coord1=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   coord2=(/ 1d0,2d0,3d0,4d0,5d0,6d0,7d0,8d0,9d0,10d0,11d0 /), &
                                   horzstagger=ESMF_GRID_HORZ_STAGGER_C_SW, &
                                   name="coordinate grid", rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating a grid with explicit coordinates Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Grid Distribute Test - should succeed.
      !EX_UTest
      DEDim1(:) = (/ 2, 3, 2, 3 /)
      DEDimX(1) = 10
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Grid Distribute Test"
      call ESMF_GridDistribute(grid, delayout=layout, &
                               countsPerDEDim1=DEDim1, &
                               countsPerDEDim2=DEDimX, rc=status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a Grid
      !EX_UTest
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_GridCreateUTest
