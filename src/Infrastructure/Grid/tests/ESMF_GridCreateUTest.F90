! $Id: ESMF_GridCreateUTest.F90,v 1.12 2004/05/20 22:17:33 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
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
! !PROGRAM: ESMF_GridUTest - One line general statement about this test
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
      '$Id: ESMF_GridCreateUTest.F90,v 1.12 2004/05/20 22:17:33 svasquez Exp $'
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
      character(ESMF_MAXSTR) :: validate_options
      character(ESMF_MAXSTR) :: print_options
      !type(ESMF_GridConfig) :: config_set
      !type(ESMF_GridConfig) :: config_get
      ! when get/set value routines enabled, comment these in and set
      ! the appropriate values, and remove the temporary integers.
      !<value type> :: value_set, value_get
      integer :: value_set, value_get


      integer :: counts(ESMF_MAXGRIDDIM)
      integer :: nDE_i, nDE_j
      type(ESMF_GridType) :: horz_gridtype, vert_gridtype
      type(ESMF_GridType) :: Rhorz_gridtype, Rvert_gridtype
      type(ESMF_GridStagger) :: horz_stagger, vert_stagger
      type(ESMF_GridStagger) :: Rhorz_stagger, Rvert_stagger
      type(ESMF_CoordSystem) :: horz_coord_system, vert_coord_system
      type(ESMF_CoordSystem) :: Rhorz_coord_system, Rvert_coord_system
      integer :: status
      integer :: phy_grid_id
      real(ESMF_KIND_R8) :: grid_min(3), grid_max(3)
      real(ESMF_KIND_R8) :: Rgrid_min(3), Rgrid_max(3)
      type(ESMF_Grid) :: grid, grid1, grid2
      type(ESMF_GridClass) :: grid_class
      type(ESMF_DELayout) :: layout, layout2
      type(ESMF_VM) :: vm


!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------

      !NEX_UTest
      call ESMF_Initialize(rc=status)
      write(name, *) "ESMF_Initialize Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !NEX_UTest
      call ESMF_VMGetGlobal(vm, status)
      write(name, *) "ESMF_VMGetGlobal Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      counts(1) = 10
      counts(2) = 12
      counts(3) = 15
      nDE_i = 2
      nDE_j = 2
      horz_gridtype = ESMF_GridType_XY
      horz_stagger = ESMF_GridStagger_A
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_gridtype = ESMF_GridType_XY
      vert_stagger = ESMF_GridStagger_VertCenter
      vert_coord_system = ESMF_CoordSystem_Cartesian
      grid_min(1) = -90.0
      grid_max(1) =  90.0
      grid_min(2) =   0.0
      grid_max(2) = 180.0
      grid_min(3) =   0.0
      grid_max(3) = 100.0
      gName = "test grid 1"
      !NEX_UTest

      layout = ESMF_DELayoutCreate(vm, rc=rc)
      write(name, *) "Creating a DELayout Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Create a Grid Test.
      !NEX_UTest
      grid = ESMF_GridCreateLogRectUniform(3, counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzGridType=horz_gridtype, &
                              horzStagger=horz_stagger, &
                              horzCoordSystem=horz_coord_system, &
                              vertGridType=horz_gridtype, &
                              vertStagger=vert_stagger, &
                              vertCoordSystem=horz_coord_system, &
                              delayout=layout, &
                              name=gName, rc=status)

      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a LogRectUniform Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !NEX_UTest 
      ! Destroy the Grid test
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the Grid Test"
      call ESMF_GridDestroy(grid, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
                              
#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------
      ! Create a Grid Test.
      !EX_UTest
      grid = ESMF_GridCreateLogRectUniform(3, counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzGridType=horz_gridtype, &
                              horzStagger=horz_stagger, &
                              horzCoordSystem=horz_coord_system, &
                              vertGridType=horz_gridtype, &
                              vertStagger=vert_stagger, &
                              vertCoordSystem=horz_coord_system, &
                              delayout=layout, &
                              name=gName, rc=status)

      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Creating a LogRectUniform Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Printing a Grid
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid horzGridType
      call ESMF_GridGet(grid, horzGridType=Rhorz_gridtype, rc=rc)
      write(name, *) "Get the Grid horzGridType Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the horzGridType is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rhorz_gridtype.eq.ESMF_GridType_XY), &
		      name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid vertGridType
      call ESMF_GridGet(grid, vertGridType=Rvert_gridtype, rc=rc)
      write(name, *) "Get the Grid vertGridType Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the vertGridType is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rvert_gridtype.eq.ESMF_GridType_XY), &
                      name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid horzStagger
      call ESMF_GridGet(grid, horzStagger=Rhorz_Stagger, rc=rc)
      write(name, *) "Get the Grid horzStagger Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the horzStagger is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rhorz_Stagger.eq.ESMF_GridStagger_A), &
                      name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid vertStagger
      call ESMF_GridGet(grid, vertStagger=Rvert_Stagger, rc=rc)
      write(name, *) "Get the Grid vertStagger Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the vertStagger is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rvert_Stagger.eq.ESMF_GridStagger_VertCenter), &
                      name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid horzCoordSystem
      call ESMF_GridGet(grid, horzCoordSystem=Rhorz_coord_system, rc=rc)
      write(name, *) "Get the Grid horzCoordSystem Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the horzCoordSystem is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rhorz_coord_system.eq.ESMF_CoordSystem_Cartesian), &
                      name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid vertCoordSystem
      call ESMF_GridGet(grid, vertCoordSystem=Rvert_coord_system, rc=rc)
      write(name, *) "Get the Grid vertCoordSystem Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the vertCoordSystem is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rvert_coord_system.eq.ESMF_CoordSystem_Cartesian), &
                       name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid minGlobalCoordPerDim
      call ESMF_GridGet(grid, minGlobalCoordPerDim=Rgrid_min, rc=rc)
      write(name, *) "Get the Grid minGlobalCoordPerDim Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the minGlobalCoordPerDim is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rgrid_min(1).eq.-90.0 .and. Rgrid_min(2).eq.0.0 .and. Rgrid_min(3).eq.0.0), &
                        name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid maxGlobalCoordPerDim
      call ESMF_GridGet(grid, maxGlobalCoordPerDim=Rgrid_max, rc=rc)
      write(name, *) "Get the Grid maxGlobalCoordPerDim Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the maxGlobalCoordPerDim is not correct"
      write(name, *) "Verify the Grid maxGlobalCoordPerDim Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. Rgrid_max(1).eq.90.0 .and. Rgrid_max(2).eq.180.0 .and. Rgrid_max(3).eq.100.0), &
                        name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      ! Get the Grid name
      call ESMF_GridGet(grid, name=RgName, rc=rc)
      write(name, *) "Get the Grid name Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS or the name is not correct"
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. RgName.eq."test grid 1"),  name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate the Grid test
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Validate the Grid Test"
      call ESMF_GridValidate(grid, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Distribute a Grid test
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Distribute the Grid Test"
      call ESMF_GridDistribute(grid, layout, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Destroy the Grid test
      write(failMsg, *) "Did not returned ESMF_SUCCESS"
      write(name, *) "Destroy the Grid Test"
      call ESMF_GridDestroy(grid, rc=rc)
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
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. RgName.eq."test grid 2"),  name, failMsg, result, ESMF_SRCLINE)
      print *, "Rgname = ", RgName
      print *, " rc = ", rc


      !------------------------------------------------------------------------
      !EX_UTest
      write(name, *) "ESMF_Finalize Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Finalize(status)
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif

      end program ESMF_GridCreateUTest
