! $Id: ESMF_GridUTest.F90,v 1.34 2004/06/02 22:54:53 svasquez Exp $
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
      program ESMF_GridUTest

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
! The code in this file drives F90 Grid unit tests.
! The companion file ESMF\_Grid.F90 contains the definitions for the
! Grid methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
    
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_GridUTest.F90,v 1.34 2004/06/02 22:54:53 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! local variables needed to pass into function/subroutine calls
      character(ESMF_MAXSTR) :: validate_options
      character(ESMF_MAXSTR) :: print_options, gname
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
      type(ESMF_GridHorzStagger) :: horz_stagger, vert_stagger
      type(ESMF_CoordSystem) :: horz_coord_system, vert_coord_system
      integer :: status
      integer :: phy_grid_id
      real(ESMF_KIND_R8) :: grid_min(2), grid_max(2)
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

      call ESMF_Initialize(rc=status)

#ifdef ESMF_EXHAUSTIVE
      call ESMF_VMGetGlobal(vm, status)

      !------------------------------------------------------------------------
      counts(1) = 10
      counts(2) = 12
      nDE_i = 2
      nDE_j = 2
      horz_stagger = ESMF_GRID_HORZ_STAGGER_A
      grid_min(1) = 0.0
      grid_max(1) = 10.0
      grid_min(2) = 0.0
      grid_max(2) = 12.0
      name = "test grid 1"




      !------------------------------------------------------------------------
      !EX_UTest
      layout = ESMF_DELayoutCreate(vm, rc=rc)
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzStagger=horz_stagger, &
                              name=name, rc=status)

      call ESMF_GridDistribute(grid, delayout=layout, rc=status)

      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Printing a Grid
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! The following code works ok because the layout is explicitly 
      ! destroyed first before being used in the grid create (which is 
      ! expected to fail).  but this still crashes randomly if the layout
      ! object is left completely uninitialized.  this should be addressed.
      ! Bug report 796975 has been filed 
      !  Comment out the Destroy because it crashes
      !EX_UTest
      layout2 = ESMF_DELayoutCreate(vm, rc=rc)
      !call ESMF_DELayoutDestroy(layout2, status)
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzStagger=horz_stagger, &
                              name=name, rc=status)

      call ESMF_GridDistribute(grid, delayout=layout2, rc=status)

      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Creating a Grid with a non created layout Test"
      call ESMF_Test((status.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      grid_min(1) = 7.0
      grid_max(1) = -10.0
      grid_min(2) = 5.0
      grid_max(2) = 1.0

      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzStagger=horz_stagger, &
                              name=name, rc=status)

      call ESMF_GridDistribute(grid, delayout=layout, rc=status)

      !EX_UTest


      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a Grid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get Coord from a Grid
      call ESMF_GridGetCoord(grid, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get Coord from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !The following code is commented out because it does not compile
      !Bug 927094 has been opened
      ! Setting the horzGridType of a Grid
      !horz_gridtype = ESMF_GridType_LatLon
      !call ESMF_GridSet(grid, horzGridType=horz_gridtype,rc=rc)
      !write(failMsg, *) "Did not return ESMF_SUCCESS"
      !write(name, *) "Setting the horz_gridtype of  Grid Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Printing a Grid
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! Test creating an internal Grid
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzStagger=horz_stagger, &
                              name=name, rc=status)

      call ESMF_GridDistribute(grid, delayout=layout, rc=status)

        write(failMsg, *) "Did not return ESMF_SUCCESS"
        write(name, *) "Creating an Internal Grid Test"
        call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Printing a Grid
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !EX_UTest


      ! Test destroy subroutine
      call  ESMF_GridDestroy(grid, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc

      !----------------------------------------------------------------------

      !EX_UTest


      ! Test creation of empty grid
      grid1 =  ESMF_GridCreate(rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an empty Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding a name to an empty Grid
      call ESMF_GridAddAttribute(grid1, name="GRID_ONE", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a name to a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !EX_UTest
      ! Test getting the name from an empty Grid
      call ESMF_GridGetAttributes(grid1, name=gname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a name from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------


      !EX_UTest
      ! Verify the name from an empty Grid is correct
      write(failMsg, *) "Returned wrong name"
      write(name, *) "Verifying a name from a Grid Test"
      call ESMF_Test((gname.eq."GRID_ONE"), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "gname= ", gname

      !------------------------------------------------------------------------
      ! The following code crashes, bug 722780 has been filed

      ! name = "test grid 1"

      ! call ESMF_GridAddPhysGrid(grid_type, counts=counts, &
      ! 		     physgrid_id=phy_grid_id, &
      !                      y_min=y_min, y_max=y_max, &
      ! 		     physgrid_name=name, rc=status)

      ! write(failMsg, *) "Did not return ESMF_SUCCESS"
      ! write(name, *) "Adding a Physical Grid Test"
      ! call ESMF_Test((status.eq.ESMF_SUCCESS), &
      !                   name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      call ESMF_Finalize(status)
      end program ESMF_GridUTest
