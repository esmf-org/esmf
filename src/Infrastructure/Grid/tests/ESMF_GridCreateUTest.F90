! $Id: ESMF_GridCreateUTest.F90,v 1.3 2004/03/19 05:00:53 svasquez Exp $
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
      '$Id: ESMF_GridCreateUTest.F90,v 1.3 2004/03/19 05:00:53 svasquez Exp $'
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
      character(ESMF_MAXSTR) :: print_options
      !type(ESMF_GridConfig) :: config_set
      !type(ESMF_GridConfig) :: config_get
      ! when get/set value routines enabled, comment these in and set
      ! the appropriate values, and remove the temporary integers.
      !<value type> :: value_set, value_get
      integer :: value_set, value_get


      integer :: counts(ESMF_MAXGRIDDIM)
      integer :: nDE_i, nDE_j
      type(ESMF_GridKind) :: horz_gridtype, vert_gridtype
      type(ESMF_GridStagger) :: horz_stagger, vert_stagger
      type(ESMF_CoordSystem) :: horz_coord_system, vert_coord_system
      integer :: status
      integer :: phy_grid_id
      real(ESMF_KIND_R8) :: grid_min(3), grid_max(3)
      type(ESMF_Grid) :: grid, grid1, grid2
      type(ESMF_GridType) :: grid_type
      type(ESMF_DELayout) :: layout, layout2


!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------

      call ESMF_Initialize(status)

      !------------------------------------------------------------------------
      counts(1) = 10
      counts(2) = 12
      counts(3) = 15
      nDE_i = 2
      nDE_j = 2
      horz_gridtype = ESMF_GridKind_XY
      horz_stagger = ESMF_GridStagger_A
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_gridtype = ESMF_GridKind_XY
      vert_stagger = ESMF_GridStagger_VertCenter
      vert_coord_system = ESMF_CoordSystem_Cartesian
      grid_min(1) = -90.0
      grid_max(1) =  90.0
      grid_min(2) =   0.0
      grid_max(2) = 180.0
      grid_min(3) =   0.0
      grid_max(3) = 100.0
      name = "test grid 1"
      !NEX_UTest

      layout = ESMF_DELayoutCreate(rc=rc)
      write(name, *) "Creating a DELayout Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! The following code is commented out until bug 918863 is addressed.
      !grid = ESMF_GridCreateLogRectUniform(3, counts=counts, &
                              !minGlobalCoordPerDim=grid_min, &
                              !maxGlobalCoordPerDim=grid_max, &
                              !horzGridKind=horz_gridtype, &
                              !horzStagger=horz_stagger, &
                              !horzCoordSystem=horz_coord_system, &
                              !vertGridKind=horz_gridtype, &
                              !vertStagger=horz_stagger, &
                              !vertCoordSystem=horz_coord_system, &
                              !layout=layout, &
                              !name=name, rc=status)

      !write(failMsg, *) "Returned ESMF_FAILURE"
      !write(name, *) "Creating a Grid Test"
      !call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Printing a Grid
      !call ESMF_GridPrint(grid, "", rc=rc)
      !write(failMsg, *) ""
      !write(name, *) "Printing a Grid Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! TODO: add more tests here


  
      !------------------------------------------------------------------------

      call ESMF_Finalize(status)

      end program ESMF_GridCreateUTest
