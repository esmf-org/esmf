! $Id: ESMF_GridUTest.F90,v 1.14 2003/07/17 20:15:50 nscollins Exp $
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
      use ESMF_GridMod  ! the class to test
      use ESMF_Mod
      use ESMF_BaseMod
      use ESMF_DELayoutMod
      use ESMF_DataMapMod
      use ESMF_CompMod
      use ESMF_IOMod
      use ESMF_FieldMod
      use ESMF_StateMod
!      use ArraysGlobalMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_GridUTest.F90,v 1.14 2003/07/17 20:15:50 nscollins Exp $'
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



      integer :: i_max, j_max
      integer :: nDE_i, nDE_j
      integer :: horz_gridtype, vert_gridtype
      integer :: horz_stagger, vert_stagger
      integer :: horz_coord_system, vert_coord_system
      integer :: status
      integer :: phy_grid_id
      real :: x_min, x_max, y_min, y_max
      type(ESMF_Grid) :: grid, grid1, grid2
      type(ESMF_GridType) :: grid_type
      type(ESMF_DELayout) :: layout


!--------------------------------------------------------------------------------
!     The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!     always run. When the environment variable, EXHAUSTIVE, is set to ON then
!     the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!     to OFF, then only the sanity unit tests.
!     Special strings (Non-exhaustive and exhaustive) have been
!     added to allow a script to count the number and types of unit tests.
!--------------------------------------------------------------------------------

      call ESMF_FrameworkInitialize(status)

      !------------------------------------------------------------------------
      i_max = 10
      j_max = 12
      nDE_i = 2
      nDE_j = 2
      horz_gridtype = ESMF_GridType_XY
      vert_gridtype = ESMF_GridType_Unknown
      horz_stagger = ESMF_GridStagger_A
      vert_stagger = ESMF_GridStagger_Unknown
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_coord_system = ESMF_CoordSystem_Unknown
      x_min = 0.0
      x_max = 10.0
      y_min = 0.0
      y_max = 12.0
      name = "test grid 1"

      !NEX_UTest


      ! Creating a layout test
      layout = ESMF_DELayoutCreate(rc=rc)
      write(name, *) "Creating a DELayout Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

     grid = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             x_min=x_min, x_max=x_max, &
			     y_min=y_min, y_max=y_max, &
		             layout=layout, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
                             name=name, rc=status)

      !NEX_UTest


      write(failMsg, *) ""
      write(name, *) "Creating a Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! Test creating an internal Grid
      ! grid2 = ESMF_GridCreateInternal(i_max=i_max, j_max=j_max, &
      !                       x_min=x_min, x_max=x_max, &
      !		             layout=layout, &
      !                       horz_gridtype=horz_gridtype, &
      !                       vert_gridtype=vert_gridtype, &
      !                       horz_stagger=horz_stagger, &
      !                       vert_stagger=vert_stagger, &
      !                       horz_coord_system=horz_coord_system, &
      !                       vert_coord_system=vert_coord_system, &
      !                       name=name, rc=status)

      !  write(failMsg, *) ""
      ! write(name, *) "Creating an Internal Grid Test"
      ! call ESMF_Test((status.eq.ESMF_SUCCESS), &
      !               name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif

      !NEX_UTest


      ! Printing a Grid
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !NEX_UTest


      ! Test destroy subroutine
      call  ESMF_GridDestroy(grid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc

      !----------------------------------------------------------------------

      !NEX_UTest


      ! Test creation ofempty 
      grid1 =  ESMF_GridCreate( rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc

      !------------------------------------------------------------------------



#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------
      ! The following code crashes, bug 722780 has been filed

      ! name = "test grid 1"

      ! call ESMF_GridAddPhysGrid(grid_type, i_max=i_max, j_max=j_max, &
      ! 		     physgrid_id=phy_grid_id, &
      !                      y_min=y_min, y_max=y_max, &
      ! 		     physgrid_name=name, rc=status)

      ! write(failMsg, *) ""
      ! write(name, *) "Adding a Physical Grid Test"
      ! call ESMF_Test((status.eq.ESMF_SUCCESS), &
      !                   name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      call ESMF_FrameworkFinalize(status)
      end program ESMF_GridUTest
