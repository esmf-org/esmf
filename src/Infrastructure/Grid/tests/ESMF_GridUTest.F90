! $Id: ESMF_GridUTest.F90,v 1.6 2003/04/16 22:17:25 svasquez Exp $
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
      '$Id: ESMF_GridUTest.F90,v 1.6 2003/04/16 22:17:25 svasquez Exp $'
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
      integer :: halo_width
      real :: x_min, x_max, y_min, y_max
      type(ESMF_Grid) :: grid
      type(ESMF_GridType) :: grid_type
      type(ESMF_DELayout) :: layout



#ifdef ESMF_EXHAUSTIVE

      ! perform exhaustive tests here;
      !   see #else below for non-exhaustive tests
      ! future release will use run-time switching mechanism

      ! for deep classes, keep create/construct routine and remove init
      ! for shallow classes, keep init and remove create/construct
     
      ! test dynamic allocation of ESMF_Grid
      grid = ESMF_GridCreate(args, rc)
      write(name, *) "ESMF_GridCreate"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_Grid
      call ESMF_GridConstruct(grid, args, rc)
      write(name, *) "ESMF_GridConstruct"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test initialization of members of statically allocated ESMF_Grid
      !   may want to read back values via Get methods for comparison
      call ESMF_GridInit(grid, args, rc)
      write(name, *) "ESMF_GridInit"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      call ESMF_GridSetConfig(grid, config_set, rc)
      write(name, *) "ESMF_GridSetConfig"
      write(failMsg, *) "rc =", rc, ", config_set =", config_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      call ESMF_GridGetConfig(grid, config_get, rc)
      write(name, *) "ESMF_GridGetConfig"
      write(failMsg, *) "rc =", rc, ", config_get =", config_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_Grid members values
      !call ESMF_GridSet<Value>(grid, value_set, rc)
      rc = ESMF_FAILURE  ! remove this when this test enabled
      write(name, *) "ESMF_GridSet<Value>"
      write(failMsg, *) "rc =", rc, ", value_set =", value_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_Grid members values,
      !   compare to values set previously
      !call ESMF_GridGet<Value>(grid, value_get, rc)
      rc = ESMF_FAILURE  ! remove this when this test enabled
      write(name, *) "ESMF_GridGet<Value>"
      write(failMsg, *) "rc =", rc, ", value_get =", value_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      call ESMF_GridValidate(grid, validate_options, rc)
      write(name, *) "ESMF_GridValidate"
      write(failMsg, *) "rc =",rc,", validate_options =", trim(validate_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_GridPrint(grid, print_options, rc)
      write(name, *) "ESMF_GridPrint"
      write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_Grid.   only valid for deep classes; remove for shallow
      call ESMF_GridDestruct(grid, rc)
      write(name, *) "ESMF_GridDestruct"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_Grid
      !   also tests destructor
      call ESMF_GridDestroy(grid, rc)
      write(name, *) "ESMF_GridDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#else
      ! perform non-exhaustive tests here;
      print *, "******************STATE NON-EXHAUSTIVE UNIT TESTS****************************"
      print *

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
      halo_width = 1
      layout = ESMF_DELayoutCreate(rc=rc)

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
			     halo_width=halo_width, &
                             name=name, rc=status)

      write(failMsg, *) ""
      write(name, *) "Creating a Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Printing a Grid
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call  ESMF_GridDestroy(grid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc
      !------------------------------------------------------------------------

      name = "test grid 1"

      call ESMF_GridAddPhysGrid(grid_type, i_max=i_max, j_max=j_max, &
		     physgrid_id=phy_grid_id, &
                             y_min=y_min, y_max=y_max, &
			     physgrid_name=name, rc=status)

      write(failMsg, *) ""
      write(name, *) "Adding a Physical Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_GridUTest
