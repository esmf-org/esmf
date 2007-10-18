! $Id: ESMF_GridUTest.F90,v 1.47.4.3 2007/10/18 02:42:53 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
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
      '$Id: ESMF_GridUTest.F90,v 1.47.4.3 2007/10/18 02:42:53 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0
      integer :: attribute, att_count
      type(ESMF_DataType) :: att_datatype
      type(ESMF_DataKind) :: att_datakind
      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! local variables needed to pass into function/subroutine calls
      !character(ESMF_MAXSTR) :: validate_options
      !character(ESMF_MAXSTR) :: print_options, gname
      character(ESMF_MAXSTR) :: gname
      !type(ESMF_GridConfig) :: config_set
      !type(ESMF_GridConfig) :: config_get



      integer :: counts(ESMF_MAXGRIDDIM)
      integer :: nDE_i, nDE_j
      type(ESMF_GridType) :: horz_gridtype
      type(ESMF_GridHorzStagger) :: horz_stagger
      integer :: status
      real(ESMF_KIND_R8) :: grid_min(3), grid_max(3)
      type(ESMF_Grid) :: grid, grid1
      type(ESMF_DELayout) :: layout, layout2
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
      !NEX_UTest
      layout = ESMF_DELayoutCreate(vm, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a DELayout Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !NEX_UTest
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      call ESMF_GridDistribute(grid, delayout=layout, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Printing a Grid
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Destroying a Grid
      call ESMF_GridDestroy(grid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a grid to use in next test
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a Grid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! The following code works ok because the layout is explicitly 
      ! destroyed first before being used in the grid distribute (which is 
      ! expected to fail).  but this still crashes randomly if the layout
      ! object is left completely uninitialized.  this should be addressed.
      layout2 = ESMF_DELayoutCreate(vm, rc=rc)
      call ESMF_DELayoutDestroy(layout2, status)
      call ESMF_GridDistribute(grid, delayout=layout2, rc=status)
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Distributing a Grid with a non-created layout Test"
      call ESMF_Test((status.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest

      grid_min(1) = 7.0
      grid_max(1) = -10.0
      grid_min(2) = 5.0
      grid_max(2) = 1.0

      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a Grid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_GridDistribute(grid, delayout=layout, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a Grid  with negative x_max Test"
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
      !EX_UTest
      ! Setting the horzGridType of a Grid
      horz_gridtype = ESMF_GRID_TYPE_LATLON
      call ESMF_GridSet(grid, horzgridtype=horz_gridtype,rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting the horz_gridtype of  Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Printing a Grid
      call ESMF_GridPrint(grid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test creating an internal Grid
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Internal Grid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_GridDistribute(grid, delayout=layout, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Distributing an Internal Grid Test"
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

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test creation of empty grid
      grid1 =  ESMF_GridCreate(rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an empty Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test adding a name to an empty Grid
      call ESMF_GridSet(grid1, name="GRID_ONE", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a name to a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting the name from an empty Grid
      call ESMF_GridGet(grid1, name=gname, rc=rc)
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
      !EX_UTest
      !  Get Attribute count from a Grid
      call ESMF_GridGetAttributeCount(grid1, attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Get Attribute from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify Attribute count from a Grid
      write(failMsg, *) "Attribute count is incorrect" 
      write(name, *) "Verify Attribute count from a Grid Test"
      call ESMF_Test((attribute.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Set Attributes in a empty Grid 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting Attributes in a Grid Test"
      call ESMF_GridSetAttribute(grid1, "test_attribute", 123456789, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in a Grid
      call ESMF_GridSetAttribute(grid1, "test_attribute", 123456789, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in a Grid
      call ESMF_GridSetAttribute(grid1, "test_attribute1", 0, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in an Grid
      call ESMF_GridSetAttribute(grid1, "test_attribute2", 0.0, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in a Grid
      call ESMF_GridSetAttribute(grid1, "test_attribute3", 6789, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in a Grid
      call ESMF_GridSetAttribute(grid1, "test_attribute4", 5.87, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
       
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Get Attribute count from a Grid
      call ESMF_GridGetAttributeCount(grid1, attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Get an Attribute from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
       
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Verify Attribute count from a Grid
      write(failMsg, *) "Attribute count is incorrect" 
      write(name, *) "Verify Attribute count from a Grid Test"
      call ESMF_Test((attribute.eq.5), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Get an Attribute from a Grid
      call ESMF_GridGetAttribute(grid1, "test_attribute", attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get an Attribute from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Verify the value of the Attribute
      write(failMsg, *) "Attribute value is wrong"
      write(name, *) "Verify Attribute value from a Grid Test"
      call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Get an Attribute from a Grid
      call ESMF_GridGetAttribute(grid1, "test_attribute", attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get an Attribute from a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Verify the value of the Attribute
      write(failMsg, *) "Attribute value is wrong"
      write(name, *) "Verify Attribute value from a Grid Test"
      call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Get Attribute Info from a Grid Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get Attribute Info from a Grid Test"
      call ESMF_GridGetAttributeInfo(grid1, "test_attribute", &
                                datatype=att_datatype, datakind=att_datakind, &
                                count=att_count, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify datatype of Attribute
      write(failMsg, *) "Attribute datatype is wrong"
      write(name, *) "Verify Attribute datatype from a Grid Test"
      call ESMF_Test((att_datatype.eq.ESMF_DATA_INTEGER), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify datakind of Attribute
      write(failMsg, *) "Attribute datakind is wrong"
      write(name, *) "Verify Attribute datakind from a Grid Test"
      call ESMF_Test((att_datakind.eq.ESMF_I4), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify count of Attribute
      write(failMsg, *) "Attribute count is wrong"
      write(name, *) "Verify Attribute count from a Grid Test"
      call ESMF_Test((att_count.eq.1), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get an Attribute from Grid with wrong data type
      call ESMF_GridGetAttribute(grid1, "test_attribute4", attribute, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS"
      write(name, *) "Get a Wrong Data type Attribute from a Grid Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test destroy subroutine
      call  ESMF_GridDestroy(grid1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


#endif
  
      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_GridUTest
