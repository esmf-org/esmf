! $Id: ESMF_GridUTest.F90,v 1.46 2005/02/14 04:36:24 theurich Exp $
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
      '$Id: ESMF_GridUTest.F90,v 1.46 2005/02/14 04:36:24 theurich Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0
      integer :: attribute, attribute1, attribute2, att_count
      real :: attribute3, attribute4
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
      integer :: status, npets
      real(ESMF_KIND_R8) :: grid_min(3), grid_max(3)
      type(ESMF_Grid) :: grid, grid1
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

      call ESMF_Initialize(vm=vm, rc=status)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      print '(/, a, i3)' , "NUMBER_OF_PROCESSORS", npets

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
                              horzstagger=horz_stagger, &
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
      !EX_UTest
      !layout2 = ESMF_DELayoutCreate(vm, rc=rc)
      !call ESMF_DELayoutDestroy(layout2, status)
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzstagger=horz_stagger, &
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
                              horzstagger=horz_stagger, &
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


      !EX_UTest
      ! Test creating an internal Grid
      grid = ESMF_GridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=grid_min, &
                              maxGlobalCoordPerDim=grid_max, &
                              horzstagger=horz_stagger, &
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

      !-------------------------------------------------------------------------------
      !  Get Attribute count from a Grid
      !EX_UTest
       write(failMsg, *) "Did not return ESMF_SUCCESS" 
       write(name, *) "Get Attribute from a Grid Test"
       call ESMF_GridGetAttributeCount(grid1, attribute, rc=rc)
       call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
      !-------------------------------------------------------------------------------
      !  Verify Attribute count from a Grid
      !EX_UTest
       write(failMsg, *) "Attribute count is incorrect" 
       write(name, *) "Verify Attribute count from a Grid Test"
       call ESMF_Test((attribute.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------------

      !EX_UTest
      ! Set Attributes in a empty Grid 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting Attributes in a Grid Test"
      call ESMF_GridSetAttribute(grid1, "test_attribute", 123456789, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

     !-------------------------------------------------------------------------------
     !
     !  Set an Attribute in a Grid
     !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_GridSetAttribute(grid1, "test_attribute", 123456789, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

     !-------------------------------------------------------------------------------
     !
     !  Set an Attribute in a Grid
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_GridSetAttribute(grid1, "test_attribute1", 0, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


     !-------------------------------------------------------------------------------
     !
     !  Set an Attribute in an Grid
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_GridSetAttribute(grid1, "test_attribute2", 0.0, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

     !-------------------------------------------------------------------------------

     !   
     !  Set an Attribute in a Grid
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_GridSetAttribute(grid1, "test_attribute3", 6789, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
     !-------------------------------------------------------------------------------
     !   
     !  Set an Attribute in a Grid
     !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a Grid Test"
      call ESMF_GridSetAttribute(grid1, "test_attribute4", 5.87, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
     !-------------------------------------------------------------------------------
     !   
     !  Get Attribute count from a Grid
     !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Get an Attribute from a Grid Test"
      call ESMF_GridGetAttributeCount(grid1, attribute, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !-------------------------------------------------------------------------------
      !   
      !  Verify Attribute count from a Grid
      !EX_UTest
      write(failMsg, *) "Attribute count is incorrect" 
      write(name, *) "Verify Attribute count from a Grid Test"
      call ESMF_Test((attribute.eq.5), name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------------
      !
      !  Get an Attribute from a Grid
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get an Attribute from a Grid Test"
      call ESMF_GridGetAttribute(grid1, "test_attribute", attribute, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------------
      !
      !  Verify the value of the Attribute
      !EX_UTest
      write(failMsg, *) "Attribute value is wrong"
      write(name, *) "Verify Attribute value from a Grid Test"
      call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------------
      !
      !  Get an Attribute from a Grid
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get an Attribute from a Grid Test"
      call ESMF_GridGetAttribute(grid1, "test_attribute", attribute, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------------
      !
      !  Verify the value of the Attribute
      !EX_UTest
      write(failMsg, *) "Attribute value is wrong"
      write(name, *) "Verify Attribute value from a Grid Test"
      call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)
      !-------------------------------------------------------------------------------
      !
      !  Get Attribute Info from a Grid Test
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get Attribute Info from a Grid Test"
      call ESMF_GridGetAttributeInfo(grid1, "test_attribute", datatype=att_datatype, &
                                      datakind=att_datakind, count=att_count, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------------
      !
      !  Verify datatype of Attribute
      !EX_UTest
       write(failMsg, *) "Attribute datatype is wrong"
       write(name, *) "Verify Attribute datatype from a Grid Test"
       call ESMF_Test((att_datatype.eq.ESMF_DATA_INTEGER), name, failMsg, result, ESMF_SRCLINE)
      !-------------------------------------------------------------------------------
      !
      !  Verify datakind of Attribute
      !EX_UTest
       write(failMsg, *) "Attribute datakind is wrong"
       write(name, *) "Verify Attribute datakind from a Grid Test"
       call ESMF_Test((att_datakind.eq.ESMF_I4), name, failMsg, result, ESMF_SRCLINE)
      !-------------------------------------------------------------------------------
      !
      !  Verify count of Attribute
       !EX_UTest
       write(failMsg, *) "Attribute count is wrong"
       write(name, *) "Verify Attribute count from a Grid Test"
       call ESMF_Test((att_count.eq.1), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------------
      !
      !  Get an Attribute from Grid with wrong data type
       !EX_UTest
       write(failMsg, *) "Should not return ESMF_SUCCESS"
       write(name, *) "Get a Wrong Data type Attribute from a Grid Test"
       call ESMF_GridGetAttribute(grid1, "test_attribute4", attribute, rc=rc)
       call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------------
      !
      !EX_UTest
      ! Test destroy subroutine
      call  ESMF_GridDestroy(grid1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a Grid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc




#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      call ESMF_Finalize(rc=status)
      end program ESMF_GridUTest
