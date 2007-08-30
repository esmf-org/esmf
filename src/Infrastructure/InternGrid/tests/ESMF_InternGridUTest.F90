! $Id: ESMF_InternGridUTest.F90,v 1.3 2007/08/30 05:06:38 cdeluca Exp $
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
      program ESMF_IGridUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_IGridUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 IGrid unit tests.
! The companion file ESMF\_IGrid.F90 contains the definitions for the
! IGrid methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
    
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternGridUTest.F90,v 1.3 2007/08/30 05:06:38 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0
      integer :: attribute, att_count
      type(ESMF_TypeKind) :: att_typekind
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
      !type(ESMF_IGridConfig) :: config_set
      !type(ESMF_IGridConfig) :: config_get



      integer :: counts(ESMF_MAXIGRIDDIM)
      integer :: nDE_i, nDE_j
      type(ESMF_IGridType) :: horz_igridtype
      type(ESMF_IGridHorzStagger) :: horz_stagger
      integer :: status
      real(ESMF_KIND_R8) :: igrid_min(3), igrid_max(3)
      type(ESMF_IGrid) :: igrid, igrid1
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
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_A
      igrid_min(1) = 0.0
      igrid_max(1) = 10.0
      igrid_min(2) = 0.0
      igrid_max(2) = 12.0
      name = "test igrid 1"




      !------------------------------------------------------------------------
      !NEX_removeUTest
      layout = ESMF_DELayoutCreate(vm, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a DELayout Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !NEX_removeUTest
      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=igrid_min, &
                              maxGlobalCoordPerDim=igrid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      call ESMF_IGridDistribute(igrid, delayout=layout, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! Printing a IGrid
      call ESMF_IGridPrint(igrid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! Destroying a IGrid
      call ESMF_IGridDestroy(igrid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Create a igrid to use in next test
      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=igrid_min, &
                              maxGlobalCoordPerDim=igrid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a IGrid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! The following code works ok because the layout is explicitly 
      ! destroyed first before being used in the igrid distribute (which is 
      ! expected to fail).  but this still crashes randomly if the layout
      ! object is left completely uninitialized.  this should be addressed.
      layout2 = ESMF_DELayoutCreate(vm, rc=rc)
      call ESMF_DELayoutDestroy(layout2, status)
      call ESMF_IGridDistribute(igrid, delayout=layout2, rc=status)
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Distributing a IGrid with a non-created layout Test"
      call ESMF_Test((status.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest

      igrid_min(1) = 7.0
      igrid_max(1) = -10.0
      igrid_min(2) = 5.0
      igrid_max(2) = 1.0

      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=igrid_min, &
                              maxGlobalCoordPerDim=igrid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a IGrid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      call ESMF_IGridDistribute(igrid, delayout=layout, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a IGrid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Get Coord from a IGrid
      call ESMF_IGridGetCoord(igrid, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get Coord from a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Setting the horzIGridType of a IGrid
      horz_igridtype = ESMF_IGRID_TYPE_LATLON
      call ESMF_IGridSet(igrid, horzigridtype=horz_igridtype,rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting the horz_igridtype of  IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Printing a IGrid
      call ESMF_IGridPrint(igrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test creating an internal IGrid
      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=igrid_min, &
                              maxGlobalCoordPerDim=igrid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Internal IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      call ESMF_IGridDistribute(igrid, delayout=layout, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Distributing an Internal IGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Printing a IGrid
      call ESMF_IGridPrint(igrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test destroy subroutine
      call  ESMF_IGridDestroy(igrid, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test creation of empty igrid
      igrid1 =  ESMF_IGridCreate(rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an empty IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test adding a name to an empty IGrid
      call ESMF_IGridSet(igrid1, name="IGRID_ONE", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a name to a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test getting the name from an empty IGrid
      call ESMF_IGridGet(igrid1, name=gname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a name from a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Verify the name from an empty IGrid is correct
      write(failMsg, *) "Returned wrong name"
      write(name, *) "Verifying a name from a IGrid Test"
      call ESMF_Test((gname.eq."IGRID_ONE"), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "gname= ", gname

      !------------------------------------------------------------------------
      !EX_removeUTest
      !  Get Attribute count from a IGrid
      call ESMF_IGridGetAttributeCount(igrid1, attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Get Attribute from a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
      !------------------------------------------------------------------------
      !EX_removeUTest
      !  Verify Attribute count from a IGrid
      write(failMsg, *) "Attribute count is incorrect" 
      write(name, *) "Verify Attribute count from a IGrid Test"
      call ESMF_Test((attribute.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Set Attributes in a empty IGrid 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting Attributes in a IGrid Test"
      call ESMF_IGridSetAttribute(igrid1, "test_attribute", 123456789, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Set an Attribute in a IGrid
      call ESMF_IGridSetAttribute(igrid1, "test_attribute", 123456789, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Set an Attribute in a IGrid
      call ESMF_IGridSetAttribute(igrid1, "test_attribute1", 0, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Set an Attribute in an IGrid
      call ESMF_IGridSetAttribute(igrid1, "test_attribute2", 0.0, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Set an Attribute in a IGrid
      call ESMF_IGridSetAttribute(igrid1, "test_attribute3", 6789, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Set an Attribute in a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Set an Attribute in a IGrid
      call ESMF_IGridSetAttribute(igrid1, "test_attribute4", 5.87, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
       
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Get Attribute count from a IGrid
      call ESMF_IGridGetAttributeCount(igrid1, attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Get an Attribute from a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
       
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Verify Attribute count from a IGrid
      write(failMsg, *) "Attribute count is incorrect" 
      write(name, *) "Verify Attribute count from a IGrid Test"
      call ESMF_Test((attribute.eq.5), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Get an Attribute from a IGrid
      call ESMF_IGridGetAttribute(igrid1, "test_attribute", attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get an Attribute from a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Verify the value of the Attribute
      write(failMsg, *) "Attribute value is wrong"
      write(name, *) "Verify Attribute value from a IGrid Test"
      call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Get an Attribute from a IGrid
      call ESMF_IGridGetAttribute(igrid1, "test_attribute", attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get an Attribute from a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Verify the value of the Attribute
      write(failMsg, *) "Attribute value is wrong"
      write(name, *) "Verify Attribute value from a IGrid Test"
      call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------
      !EX_removeUTest
      !  Get Attribute Info from a IGrid Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get Attribute Info from a IGrid Test"
      call ESMF_IGridGetAttributeInfo(igrid1, "test_attribute", &
        typekind=att_typekind, count=att_count, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_removeUTest
      !  Verify typekind of Attribute
      write(failMsg, *) "Attribute typekind is wrong"
      write(name, *) "Verify Attribute typekind from a IGrid Test"
      call ESMF_Test((att_typekind.eq.ESMF_TYPEKIND_I4), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      !  Verify count of Attribute
      write(failMsg, *) "Attribute count is wrong"
      write(name, *) "Verify Attribute count from a IGrid Test"
      call ESMF_Test((att_count.eq.1), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_removeUTest
      !  Get an Attribute from IGrid with wrong data type
      call ESMF_IGridGetAttribute(igrid1, "test_attribute4", attribute, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS"
      write(name, *) "Get a Wrong Data type Attribute from a IGrid Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test destroy subroutine
      call  ESMF_IGridDestroy(igrid1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a IGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


#endif
  
      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_IGridUTest
