! $Id: ESMF_InternGridUTest.F90,v 1.1 2007/06/22 23:21:38 cdeluca Exp $
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
      program ESMF_InternGridUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Macros.inc>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_InternGridUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 InternGrid unit tests.
! The companion file ESMF\_InternGrid.F90 contains the definitions for the
! InternGrid methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
    
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_InternGridUTest.F90,v 1.1 2007/06/22 23:21:38 cdeluca Exp $'
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
      !type(ESMF_InternGridConfig) :: config_set
      !type(ESMF_InternGridConfig) :: config_get



      integer :: counts(ESMF_MAXIGRIDDIM)
      integer :: nDE_i, nDE_j
      type(ESMF_InternGridType) :: horz_interngridtype
      type(ESMF_InternGridHorzStagger) :: horz_stagger
      integer :: status
      real(ESMF_KIND_R8) :: interngrid_min(3), interngrid_max(3)
      type(ESMF_InternGrid) :: interngrid, interngrid1
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
      interngrid_min(1) = 0.0
      interngrid_max(1) = 10.0
      interngrid_min(2) = 0.0
      interngrid_max(2) = 12.0
      name = "test interngrid 1"




      !------------------------------------------------------------------------
      !NEX_UTest
      layout = ESMF_DELayoutCreate(vm, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a DELayout Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !NEX_UTest
      interngrid = ESMF_InternGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=interngrid_min, &
                              maxGlobalCoordPerDim=interngrid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Printing a InternGrid
      call ESMF_InternGridPrint(interngrid, "", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Destroying a InternGrid
      call ESMF_InternGridDestroy(interngrid, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_UTest
      ! Create a interngrid to use in next test
      interngrid = ESMF_InternGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=interngrid_min, &
                              maxGlobalCoordPerDim=interngrid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a InternGrid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! The following code works ok because the layout is explicitly 
      ! destroyed first before being used in the interngrid distribute (which is 
      ! expected to fail).  but this still crashes randomly if the layout
      ! object is left completely uninitialized.  this should be addressed.
      layout2 = ESMF_DELayoutCreate(vm, rc=rc)
      call ESMF_DELayoutDestroy(layout2, status)
      call ESMF_InternGridDistribute(interngrid, delayout=layout2, rc=status)
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Distributing a InternGrid with a non-created layout Test"
      call ESMF_Test((status.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest

      interngrid_min(1) = 7.0
      interngrid_max(1) = -10.0
      interngrid_min(2) = 5.0
      interngrid_max(2) = 1.0

      interngrid = ESMF_InternGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=interngrid_min, &
                              maxGlobalCoordPerDim=interngrid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Creating a InternGrid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=status)
      write(failMsg, *) "Returned ESMF_FAILURE"
      write(name, *) "Distributing a InternGrid  with negative x_max Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Get Coord from a InternGrid
      call ESMF_InternGridGetCoord(interngrid, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get Coord from a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Setting the horzInternGridType of a InternGrid
      horz_interngridtype = ESMF_IGRID_TYPE_LATLON
      call ESMF_InternGridSet(interngrid, horzinterngridtype=horz_interngridtype,rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting the horz_interngridtype of  InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Printing a InternGrid
      call ESMF_InternGridPrint(interngrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test creating an internal InternGrid
      interngrid = ESMF_InternGridCreateHorzXYUni(counts=counts, &
                              minGlobalCoordPerDim=interngrid_min, &
                              maxGlobalCoordPerDim=interngrid_max, &
                              horzstagger=horz_stagger, &
                              name=name, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an Internal InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_InternGridDistribute(interngrid, delayout=layout, rc=status)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Distributing an Internal InternGrid Test"
      call ESMF_Test((status.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Printing a InternGrid
      call ESMF_InternGridPrint(interngrid, "", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Printing a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test destroy subroutine
      call  ESMF_InternGridDestroy(interngrid, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test creation of empty interngrid
      interngrid1 =  ESMF_InternGridCreate(rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Creating an empty InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test adding a name to an empty InternGrid
      call ESMF_InternGridSet(interngrid1, name="IGRID_ONE", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a name to a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting the name from an empty InternGrid
      call ESMF_InternGridGet(interngrid1, name=gname, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a name from a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Verify the name from an empty InternGrid is correct
      write(failMsg, *) "Returned wrong name"
      write(name, *) "Verifying a name from a InternGrid Test"
      call ESMF_Test((gname.eq."IGRID_ONE"), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "gname= ", gname

      !------------------------------------------------------------------------
      !EX_UTest
      !  Get Attribute count from a InternGrid
      call ESMF_InternGridGetAttributeCount(interngrid1, attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Get Attribute from a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify Attribute count from a InternGrid
      write(failMsg, *) "Attribute count is incorrect" 
      write(name, *) "Verify Attribute count from a InternGrid Test"
      call ESMF_Test((attribute.eq.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Set Attributes in a empty InternGrid 
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Setting Attributes in a InternGrid Test"
      call ESMF_InternGridSetAttribute(interngrid1, "test_attribute", 123456789, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in a InternGrid
      call ESMF_InternGridSetAttribute(interngrid1, "test_attribute", 123456789, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in a InternGrid
      call ESMF_InternGridSetAttribute(interngrid1, "test_attribute1", 0, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in an InternGrid
      call ESMF_InternGridSetAttribute(interngrid1, "test_attribute2", 0.0, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in a InternGrid
      call ESMF_InternGridSetAttribute(interngrid1, "test_attribute3", 6789, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Set an Attribute in a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Set an Attribute in a InternGrid
      call ESMF_InternGridSetAttribute(interngrid1, "test_attribute4", 5.87, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set an Attribute in a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
       
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Get Attribute count from a InternGrid
      call ESMF_InternGridGetAttributeCount(interngrid1, attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS" 
      write(name, *) "Get an Attribute from a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
       
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Verify Attribute count from a InternGrid
      write(failMsg, *) "Attribute count is incorrect" 
      write(name, *) "Verify Attribute count from a InternGrid Test"
      call ESMF_Test((attribute.eq.5), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Get an Attribute from a InternGrid
      call ESMF_InternGridGetAttribute(interngrid1, "test_attribute", attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get an Attribute from a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Verify the value of the Attribute
      write(failMsg, *) "Attribute value is wrong"
      write(name, *) "Verify Attribute value from a InternGrid Test"
      call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Get an Attribute from a InternGrid
      call ESMF_InternGridGetAttribute(interngrid1, "test_attribute", attribute, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get an Attribute from a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !-------------------------------------------------------------------------
      !EX_UTest
      !  Verify the value of the Attribute
      write(failMsg, *) "Attribute value is wrong"
      write(name, *) "Verify Attribute value from a InternGrid Test"
      call ESMF_Test((attribute.eq.123456789), name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------
      !EX_UTest
      !  Get Attribute Info from a InternGrid Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Get Attribute Info from a InternGrid Test"
      call ESMF_InternGridGetAttributeInfo(interngrid1, "test_attribute", &
        typekind=att_typekind, count=att_count, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify typekind of Attribute
      write(failMsg, *) "Attribute typekind is wrong"
      write(name, *) "Verify Attribute typekind from a InternGrid Test"
      call ESMF_Test((att_typekind.eq.ESMF_TYPEKIND_I4), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      !  Verify count of Attribute
      write(failMsg, *) "Attribute count is wrong"
      write(name, *) "Verify Attribute count from a InternGrid Test"
      call ESMF_Test((att_count.eq.1), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_UTest
      !  Get an Attribute from InternGrid with wrong data type
      call ESMF_InternGridGetAttribute(interngrid1, "test_attribute4", attribute, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS"
      write(name, *) "Get a Wrong Data type Attribute from a InternGrid Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
 
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test destroy subroutine
      call  ESMF_InternGridDestroy(interngrid1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroying a InternGrid Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


#endif
  
      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_InternGridUTest
