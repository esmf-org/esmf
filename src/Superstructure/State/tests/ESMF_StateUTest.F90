! $Id: ESMF_StateUTest.F90,v 1.37.2.3 2007/10/18 02:44:03 cdeluca Exp $
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
      program ESMF_StateUTest

!==============================================================================
!
#include <ESMF.h>
!
!BOP
! !PROGRAM: ESMF_StateUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 State unit tests.
! The companion file ESMF\_State.F90 contains the definitions for the
! State methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod 
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_StateUTest.F90,v 1.37.2.3 2007/10/18 02:44:03 cdeluca Exp $'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: x, rc, num, number
      logical :: IsNeeded
      character(ESMF_MAXSTR) :: statename, bundlename, bname
      character(ESMF_MAXSTR) :: fieldname, fname, aname, arrayname
      type(ESMF_Field) :: field1, field2, field3(3), field4, field5(3), nofield
      type(ESMF_Bundle) :: bundle1, bundle2(1), bundle3, bundle5, nobundle
      type(ESMF_State) :: state1, state2, state3, nostate
      type(ESMF_Array) :: array1, array2(2), array3, array3a, noarray
      type(ESMF_StateItemType) :: stateItemType
      type(ESMF_NeededFlag) :: needed
      real, dimension(:,:), pointer :: f90ptr1

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test failure messages
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables needed to pass into function/subroutine calls
      !character(ESMF_MAXSTR) :: validate_options
      !character(ESMF_MAXSTR) :: print_options

!-------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   to OFF, then only the sanity unit tests.
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      
      !------------------------------------------------------------------------
      
      !NEX_UTest      
      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(statename, ESMF_STATE_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

      ! set up destroyed objects for use below
      nostate = ESMF_StateCreate(rc=rc)
      call ESMF_StateDestroy(nostate, rc=rc)
      nobundle = ESMF_BundleCreate(rc=rc)
      call ESMF_BundleDestroy(nobundle, rc=rc)
      nofield = ESMF_FieldCreateNoData(rc=rc)
      call ESMF_FieldDestroy(nofield, rc=rc)
      noarray = ESMF_ArrayCreate(1,ESMF_DATA_REAL,ESMF_R8,(/1/),rc=rc)
      call ESMF_ArrayDestroy(noarray, rc=rc)


      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(statename, ESMF_STATE_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Get Item Info from an empty import State 
      call ESMF_StateGetItemInfo(state1, name="Bundle1", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting item info from an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Verifying Item Info from an empty import State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying item info from an empty import State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_NOTFOUND), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Get Item Info from destroyed import State 
      call ESMF_StateGetItemInfo(state1, name="Bundle1", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Getting item info from destroyed State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Creation of an empty export State 
      statename = "Atmosphere Out"
      state1 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding Bundle to a State
      bundlename = "Temperature"
      bundle1 = ESMF_BundleCreate(name=bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_StateAddBundle(state1, bundle1, rc)
      write(name, *) "Adding a Bundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest 
      ! Test Get Item Info from State 
      call ESMF_StateGetItemInfo(state1, name="Temperature", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Bundle item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Verifying Item Info from a State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying Bundle item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_BUNDLE), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding a second Bundle to a State
      bundlename = "Temperature"
      bundle1 = ESMF_BundleCreate(name=bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !EX_UTest
      call ESMF_StateAddBundle(state1, bundle1, rc)
      write(name, *) "Adding a second Bundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)


      !EX_UTest
      ! Test adding a Bundle with Fields to a State
      bundle3 = ESMF_BundleCreate(name="Atmosphere", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      field5(1) = ESMF_FieldCreateNoData("heat flux", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      field5(2) = ESMF_FieldCreateNoData("density", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      field5(3) = ESMF_FieldCreateNoData("sea surface temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      call ESMF_BundleAddField(bundle3, 3, field5, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding 3 Fields to a Bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      call ESMF_StateAddBundle(state1, bundle3, rc)
      write(name, *) "Adding a Bundle with Fields to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)

      !EX_UTest
      ! Test adding Field to a State
      fieldname = "Humidity"
      field1 = ESMF_FieldCreateNoData(fieldname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !EX_UTest
      call ESMF_StateAddField(state1, field1, rc)
      write(name, *) "Adding a Field to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest 
      ! Test Get Item Info from State 
      call ESMF_StateGetItemInfo(state1, name="Humidity", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Verifying Item Info from a State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying Field item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_FIELD), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest 
      ! Test Get Unknown Item Info from State 
      call ESMF_StateGetItemInfo(state1, name="Humanity", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting unknown item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Verifying Unknown Item Info from a State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying unknown item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_NOTFOUND), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test adding an Array to a State
      allocate(f90ptr1(10,20))
      array1 = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test setting an array name
      call ESMF_ArraySet(array1, name="ArrayOne", rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Setting an Array Name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting a name from an array
      call ESMF_ArrayGet(array1, name=aname, rc=rc)  ! get the name for later
      write(failMsg, *) ""
      write(name, *) "Getting an Array Name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      !Test adding an Array to a State
      call ESMF_StateAddArray(state1, array1, rc)
      write(name, *) "Adding an Array to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting an Array from a State
      call ESMF_StateGetArray(state1, arrayname=aname, array=array3, rc=rc)
      write(failMsg, *) "DId not return ESMF_SUCCESS"
      write(name, *) "Getting an Array from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test printing of State
      call  ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Adding a name to a State
      call ESMF_StateAddNameOnly(state1, name="StateOne", rc=rc)
      write(failMsg, *) "DId not return ESMF_SUCCESS"
      write(name, *) "Adding a name to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test printing of State
      call  ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting Attribute Count from a state
      call  ESMF_StateGetAttributeCount(state1, num, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong value"
      write(name, *) "Getting an attribute count from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(num.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Attribute count =", num

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test adding an Attribute to a state
      call  ESMF_StateSetAttribute(state1, name="newAttribute", value=12345, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an attribute to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

     !------------------------------------------------------------------------
      
      !EX_UTest
      ! Test Get Attribute Item Info from State
      call ESMF_StateGetItemInfo(state1, name="newAttribute", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting unknown item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Verifying Attribute Item Info from a State
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying attribute item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_NOTFOUND), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting an Attribute from a state
      call  ESMF_StateGetAttribute(state1, name="newAttribute", value=num, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong value"
      write(name, *) "Getting an attribute from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(num.eq.12345), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Attribute num =", num
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting Attribute Count from a state
      call  ESMF_StateGetAttributeCount(state1, num, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong value"
      write(name, *) "Getting an attribute count from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(num.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Attribute count =", num

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting Attribute Info from a state
      call  ESMF_StateGetAttributeInfo(state1, name="newAttribute", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong value"
      write(name, *) "Getting attribute Info from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Attribute count =", number

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting Bundle from State
      call  ESMF_StateGetBundle(state1, bundlename, bundle2(1), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Bundle from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_BundleGet(bundle2(1), name=bname, rc=rc)
      write(failMsg, *) "Bundle name not 'Temperature'"
      write(name, *) "Verifying that the Bundle has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bname.eq."Temperature"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test State for Bundle being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(failMsg, *) ""
      write(name, *) "Query if Bundle is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test State for Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc)
      write(failMsg, *) ""
      write(name, *) "Query if Field is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test State for Field being needed
      call ESMF_StateGetNeeded(state1, "Humidity", needed, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query if Field is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(needed.eq.ESMF_NEEDED), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test State for Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc)
      write(failMsg, *) ""
      write(name, *) "Query if Field is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test State for non-existant Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidty", rc)
      write(failMsg, *) ""
      write(name, *) "Query if non existant Field is needed in a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test setting Field as not needed in a State
      call ESMF_StateSetNeeded(state1, "Humidity", ESMF_NOTNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Field as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc)
      write(name, *) "Test if Field is not needed in a State Test"
      call ESMF_Test((.not.IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test State for Array being needed
      IsNeeded = ESMF_StateIsNeeded(state1, aname, rc)
      write(failMsg, *) ""
      write(name, *) "Query if Array is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test setting Bundle as not needed in a State
      call ESMF_StateSetNeeded(state1, "Temperature", ESMF_NOTNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Bundle as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(name, *) "Test if Bundle is not needed in a State Test"
      call ESMF_Test((.not.IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test setting Array as not needed in a State
      call ESMF_StateSetNeeded(state1, aname, ESMF_NOTNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Array as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      IsNeeded = ESMF_StateIsNeeded(state1, aname, rc)
      write(name, *) "Test if Array is not needed in a State Test"
      call ESMF_Test((.not.IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting Array from State
      call  ESMF_StateGetArray(state1, aname, array3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Arrray from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      !Test getting Array name
      call ESMF_ArrayGet(array1, name=arrayname, rc=rc)
      write(failMsg, *) "Wrong Array name  and/or did not return ESMF_SUCCESS"
      write(name, *) "Verifying that the Array has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(arrayname.eq.aname), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting Field from State
      call  ESMF_StateGetField(state1, fieldname, field2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Field from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      call ESMF_FieldGet(field2, name=fname, rc=rc)
      write(failMsg, *) "Wrong Field name "
      write(name, *) "Verifying that the Field has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname.eq."Humidity"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)

      !EX_UTest
      ! Test setting Bundle as needed in a State
      call ESMF_StateSetNeeded(state1, "Temperature", ESMF_NEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Bundle as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(name, *) "Test if Bundle is needed in a State Test"
      call ESMF_Test((IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test setting Field as needed in a State
      call ESMF_StateSetNeeded(state1, "Humidity", ESMF_NEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Field as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !EX_UTest
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(name, *) "Test if Field is needed in a State Test"
      call ESMF_Test((IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test setting Array as needed in a State
      call ESMF_StateSetNeeded(state1, aname, ESMF_NEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Array as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      IsNeeded = ESMF_StateIsNeeded(state1, aname, rc)
      write(name, *) "Test if Array is needed in a State Test"
      call ESMF_Test((IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------
 
      !EX_UTest
#ifdef ESMF_NO_INITIALIZERS
      state3 = nostate
#endif
      call ESMF_StateAddState(state1, state3, rc=rc)
      write(name, *) "Adding an uninitialized State to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
#ifdef ESMF_NO_INITIALIZERS
      bundle5 = nobundle
#endif
      call ESMF_StateAddBundle(state1, bundle5, rc=rc)
      write(name, *) "Adding an uninitialized Bundle to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding an uninitialized Field to a State
#ifdef ESMF_NO_INITIALIZERS
      field4 = nofield
#endif
      call ESMF_StateAddField(state1, field4, rc)
      write(name, *) "Adding an uninitialized Field to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding an uninitialized Array to a State
#ifdef ESMF_NO_INITIALIZERS
      array3a = noarray
#endif
      call ESMF_StateAddArray(state1, array3a, rc)
      write(name, *) "Adding an uninitialized Array to a State Test"
      print *, "rc = ", rc
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                        name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Creation of an export State with Bundle
      bundlename = "Humidity"
      statename = "Export State"
      x = 1
      bundle2(1) = ESMF_BundleCreate(name=bundlename, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
                                bundleList=bundle2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding a State to a State 
      call ESMF_StateAddState(state2, state1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a State to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting a State from a State 
      call ESMF_StateGetState(state2, "Atmosphere Out", state3, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a State from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding the same State to a State -- should fail
      ! Note that you cannot pass the same object in for multiple arguments;
      ! this is illegal in fortran but not all compilers catch it - they just
      ! do bizarre things with them.  So use an alias to the same State.
      state3 = state2
      call ESMF_StateAddState(state2, state3, rc=rc)
      write(failMsg, *) "Did return ESMF_SUCCESS"
      write(name, *) "Adding the same State to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test State Validation
      call ESMF_StateValidate(state2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validating a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Creation of an export State with the wrong number of Fields
      statename = "Export State"
      x = 4
      fieldname = "Precipitation"
      field3(1) = ESMF_FieldCreateNoData(fieldname, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
                                fieldList=field3, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a bad Field list"
      print *, "itemcount mismatch, rc = ", rc
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------


      !EX_UTest
      ! Test Creation of an export State with a Field
      statename = "Export State"
      x = 1
      fieldname = "Precipitation"
      field3(1) = ESMF_FieldCreateNoData(fieldname, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
                                fieldList=field3(1:1), itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Creation of an export State with an array
      statename = "Export State"
      x  = 1
      allocate(f90ptr1(10,20))
      array2(1) = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_REF, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
                                arrayList=array2(1:1), itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                        name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Creation of an export State with an array
      statename = "Export State"
      x  = 2
      allocate(f90ptr1(10,20))
      array2(1) = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_COPY, rc=rc)
      array2(2) = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_COPY, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
                                arrayList=array2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Array list Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                        name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      call ESMF_ArrayPrint(array1, rc=rc)
      call  ESMF_StatePrint(state1, rc=rc)

      !EX_UTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test State Validation
      call ESMF_StateValidate(state1, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS"
      write(name, *) "Validating a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Creation of an export State with an array list
      statename = "Export State"
      x  = 2
      allocate(f90ptr1(10,20))
      array2(1) = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_COPY, rc=rc)
      array2(2) = ESMF_ArrayCreate(f90ptr1, ESMF_DATA_COPY, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
                                arrayList=array2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Array list Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                        name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Destruction of a destroyed State
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Destruction of a destroyed State Test"
      call  ESMF_StateDestroy(state1, rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      call ESMF_TestEnd(result, ESMF_SRCLINE)
 
  
      end program ESMF_StateUTest
