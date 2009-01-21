! $Id: ESMF_StateUTest.F90,v 1.55.2.8 2009/01/21 21:25:25 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
#include "ESMF.h"
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
      '$Id: ESMF_StateUTest.F90,v 1.55.2.8 2009/01/21 21:25:25 cdeluca Exp $'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: x, rc, num, number,localrc
      logical :: IsNeeded, correct
      character(ESMF_MAXSTR) :: statename, bundlename, bname
      character(ESMF_MAXSTR) :: fieldname, fname, aname, arrayname
      type(ESMF_Field) :: field1, field2, field3(3), field4, field5(3),fieldGDP
      type(ESMF_FieldBundle) :: bundle1, bundle2(1), bundle3, bundle5, bundleGDP
      type(ESMF_State) :: state1, state2, state3, state4,stateGDP
      type(ESMF_StateItemType) :: stateItemType
      type(ESMF_NeededFlag) :: needed
      real, dimension(:,:), pointer :: f90ptr1
      real(ESMF_KIND_R8), pointer :: ptrGDP1(:,:),ptrGDP2(:,:)
      
      type(ESMF_ArraySpec)  :: arrayspec
      type(ESMF_DistGrid)   :: distgrid
      type(ESMF_Array)      :: array, array2, arrayGDP, testarray


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
      
      !NEX_removeUTest      
      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(statename, ESMF_STATE_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !NEX_removeUTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Add  uncreated array to uncreated  State 
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Add uncreated array to uncreated State Test"
      call ESMF_StateAdd(state4, testarray, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Get  uncreated array from uncreated  State 
      arrayname = "Test Array"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Get uncreated array from uncreated State Test"
      call ESMF_StateGet(state4, arrayname, testarray, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Test Creation of an empty import State 
      statename = "Test Array State"
      state4 = ESMF_StateCreate(statename, ESMF_STATE_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Add  uncreated array to a  State 
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Add uncreated array to a State Test"
      call ESMF_StateAdd(state4, testarray, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Get  uncreated array from State 
      arrayname = "Test Array"
      write(failMsg, *) "Did not return ESMF_RC_ARG_INCOMP"
      write(name, *) "Get uncreated array from a State Test"
      call ESMF_StateGet(state4, arrayname, testarray, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_ARG_INCOMP), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX______UTest 
      ! This test is commented out until bug 1707501 
      ! is resolved.  The failure was on an InternArray object,
      ! which was switched over to new Array in 10/07.   
      ! The code to create and destroy the InternArray prior to 
      ! this test was deleted.  To reinstate this test the code
      ! to create and destroy an Array before this test needs to
      ! be restored.
      ! 
      ! Get a destroyed array from State 
      !arrayname = "Test Array"
      !write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      !write(name, *) "Get a destroyed array from a State Test"
      !call ESMF_StateGet(state4, arrayname, testarray, rc=rc)
      !call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
      !                name, failMsg, result, ESMF_SRCLINE)

      !-------------------------------------------------------------------------------
      !EX_removeUTest
      ! Destroy State Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroy State Test"
      call ESMF_StateDestroy(state4, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(statename, ESMF_STATE_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Test Get Item Info from an empty import State 
      call ESMF_StateGet(state1, name="FieldBundle1", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting item info from an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Test Verifying Item Info from an empty import State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying item info from an empty import State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_NOTFOUND), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Test Get Item Info from destroyed import State 
      call ESMF_StateGet(state1, name="FieldBundle1", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Getting item info from destroyed State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test Creation of an empty export State 
      statename = "Atmosphere Out"
      state1 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test adding FieldBundle to a State
      bundlename = "Temperature"
      bundle1 = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest
      call ESMF_StateAdd(state1, bundle1, rc)
      write(name, *) "Adding a FieldBundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest 
      ! Test Get Item Info from State 
      call ESMF_StateGet(state1, name="Temperature", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting FieldBundle item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Test Verifying Item Info from a State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying FieldBundle item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_BUNDLE), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test adding a second FieldBundle to a State
      bundlename = "Temperature"
      bundle1 = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !EX_removeUTest
      call ESMF_StateAdd(state1, bundle1, rc)
      write(name, *) "Adding a second FieldBundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)


      !EX_removeUTest
      ! Test adding a FieldBundle with Fields to a State
      bundle3 = ESMF_FieldBundleCreate(name="Atmosphere", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_removeUTest
      field5(1) = ESMF_FieldCreateNoData("heat flux", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_removeUTest
      field5(2) = ESMF_FieldCreateNoData("density", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_removeUTest
      field5(3) = ESMF_FieldCreateNoData("sea surface temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_removeUTest
      call ESMF_FieldBundleAdd(bundle3, 3, field5, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding 3 Fields to a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_removeUTest
      call ESMF_StateAdd(state1, bundle3, rc)
      write(name, *) "Adding a FieldBundle with Fields to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)

      !EX_removeUTest
      ! Test adding Field to a State
      fieldname = "Humidity"
      field1 = ESMF_FieldCreateNoData(fieldname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !EX_removeUTest
      call ESMF_StateAdd(state1, field1, rc)
      write(name, *) "Adding a Field to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest 
      ! Test Get Item Info from State 
      call ESMF_StateGet(state1, name="Humidity", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Field item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Test Verifying Item Info from a State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying Field item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_FIELD), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest 
      ! Test Get Unknown Item Info from State 
      call ESMF_StateGet(state1, name="Humanity", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting unknown item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_removeUTest 
      ! Test Verifying Unknown Item Info from a State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying unknown item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_NOTFOUND), &
                      name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  ! Test adding an Array to a State
  call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, &
    rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
!call ESMF_ArrayPrint(array)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_ArraySet(array, name="testArray", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
  !------------------------------------------------------------------------
  !EX_removeUTest
  call ESMF_ArrayGet(array, name=aname, rc=rc)
  write(failMsg, *) "Wrong Array name "
  write(name, *) "Verifying that the Array has correct name Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aname.eq."testArray"), name, failMsg,&
    result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_removeUTest
  call ESMF_StateAdd(state1, array, rc)
  write(name, *) "Adding an Array to a State Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
    ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_removeUTest 
  ! Test Get Item Info from State 
  call ESMF_StateGet(state1, name="testArray", stateitemtype=stateItemType, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Getting Array item info from State Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
    ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_removeUTest 
  ! Test Verifying Item Info from a State 
  write(failMsg, *) "Item info incorrect"
  write(name, *) "Verifying Array item info from a State Test"
  call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_ARRAY), name, failMsg, &
    result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test printing of State
      call  ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test Adding a name to a State
      call ESMF_StateAdd(state1, name="StateOne", rc=rc)
      write(failMsg, *) "DId not return ESMF_SUCCESS"
      write(name, *) "Adding a name to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test printing of State
      call  ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test getting Attribute Count from a state
      call  ESMF_AttributeGet(state1, num, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong value"
      write(name, *) "Getting an attribute count from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(num.eq.0), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Attribute count =", num

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test adding an Attribute to a state
      call  ESMF_AttributeSet(state1, name="newAttribute", value=12345, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding an attribute to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test Get Attribute Item Info from State
      call ESMF_StateGet(state1, name="newAttribute", stateitemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting unknown item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), & 
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test Verifying Attribute Item Info from a State
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying attribute item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_NOTFOUND), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test getting an Attribute from a state
      call  ESMF_AttributeGet(state1, name="newAttribute", value=num, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong value"
      write(name, *) "Getting an attribute from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(num.eq.12345), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Attribute num =", num

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test getting Attribute Count from a state
      call  ESMF_AttributeGet(state1, num, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong value"
      write(name, *) "Getting an attribute count from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(num.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Attribute count =", num

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test getting Attribute Info from a state
      call  ESMF_AttributeGet(state1, name="newAttribute", count=number, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS or returned wrong value"
      write(name, *) "Getting attribute Info from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(number.eq.1), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "Attribute count =", number

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for FieldBundle being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(failMsg, *) ""
      write(name, *) "Query if FieldBundle is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), name, failMsg, &
        result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test setting FieldBundle as not needed in a State
      call ESMF_StateSetNeeded(state1, "Temperature", ESMF_NOTNEEDED, rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set FieldBundle as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for FieldBundle NOT being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(name, *) "Test if FieldBundle is NOT needed in a State Test"
      call ESMF_Test((.not.IsNeeded), name, failMsg, result, &
        ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test getting FieldBundle from State
      call  ESMF_StateGet(state1, bundlename, bundle2(1), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting FieldBundle from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test name of FieldBundle from State
      call ESMF_FieldBundleGet(bundle2(1), name=bname, rc=rc)
      write(failMsg, *) "FieldBundle name not 'Temperature'"
      write(name, *) "Verifying that the FieldBundle has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bname.eq."Temperature"), name, &
        failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc)
      write(failMsg, *) ""
      write(name, *) "Query if Field is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for Field being needed
      call ESMF_StateGetNeeded(state1, "Humidity", needed, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query if Field is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(needed.eq.ESMF_NEEDED), name, &
        failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for non-existent Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidty", rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Query if non existant Field is needed in a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test setting Field as not needed in a State
      call ESMF_StateSetNeeded(state1, "Humidity", ESMF_NOTNEEDED, rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Field as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for Field NOT being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc)
      write(failMsg, *) ""
      write(name, *) "Test if Field is NOT needed in a State Test"
      call ESMF_Test((.not.IsNeeded), name, failMsg, result, &
        ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test getting Field from State
      call  ESMF_StateGet(state1, fieldname, field2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Field from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test name of Field from State
      call ESMF_FieldGet(field2, name=fname, rc=rc)
      write(failMsg, *) "Wrong Field name "
      write(name, *) "Verifying that the Field has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname.eq."Humidity"), name, &
        failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      
      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for Array being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "testArray", rc)
      write(failMsg, *) ""
      write(name, *) "Query if Array is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for Array being needed
      call ESMF_StateGetNeeded(state1, "testArray", needed, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query if Array is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(needed.eq.ESMF_NEEDED), name, &
        failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test setting Array as not needed in a State
      call ESMF_StateSetNeeded(state1, "testArray", ESMF_NOTNEEDED, rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Array as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for Array NOT being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "testArray", rc)
      write(failMsg, *) ""
      write(name, *) "Test if Array is NOT needed in a State Test"
      call ESMF_Test((.not.IsNeeded), name, failMsg, result, &
        ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test getting Array from State
      call  ESMF_StateGet(state1, "testArray", array2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Array from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test name of Array from State
      call ESMF_ArrayGet(array2, name=aname, rc=rc)
      write(failMsg, *) "Wrong Array name "
      write(name, *) "Verifying that the Array has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aname.eq."testArray"), name, &
        failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test setting FieldBundle as needed in a State
      call ESMF_StateSetNeeded(state1, "Temperature", ESMF_NEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set FieldBundle as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for FieldBundle being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(failMsg, *) ""
      write(name, *) "Test if FieldBundle is needed in a State Test"
      call ESMF_Test((IsNeeded), name, failMsg, result, &
        ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test setting Field as needed in a State
      call ESMF_StateSetNeeded(state1, "Humidity", ESMF_NEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Field as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(failMsg, *) ""
      write(name, *) "Test if Field is needed in a State Test"
      call ESMF_Test((IsNeeded), name, failMsg, result, &
        ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test setting Array as needed in a State
      call ESMF_StateSetNeeded(state1, "testArray", ESMF_NEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Array as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest
      ! Test State for Array being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "testArray", rc)
      write(failMsg, *) ""
      write(name, *) "Test if Array is needed in a State Test"
      call ESMF_Test((IsNeeded), name, failMsg, result, &
        ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

 
      !EX_removeUTest
      call ESMF_StateAdd(state1, state3, rc=rc)
      write(name, *) "Adding an uninitialized State to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest
      call ESMF_StateAdd(state1, bundle5, rc=rc)
      write(name, *) "Adding an uninitialized FieldBundle to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test adding an uninitialized Field to a State
      call ESMF_StateAdd(state1, field4, rc)
      write(name, *) "Adding an uninitialized Field to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding an uninitialized Array to a State

      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test Creation of an export State with FieldBundle
      bundlename = "Humidity"
      statename = "Export State"
      x = 1
      bundle2(1) = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
                                bundleList=bundle2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test adding a State to a State 
      call ESMF_StateAdd(state2, state1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a State to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)

      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test getting a State from a State 
      call ESMF_StateGet(state2, "Atmosphere Out", state3, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting a State from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)

      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test adding the same State to a State -- should fail
      ! Note that you cannot pass the same object in for multiple arguments;
      ! this is illegal in fortran but not all compilers catch it - they just
      ! do bizarre things with them.  So use an alias to the same State.
      state3 = state2
      call ESMF_StateAdd(state2, state3, rc=rc)
      write(failMsg, *) "Did return ESMF_SUCCESS"
      write(name, *) "Adding the same State to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test State Validation
      call ESMF_StateValidate(state2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validating a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------

      !EX_removeUTest
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


      !EX_removeUTest
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

      ! Test Creation of an export State with an array
      !------------------------------------------------------------------------
      !state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
      !                          arrayList=array2(1:1), itemcount=x, rc=rc)

      ! Test Creation of an export State with an array
      !state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
      !                          arrayList=array2, itemcount=x, rc=rc)
      !------------------------------------------------------------------------

      call  ESMF_StatePrint(state1, rc=rc)

      !EX_removeUTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test State Validation
      call ESMF_StateValidate(state1, rc=rc)
      write(failMsg, *) "Should not return ESMF_SUCCESS"
      write(name, *) "Validating a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test Creation of an export State with an array list
      !state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
      !                          arrayList=array2, itemcount=x, rc=rc)
      !------------------------------------------------------------------------

      !EX_removeUTest
      ! Test Destruction of a destroyed State
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Destruction of a destroyed State Test"
      call  ESMF_StateDestroy(state1, rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)
      print *, "rc = ", rc

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                    Test StateGetDataPointer                        !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      stateGDP = ESMF_StateCreate("stateGDP", ESMF_STATE_EXPORT, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
 
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      arrayGDP = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
               indexflag=ESMF_INDEX_GLOBAL, name="arrayGDP", rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  
      call ESMF_StateAdd(stateGDP, arrayGDP, rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      !------------------------------------------------------------------------
      !EX_removeUTest
 
      ! init variables
      localrc=ESMF_SUCCESS
      correct=.true.
      nullify(ptrGDP1)
      nullify(ptrGDP2)

      ! Get Array Data Pointer From State
      call ESMF_StateGetDataPointer(state=stateGDP, itemName="arrayGDP", &
                                    dataPointer=ptrGDP1, copyflag=ESMF_DATA_REF, rc=localrc) 
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Get Array Data Pointer From Array
      call ESMF_ArrayGet(array=arrayGDP, farrayPtr=ptrGDP2, rc=localrc)    
      if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

      ! Make sure the pointers are the same
      if (.not. associated(ptrGDP1,ptrGDP2)) correct=.false.

      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting Array data pointer from state"
      call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! TODO: Add Tests for StateGetDataPointer for Field and FieldBundle when they're
      !       working again. 

      call ESMF_StateDestroy(stateGDP, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      call ESMF_ArrayDestroy(arrayGDP, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      call ESMF_DistGridDestroy(distgrid, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      call ESMF_TestEnd(result, ESMF_SRCLINE)
 
  
      end program ESMF_StateUTest
