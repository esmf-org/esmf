! $Id: ESMF_StateUTest.F90,v 1.92 2011/05/19 19:13:53 feiliu Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
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

#define ESMF_ENABLESTATENEEDED

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_StateUTest.F90,v 1.92 2011/05/19 19:13:53 feiliu Exp $'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: x, rc, num, number,localrc
      type(ESMF_VM) :: vm
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

      type(ESMF_Array)      :: array10, array11, array12
      type(ESMF_ArrayBundle):: abund10, abund11
      type(ESMF_Array)      :: alist10(1), alist11(1)
      type(ESMF_Field)      :: field10, field11, field12
      type(ESMF_State)      :: state10, state11, state12

#if defined (ESMF_TESTEXHAUSTIVE)
      integer :: itemcount
      type(ESMF_FieldBundle) :: bundle2inner(1)

      type(ESMF_LocStream)   :: lstream
      type(ESMF_ArraySpec)   :: aspec
      type(ESMF_Field)       :: fields(5)
      type(ESMF_FieldBundle) :: fbundle

      integer :: i
#endif

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

      call ESMF_VMGetGlobal (vm, rc=rc)

      !------------------------------------------------------------------------
      
      !NEX_UTest      
      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(name=statename, stateType=ESMF_STATE_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Add  uncreated array to uncreated  State 
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Add uncreated array to uncreated State Test"
      call ESMF_StateAdd(state4, testarray, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Get  uncreated array from uncreated  State 
      arrayname = "Test Array"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Get uncreated array from uncreated State Test"
      call ESMF_StateGet(state4, arrayname, testarray, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Creation of an empty import State 
      statename = "Test Array State"
      state4 = ESMF_StateCreate(name=statename, stateType=ESMF_STATE_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Add  uncreated array to a  State 
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Add uncreated array to a State Test"
      call ESMF_StateAdd(state4, testarray, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest 
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
      ! is resolved. To reinstate this test the code
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
      !EX_UTest
      ! Destroy State Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Destroy State Test"
      call ESMF_StateDestroy(state4, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(name=statename, stateType=ESMF_STATE_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Get Item Info from an empty import State 
      call ESMF_StateGet(state1, itemName="FieldBundle1", itemtype=stateItemType, rc=rc)
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
      call  ESMF_StateDestroy(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Get Item Info from destroyed import State 
      call ESMF_StateGet(state1, itemName="FieldBundle1", itemtype=stateItemType, rc=rc)
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Getting item info from destroyed State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Creation of an empty export State 
      statename = "Atmosphere Out"
      state1 = ESMF_StateCreate(name=statename, stateType=ESMF_STATE_EXPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding FieldBundle to a State
      bundlename = "Temperature"
      bundle1 = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      call ESMF_StateAdd(state1, bundle1, rc=rc)
      write(name, *) "Adding a FieldBundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Search for item name
      call ESMF_StateGet (state1,  &
          itemSearch="Temperature", itemCount=itemcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Searching for known item from State Test"
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test for correct count
      write (failMsg, *) "Did not return itemcount == 1"
      write (name, *) "Testing itemCount"
      call ESMF_Test((itemcount == 1), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Search for item name that is not present
      call ESMF_StateGet (state1,  &
          itemSearch="unknownItem", itemCount=itemcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Searching for unknown item from State Test"
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test for correct count
      write (failMsg, *) "Did not return itemcount == 0"
      write (name, *) "Testing itemCount"
      call ESMF_Test((itemcount == 0), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Get Item Info from State 
      call ESMF_StateGet(state1, itemName="Temperature", itemtype=stateItemType, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting FieldBundle item info from State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Verifying Item Info from a State 
      write(failMsg, *) "Item info incorrect"
      write(name, *) "Verifying FieldBundle item info from a State Test"
      call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_FIELDBUNDLE), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test replacing the FieldBundle with a second FieldBundle in the State
      bundlename = "Temperature"
      bundle1 = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a replacement FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !EX_UTest
      call ESMF_StateAdd(state1, bundle1, rc=rc)
      write(name, *) "Replacing a FieldBundle with a second FieldBundle in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)

      !EX_UTest
      ! Test adding a FieldBundle with Fields to a State
      bundle3 = ESMF_FieldBundleCreate(name="Atmosphere", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      field5(1) = ESMF_FieldEmptyCreate(name="heat flux", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 1"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      field5(2) = ESMF_FieldEmptyCreate(name="density", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 2"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      field5(3) = ESMF_FieldEmptyCreate(name="sea surface temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test 3"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      call ESMF_FieldBundleAdd(bundle3, field5(1:3), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding 3 Fields to a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      call ESMF_StateAdd(state1, bundle3, rc=rc)
      write(name, *) "Adding a FieldBundle with Fields to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)

      !EX_UTest
      ! Test adding Field to a State
      fieldname = "Humidity"
      field1 = ESMF_FieldEmptyCreate(name=fieldname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !EX_UTest
      call ESMF_StateAdd(state1, field1, rc=rc)
      write(name, *) "Adding a Field to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest 
      ! Test Get Item Info from State 
      call ESMF_StateGet(state1, itemName="Humidity", itemtype=stateItemType, rc=rc)
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
      call ESMF_StateGet(state1, itemName="Humanity", itemtype=stateItemType, rc=rc)
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
  !EX_UTest
  call ESMF_ArrayGet(array, name=aname, rc=rc)
  write(failMsg, *) "Wrong Array name "
  write(name, *) "Verifying that the Array has correct name Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aname.eq."testArray"), name, failMsg,&
    result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_StateAdd(state1, array, rc=rc)
  write(name, *) "Adding an Array to a State Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
    ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest 
  ! Test Get Item Info from State 
  call ESMF_StateGet(state1, itemName="testArray", itemtype=stateItemType, rc=rc)
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  write(name, *) "Getting Array item info from State Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
    ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest 
  ! Test Verifying Item Info from a State 
  write(failMsg, *) "Item info incorrect"
  write(name, *) "Verifying Array item info from a State Test"
  call ESMF_Test((stateItemType.eq.ESMF_STATEITEM_ARRAY), name, failMsg, &
    result, ESMF_SRCLINE)


  !------------------------------------------------------------------------
  !EX_UTest
  ! Test printing of State - default option
  call  ESMF_StatePrint(state1, rc=rc)
  write(failMsg, *) ""
  write(name, *) "Printing of a State Test - default option"
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest
  ! Test printing of State - long print option
  call  ESMF_StatePrint(state1, options='long', rc=rc)
  write(failMsg, *) ""
  write(name, *) "Printing of a State Test - long option"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest
  ! Test printing of State - nested option
  call  ESMF_StatePrint(state1, nestedFlag=.true., rc=rc)
  write(failMsg, *) ""
  write(name, *) "Printing of a State Test - deep option"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest
  ! Test printing of State - nested and long options
  call  ESMF_StatePrint(state1,  &
      nestedFlag=.true., options='long', rc=rc)
  write(failMsg, *) ""
  write(name, *) "Printing of a State Test - nested and long options"
  call ESMF_Test((rc == ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest
  ! Test printing of State - illegal option
  call  ESMF_StatePrint(state1, options='illegal', rc=rc)
  write(failMsg, *) ""
  write(name, *) "Printing of a State Test - illegal option"
  call ESMF_Test((rc /= ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Adding a name to a State
      call ESMF_StateAdd(state1, name="StateOne", rc=rc)
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

#if defined (ESMF_ENABLESTATENEEDED)
      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for FieldBundle being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query if FieldBundle is needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), name, failMsg, &
        !result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test setting FieldBundle as not needed in a State
      call ESMF_StateSetNeeded(state1, "Temperature", ESMF_NOTNEEDED, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set FieldBundle as not needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
       !               name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for FieldBundle NOT being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc=rc)
      write(name, *) "Test if FieldBundle is NOT needed in a State Test"
      !call ESMF_Test((.not.IsNeeded), name, failMsg, result, &
        !ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
#endif

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting FieldBundle from State
      call  ESMF_StateGet(state1, bundlename, bundle2(1), rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Getting FieldBundle from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test name of FieldBundle from State
      call ESMF_FieldBundleGet(bundle2(1), name=bname, rc=rc)
      write(failMsg, *) "FieldBundle name not 'Temperature'"
      write(name, *) "Verifying that the FieldBundle has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bname.eq."Temperature"), name, &
        failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

#if defined (ESMF_ENABLESTATENEEDED)
      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query if Field is needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      !name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for Field being needed
      call ESMF_StateGetNeeded(state1, "Humidity", needed, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query if Field is needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(needed.eq.ESMF_NEEDED), name, &
        !failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for non-existent Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidty", rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Query if non existant Field is needed in a State Test"
      !call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, &
        !ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test setting Field as not needed in a State
      call ESMF_StateSetNeeded(state1, "Humidity", ESMF_NOTNEEDED, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Field as not needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        !ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for Field NOT being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test if Field is NOT needed in a State Test"
      !call ESMF_Test((.not.IsNeeded), name, failMsg, result, &
        !ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
#endif

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting Field from State
      call  ESMF_StateGet(state1, fieldname, field2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Field from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test name of Field from State
      call ESMF_FieldGet(field2, name=fname, rc=rc)
      write(failMsg, *) "Wrong Field name "
      write(name, *) "Verifying that the Field has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname.eq."Humidity"), name, &
        failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

#if defined (ESMF_ENABLESTATENEEDED)
      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for Array being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "testArray", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query if Array is needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      !name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for Array being needed
      call ESMF_StateGetNeeded(state1, "testArray", needed, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Query if Array is needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(needed.eq.ESMF_NEEDED), name, &
        !failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test setting Array as not needed in a State
      call ESMF_StateSetNeeded(state1, "testArray", ESMF_NOTNEEDED, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Set Array as not needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        !ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for Array NOT being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "testArray", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test if Array is NOT needed in a State Test"
      !call ESMF_Test((.not.IsNeeded), name, failMsg, result, &
        !ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
#endif

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test getting Array from State
      call  ESMF_StateGet(state1, "testArray", array2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting Array from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
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

#if defined (ESMF_ENABLESTATENEEDED)
      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test setting FieldBundle as needed in a State
      call ESMF_StateSetNeeded(state1, "Temperature", ESMF_NEEDED, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Set FieldBundle as needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        !ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for FieldBundle being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test if FieldBundle is needed in a State Test"
      !call ESMF_Test((IsNeeded), name, failMsg, result, &
        !ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test setting Field as needed in a State
      call ESMF_StateSetNeeded(state1, "Humidity", ESMF_NEEDED, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Set Field as needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        !ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test if Field is needed in a State Test"
      !call ESMF_Test((IsNeeded), name, failMsg, result, &
        !ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test setting Array as needed in a State
      call ESMF_StateSetNeeded(state1, "testArray", ESMF_NEEDED, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Set Array as needed in a State Test"
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, &
        !ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_______UTest
      ! Test State for Array being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "testArray", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Test if Array is needed in a State Test"
      !call ESMF_Test((IsNeeded), name, failMsg, result, &
        !ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------
#endif
 
      !EX_UTest
      call ESMF_StateAdd(state1, state3, rc=rc)
      write(name, *) "Adding an uninitialized State to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      call ESMF_StateAdd(state1, bundle5, rc=rc)
      write(name, *) "Adding an uninitialized FieldBundle to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding an uninitialized Field to a State
      call ESMF_StateAdd(state1, field4, rc=rc)
      write(name, *) "Adding an uninitialized Field to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding an uninitialized Array to a State

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Creation of an export State with FieldBundle
      bundlename = "Humidity"
      statename = "Export State"
      x = 1
      bundle2(1) = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      state2 = ESMF_StateCreate(name=statename, stateType=ESMF_STATE_EXPORT, &
                                fieldbundleList=bundle2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding a State to a State 
      call ESMF_StateAdd(state2, state1, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Adding a State to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test getting a State from a State 
      call ESMF_StateGet(state2, "Atmosphere Out", state3, rc=rc)
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
      call ESMF_StateAdd(state2, state3, rc=rc)
      write(failMsg, *) "Did return ESMF_SUCCESS"
      write(name, *) "Adding the same State to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Add a FieldBundle to the outer State which has the same name
      ! as a FieldBundle in the nested State.
      bundlename = "Humidity"
      bundle2inner(1) = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      call ESMF_StateAdd(state1, &
                         fieldBundle=bundle2inner(1),  &
                         rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a FieldBundle to a outer State test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !call  ESMF_StatePrint(state2, nestedFlag=ESMF_NESTED_ON, rc=rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test State Validation
      call ESMF_StateGet (state2,  &
          itemSearch="Humidity", nestedFlag=.true.,  &
          itemCount=itemcount, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Counting duplicate names in nested States test"
      call ESMF_Test((rc == ESMF_SUCCESS) .and. itemcount == 2, &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! Test State Validation
      call ESMF_StateValidate(state2, rc=rc)
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Validating a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test nested State Validation
      call ESMF_StateValidate(state2, nestedFlag=.true., rc=rc)
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
      field3(1) = ESMF_FieldEmptyCreate(name=fieldname, rc=rc)
      state2 = ESMF_StateCreate(name=statename, stateType=ESMF_STATE_EXPORT, &
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
      field3(1) = ESMF_FieldEmptyCreate(name=fieldname, rc=rc)
      state2 = ESMF_StateCreate(name=statename, stateType=ESMF_STATE_EXPORT, &
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

      !EX_UTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc=rc)
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
      ! Test Creation of an export State with an array list
      !state2 = ESMF_StateCreate(statename, ESMF_STATE_EXPORT, &
      !                          arrayList=array2, itemcount=x, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Destruction of a destroyed State
      write(failMsg, *) "Returned ESMF_SUCCESS"
      write(name, *) "Destruction of a destroyed State Test"
      call  ESMF_StateDestroy(state1, rc=rc)
      call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, &
        ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace on Array
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test create an empty State
      state10 = ESMF_StateCreate (name="stateContainer",  &
        statetype=ESMF_STATE_EXPORT, rc=rc)
      write (failmsg, *) "Creating state10 for replacement"
      write (name, *) "Creating state10 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test create an Array for replacement

      array10 = ESMF_ArrayCreate (name="temperatures",  &
        arrayspec=arrayspec, distgrid=distgrid, &
        indexflag=ESMF_INDEX_GLOBAL, rc=rc)
      write (failmsg, *) "Creating array10 for replacement"
      write (name, *) "Creating array10 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacement where none exists (should fail)
      call ESMF_StateReplace (state10, array10, rc=rc)
      write (failmsg, *) "Replaced an Array which did not exist"
      write (name, *) "Replace an Array which does not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding in the Array
      call ESMF_StateAdd (state10, array10, rc=rc)
      write (failmsg, *) "Adding an Array into a State"
      write (name, *) "Add an Array which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test create a 2nd Array for replacement, but has same name
      ! as the 1st Array
      array11 = ESMF_ArrayCreate (name="temperatures",  &
        arrayspec=arrayspec, distgrid=distgrid, &
        indexflag=ESMF_INDEX_GLOBAL, rc=rc)
      write (failmsg, *) "Creating array11 for replacement"
      write (name, *) "Creating array11 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacing the 1st Array with the 2nd one
      call ESMF_StateReplace (state10, array11, rc=rc)
      write (failmsg, *) "Replacing a pre-existing Array in a State"
      write (name, *) "Replace an Array which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace on ArrayBundle
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test create an ArrayBundle for replacement
      alist10(1) = array10
      abund10 = ESMF_ArrayBundleCreate (name='temp bundle', arrayList=alist10, rc=rc)
      write (failmsg,*) "Creating ArrayBundle for replacement"
      write (name, *) "Creating ArrayBundle for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacement where none exists (should fail)
      call ESMF_StateReplace (state10, abund10, rc=rc)
      write (failmsg, *) "Replaced an ArrayBundle which did not exist"
      write (name, *) "Replace an ArrayBundle which does not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding in the ArrayBundle
      call ESMF_StateAdd (state10, abund10, rc=rc)
      write (failmsg, *) "Adding an ArrayBundle into a State"
      write (name, *) "Add an ArrayBundle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test create a 2nd ArrayBundle for replacement, but has same name
      ! as the 1st ArrayBundle
      alist11(1) = array11
      abund11 = ESMF_ArrayBundleCreate (name='temp bundle', arrayList=alist11, rc=rc)
      write (failmsg,*) "Creating ArrayBundle11 for replacement"
      write (name, *) "Creating ArrayBundle11 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)


      !EX_UTest
      ! Test replacing the 1st ArrayBundle with the 2nd one
      call ESMF_StateReplace (state10, abund11, rc=rc)
      write (failmsg, *) "Replacing a pre-existing ArrayBundle in a State"
      write (name, *) "Replace an ArrayBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace on Field
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test create an Field for replacement

      field10 = ESMF_FieldEmptyCreate (name="pressures",  &
        rc=rc)
      write (failmsg, *) "Creating field10 for replacement"
      write (name, *) "Creating field10 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacement where none exists (should fail)
      call ESMF_StateReplace (state10, field10, rc=rc)
      write (failmsg, *) "Replaced an Field which did not exist"
      write (name, *) "Replace an Field which does not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding in the Field
      call ESMF_StateAdd (state10, field10, rc=rc)
      write (failmsg, *) "Adding an Field into a State"
      write (name, *) "Add an Field which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test create a 2nd Field for replacement, but has same name
      ! as the 1st Field
      field11 = ESMF_FieldEmptyCreate (name="pressures",  &
        rc=rc)
      write (failmsg, *) "Creating field11 for replacement"
      write (name, *) "Creating field11 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacing the 1st Field with the 2nd one
      call ESMF_StateReplace (state10, field11, rc=rc)
      write (failmsg, *) "Replacing a pre-existing Field in a State"
      write (name, *) "Replace an Field which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace on FieldBundle
      !------------------------------------------------------------------------

      !EX______UTest

      !------------------------------------------------------------------------
      ! Test StateReplace on nested State
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test create a State for replacement

      state11 = ESMF_StateCreate (name="stateContainer2",  &
        statetype=ESMF_STATE_EXPORT, rc=rc)
      write (failmsg, *) "Creating state11 for replacement"
      write (name, *) "Creating state11 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacement where none exists (should fail)
      call ESMF_StateReplace (state10, nestedState=state11, rc=rc)
      write (failmsg, *) "Replacing a State which did not exist"
      write (name, *) "Replace a State which does not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding in the State - making a nested State
      call ESMF_StateAdd (state10, nestedState=state11, rc=rc)
      write (failmsg, *) "Adding a nested State into a State"
      write (name, *) "Add a nested State which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test create a 2nd State for replacement, but has same name
      ! as the 1st State
      state12 = ESMF_StateCreate (name="stateContainer2",  &
        statetype=ESMF_STATE_EXPORT, rc=rc)
      write (failmsg, *) "Creating state12 for replacement"
      write (name, *) "Creating state12 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacing the 1st State with the 2nd one
      call ESMF_StateReplace (state10, nestedState=state12, rc=rc)
      write (failmsg, *) "Replacing a pre-existing nested State in a State"
      write (name, *) "Replace a State which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateRemove
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test removing an item
      call ESMF_StateRemove (state10, itemName="temperatures", rc=rc)
      write (failmsg, *) "Removing an existing State item"
      write (name, *) "Remove an item which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test removing an item which shouldn't exist
      call ESMF_StateRemove (state10, itemName="temperatures", rc=rc)
      write (failmsg, *) "Removing an non-existing State item"
      write (name, *) "Remove a non-existing item test"
      call ESMF_Test (rc == ESMF_RC_NOT_FOUND, name, failMsg,  &
        result, ESMF_SRCLINE)

      ! StateRemove of FieldBundles

      !EX_UTest
      ! Create a locstream
      lstream = ESMF_LocStreamCreate (maxIndex=32, rc=rc)
      write (failmsg, *) "Creating LocStream for FieldBundle remove test"
      write (name, *) "Create LocStream for FieldBundle StateRemove test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Set ArraySpec
      call ESMF_ArraySpecSet(aspec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=rc)
      write (failmsg, *) "Creating ArraySpec for FieldBundle remove test"
      write (name, *) "Create ArraySpec for FieldBundle StateRemove test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Create a few Fields
      do, i=1, size (fields)
        write (fieldname,'(a,i1)') 'FB field ', i
        fields(i) = ESMF_FieldCreate (name=fieldname,  &
                    locstream=lstream, arrayspec=aspec, rc=rc)
        if (rc /= ESMF_SUCCESS) exit
      end do
      write (failmsg, *) "Creating Fields for FieldBundle remove test"
      write (name, *) "Create Field array for FieldBundle StateRemove test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Create a FieldBundle
      fbundle = ESMF_FieldBundleCreate (name="testbundle",  &
                  fieldList=fields, rc=rc)
      write (failmsg, *) "Creating FieldBundle for FieldBundle remove test"
      write (name, *) "Create FieldBundle for FieldBundle StateRemove test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Insert FieldBundle into a State
      call ESMF_StateAdd (state10, fbundle, rc=rc)
      write (failmsg, *) "Adding FieldBundle into a State"
      write (name, *) "Add FieldBundle into a State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Try removing an indirect Field
      call ESMF_StateRemove (state10, itemName='FB field 2', rc=rc)
      write (failmsg, *) "Did not fail when attempting to remove indirect Field"
      write (name, *) "Try removing an indirect Field test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Remove the FieldBundle
      call ESMF_StateRemove (state10, itemName='testbundle', rc=rc)
      write (failmsg, *) "Removing an existing FieldBundle from the State"
      write (name, *) "Remove a FieldBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Insert FieldBundle into the nested State
      call ESMF_StateAdd (state12, fbundle, rc=rc)
      write (failmsg, *) "Adding FieldBundle into a nested State"
      write (name, *) "Add FieldBundle into a nested State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

! call ESMF_StatePrint (state10, options='debug', nestedFlag=.true.)

      !EX_UTest
      ! Try removing an indirect Field from nested State
      call ESMF_StateRemove (state10, itemName='stateContainer2/FB field 2', rc=rc)
      write (failmsg, *) "Did not fail when attempting to remove indirect Field"
      write (name, *) "Try removing an indirect Field from nested State test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Remove the FieldBundle
      call ESMF_StateRemove (state10, itemName='stateContainer2/testbundle', rc=rc)
      write (failmsg, *) "Removing an existing FieldBundle from the State"
      write (name, *) "Remove a nested State FieldBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

! call ESMF_StatePrint (state10, options='debug', nestedFlag=.true.)

#if 0
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
      !call ESMF_Test(((rc.eq.ESMF_SUCCESS) .and. correct), &
                      !name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! TODO: Add Tests for StateGetDataPointer for Field and FieldBundle when they're
      !       working again. 

      call ESMF_StateDestroy(stateGDP, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

      call ESMF_ArrayDestroy(arrayGDP, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
#endif

      call ESMF_DistGridDestroy(distgrid, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      call ESMF_TestEnd(result, ESMF_SRCLINE)
 
  
      end program ESMF_StateUTest
