! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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
      use ESMF 
      use ESMF_StateAPImod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: rc, localrc
      logical :: isCreated
      type(ESMF_VM) :: vm
      character(ESMF_MAXSTR) :: statename, bundlename, bname
      character(ESMF_MAXSTR) :: fieldname, fname, aname, arrayname
      type(ESMF_Field) :: field1, field2, field3(3), field4, field5(3)
      type(ESMF_FieldBundle) :: bundle1, bundle2(1), bundle3, bundle5
      type(ESMF_State) :: state1, state2, state3, state4
      type(ESMF_StateItem_Flag) :: stateItemType
      
      type(ESMF_ArraySpec)  :: arrayspec
      type(ESMF_DistGrid)   :: distgrid
      type(ESMF_Array)      :: array, array2, testarray

#if defined (ESMF_TESTEXHAUSTIVE)
      integer :: itemcount
      type(ESMF_FieldBundle) :: bundle2inner(1)

      type(ESMF_LocStream)   :: lstream
      type(ESMF_ArraySpec)   :: aspec
      type(ESMF_Field)       :: fields(5)
      type(ESMF_FieldBundle) :: fbundle

      type(ESMF_Array)       :: array10, array11
      type(ESMF_Array)       :: array1x
      type(ESMF_ArrayBundle) :: abund10, abund11
      type(ESMF_ArrayBundle) :: abund1x
      type(ESMF_Array)       :: alist10(1), alist11(1)
      type(ESMF_Field)       :: field10, field11
      type(ESMF_Field)       :: field1x
      type(ESMF_Field)       :: fieldrnd(30)
      type(ESMF_FieldBundle) :: fbundle12
      type(ESMF_FieldBundle) :: fbundle1x
      type(ESMF_RouteHandle) :: routehandle11, routehandle12
      type(ESMF_RouteHandle) :: routehandle1x
      type(ESMF_State)       :: state10, state11, state12, state_attr
      type(ESMF_State)       :: state20, state30
      type(ESMF_State)       :: staternd

      character(4)           :: rndchars
      character(20)          :: rndfieldnames(size (fieldrnd))
      character(20)          :: namelist(size (fieldrnd))
      character(20)          :: sortedfieldnames (size (fieldrnd))
      real                   :: rndnums(size (fieldrnd) * len (rndchars))
      real                   :: rndnums2d(len (rndchars), size (fieldrnd))
      equivalence              (rndnums, rndnums2d)
      integer :: i
      integer :: linkcount

#if 0
      logical :: correct
      type(ESMF_Field) :: fieldGDP
      type(ESMF_FieldBundle) :: bundleGDP
      type(ESMF_State) :: stateGDP
      real(ESMF_KIND_R8), pointer :: ptrGDP1(:,:),ptrGDP2(:,:)
      type(ESMF_Array)      :: arrayGDP
#endif
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
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_VMGetGlobal (vm, rc=rc)

      !------------------------------------------------------------------------
      
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing State IsCreated for uncreated object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_StateIsCreated(state1)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing State IsCreated for uncreated object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_StateIsCreated(state1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

      !NEX_UTest      
      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(name=statename, stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing State IsCreated for created object"
  write(failMsg, *) "Did not return .true."
  isCreated = ESMF_StateIsCreated(state1)
  call ESMF_Test((isCreated .eqv. .true.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing State IsCreated for created object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_StateIsCreated(state1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing State IsCreated for destroyed object"
  write(failMsg, *) "Did not return .false."
  isCreated = ESMF_StateIsCreated(state1)
  call ESMF_Test((isCreated .eqv. .false.), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Testing State IsCreated for destroyed object"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  isCreated = ESMF_StateIsCreated(state1, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  !------------------------------------------------------------------------



      !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Add  uncreated array to uncreated  State 
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Add uncreated array to uncreated State Test"
      call ESMF_StateAdd(state4, (/testarray/), rc=rc)
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
      state4 = ESMF_StateCreate(name=statename, stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Add  uncreated array to a  State 
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Add uncreated array to a State Test"
      call ESMF_StateAdd(state4, (/testarray/), rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest 
      ! Get  uncreated array from State 
      arrayname = "Test Array"
      write(failMsg, *) "Did not return ESMF_RC_NOT_FOUND"
      write(name, *) "Get uncreated array from a State Test"
      call ESMF_StateGet(state4, arrayname, testarray, rc=rc)
      call ESMF_Test((rc.eq.ESMF_RC_NOT_FOUND), &
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
      state1 = ESMF_StateCreate(name=statename, stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
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
      state1 = ESMF_StateCreate(name=statename, stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
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
      call ESMF_StateAdd(state1, (/bundle1/), rc=rc)
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
      ! Add should should fail to replace existing item
      call ESMF_StateAdd(state1, (/bundle1/), rc=rc)
      write(name, *) "Replacing a FieldBundle with a second FieldBundle via Add in a State Test"
      call ESMF_Test((rc /= ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! Replace the existing item
      call ESMF_StateReplace (state1, (/bundle1/), rc=rc)
      write(name, *) "Replacing a FieldBundle with a second FieldBundle in a State Test"
      call ESMF_Test((rc == ESMF_SUCCESS), &
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
      call ESMF_StateAdd(state1, (/bundle3/), rc=rc)
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
      call ESMF_StateAdd(state1, (/field1/), rc=rc)
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
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), &
    regDecomp=(/2,2/), rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  array = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
    indexflag=ESMF_INDEX_GLOBAL, rc=rc)
!call ESMF_ArrayPrint(array)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ArraySet(array, name="testArray", rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_ArrayGet(array, name=aname, rc=rc)
  write(failMsg, *) "Wrong Array name "
  write(name, *) "Verifying that the Array has correct name Test"
  call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(aname.eq."testArray"), name, failMsg,&
    result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest
  call ESMF_StateAdd(state1, (/array/), rc=rc)
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
      ! Test printing of State
      call  ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

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
      !EX_UTest
      call ESMF_StateAdd(state1, (/state3/), rc=rc)
      write(name, *) "Adding an uninitialized State to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      call ESMF_StateAdd(state1, (/bundle5/), rc=rc)
      write(name, *) "Adding an uninitialized FieldBundle to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding an uninitialized Field to a State
      call ESMF_StateAdd(state1, (/field4/), rc=rc)
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
      bundle2(1) = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      state2 = ESMF_StateCreate(name=statename, stateintent=ESMF_STATEINTENT_EXPORT, &
                                fieldbundleList=bundle2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a FieldBundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test adding a State to a State 
      call ESMF_StateAdd(state2, (/state1/), rc=rc)
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
      call ESMF_StateAdd(state2, (/state3/), rc=rc)
      write(failMsg, *) "Did return ESMF_SUCCESS"
      write(name, *) "Adding the same State to a State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Add a FieldBundle to the outer State which has the same name
      ! as a FieldBundle in the nested State.  Add should fail since it
      ! has the same name as an existing name.
      bundlename = "Humidity"
      bundle2inner(1) = ESMF_FieldBundleCreate(name=bundlename, rc=rc)
      call ESMF_StateAdd(state1, &
                         fieldBundleList=bundle2inner,  &
                         rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a FieldBundle to a outer State test"
      call ESMF_Test((rc /= ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !EX_UTest
      ! Replace a FieldBundle to the outer State which has the same name
      ! as a FieldBundle in the nested State.
      call ESMF_StateReplace(state1, &
                         fieldBundleList=bundle2inner,  &
                         rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a FieldBundle to a outer State test"
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !call  ESMF_StatePrint(state2, nestedFlag=.true., rc=rc)

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
      ! Test Creation of an export State with a Field
      statename = "Export State"
      fieldname = "Precipitation"
      field3(1) = ESMF_FieldEmptyCreate(name=fieldname, rc=rc)
      state2 = ESMF_StateCreate(name=statename, stateintent=ESMF_STATEINTENT_EXPORT, &
                                fieldList=field3(1:1), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      ! Test Creation of an export State with an array
      !------------------------------------------------------------------------
      !state2 = ESMF_StateCreate(statename, ESMF_STATEINTENT_EXPORT, &
      !                          arrayList=array2(1:1), itemcount=x, rc=rc)

      ! Test Creation of an export State with an array
      !state2 = ESMF_StateCreate(statename, ESMF_STATEINTENT_EXPORT, &
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
      !state2 = ESMF_StateCreate(statename, ESMF_STATEINTENT_EXPORT, &
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
        stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
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
      call ESMF_StateReplace (state10, (/array10/), rc=rc)
      write (failmsg, *) "Replaced an Array which did not exist"
      write (name, *) "Replace an Array which does not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding in the Array
      call ESMF_StateAdd (state10, (/array10/), rc=rc)
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
      call ESMF_StateReplace (state10, (/array11/), rc=rc)
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
      call ESMF_StateReplace (state10, (/abund10/), rc=rc)
      write (failmsg, *) "Replaced an ArrayBundle which did not exist"
      write (name, *) "Replace an ArrayBundle which does not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding in the ArrayBundle
      call ESMF_StateAdd (state10, (/abund10/), rc=rc)
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
      call ESMF_StateReplace (state10, (/abund11/), rc=rc)
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
      call ESMF_StateReplace (state10, (/field10/), rc=rc)
      write (failmsg, *) "Replaced a Field which did not exist"
      write (name, *) "Replace a Field which does not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding in the Field
      call ESMF_StateAdd (state10, (/field10/), rc=rc)
      write (failmsg, *) "Adding a Field into a State"
      write (name, *) "Add a Field which does not exist test"
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
      call ESMF_StateReplace (state10, (/field11/), rc=rc)
      write (failmsg, *) "Replacing a pre-existing Field in a State"
      write (name, *) "Replace a Field which pre-exists test"
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
        stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
      write (failmsg, *) "Creating state11 for replacement"
      write (name, *) "Creating state11 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacement where none exists (should fail)
      call ESMF_StateReplace (state10, nestedStateList=(/state11/), rc=rc)
      write (failmsg, *) "Replacing a State which did not exist"
      write (name, *) "Replace a State which does not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding in the State - making a nested State
      call ESMF_StateAdd (state10, nestedStateList=(/state11/), rc=rc)
      write (failmsg, *) "Adding a nested State into a State"
      write (name, *) "Add a nested State which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test create a 2nd State for replacement, but has same name
      ! as the 1st State
      state12 = ESMF_StateCreate (name="stateContainer2",  &
        stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
      write (failmsg, *) "Creating state12 for replacement"
      write (name, *) "Creating state12 for replacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacing the 1st State with the 2nd one
      call ESMF_StateReplace (state10, nestedStateList=(/state12/), rc=rc)
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

      !EX_UTest
      ! Test removing an item which shouldn't exist
      call ESMF_StateRemove (state10, itemName="temperatures",  &
          relaxedFlag=.true., rc=rc)
      write (failmsg, *) "Relaxed removing an non-existing State item"
      write (name, *) "Relaxed remove a non-existing item test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
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
      call ESMF_StateAdd (state10, (/fbundle/), rc=rc)
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
      call ESMF_StateAdd (state12, (/fbundle/), rc=rc)
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

      ! StateRemove on a State that has attributes

      !EX_UTest
      ! Test create an empty State with attribute
      state_attr = ESMF_StateCreate (name="ImportState",  &
        stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
      write (failmsg, *) "Creating state_attr"
      write (name, *) "Creating state_attr test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test attribute count
      call ESMF_AttributeGet(state_attr, count=linkcount, &
          attcountflag=ESMF_ATTGETCOUNT_ATTLINK,  &
          rc=localrc)
      write (failmsg, *) "Attribute link count /= 0 (", linkcount, ")"
      write (name, *) "Empty state attribute count test"
      call ESMF_Test (linkcount == 0, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Add an Array
      call ESMF_StateAdd (state_attr, (/array10/), rc=rc)
      write (failmsg, *) "Adding an Array for Attribute count testing"
      write (name, *) "Adding an Array for attribute count testing"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test attribute count
      call ESMF_AttributeGet(state_attr, count=linkcount, &
          attcountflag=ESMF_ATTGETCOUNT_ATTLINK,  &
          rc=localrc)
      write (failmsg, *) "Attribute link count /= 1 (", linkcount, ")"
      write (name, *) "State with Array item attribute count test"
      call ESMF_Test (linkcount == 1, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Remove the Array
      call ESMF_StateRemove (state_attr, itemName='temperatures', rc=rc)
      write (failmsg, *) "Removinging an Array for Attribute count testing"
      write (name, *) "Removing an Array for attribute count testing"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test attribute count
      call ESMF_AttributeGet(state_attr, count=linkcount, &
          attcountflag=ESMF_ATTGETCOUNT_ATTLINK,  &
          rc=localrc)
      write (failmsg, *) "Attribute link count /= 0 (", linkcount, ")"
      write (name, *) "State with Array item attribute count test"
      call ESMF_Test (linkcount == 0, name, failMsg,  &
        result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      ! Test StateAddReplace on Array
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test create an empty State
      state20 = ESMF_StateCreate (name="stateContainer",  &
        stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
      write (failmsg, *) "Creating state20 for AddReplacement"
      write (name, *) "Creating state20 for AddReplacement test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test AddReplace where none exists
      call ESMF_StateAddReplace (state20, (/array10/), rc=rc)
      write (failmsg, *) "AddReplacing an Array which did not exist"
      write (name, *) "AddReplace an Array which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacing in the Array with itself
      call ESMF_StateAddReplace (state20, (/array10/), rc=rc)
      write (failmsg, *) "AddReplacing an Array into a State"
      write (name, *) "AddReplace an Array with itself test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Test replacing the 1st Array with a 2nd one with same name
      call ESMF_StateReplace (state20, (/array11/), rc=rc)
      write (failmsg, *) "Replacing a pre-existing Array in a State"
      write (name, *) "Replace an Array which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

! call ESMF_StatePrint (state10, options='debug', nestedFlag=.true.)

      !------------------------------------------------------------------------
      ! Tests for relaxed flag on each supported type
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test create an empty State
      state30 = ESMF_StateCreate (name="stateContainer",  &
        stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)
      write (failmsg, *) "Creating state30 for Add/relaxed"
      write (name, *) "Creating state30 for Add/relaxed test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateAdd w/relaxed on Array
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/array10/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed an Array which did not exist"
      write (name, *) "Add/relaxed an Array which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding a 2nd Array which has the same name as the first
      call ESMF_StateAdd (state30, (/array11/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed a pre-existing Array in a State"
      write (name, *) "Add/relaxed an Array which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace w/relaxed on Array
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test removing the Array
      call ESMF_StateRemove (state30, itemName='temperatures', rc=rc)
      write (failmsg, *) "Remove a pre-existing Array in a State"
      write (name, *) "Remove an Array which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed where none exists
      call ESMF_StateReplace (state30, (/array10/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed an Array which did not exist"
      write (name, *) "Replace/relaxed an Array which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test getting the Array (should fail)
      call ESMF_StateGet (state30, itemName='temperatures',  &
          array=array1x, rc=rc)
      write (failmsg, *) "Got an Array which should not exist in a State"
      write (name, *) "Getting an Array which should not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/array10/), rc=rc)
      write (failmsg, *) "Add an Array which did not exist"
      write (name, *) "Add an Array which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed a 2nd Array which has the same name as the first
      call ESMF_StateReplace (state30, (/array11/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed a pre-existing Array in a State"
      write (name, *) "Replace/relaxed an Array which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateAdd w/relaxed on ArrayBundle
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/abund10/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed an ArrayBundle which did not exist"
      write (name, *) "Add/relaxed an ArrayBundle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding a 2nd ArrayBundle which has the same name as the first
      call ESMF_StateAdd (state30, (/abund11/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed a pre-existing ArrayBundle in a State"
      write (name, *) "Add/relaxed an ArrayBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace w/relaxed on ArrayBundle
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test removing the ArrayBundle
      call ESMF_StateRemove (state30, itemName='temp bundle', rc=rc)
      write (failmsg, *) "Remove a pre-existing ArrayBundle in a State"
      write (name, *) "Remove an ArrayBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed where none exists
      call ESMF_StateReplace (state30, (/abund10/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed an ArrayBundle which did not exist"
      write (name, *) "Replace/relaxed an ArrayBundle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test getting the ArrayBundle (should fail)
      call ESMF_StateGet (state30, itemName='temp bundle',  &
          arraybundle=abund1x, rc=rc)
      write (failmsg, *) "Got an ArrayBundle which should not exist in a State"
      write (name, *) "Getting an ArrayBundle which should not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/abund10/), rc=rc)
      write (failmsg, *) "Add an ArrayBundle which did not exist"
      write (name, *) "Add an ArrayBundle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed a 2nd ArrayBundle which has the same name as the first
      call ESMF_StateReplace (state30, (/abund11/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed a pre-existing ArrayBundle in a State"
      write (name, *) "Replace/relaxed an ArrayBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateAdd w/relaxed on Field
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/field10/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed an Field which did not exist"
      write (name, *) "Add/relaxed an Field which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding a 2nd Field which has the same name as the first
      call ESMF_StateAdd (state30, (/field11/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed a pre-existing Field in a State"
      write (name, *) "Add/relaxed an Field which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace w/relaxed on Field
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test removing the Field
      call ESMF_StateRemove (state30, itemName='pressures', rc=rc)
      write (failmsg, *) "Remove a pre-existing Field in a State"
      write (name, *) "Remove an Field which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed where none exists
      call ESMF_StateReplace (state30, (/field10/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed an Field which did not exist"
      write (name, *) "Replace/relaxed an Field which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test getting the Field (should fail)
      call ESMF_StateGet (state30, itemName='pressures',  &
          field=field1x, rc=rc)
      write (failmsg, *) "Got an Field which should not exist in a State"
      write (name, *) "Getting an Field which should not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/field10/), rc=rc)
      write (failmsg, *) "Add an Field which did not exist"
      write (name, *) "Add an Field which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed a 2nd Field which has the same name as the first
      call ESMF_StateReplace (state30, (/field11/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed a pre-existing Field in a State"
      write (name, *) "Replace/relaxed an Field which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateAdd w/relaxed on FieldBundle
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test creating a 2nd FieldBundle
      fbundle12 = ESMF_FieldBundleCreate (name="testbundle", rc=rc)
      write (failmsg, *) "Creating a FieldBundle test"
      write (name, *) "Create a FieldBundle test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/fbundle/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed a FieldBundle which did not exist"
      write (name, *) "Add/relaxed a FieldBundle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding a 2nd FieldBundle which has the same name as the first
      call ESMF_StateAdd (state30, (/fbundle12/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed a pre-existing FieldBundle in a State"
      write (name, *) "Add/relaxed a FieldBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace w/relaxed on FieldBundle
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test removing the FieldBundle
      call ESMF_StateRemove (state30, itemName='testbundle', rc=rc)
      write (failmsg, *) "Remove a pre-existing FieldBundle in a State"
      write (name, *) "Remove a FieldBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed where none exists
      call ESMF_StateReplace (state30, (/fbundle/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed an FieldBundle which did not exist"
      write (name, *) "Replace/relaxed an FieldBundle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test getting the FieldBundle (should fail)
      call ESMF_StateGet (state30, itemName='testbundle',  &
          fieldbundle=fbundle1x, rc=rc)
      write (failmsg, *) "Got an FieldBundle which should not exist in a State"
      write (name, *) "Getting an FieldBundle which should not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/fbundle/), rc=rc)
      write (failmsg, *) "Add an FieldBundle which did not exist"
      write (name, *) "Add a FieldBundle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed a 2nd FieldBundle which has the same name as the first
      call ESMF_StateReplace (state30, (/fbundle12/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed a pre-existing FieldBundle in a State"
      write (name, *) "Replace/relaxed a FieldBundle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateAdd w/relaxed on RouteHandle
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test creating a RouteHandle
      routehandle11 = ESMF_RouteHandleCreate (rc)
      write (failmsg, *) "Creating a RouteHandle test"
      write (name, *) "Create a RouteHandle test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test set the RouteHandle name
      call ESMF_RouteHandleSet (routehandle11, name='rhandle', rc=rc)
      write (failmsg, *) "Setting a RouteHandle name test"
      write (name, *) "Set a RouteHandle name test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test creating a 2nd RouteHandle
      routehandle12 = ESMF_RouteHandleCreate (rc)
      write (failmsg, *) "Creating a RouteHandle test"
      write (name, *) "Create a RouteHandle test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test set the 2nd RouteHandle name
      call ESMF_RouteHandleSet (routehandle12, name='rhandle', rc=rc)
      write (failmsg, *) "Setting a 2nd RouteHandle name test"
      write (name, *) "Set a 2nd RouteHandle name test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/routehandle11/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed a RouteHandle which did not exist"
      write (name, *) "Add/relaxed a RouteHandle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test adding a 2nd RouteHandle which has the same name as the first
      call ESMF_StateAdd (state30, (/routehandle12/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Add/relaxed a pre-existing RouteHandle in a State"
      write (name, *) "Add/relaxed a RouteHandle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateReplace w/relaxed on RouteHandle
      !------------------------------------------------------------------------

      !EX_UTest
      ! Test removing the RouteHandle
      call ESMF_StateRemove (state30, itemName='rhandle', rc=rc)
      write (failmsg, *) "Remove a pre-existing RouteHandle in a State"
      write (name, *) "Remove a RouteHandle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed where none exists
      call ESMF_StateReplace (state30, (/routehandle11/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed an RouteHandle which did not exist"
      write (name, *) "Replace/relaxed an RouteHandle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test getting the RouteHandle (should fail)
      call ESMF_StateGet (state30, itemName='rhandle',  &
          routehandle=routehandle1x, rc=rc)
      write (failmsg, *) "Got an RouteHandle which should not exist in a State"
      write (name, *) "Getting an RouteHandle which should not exist test"
      call ESMF_Test (rc /= ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Add where none exists
      call ESMF_StateAdd (state30, (/routehandle11/), rc=rc)
      write (failmsg, *) "Add an RouteHandle which did not exist"
      write (name, *) "Add a RouteHandle which does not exist test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !EX_UTest
      ! Test Replace/relaxed a 2nd RouteHandle which has the same name as the first
      call ESMF_StateReplace (state30, (/routehandle12/), relaxedflag=.true., rc=rc)
      write (failmsg, *) "Replace/relaxed a pre-existing RouteHandle in a State"
      write (name, *) "Replace/relaxed a RouteHandle which pre-exists test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
          result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Test StateAdd w/relaxed on nested State
      !------------------------------------------------------------------------
      ! insert test code here
      !------------------------------------------------------------------------
      ! Test StateReplace w/relaxed on nested State
      !------------------------------------------------------------------------

! call ESMF_StatePrint (state30, options='debug', nestedFlag=.true.)

      !------------------------------------------------------------------------
      ! Test adding Fields with random names, and obtaining lists
      !------------------------------------------------------------------------

      !EX_UTest
      ! Create a few Fields with random names
      call random_number (rndnums)
      rndnums = rndnums*26 + 65  ! Convert to ASCII A-Z
      do, i=1, size (fieldrnd)
        write (rndfieldnames(i),'(a,4a)')  &
            'random field ', achar (int (rndnums2d(:,i)))
        fieldrnd(i) = ESMF_FieldCreate (name=rndfieldnames(i),  &
            locstream=lstream, arrayspec=aspec, rc=rc)
        if (rc /= ESMF_SUCCESS) exit
      end do

      write (failmsg, *) "Creating Fields for random Field test"
      write (name, *) "Create Field array for random Field test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Create a State for random Fields
      staternd = ESMF_StateCreate (name='random Field State', rc=rc)
      write (failmsg, *) "Creating Fields for random Field test"
      write (name, *) "Create Field array for random Field test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Add random Fields to the State
      call ESMF_StateAdd (staternd, FieldList=fieldrnd, rc=rc)
      write (failmsg, *) "Adding random Fields to State test"
      write (name, *) "Adding random Fields to State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Sort names
      sortedfieldnames = rndfieldnames
      call ESMF_UtilSort (sortedfieldnames,  &
          direction=ESMF_SORTFLAG_ASCENDING, rc=rc)
      write (failmsg, *) "Adding random Fields to State test"
      write (name, *) "Adding random Fields to State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)
!      print '(a, (a))', 'sorted Field name:', ' ',  &
!          (trim (sortedfieldnames(i)),i=1, size (sortedfieldnames))

      !EX_UTest
      ! Get list of names from State
      call ESMF_StateGet (staternd, itemNameList=namelist,  &
          rc=rc)
      write (failmsg, *) "Obtaining names from State test"
      write (name, *) "Obtaining names from State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Verify they are in sorted order (default)
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (namelist == sortedfieldnames))
      write (failmsg, *) "Verification error in sorted names from State test"
      write (name, *) "Verifying sorted names from State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Get list of explicitly sorted names from State
      call ESMF_StateGet (staternd, itemNameList=namelist,  &
          itemorderflag=ESMF_ITEMORDER_ABC,  &
          rc=rc)
      write (failmsg, *) "Obtaining explicitly sorted names from State test"
      write (name, *) "Obtaining explicitly sorted names from State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Verify they are in sorted order
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (namelist == sortedfieldnames))
      write (failmsg, *) "Verification error in explicitly sorted names from State test"
      write (name, *) "Verifying explicitly sorted sorted names from State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Get list of names from State in order added
      call ESMF_StateGet (staternd, itemNameList=namelist,  &
          itemorderflag=ESMF_ITEMORDER_ADDORDER,  &
          rc=rc)
      write (failmsg, *) "Obtaining added order names from State test"
      write (name, *) "Obtaining added order names from State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !EX_UTest
      ! Verify they are in added order
      rc = merge (ESMF_SUCCESS, ESMF_FAILURE, all (namelist == rndfieldnames))
      write (failmsg, *) "Verification error in added order names from State test"
      write (name, *) "Verifying added order names from State test"
      call ESMF_Test (rc == ESMF_SUCCESS, name, failMsg,  &
        result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      ! Clean up
      !------------------------------------------------------------------------

      call ESMF_StateDestroy (state10)

#if 0
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!                    Test StateGetDataPointer                        !!
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      stateGDP = ESMF_StateCreate("stateGDP", ESMF_STATEINTENT_EXPORT, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
      call ESMF_ArraySpecSet(arrayspec, typekind=ESMF_TYPEKIND_R8, rank=2, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/15,23/), rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      arrayGDP = ESMF_ArrayCreate(arrayspec=arrayspec, distgrid=distgrid, &
               indexflag=ESMF_INDEX_GLOBAL, name="arrayGDP", rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  
      call ESMF_StateAdd(stateGDP, arrayGDP, rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      !------------------------------------------------------------------------
      !EX_removeUTest

      ! init variables
      localrc=ESMF_SUCCESS
      correct=.true.
      nullify(ptrGDP1)
      nullify(ptrGDP2)

      ! Get Array Data Pointer From State
      call ESMF_StateGetDataPointer(state=stateGDP, itemName="arrayGDP", &
                                    dataPointer=ptrGDP1, datacopyflag=ESMF_DATACOPY_REFERENCE, rc=localrc) 
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
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_ArrayDestroy(arrayGDP, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

      call ESMF_DistGridDestroy(distgrid, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      call ESMF_TestEnd(ESMF_SRCLINE)
 
  
      end program ESMF_StateUTest
