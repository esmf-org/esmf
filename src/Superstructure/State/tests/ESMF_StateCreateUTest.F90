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

!--- Module used to test attachable methods in unit tests below ----------------
module userMethodMod
  use ESMF
  implicit none
  public myUserMethod
contains
  subroutine myUserMethod(state, rc)
    type(ESMF_State)     :: state
    integer, intent(out) :: rc
    print *, "Hi from myUserMethod"
    call ESMF_StatePrint(state, rc=rc)
  end subroutine
end module
!-------------------------------------------------------------------------------

      program ESMF_StateCreateUTest

!==============================================================================
!
#include "ESMF.h"
!
!BOP
! !PROGRAM: ESMF_StateCreateUTest - Test code which creates new States.  
!
! !DESCRIPTION:
!
! The code in this file drives F90 State Create unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF
      use ESMF_StateContainerMod
      use ESMF_StateItemMod
      use userMethodMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
!------------------------------------------------------------------------------
!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: sname
    character(1000) :: testName
    !type(ESMF_Field) :: field1, field2
    type(ESMF_State) :: state1, stateAlias
    logical:: stateBool


    ! individual test failure messages
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0


  type(ESMF_Container)            :: container
  type(ESMF_StateItem), pointer   :: si
  
  type(ESMF_StateItemWrap):: siw
  type(ESMF_StateItemWrap), pointer:: siwOut(:)

    ! local variables needed to pass into function/subroutine calls
    !character(ESMF_MAXSTR) :: validate_options
    !character(ESMF_MAXSTR) :: print_options

#ifdef ESMF_TESTEXHAUSTIVE
    character(ESMF_MAXSTR) :: bname
    type(ESMF_State) :: state2, state3, state4, state5
    type(ESMF_FieldBundle) :: bundle1, bundle2, bundle3, qbundle
    type(ESMF_FieldBundle) :: bundle5, bundle6, bundle7
    type(ESMF_VM) :: vm
    logical :: isNeeded

    integer :: itemcount, itemcountnested
    character(ESMF_MAXSTR), allocatable :: itemlist(:)
    type(ESMF_StateItem_Flag), allocatable :: itemtypelist(:)
#endif

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
      
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Create/Destroy an Empty State.
      sname = "Atmosphere Import"
      state1 = ESMF_StateCreate(name=sname, stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating an empty State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "State equality before assignment Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      stateBool = (stateAlias.eq.state1)
      call ESMF_Test(.not.stateBool, name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Testing ESMF_StateAssignment(=)()
      write(name, *) "State assignment and equality Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      stateAlias = state1
      stateBool = (stateAlias.eq.state1)
      call ESMF_Test(stateBool, name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "StateDestroy Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_StateDestroy(state1, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Testing ESMF_StateOperator(==)()
      write(name, *) "State equality after destroy Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      stateBool = (stateAlias==state1)
      call ESMF_Test(.not.stateBool, name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Testing ESMF_StateOperator(/=)()
      write(name, *) "State non-equality after destroy Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      stateBool = (stateAlias/=state1)
      call ESMF_Test(stateBool, name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest
      write(name, *) "Double StateDestroy through alias Test"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_StateDestroy(stateAlias, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
      !------------------------------------------------------------------------
      !NEX_UTest   
      ! Create/Destroy an Empty State.
      sname = "Atmosphere Import"
      state1 = ESMF_StateCreate(name=sname, stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating an empty State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test printing an Empty State
      call ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing an empty State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test getting name from an Empty State
      call ESMF_StateGet(state1, name=testName, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting name from an empty State Test"
      print *,"testName: ", trim (testName)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
                      
      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test attaching user method to an Empty State
      call ESMF_MethodAdd(state1, label="user1", userRoutine=myUserMethod, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Attach user method to an empty State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
                      
      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test attaching existing user method to an Empty State
      call ESMF_MethodAdd(state1, label="user1", userRoutine=myUserMethod, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Attach existing user method to an empty State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
                      
                      
      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test attaching user method to an Empty State
      call ESMF_MethodAdd(state1, label="user2", userRoutine=myUserMethod, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Attach user method to an empty State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
                      
      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test removing attached user method from Empty State
      call ESMF_MethodRemove(state1, label="user1", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Removing user method from empty State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
                      

      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test removing non-existing attached user method from Empty State
      call ESMF_MethodRemove(state1, label="user1", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Removing non-existing user method from empty State Test"
      call ESMF_Test((rc.ne.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
                      

      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test executing attached user method in an Empty State
      call ESMF_MethodExecute(state1, label="user2", rc=rc)
      write(failMsg, *) ""
      write(name, *) "Executing user method in an empty State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
                      

      !------------------------------------------------------------------------
      !NEX_UTest      
      ! Test Destruction of an empty import State 
      call ESMF_StateDestroy(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying an empty State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      
  ! CONTAINER USE TESTING --- internal API, subject to change!!!!!!!!!
      
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Create Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  container = ESMF_ContainerCreate(rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  allocate(si)
  si%namep = "test this string"
  si%datap%fp = ESMF_FieldEmptyCreate(name="testField1", rc=rc)
  si%otype = ESMF_STATEITEM_FIELD
  
  siw%si => si
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Add si Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerAdd(container, itemList=(/siw/), rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Print Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerPrint(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get item Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  siwOut => null ()
  call ESMF_ContainerGet(container, itemList=siwOut, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "string in siwOut: ", siwOut(1)%si%namep
  print *, "string in si: ", si%namep
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Verify Container Get item Test"
  write(failMsg, *) "Did not verify"
  call ESMF_Test((trim(siwOut(1)%si%namep)==trim(si%namep)), name, failMsg, result, ESMF_SRCLINE)

  deallocate (siwOut)
  
  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Get item Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  siw%si => null ()
  call ESMF_ContainerGet(container, itemName="testField1", item=siw, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  print *, "string in siw%si: ", siw%si%namep

  call ESMF_FieldDestroy (siw%si%datap%fp)

  !------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Container Destroy Test"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ContainerDestroy(container, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     
  deallocate(si)    
      
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Creation of an empty export State 
      sname = "Ocean Export"
      state2 = ESMF_StateCreate(name=sname, stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Create a bundle to use in the subsequent tests
      bname="Surface pressure"
      bundle1 = ESMF_FieldBundleCreate(name=bname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty bundle for State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test adding a bundle to a state
      call ESMF_StateAdd(state2, (/bundle1/), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a FieldBundle to a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test printing a State with 1 FieldBundle
      call ESMF_StatePrint(state2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a State with 1 FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Create a second bundle to use in the subsequent tests
      bname="Temperature"
      bundle2 = ESMF_FieldBundleCreate(name=bname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty bundle for State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test adding a second bundle to a state
      call ESMF_StateAdd(state2, (/bundle2/), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a second FieldBundle to a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test printing a State with 2 FieldBundles
      call ESMF_StatePrint(state2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a State with 2 FieldBundles"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a FieldBundle by name
      call ESMF_StateGet(state2, "Surface pressure", qbundle, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a FieldBundle from a State by name"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a State
      call ESMF_StateDestroy(state2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a FieldBundle
      call ESMF_FieldBundleDestroy(bundle1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying the other FieldBundle
      call ESMF_FieldBundleDestroy(bundle2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Creating a State
      sname = "Ocean Export"
      state3 = ESMF_StateCreate(name=sname, stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Adding a name only
      sname = "Downward wind:needed"
      call ESMF_AttributeSet (state3, sname, value=.false., rc=rc)
!      call ESMF_StateAdd(state3, sname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a name only"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Marking an item needed
      sname = "Downward wind:needed"
      call ESMF_AttributeSet (state3, sname, value=.true., rc=rc)
!      call ESMF_StateSetNeeded(state3, sname, ESMF_NEEDED, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Marking an item needed"
      Call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Querying if an item is needed, using sname from above
      call ESMF_AttributeGet (state3, sname, value=isNeeded, rc=rc)
!      isNeeded =  ESMF_StateIsNeeded(state3, sname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Querying if an item is needed"
      call ESMF_Test(((rc.eq.ESMF_SUCCESS) .or. (.not. isNeeded)), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Creating a FieldBundle to add to a State
      bundle2 = ESMF_FieldBundleCreate(name=sname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Replacing a name placeholder with a real item, using bundle from above
      call ESMF_StateAdd(state3, (/bundle2/), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Replacing a name placeholder with a bundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a State
      call ESMF_StateDestroy(state3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a FieldBundle
      call ESMF_FieldBundleDestroy(bundle2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a FieldBundle"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
    
      ! other tests to be added here
      state4 = state3

      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Create a State which will contain other nested States
      sname = "Coupler Statelist"
      state5 = ESMF_StateCreate(name=sname, stateintent=ESMF_STATEINTENT_UNSPECIFIED, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating a State for nested State test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Create a State which will be put into the other
      sname = "Atmosphere Import"
      state1 = ESMF_StateCreate(name=sname, stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest 
      ! Create a bundle to use in the subsequent tests
      bname="Temperature"
      bundle2 = ESMF_FieldBundleCreate(name=bname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty bundle for nested State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Add the Fieldbundle to the State
      call ESMF_StateAdd(state1, (/bundle2/), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a FieldBundle to a nested State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Add a nested State to another
      call ESMF_StateAdd(state5, (/state1/), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Add a nested State into another State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Create another State which will be put into a State
      sname = "Ocean Export"
      state2 = ESMF_StateCreate(name=sname, stateintent=ESMF_STATEINTENT_EXPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Add another nested State to the first
      call ESMF_StateAdd(state5, (/state2/), rc=rc)
      write(failMsg, *) ""
      write(name, *) "Add a second nested State into another State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate the nested State
      call ESMF_StateValidate (state5, nestedFlag=.false., rc=rc)
      write(failMsg, *) ""
      write(name, *) "Validating a nested State - single level"
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Validate the nested State with full depth
      call ESMF_StateValidate (state5, nestedFlag=.true., rc=rc)
      write(failMsg, *) ""
      write(name, *) "Validating a nested State - nested"
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Print the nested State
      call ESMF_StatePrint (state5, nestedFlag=.false., options='long', rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a nested State - single level"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a nested State object
      call ESMF_StateGet(state=state5,  &
         itemname="Atmosphere Import/Temperature",  &
         FieldBundle=bundle3, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting a FieldBundle from a nested State"
      print *,"testName: ", trim (testName)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a nested State object Info item count at the current level
      call ESMF_StateGet(state=state5,  &
         itemCount=itemcount, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting item count from a nested State - single level"
      print *,"item count: ", itemcount
      call ESMF_Test(rc == ESMF_SUCCESS .and. itemCount == 2, &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a nested State object Info item name list
      allocate (itemlist(itemcount))
      call ESMF_StateGet(state=state5,  &
         itemNameList=itemlist, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting item name list from a nested State - single level"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a nested State object Info item type list
      allocate (itemtypelist(itemcount))
      call ESMF_StateGet(state=state5,  &
         itemtypeList=itemtypelist, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting item type list from a nested State - single level"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call print_itemlist (itemlist, itemtypelist)
      deallocate (itemtypelist, itemlist)
      !------------------------------------------------------------------------
      !EX_UTest
      ! Print the nested State
      call ESMF_StatePrint (state5, nestedFlag=.true., options='long', rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing a nested State - nested"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a nested State object Info item count - including nested
      ! objects
      call ESMF_StateGet(state=state5, nestedFlag=.true., &
         itemCount=itemcountnested, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting item count from a nested State - nested"
      print *,"nested item count: ", itemcountnested
      call ESMF_Test(rc == ESMF_SUCCESS .and. itemCountNested == 3, &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a nested State object Info item name list
      allocate (itemlist(itemcountnested))
      itemlist = " "
      call ESMF_StateGet(state=state5, nestedFlag=.true.,  &
         itemNameList=itemlist, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting item name list from a nested State - nested"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a nested State object Info item type list
      allocate (itemtypelist(itemcountnested))
      call ESMF_StateGet(state=state5, nestedFlag=.true.,  &
         itemtypeList=itemtypelist, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting item type list from a nested State - nested"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call print_itemlist (itemlist, itemtypelist)
      deallocate (itemtypelist, itemlist)

      !------------------------------------------------------------------------
      !EX_UxxTest      
      ! Test getting a nested State object using nestedFlag
      !call ESMF_StateGet(state=state5, nestedFlag=.true.,  &
      !   itemName="Temperature", fieldBundle=bundle4, rc=rc)
      !write(failMsg, *) ""
      !write(name, *) "Getting nested FieldBundle from a nested State using nestedFlag"
      !call ESMF_Test((rc == ESMF_SUCCESS), &
      !                name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a non-existant nested State object
      call ESMF_StateGet(state=state5,  &
         itemName="TemperatureXYZZY", fieldBundle=bundle5, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting nested non-existant FieldBundle from a nested State using nestedFlag"
      call ESMF_Test((rc /= ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a nested State object using path style
      call ESMF_StateGet(state=state5,  &
         itemName="Atmosphere Import/Temperature", fieldBundle=bundle6, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting nested FieldBundle from a nested State using path style"
      call ESMF_Test((rc == ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a non-existant nested State object
      call ESMF_StateGet(state=state5,  &
         itemName="Atmosphere Import/TemperatureXYZZY", fieldBundle=bundle7, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting nested FieldBundle from a nested State using path style - bad end item"
      call ESMF_Test((rc /= ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Test getting a non-existant nested State object
      call ESMF_StateGet(state=state5,  &
         itemName="Atmosphere ImportXYZZY/Temperature", fieldBundle=bundle7, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Getting nested non-existant FieldBundle from a nested State using path style - bad nested name"
      call ESMF_Test((rc /= ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UxxTest      
      ! Test getting a nested State object with bad argument combination.
      !call ESMF_StateGet(state=state5, nestedFlag=.true.,  &
      !   itemName="Atmosphere Import/Temperature", fieldBundle=bundle7, rc=rc)
      !write(failMsg, *) ""
      !write(name, *) "Get with both path-style name and .true. set"
      !call ESMF_Test((rc /= ESMF_SUCCESS), &
      !                name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a State
      call ESMF_StateDestroy(state5, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a State
      call ESMF_StateDestroy(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a State
      call ESMF_StateDestroy(state2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! Create an Array for use below
      !------------------------------------------------------------------------
      ! Create another Array for use below
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Create an empty State
      sname = "Atmosphere Import"
      state1 = ESMF_StateCreate(name=sname, stateintent=ESMF_STATEINTENT_IMPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Create a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      ! Add an Array to a State
      !------------------------------------------------------------------------
      ! Add a second Array to a State
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Destroying a State
      call ESMF_StateDestroy(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Destroying a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      ! Destroying an Array
      !------------------------------------------------------------------------
      ! Destroying an Array
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      ! End of Exhaustive tests
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      call ESMF_TestEnd(ESMF_SRCLINE)
 
      contains

        subroutine print_itemlist (names, types)
          character(*), intent(in) :: names(:)
          type(ESMF_StateItem_Flag), intent(in) :: types(:)

          integer :: i

          print *, 'size(names) =', size (names)
          do, i=1, size (names)
            select case (types(i)%ot)
            case (ESMF_STATEITEM_FIELD%ot)
              print *, 'ESMF_STATEITEM_FIELD:       ', trim (names(i))
            case (ESMF_STATEITEM_FIELDBUNDLE%ot)
              print *, 'ESMF_STATEITEM_FIELDBUNDLE: ', trim (names(i))
            case (ESMF_STATEITEM_ARRAY%ot)
              print *, 'ESMF_STATEITEM_ARRAY:       ', trim (names(i))
            case (ESMF_STATEITEM_ARRAYBUNDLE%ot)
              print *, 'ESMF_STATEITEM_ARRAYBUNDLE: ', trim (names(i))
            case (ESMF_STATEITEM_ROUTEHANDLE%ot)
              print *, 'ESMF_STATEITEM_ROUTEHANDLE: ', trim (names(i))
            case (ESMF_STATEITEM_STATE%ot)      
              print *, 'ESMF_STATEITEM_STATE:       ', trim (names(i))
            case (ESMF_STATEITEM_UNKNOWN%ot)    
              print *, 'ESMF_STATEITEM_UNKNOWN:     ', trim (names(i))
            case (ESMF_STATEITEM_NOTFOUND%ot)   
              print *, 'ESMF_STATEITEM_NOTFOUND:    ', trim (names(i))
            case default
              print *, '(unknown type):             ', trim (names(i))
            end select
          end do

        end subroutine print_itemlist

      end program ESMF_StateCreateUTest

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------


#if 0
! older tests which have not yet been convereted to the normal template form.
    sname = "Sea Ice Export"
    state4 = ESMF_StateCreate(sname, ESMF_STATEINTENT_EXPORT, rc=rc)

    sname = "Surface pressure"
    call ESMF_StateAdd(state4, sname, rc=rc)
    
    call ESMF_StateSetNeeded(state4, sname, ESMF_NEEDED, rc=rc)
    
    sname = "Energy Flux"
    call ESMF_StateAdd(state4, sname, rc=rc)
    
    call ESMF_StateSetNeeded(state4, sname, ESMF_NEEDED, rc=rc)
    
    sname = "Humidity"
    call ESMF_StateAdd(state4, sname, rc=rc)
    
    call ESMF_StateSetNeeded(state4, sname, ESMF_NEEDED, rc=rc)
    
    call ESMF_StatePrint(state4, rc=rc)

    bname = "Collected quantities"
    bundle2 = ESMF_FieldBundleCreate(name=bname, rc=rc)
      
    fname = "Surface pressure"
    field1 = ESMF_FieldCreateNoData(fname, rc=rc)

    call ESMF_FieldBundleAdd(bundle2, (/field1/), rc=rc) 

    fname = "Energy Flux"
    field2 = ESMF_FieldCreateNoData(fname, rc=rc)

    call ESMF_FieldBundleAdd(bundle2, (/field2/), rc=rc) 

    call ESMF_FieldBundlePrint(bundle2, "", rc=rc)


    call ESMF_StateAdd(state4, bundle2, rc=rc)

    call ESMF_StatePrint(state4, rc=rc)
    
    call ESMF_StateDestroy(state4, rc=rc)

    call ESMF_FieldBundleDestroy(bundle2, rc=rc)

    call ESMF_FieldDestroy(field1, rc=rc)

    call ESMF_FieldDestroy(field2, rc=rc)
#endif
    
