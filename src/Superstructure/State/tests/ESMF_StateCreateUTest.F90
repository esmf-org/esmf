! $Id: ESMF_StateCreateUTest.F90,v 1.7.2.8 2009/01/21 21:25:25 cdeluca Exp $
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
      use ESMF_Mod 
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_StateCreateUTest.F90,v 1.7.2.8 2009/01/21 21:25:25 cdeluca Exp $'
!------------------------------------------------------------------------------

!   ! Local variables
    integer :: rc
    character(ESMF_MAXSTR) :: sname, bname
    character(1000) :: testName
    !type(ESMF_Field) :: field1, field2
    type(ESMF_FieldBundle) :: bundle1, bundle2, qbundle
    type(ESMF_State) :: state1, state2, state3, state4, state5
    logical :: isNeeded

    ! individual test failure messages
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

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
      ! Create/Destroy an Empty State.
      sname = "Atmosphere Import"
      state1 = ESMF_StateCreate(sname, ESMF_STATE_IMPORT, rc=rc)  
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
      print *,"testName: ",testName
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
      
#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest 
      ! Test Creation of an empty export State 
      sname = "Ocean Export"
      state2 = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)  
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
      call ESMF_StateAdd(state2, bundle1, rc=rc)
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
      call ESMF_StateAdd(state2, bundle2, rc=rc)
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
      state3 = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Adding a name only
      sname = "Downward wind"
      call ESMF_StateAdd(state3, sname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Adding a name only"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Marking an item needed
      sname = "Downward wind"
      call ESMF_StateSetNeeded(state3, sname, ESMF_NEEDED, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Marking an item needed"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Querying if an item is needed, using sname from above
      isNeeded =  ESMF_StateIsNeeded(state3, sname, rc=rc)
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
      call ESMF_StateAdd(state3, bundle2, rc=rc)
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
      state5 = ESMF_StateCreate(sname, ESMF_STATE_UNSPECIFIED, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating a State for nested State test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Create a State which will be put into the other
      sname = "Atmosphere Import"
      state1 = ESMF_StateCreate(sname, ESMF_STATE_IMPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Add a nested State to another
      call ESMF_StateAdd(state5, state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Add a nested State into another State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Create another State which will be put into a State
      sname = "Ocean Export"
      state2 = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)  
      write(failMsg, *) ""
      write(name, *) "Creating a State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !EX_UTest      
      ! Add another nested State to the first
      call ESMF_StateAdd(state5, state2, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Add a second nested State into another State"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
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
      state1 = ESMF_StateCreate(sname, ESMF_STATE_IMPORT, rc=rc)  
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

      call ESMF_TestEnd(result, ESMF_SRCLINE)
 
  
      end program ESMF_StateCreateUTest

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------


#if 0
! older tests which have not yet been convereted to the normal template form.
    sname = "Sea Ice Export"
    state4 = ESMF_StateCreate(sname, ESMF_STATE_EXPORT, rc=rc)

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

    call ESMF_FieldBundleAdd(bundle2, field1, rc=rc) 

    fname = "Energy Flux"
    field2 = ESMF_FieldCreateNoData(fname, rc=rc)

    call ESMF_FieldBundleAdd(bundle2, field2, rc=rc) 

    call ESMF_FieldBundlePrint(bundle2, "", rc=rc)


    call ESMF_StateAdd(state4, bundle2, rc=rc)

    call ESMF_StatePrint(state4, rc=rc)
    
    call ESMF_StateDestroy(state4, rc=rc)

    call ESMF_FieldBundleDestroy(bundle2, rc=rc)

    call ESMF_FieldDestroy(field1, rc=rc)

    call ESMF_FieldDestroy(field2, rc=rc)
#endif
    
