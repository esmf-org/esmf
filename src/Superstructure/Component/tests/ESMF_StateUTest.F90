! $Id: ESMF_StateUTest.F90,v 1.14 2003/04/16 19:31:49 svasquez Exp $
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
      program ESMF_StateUTest

!==============================================================================
!
#include "ESMF_Macros.inc"
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
      '$Id: ESMF_StateUTest.F90,v 1.14 2003/04/16 19:31:49 svasquez Exp $'
!------------------------------------------------------------------------------

!     ! Local variables
      integer :: x, y, rc
      logical :: IsNeeded
      character(ESMF_MAXSTR) :: compname, statename, bundlename, dataname, bname
      character(ESMF_MAXSTR) :: fieldname, fname
      type(ESMF_Field) :: field1, field2, field3(3), field4
      type(ESMF_Bundle) :: bundle1, bundle2(1), bundle3(1), bundle4(1), bundle5, bundle6
      type(ESMF_State) :: state1, state2, state3, state4
      type(ESMF_Array) :: array1, array2(2), array3
      real, dimension(:,:), pointer :: f90ptr1

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables needed to pass into function/subroutine calls
      character(ESMF_MAXSTR) :: validate_options
      character(ESMF_MAXSTR) :: print_options
      !type(ESMF_StateConfig) :: config_set
      !type(ESMF_StateConfig) :: config_get
      ! when get/set value routines enabled, comment these in and set
      ! the appropriate values.  then remove the temporary integers.
      !<value type> :: value_set, value_get
      ! integer :: value_set, value_get 

      ! instantiate a State 
      type(ESMF_State) :: state

#ifdef ESMF_EXHAUSTIVE

      ! perform exhaustive tests here;
      !   see #else below for non-exhaustive tests
      ! future release will use run-time switching mechanism

      ! for deep classes, keep create/construct routine and remove init
      ! for shallow classes, keep init and remove create/construct
     
      ! test dynamic allocation of ESMF_State
      state = ESMF_StateCreate(rc=rc)
      write(name, *) "ESMF_StateCreate"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_State
      call ESMF_StateConstruct(state, rc)
      write(name, *) "ESMF_StateConstruct"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      call ESMF_StateSetConfig(state, config_set, rc)
      write(name, *) "ESMF_StateSetConfig"
      write(failMsg, *) "rc =", rc, ", config_set =", config_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      call ESMF_StateGetConfig(state, config_get, rc)
      write(name, *) "ESMF_StateGetConfig"
      write(failMsg, *) "rc =", rc, ", config_get =", config_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_State members values
      !call ESMF_StateSet<Value>(state, value_set, rc)
      rc = ESMF_FAILURE  ! remove this when this test enabled
      write(name, *) "ESMF_StateSet<Value>"
      write(failMsg, *) "rc =", rc, ", value_set =", value_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_State members values,
      !   compare to values set previously
      !<value type> :: value_get
      !call ESMF_StateGet<Value>(state, value_get, rc)
      rc = ESMF_FAILURE  ! remove this when this test enabled
      write(name, *) "ESMF_StateGet<Value>"
      write(failMsg, *) "rc =", rc, ", value_get =", value_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      call ESMF_StateValidate(state, validate_options, rc)
      write(name, *) "ESMF_StateValidate"
      write(failMsg, *) "rc =",rc,", validate_options =", trim(validate_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_StatePrint(state, print_options, rc)
      write(name, *) "ESMF_StatePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_State.   only valid for deep classes; remove for shallow
      call ESMF_StateDestruct(state, rc)
      write(name, *) "ESMF_StateDestruct"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_State
      !   also tests destructor
      call ESMF_StateDestroy(state, rc)
      write(name, *) "ESMF_StateDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! perform exhaustive tests here;
      print *, "******************STATE EXHAUSTIVE UNIT TESTS****************************"
      print *

      !------------------------------------------------------------------------

      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(statename, ESMF_STATEIMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test Creation of an empty export State 
      statename = "Atmosphere Out"
      state1 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding Bundle to a State
      bundlename = "Temperature"
      bundle1 = ESMF_BundleCreate(bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_StateAddData(state1, bundle1, rc)
      write(name, *) "Adding a Bundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding Field to a State
      fieldname = "Humidity"
      field1 = ESMF_FieldCreateNoData(fieldname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_StateAddData(state1, field1, rc)
      write(name, *) "Adding a Field to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding an Array to a State
      allocate(f90ptr1(10,20))
      array1 = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_StateAddData(state1,array1, rc)
      write(name, *) "Adding an Array to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test printing of State
      call  ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test getting Bundle from State
      call  ESMF_StateGetData(state1, bundlename, bundle2(1), rc)
      write(failMsg, *) ""
      write(name, *) "Getting Bundle from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_BundleGetName(bundle2(1), bname, rc)
      write(failMsg, *) "Bundle name not 'Temperature'"
      write(name, *) "Verifying that the Bundle has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bname.eq."Temperature"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test getting Field from State
      call  ESMF_StateGetData(state1, fieldname, field2, rc)
      write(failMsg, *) ""
      write(name, *) "Getting Field from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGetName(field2, fname, rc)
      write(failMsg, *) "Field name not 'Humidity'"
      write(name, *) "Verifying that the Field has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname.eq."Humidity"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test Creation of an export State with Bundle
      bundlename = "Humidity"
      compname = "Atmosphere2"
      statename = "Atm Export State"
      x = 1
      bundle2(1) = ESMF_BundleCreate(bundlename, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, compname, &
			                   bundles=bundle2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      ! Test Creation of an export State with a Field
      compname = "Atmosphere2"
      statename = "Atm2 Export State"
      x = 1
      fieldname = "Precipitation"
      field3(3) = ESMF_FieldCreateNoData(fieldname, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, compname, &
			                  fields=field3(3), itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                       name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      ! Test Creation of an export State with an array
      compname = "Atmosphere3"
      statename = "Atm3 Export State"
      x  = 2
      allocate(f90ptr1(10,20))
      array2(1) = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      array2(2) = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      state2 = ESMF_StateCreate(statname, ESMF_STATEEXPORT, compname, &
			                    arrays=array2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                        name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
#else

      ! perform non-exhaustive tests here;
      print *, "******************STATE NON-EXHAUSTIVE UNIT TESTS****************************"
      print *

      !------------------------------------------------------------------------

      ! Test Creation of an empty import State 
      statename = "Atmosphere In"
      state1 = ESMF_StateCreate(statename, ESMF_STATEIMPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty import State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test Creation of an empty export State 
      statename = "Atmosphere Out"
      state1 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding Bundle to a State
      bundlename = "Temperature"
      bundle1 = ESMF_BundleCreate(bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_StateAddData(state1, bundle1, rc)
      write(name, *) "Adding a Bundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      ! Test adding a second Bundle to a State
      bundlename = "Temperature"
      bundle1 = ESMF_BundleCreate(bundlename, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_StateAddData(state1, bundle1, rc)
      write(name, *) "Adding a second Bundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      call  ESMF_StatePrint(state1, rc=rc)

      ! Test adding Field to a State
      fieldname = "Humidity"
      field1 = ESMF_FieldCreateNoData(fieldname, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_StateAddData(state1, field1, rc)
      write(name, *) "Adding a Field to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding an Array to a State
      allocate(f90ptr1(10,20))
      array1 = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_StateAddData(state1,array1, rc)
      write(name, *) "Adding an Array to a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test printing of State
      call  ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test getting Bundle from State
      call  ESMF_StateGetData(state1, bundlename, bundle2(1), rc)
      write(failMsg, *) ""
      write(name, *) "Getting Bundle from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_BundleGetName(bundle2(1), bname, rc)
      write(failMsg, *) "Bundle name not 'Temperature'"
      write(name, *) "Verifying that the Bundle has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(bname.eq."Temperature"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test State for Bundle being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(failMsg, *) ""
      write(name, *) "Query if Bundle is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test State for Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc)
      write(failMsg, *) ""
      write(name, *) "Query if Field is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test State for non-existant Field being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidty", rc)
      write(failMsg, *) ""
      write(name, *) "Query if non existant Field is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test setting Field as not needed in a State
      call ESMF_StateSetNeeded(state1, "Humidity", ESMF_STATEDATANOTNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Field as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      IsNeeded = ESMF_StateIsNeeded(state1, "Humidity", rc)
      write(name, *) "Test if Field is not needed in a State Test"
      call ESMF_Test((.not.IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test State for Array being needed
      IsNeeded = ESMF_StateIsNeeded(state1, "default array name", rc)
      write(failMsg, *) ""
      write(name, *) "Query if Array is needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test setting Bundle as not needed in a State
      call ESMF_StateSetNeeded(state1, "Temperature", ESMF_STATEDATANOTNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Bundle as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(name, *) "Test if Bundle is not needed in a State Test"
      call ESMF_Test((.not.IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test setting Array as not needed in a State
      call ESMF_StateSetNeeded(state1, "default array name", ESMF_STATEDATANOTNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Array as not needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      IsNeeded = ESMF_StateIsNeeded(state1, "default array name", rc)
      write(name, *) "Test if Array is not needed in a State Test"
      call ESMF_Test((.not.IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test getting Field from State
      call  ESMF_StateGetData(state1, fieldname, field2, rc)
      write(failMsg, *) ""
      write(name, *) "Getting Field from a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call ESMF_FieldGetName(field2, fname, rc)
      write(failMsg, *) "Field name not 'Humidity'"
      write(name, *) "Verifying that the Field has correct name Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS).and.(fname.eq."Humidity"), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      call  ESMF_StatePrint(state1, rc=rc)


      ! Test setting Bundle as needed in a State
      call ESMF_StateSetNeeded(state1, "Temperature", ESMF_STATEDATAISNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Bundle as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(name, *) "Test if Bundle is needed in a State Test"
      call ESMF_Test((IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test setting Field as needed in a State
      call ESMF_StateSetNeeded(state1, "Humidity", ESMF_STATEDATAISNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Field as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      IsNeeded = ESMF_StateIsNeeded(state1, "Temperature", rc)
      write(name, *) "Test if Field is needed in a State Test"
      call ESMF_Test((IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test setting Array as needed in a State
      call ESMF_StateSetNeeded(state1, "default array name", ESMF_STATEDATAISNEEDED, rc)
      write(failMsg, *) ""
      write(name, *) "Set Array as needed in a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      IsNeeded = ESMF_StateIsNeeded(state1, "default array name", rc)
      write(name, *) "Test if Array is needed in a State Test"
      call ESMF_Test((IsNeeded), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "IsNeeded = ", IsNeeded
      !------------------------------------------------------------------------

      ! Test adding an uninitialized Bundle to a State
      call ESMF_StateAddData(state1, bundle5, rc)
      write(name, *) "Adding an uninitialized  Bundle to a State Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding an uninitialized Field to a State
      ! This code crashes, it will be commented out until
      ! bug 709032 is fixed.
      call ESMF_StateAddData(state1, field4, rc)
      write(name, *) "Adding an uninitialized  Field to a State Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                        name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding an uninitialized Array to a State
      call ESMF_StateAddData(state1, array3, rc)
      write(name, *) "Adding an uninitialized Array to a State Test"
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                        name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc

      !------------------------------------------------------------------------
      ! Test Creation of an export State with Bundle
      bundlename = "Humidity"
      compname = "Atmosphere2"
      statename = "Export State"
      x = 1
      bundle2(1) = ESMF_BundleCreate(bundlename, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, compname, &
			                   bundles=bundle2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Bundle Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------
      ! Test Creation of an export State with the wrong number of Fields
      compname = "Atmosphere2"
      statename = "Export State"
      x = 1
      fieldname = "Precipitation"
      field3(1) = ESMF_FieldCreateNoData(fieldname, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, compname, &
          			            fields=field3, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a bad Field list"
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------
      ! Test Creation of an export State with a Field
      compname = "Atmosphere2"
      statename = "Export State"
      x = 1
      fieldname = "Precipitation"
      field3(1) = ESMF_FieldCreateNoData(fieldname, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, compname, &
			               fields=field3(1:1), itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Field Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------
      ! Test Creation of an export State with an array
      compname = "Atmosphere3"
      statename = "Export State"
      x  = 1
      allocate(f90ptr1(10,20))
      array2(1) = ESMF_ArrayCreate(f90ptr1, ESMF_NO_COPY, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, compname, &
			               arrays=array2(1:1), itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Array Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                        name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------
      ! Test Creation of an export State with an array
      compname = "Atmosphere3"
      statename = "Export State"
      x  = 2
      allocate(f90ptr1(10,20))
      array2(1) = ESMF_ArrayCreate(f90ptr1, ESMF_DO_COPY, rc=rc)
      array2(2) = ESMF_ArrayCreate(f90ptr1, ESMF_DO_COPY, rc=rc)
      state2 = ESMF_StateCreate(statename, ESMF_STATEEXPORT, compname, &
			                    arrays=array2, itemcount=x, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Creating an export State with a Array list Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                        name, failMsg, result, ESMF_SRCLINE)
      call  ESMF_StatePrint(state2, rc=rc)
      !------------------------------------------------------------------------

      call  ESMF_StatePrint(state1, rc=rc)

      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test Destruction of a destroyed State
      ! This code crashes, it will be commented out until
      ! bug 70054 is fixed.
      write(failMsg, *) ""
      write(name, *) "Destruction of a destroyed State Test"
      call  ESMF_StateDestroy(state1, rc)
      call ESMF_Test((rc.eq.ESMF_FAILURE), &
                      name, failMsg, result, ESMF_SRCLINE)
      print *, "rc = ", rc

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_StateUTest
