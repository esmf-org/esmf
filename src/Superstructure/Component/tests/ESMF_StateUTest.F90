! $Id: ESMF_StateUTest.F90,v 1.3 2003/03/21 21:52:50 svasquez Exp $
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

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
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
      use ESMF_StateMod  ! the class to test
      use ESMF_IOMod
      use ESMF_ArrayMod
      use ESMF_FieldMod
      use ESMF_BundleMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_StateUTest.F90,v 1.3 2003/03/21 21:52:50 svasquez Exp $'
!------------------------------------------------------------------------------

      ! Some common definitions.  This requires the C preprocessor.
      #include "ESMF.h"


!     ! Local variables
      integer :: x, y, rc
      character(ESMF_MAXSTR) :: compname, statename, bundlename, dataname
      type(ESMF_Field) :: field1
      type(ESMF_Bundle) :: bundle1, bundle2(1)
      type(ESMF_State) :: state1, state2, state3, state4

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
      state = ESMF_StateCreate(args, rc)
      write(name, *) "ESMF_StateCreate"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_State
      call ESMF_StateConstruct(state, args, rc)
      write(name, *) "ESMF_StateConstruct"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test initialization of members of statically allocated ESMF_State
      !   may want to read back values via Get methods for comparison
      call ESMF_StateInit(state, args, rc)
      write(name, *) "ESMF_StateInit"
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

print *, "StateUnitTest EXHAUSTIVE"
#else

      ! perform non-exhaustive tests here;
      print *, "******************STATE NON-EXHAUSTIVE UNIT TESTS****************************"
      print *

      !------------------------------------------------------------------------

      ! Test Creation of an empty import State 
      compname = "Atmosphere"
      state1 = ESMF_StateCreate(compname, ESMF_STATEIMPORT, rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty input State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test Creation of an empty export State 
      state1 = ESMF_StateCreate(compname, ESMF_STATEEXPORT, rc)
      write(failMsg, *) ""
      write(name, *) "Creating an empty export State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test adding Bundle to State
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

      ! Test printing of State
      call  ESMF_StatePrint(state1, rc=rc)
      write(failMsg, *) ""
      write(name, *) "Printing of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      ! Test Creation of an export State with Bundle
      ! This code crashes, it will be commented out until
      ! bug 707751 is fixed.
      ! bundlename = "Humidity"
      ! compname = "Atmosphere2"
      ! statename = " Export State"
      ! x = 1
      ! bundle2(1) = ESMF_BundleCreate(bundlename, rc=rc)
      ! state2 = ESMF_StateCreate(compname, ESMF_STATEEXPORT, x, &
				!bundles=bundle2, statename=statename, rc=rc)
      ! write(failMsg, *) ""
      ! write(name, *) "Creating an export State with a Bundle Test"
      ! call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      !name, failMsg, result, ESMF_SRCLINE)
      ! call  ESMF_StatePrint(state2, rc=rc)

      !------------------------------------------------------------------------
      ! Test Destruction of State
      call  ESMF_StateDestroy(state1, rc)
      write(failMsg, *) ""
      write(name, *) "Destruction of a State Test"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_StateUTest
