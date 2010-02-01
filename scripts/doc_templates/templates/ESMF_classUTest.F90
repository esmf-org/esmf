! $Id: ESMF_classUTest.F90,v 1.7.2.3 2010/02/01 20:48:49 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_<Class>UTest

-----------------------------------------------------------------------
! Include file, for ESMF_SRCLINE which must be done by the preprocessor

#include <ESMF_Macros.inc>

-----------------------------------------------------------------------
!BOP
! !PROGRAM: ESMF_<Class>UTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 <Class> unit tests.
! The companion file ESMF\_<Class>.F90 contains the definitions for the
! <Class> methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF framework, including the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_classUTest.F90,v 1.7.2.3 2010/02/01 20:48:49 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure messages
      character(ESMF_MAXSTR*2) :: failMsg

      ! local variables needed to pass into function/subroutine calls
      character(ESMF_MAXSTR) :: validate_options
      character(ESMF_MAXSTR) :: print_options
      type(ESMF_<Class>Config) :: config_set
      type(ESMF_<Class>Config) :: config_get
      ! when get/set value routines enabled, comment these in and set
      ! the appropriate values, and remove the temporary integers.
      !<value type> :: value_set, value_get
      integer :: value_set, value_get

      ! instantiate a <Class> 
      type(ESMF_<Class>) :: <class>

#ifdef ESMF_EXHAUSTIVE

      ! perform exhaustive tests here;
      !   see #else below for non-exhaustive tests
      ! future release will use run-time switching mechanism

      ! for deep classes, keep create/construct routine and remove init
      ! for shallow classes, keep init and remove create/construct
     
      ! test dynamic allocation of ESMF_<Class>
      <class> = ESMF_<Class>Create(args, rc)
      write(name, *) "ESMF_<Class>Create"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_<Class>
      call ESMF_<Class>Construct(<class>, args, rc)
      write(name, *) "ESMF_<Class>Construct"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test initialization of members of statically allocated ESMF_<Class>
      !   may want to read back values via Get methods for comparison
      call ESMF_<Class>Init(<class>, args, rc)
      write(name, *) "ESMF_<Class>Init"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      call ESMF_<Class>SetConfig(<class>, config_set, rc)
      write(name, *) "ESMF_<Class>SetConfig"
      write(failMsg, *) "rc =", rc, ", config_set =", config_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      call ESMF_<Class>GetConfig(<class>, config_get, rc)
      write(name, *) "ESMF_<Class>GetConfig"
      write(failMsg, *) "rc =", rc, ", config_get =", config_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_<Class> members values
      !call ESMF_<Class>Set<Value>(<class>, value_set, rc)
      rc = ESMF_FAILURE  ! remove this when this test enabled
      write(name, *) "ESMF_<Class>Set<Value>"
      write(failMsg, *) "rc =", rc, ", value_set =", value_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_<Class> members values,
      !   compare to values set previously
      !call ESMF_<Class>Get<Value>(<class>, value_get, rc)
      rc = ESMF_FAILURE  ! remove this when this test enabled
      write(name, *) "ESMF_<Class>Get<Value>"
      write(failMsg, *) "rc =", rc, ", value_get =", value_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      call ESMF_<Class>Validate(<class>, validate_options, rc)
      write(name, *) "ESMF_<Class>Validate"
      write(failMsg, *) "rc =",rc,", validate_options =", trim(validate_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_<Class>Print(<class>, print_options, rc)
      write(name, *) "ESMF_<Class>Print"
      write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_<Class>.   only valid for deep classes; remove for shallow
      call ESMF_<Class>Destruct(<class>, rc)
      write(name, *) "ESMF_<Class>Destruct"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_<Class>
      !   also tests destructor
      call ESMF_<Class>Destroy(<class>, rc)
      write(name, *) "ESMF_<Class>Destroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#else

      ! perform non-exhaustive tests here;
      !   use same templates as above

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_<Class>UTest
