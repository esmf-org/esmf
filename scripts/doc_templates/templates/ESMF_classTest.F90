! $Id: ESMF_classTest.F90,v 1.1 2003/02/28 01:05:40 eschwab Exp $
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
      program ESMF_<Class>Test

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_<Comp>.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_<Class>Test - One line general statement about this test
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
      use ESMF_<Class>Mod  ! the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_classTest.F90,v 1.1 2003/02/28 01:05:40 eschwab Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate a <Class> 
      type(ESMF_<Class>) :: <class>

      ! test dynamic allocation of ESMF_<Class>
      <class> = ESMF_<Class>Create(args, rc)
      write(failMsg, *) "rc =", rc, ", <class> =", <class>, ", args =", args
      call ESMF_Test((<class>.ne.0 .and. rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)
    
      ! test dynamic deallocation of ESMF_<Class>
      !   also tests destructor
      rc = ESMF_<Class>Destroy(<class>)
      write(failMsg, *) "rc =", rc, ", <class> =", <class>
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic allocation within statically allocated
      !   ESMF_<Class>
      rc = <class>.ESMF_<Class>Construct(args)
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_<Class>
      rc = <class>.ESMF_<Class>Destruct()
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test initialization of members of statically allocated ESMF_<Class>
      !   may want to read back values via Get methods for comparison
      rc = <class>.ESMF_<Class>Init(args)
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      type(ESMF_<Class>Config) config_set
      rc = <class>.ESMF_<Class>SetConfig(config_set)
      write(failMsg, *) "rc =", rc, ", config_set =", config_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      type(ESMF_<Class>Config) :: config_get
      rc = <class>.ESMF_<Class>GetConfig(config_get)
      write(failMsg, *) "rc =", rc, ", config_get =", config_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_<Class> members values
      <value type> :: value_set
      rc = <class>.ESMF_<Class>Set<Value>(value_set)
      write(failMsg, *) "rc =", rc, ", value_set =", value_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_<Class> members values,
      !   compare to values set previously
      <value type> :: value_get
      rc = <class>.ESMF_<Class>Get<Value>(value_get)
      write(failMsg, *) "rc =", rc, ", value_get =", value_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      character(ESMF_MAXSTR) :: validate_options
      rc = <class>.ESMF_<Class>Validate(validate_options)
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      character(ESMF_MAXSTR) :: print_options
      rc = <class>.ESMF_<Class>Print(print_options)
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_<Class>Test
