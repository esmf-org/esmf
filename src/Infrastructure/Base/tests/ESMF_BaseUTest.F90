! $Id: ESMF_BaseUTest.F90,v 1.1 2003/03/26 18:02:03 nscollins Exp $
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
      program ESMF_BaseUTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_BaseUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Base unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_BaseMod  ! the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_BaseUTest.F90,v 1.1 2003/03/26 18:02:03 nscollins Exp $'
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
      !type(ESMF_BaseConfig) :: config_set
      !type(ESMF_BaseConfig) :: config_get
      character(ESMF_MAXSTR) :: name_set, name_get

      ! instantiate a Base 
      type(ESMF_Base) :: base

#ifdef ESMF_EXHAUSTIVE

      ! perform exhaustive tests here;
      !   use same templates as below

      ! future release will use run-time switching mechanism
#else

      ! perform non-exhaustive tests here

      ! test setting of configuration values
      !call ESMF_BaseSetConfig(base, config_set, rc)
      !write(name, *) "ESMF_BaseSetConfig"
      !write(failMsg, *) "rc =", rc, ", config_set =", config_set
      !call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      !call ESMF_BaseGetConfig(base, config_get, rc)
      !write(name, *) "ESMF_BaseGetConfig"
      !write(failMsg, *) "rc =", rc, ", config_get =", config_get
      !call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_Base members values
      name_set = "fred"
      call ESMF_SetName(base, name_set, "Base", rc)
      write(name, *) "ESMF_SetName"
      write(failMsg, *) "rc =", rc, ", name =", trim(name_set)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_Base members values,
      !   compare to values set previously
      call ESMF_GetName(base, name_get, rc)
      write(name, *) "ESMF_GetName"
      write(failMsg, *) "rc =", rc, ", name =", name_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. name_get .eq. name_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      !call ESMF_Validate(base, validate_options, rc)
      !write(name, *) "ESMF_Validate"
      !write(failMsg, *) "rc =",rc,", validate_options =", trim(validate_options)
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
      !                name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      !call ESMF_Print(base, print_options, rc)
      !write(name, *) "ESMF_Print"
      !write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
      !                name, failMsg, result, ESMF_SRCLINE)

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_BaseUTest
