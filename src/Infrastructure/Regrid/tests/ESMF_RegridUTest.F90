! $Id: ESMF_RegridUTest.F90,v 1.4 2004/10/05 16:18:31 svasquez Exp $
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
      program ESMF_RegridTest

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RegridTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Regrid unit tests.
! The companion file ESMF\_Regrid.F90 contains the definitions for the
! Regrid methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      use ESMF_TestMod    ! test methods
      use ESMF_RegridMod  ! the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RegridUTest.F90,v 1.4 2004/10/05 16:18:31 svasquez Exp $'
!------------------------------------------------------------------------------
      type(ESMF_VM):: vm
      integer :: npets

      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      print '(/, a, i3)' , "NUMBER_OF_PROCESSORS", npets


      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate a Regrid 
      type(ESMF_Regrid) :: regrid
      type(ESMF_RegridConfig) config_set
      type(ESMF_RegridConfig) :: config_get
      character(ESMF_MAXSTR) :: validate_options
      character(ESMF_MAXSTR) :: print_options

      ! test dynamic allocation of ESMF_Regrid
      regrid = ESMF_RegridCreate(args, rc)
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_Regrid
      call ESMF_RegridConstruct(regrid, args, rc)
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      call ESMF_RegridSetConfig(regrid, config_set, rc)
      write(failMsg, *) "rc =", rc, ", config_set =", config_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      call ESMF_RegridGetConfig(regrid, config_get, rc)
      write(failMsg, *) "rc =", rc, ", config_get =", config_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_Regrid members values
      !<value type> :: value_set
      !call ESMF_RegridSet<Value>(regrid, value_set, rc)
      !write(failMsg, *) "rc =", rc, ", value_set =", value_set
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), &
      !                failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_Regrid members values,
      !   compare to values set previously
      !<value type> :: value_get
      !call ESMF_RegridGet<Value>(regrid, value_get, rc)
      !write(failMsg, *) "rc =", rc, ", value_get =", value_get
      !call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
      !                failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      call ESMF_RegridValidate(regrid, validate_options, rc)
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_RegridPrint(regrid, print_options, rc)
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_Regrid
      call ESMF_RegridDestruct(regrid, rc)
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_Regrid
      !   also tests destructor
      call ESMF_RegridDestroy(regrid, rc)
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_RegridTest
