! $Id: ESMF_FieldDataMapUTest.F90,v 1.1 2004/05/04 13:53:27 nscollins Exp $
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
      program ESMF_FieldDataMapUTest

-----------------------------------------------------------------------
! Include file, for ESMF_SRCLINE which must be done by the preprocessor

#include <ESMF_Macros.inc>

-----------------------------------------------------------------------
!BOP
! !PROGRAM: ESMF_FieldDataMapUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 FieldDataMap unit tests.
! The companion file ESMF\_FieldDataMap.F90 contains the definitions for the
! FieldDataMap methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! the ESMF framework, including the class to test
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FieldDataMapUTest.F90,v 1.1 2004/05/04 13:53:27 nscollins Exp $'
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
      type(ESMF_FieldDataMapConfig) :: config_set
      type(ESMF_FieldDataMapConfig) :: config_get
      ! when get/set value routines enabled, comment these in and set
      ! the appropriate values, and remove the temporary integers.
      !<value type> :: value_set, value_get
      integer :: value_set, value_get

      ! instantiate a FieldDataMap 
      type(ESMF_FieldDataMap) :: fielddatamap

#ifdef ESMF_EXHAUSTIVE

      ! perform exhaustive tests here;
      !   see #else below for non-exhaustive tests
      ! future release will use run-time switching mechanism

      ! for deep classes, keep create/construct routine and remove init
      ! for shallow classes, keep init and remove create/construct
     
      ! test dynamic allocation of ESMF_FieldDataMap
      fielddatamap = ESMF_FieldDataMapCreate(args, rc)
      write(name, *) "ESMF_FieldDataMapCreate"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_FieldDataMap
      call ESMF_FieldDataMapConstruct(fielddatamap, args, rc)
      write(name, *) "ESMF_FieldDataMapConstruct"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test initialization of members of statically allocated ESMF_FieldDataMap
      !   may want to read back values via Get methods for comparison
      call ESMF_FieldDataMapInit(fielddatamap, args, rc)
      write(name, *) "ESMF_FieldDataMapInit"
      write(failMsg, *) "rc =", rc, ", args =", args
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of configuration values
      call ESMF_FieldDataMapSetConfig(fielddatamap, config_set, rc)
      write(name, *) "ESMF_FieldDataMapSetConfig"
      write(failMsg, *) "rc =", rc, ", config_set =", config_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS),  &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of configuration values,
      !  compare to values set previously
      call ESMF_FieldDataMapGetConfig(fielddatamap, config_get, rc)
      write(name, *) "ESMF_FieldDataMapGetConfig"
      write(failMsg, *) "rc =", rc, ", config_get =", config_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. config_get .eq. config_set), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test setting of ESMF_FieldDataMap members values
      !call ESMF_FieldDataMapSet<Value>(fielddatamap, value_set, rc)
      rc = ESMF_FAILURE  ! remove this when this test enabled
      write(name, *) "ESMF_FieldDataMapSet<Value>"
      write(failMsg, *) "rc =", rc, ", value_set =", value_set
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test getting of ESMF_FieldDataMap members values,
      !   compare to values set previously
      !call ESMF_FieldDataMapGet<Value>(fielddatamap, value_get, rc)
      rc = ESMF_FAILURE  ! remove this when this test enabled
      write(name, *) "ESMF_FieldDataMapGet<Value>"
      write(failMsg, *) "rc =", rc, ", value_get =", value_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. value_get .eq. value_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      call ESMF_FieldDataMapValidate(fielddatamap, validate_options, rc)
      write(name, *) "ESMF_FieldDataMapValidate"
      write(failMsg, *) "rc =",rc,", validate_options =", trim(validate_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_FieldDataMapPrint(fielddatamap, print_options, rc)
      write(name, *) "ESMF_FieldDataMapPrint"
      write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_FieldDataMap.   only valid for deep classes; remove for shallow
      call ESMF_FieldDataMapDestruct(fielddatamap, rc)
      write(name, *) "ESMF_FieldDataMapDestruct"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_FieldDataMap
      !   also tests destructor
      call ESMF_FieldDataMapDestroy(fielddatamap, rc)
      write(name, *) "ESMF_FieldDataMapDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#else

      ! perform non-exhaustive tests here;
      !   use same templates as above

#endif

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
  
      end program ESMF_FieldDataMapUTest
