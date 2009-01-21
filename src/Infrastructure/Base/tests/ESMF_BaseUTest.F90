! $Id: ESMF_BaseUTest.F90,v 1.24.2.5 2009/01/21 21:25:20 cdeluca Exp $
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
      program ESMF_BaseUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
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
      use ESMF_Mod         ! the ESMF Framework
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_BaseUTest.F90,v 1.24.2.5 2009/01/21 21:25:20 cdeluca Exp $'
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
      type(ESMF_DataValue) :: data_value
      character(ESMF_MAXSTR) :: name_set, name_get

      ! instantiate a Base 
      type(ESMF_Base) :: base, base1

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)


      !NEX_UTest
      ! test creation of base objects
      call ESMF_BaseCreate(base, "Base", "test object", 0, rc)
      write(name, *) "ESMF_BaseCreate"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

                      
      !NEX_UTest
      ! destroy base object
      call ESMF_BaseDestroy(base, rc)
      write(name, *) "ESMF_Destroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
      

#ifdef ESMF_TESTEXHAUSTIVE

                      
      !EX_UTest
      ! destroy base object
      call ESMF_BaseDestroy(base, rc)
      write(name, *) "Destroy a destroyed Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)
      
                      
      !EX_UTest
      ! destroy base object
      call ESMF_BaseDestroy(base1, rc)
      write(name, *) "Destroy a non-created Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test print method of deleted base via option string
      print_options = "brief"
      call ESMF_BasePrint(base, print_options, rc)
      write(name, *) "ESMF_BasePrint of deleted Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test print method of non-created base via option string
      print_options = "brief"
      call ESMF_BasePrint(base1, print_options, rc)
      write(name, *) "ESMF_BasePrint of non-created Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test setting of ESMF Base attribute values of deleted Base
      call ESMF_BaseAttSet(base, name, data_value, rc)
      write(name, *) "ESMF_BaseAttSet of deleted Base"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), &
                      name, failMsg, result, ESMF_SRCLINE)



      !EX_UTest
      ! test setting of ESMF_Base members values of uncreated Base
      ! Note That this will recreate the base
      name_set = "fred"
      call ESMF_SetName(base1, name_set, "Base", rc)
      write(name, *) "ESMF_SetName of non-created Base"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test setting of ESMF_Base members values of deleted Base
      ! Note That this will recreate the base
      name_set = "fred"
      call ESMF_SetName(base, name_set, "Base", rc)
      write(name, *) "ESMF_SetName of deleted Base"
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      ! destroy base objects created by ESMF_SetName
      call ESMF_BaseDestroy(base, rc)
      call ESMF_BaseDestroy(base1, rc)


      
      !EX_UTest
      ! test creation of base objects
      call ESMF_BaseCreate(base, "Base", "test object", 0, rc)
      write(name, *) "ESMF_BaseCreate"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)


      !EX_UTest
      ! test setting of ESMF_Base members values
      name_set = "fred"
      call ESMF_SetName(base, name_set, "Base", rc)
      write(name, *) "ESMF_SetName"
      write(failMsg, *) "rc =", rc, ", name =", trim(name_set)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test getting of ESMF_Base members values,
      !   compare to values set previously
      call ESMF_GetName(base, name_get, rc)
      write(name, *) "ESMF_GetName"
      write(failMsg, *) "rc =", rc, ", name =", name_get
      call ESMF_Test((rc.eq.ESMF_SUCCESS .and. name_get .eq. name_set), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test validate method via option string
      ! commented out because it crashes
      ! Bug report 969866 opened
      !EX_UTest
      call ESMF_BaseValidate(base, validate_options, rc)
      write(name, *) "ESMF_BaseValidate"
      write(failMsg, *) "rc =",rc,", validate_options =", trim(validate_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      !EX_UTest
      ! test print method via option string
      print_options = "brief"
      call ESMF_BasePrint(base, print_options, rc)
      write(name, *) "ESMF_BasePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?

      !EX_UTest
      ! destroy base object
      call ESMF_BaseDestroy(base, rc)
      write(name, *) "ESMF_Destroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

#endif

      ! This calls finalize before returning, so it must be the last
      ! ESMF-related thing the test does.
      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      end program ESMF_BaseUTest
