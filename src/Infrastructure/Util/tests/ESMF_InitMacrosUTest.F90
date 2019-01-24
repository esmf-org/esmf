! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_InitMacrosUTest.F90"


      program ESMF_InitMacrosTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InitMacrosUTest"
!==============================================================================
!BOP
! !PROGRAM: ESMF_InitMacrosTest - tests the init macros
!
! !DESCRIPTION:
!
! The code in this file drives F90 Util unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF         ! the ESMF Framework
      use ESMF_InitMacrosTestTypesMod
      use ESMF_InitMacrosMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id$'
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
      !character(ESMF_MAXSTR) :: validate_options
      !character(ESMF_MAXSTR) :: print_options
      !type(ESMF_UtilConfig) :: config_set
      !type(ESMF_UtilConfig) :: config_get
      !character(ESMF_MAXSTR) :: name_set, name_get

      type(ESMF_Shallow)  :: sObj
      type(ESMF_Deep)     :: dObj


!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
      if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      ! add tests here

#ifdef ESMF_INITMACROS_ON
      !NEX_UTest
      ! Shallow type test
      write(name,*) "Testing ESMF_INIT_CHECK_SET_SHALLOW"
      write(failMsg,*) "Did not initialize value"
      ESMF_INIT_CHECK_SET_SHALLOW(ESMF_ShallowGetInitVal,ESMF_ShallowInit,sObj)
      call ESMF_Test((sObj%num .eq. 4), name, failMsg, result, ESMF_SRCLINE)


      !NEX_UTest
      ! Deep type tests     
      !! Test Undefined Check
      write(name,*) "Testing undefined object in ESMF_INIT_CHECK_DEEP"
      write(failMsg,*) "Did not recognize undefined state"
      call DeepTest(d=dObj,rc=rc)
      call ESMF_Test((rc .eq. ESMF_RC_OBJ_NOT_CREATED),name,failMsg, &
                 result,ESMF_SRCLINE)

      !NEX_UTest
      !! Test Created Check
      write(name,*) "Testing created object in ESMF_INIT_CHECK_DEEP"
      write(failMsg,*) "Did not recognize created state"
      dObj=ESMF_DeepCreate(val=7)
      call DeepTest(d=dObj,rc=rc)
      call ESMF_Test((rc .eq. ESMF_SUCCESS),name,failMsg, &
                     result,ESMF_SRCLINE)         

      !NEX_UTest
      !! Test Deleted Check
      write(name,*) "Testing deleted object in ESMF_INIT_CHECK_DEEP"
      write(failMsg,*) "Did not recognize deleted state"
      call ESMF_DeepDestroy(d=dObj)
      call DeepTest(d=dObj,rc=rc)
      call ESMF_Test((rc .eq. ESMF_RC_OBJ_DELETED),name,failMsg, &
                result,ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
      ! Currently no exhaustive tests
#endif

#endif

      ! This calls finalize before returning, so it must be the last
      ! ESMF-related thing the test does.
      call ESMF_TestEnd(ESMF_SRCLINE)

      contains 
         subroutine DeepTest(d,rc)
           type(ESMF_Deep), intent(in) :: d
           integer, intent(inout) :: rc
            
            ! init return code
            rc=ESMF_RC_NOT_IMPL

            ! check status
            ESMF_INIT_CHECK_DEEP(ESMF_DeepGetInitVal,d,rc)
            
            ! if we pass status check then return success
            rc=ESMF_SUCCESS
   
         end subroutine DeepTest
 
      end program ESMF_InitMacrosTest
