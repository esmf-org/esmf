! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_ArraySpecUTest

!------------------------------------------------------------------------------
 
#include "ESMF.h"

!==============================================================================
!BOP
! !PROGRAM: ESMF_ArraySpecUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 ArraySpec unit tests.
! The companion file ESMF\_ArraySpec.F90 contains the definitions for the
! ArraySpec methods.
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF

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

  ! individual test failure message
  character(ESMF_MAXSTR) :: failMsg
  character(ESMF_MAXSTR) :: name
  
  type(ESMF_ArraySpec)   :: arrayspec, arrayspec2
  integer                :: rank
  type(ESMF_TypeKind_Flag)    :: typekind

!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  !------------------------------------------------------------------------
  !NEX_UTest 
  write(name, *) "Test ESMF_ArraySpecPrint() before setting arrayspec"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArraySpecPrint(arrayspec, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest 
  write(name, *) "Test ESMF_ArraySpecGet() before setting arrayspec"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArraySpecGet(arrayspec, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest 
  write(name, *) "Test ESMF_ArraySpecSet()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest 
  write(name, *) "Test ESMF_ArraySpecPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecPrint(arrayspec, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !NEX_UTest 
  write(name, *) "Test ESMF_ArraySpecGet()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecGet(arrayspec, rank=rank, typekind=typekind, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest 
  write(name, *) "Validate ESMF_ArraySpecGet() results"
  write(failMsg, *) "Returned rank and/or typekind incorrect"
  call ESMF_Test(((rank==2).and.(typekind==ESMF_TYPEKIND_R8)), name, failMsg, &
    result, ESMF_SRCLINE)
    
  !------------------------------------------------------------------------
  !NEX_UTest 
  ! Test ESMF_ArraySpecAssignment(=)(arraySpec, arraySpec)
  ! Test ESMF_ArraySpecOperator(==)(arraySpec1, arraySpec2)
  write(name, *) "Test ESMF_ArraySpec assignment and (==) operator"
  write(failMsg, *) "Incorrect behavior"
  arrayspec2 = arrayspec
  call ESMF_Test(arrayspec2==arrayspec, name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !NEX_UTest 
  ! Test ESMF_ArraySpecOperator(/=)(arraySpec1, arraySpec2)
  write(name, *) "Test ESMF_ArraySpec (/=) operator"
  write(failMsg, *) "Incorrect behavior"
  call ESMF_Test(.not.(arrayspec2/=arrayspec), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
  
  !------------------------------------------------------------------------
  !EX_UTest 
  write(name, *) "Test re-setting ArraySpec with ESMF_ArraySpecSet()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_I4, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest 
  write(name, *) "Test re-set ArraySpec with ESMF_ArraySpecPrint()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecPrint(arrayspec, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest 
  write(name, *) "Test re-set ArraySpec with ESMF_ArraySpecGet()"
  write(failMsg, *) "Did not return ESMF_SUCCESS"
  call ESMF_ArraySpecGet(arrayspec, rank=rank, typekind=typekind, rc=rc)
  call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !EX_UTest 
  write(name, *) "Validate re-set ArraySpec with ESMF_ArraySpecGet() results"
  write(failMsg, *) "Returned rank and/or typekind incorrect"
  call ESMF_Test(((rank==3).and.(typekind==ESMF_TYPEKIND_I4)), name, failMsg, &
    result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest 
  ! Test ESMF_ArraySpecOperator(==)(arraySpec1, arraySpec2)
  write(name, *) "Test ESMF_ArraySpec (==) operator"
  write(failMsg, *) "Incorrect behavior"
  call ESMF_Test(.not.(arrayspec2==arrayspec), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !EX_UTest 
  ! Test ESMF_ArraySpecOperator(/=)(arraySpec1, arraySpec2)
  write(name, *) "Test ESMF_ArraySpec (/=) operator"
  write(failMsg, *) "Incorrect behavior"
  call ESMF_Test(arrayspec2/=arrayspec, name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest 
  write(name, *) "Test re-setting ArraySpec invalid rank with ESMF_ArraySpecSet()"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArraySpecSet(arrayspec, 10, ESMF_TYPEKIND_R8, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest 
  write(name, *) "Test re-set invalid ArraySpec with ESMF_ArraySpecPrint()"
  write(failMsg, *) "Did return ESMF_SUCCESS"
  call ESMF_ArraySpecPrint(arrayspec, rc=rc)
  call ESMF_Test((rc.ne.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

  !------------------------------------------------------------------------
  !EX_UTest 
  ! Test ESMF_ArraySpecOperator(==)(arraySpec1, arraySpec2)
  write(name, *) "Test ESMF_ArraySpec (==) operator"
  write(failMsg, *) "Incorrect behavior"
  call ESMF_Test(.not.(arrayspec2==arrayspec), name, failMsg, result, ESMF_SRCLINE)
  
  !------------------------------------------------------------------------
  !EX_UTest 
  ! Test ESMF_ArraySpecOperator(/=)(arraySpec1, arraySpec2)
  write(name, *) "Test ESMF_ArraySpec (/=) operator"
  write(failMsg, *) "Incorrect behavior"
  call ESMF_Test(arrayspec2/=arrayspec, name, failMsg, result, ESMF_SRCLINE)

#endif


  call ESMF_TestEnd(ESMF_SRCLINE)

end program ESMF_ArraySpecUTest
