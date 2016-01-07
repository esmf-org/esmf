! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

    program FeatureTest
    
#include "ESMF.h"

    use ESMF
    use ESMF_TestMod
    implicit none

    integer :: rc, result
    character(len=ESMF_MAXSTR) :: failMsg, name

    type(ESMF_Logical) :: tf_c
    logical :: tf

    interface
      subroutine ESMC_Present_test (arg, present_val)
        use ESMF, only: ESMF_Logical
        implicit none
        integer, intent(in), optional :: arg
        type(ESMF_Logical), intent(out) :: present_val
      end subroutine
    end interface

!------------------------------------------------------------------------
! Tests for various Fortran features and implementation issues that ESMF
! depends on.

    result = 0

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!------------------------------------------------------------------------
! Test to ensure that NULL is used for an OPTIONAL argument when the
! argument is not present.  NULL conforms to the value specified in
! Section 8.7 of Fortran TS 29113 (WG5/N1942 - 9/19/2012.)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_UTest
    name = "Fortran argument present in C test"
    failMsg = "Argument was not indicated as present when present"
    call ESMC_Present_test (arg=42, present_val=tf_c)
    tf = tf_c == ESMF_TRUE
    call ESMF_Test(tf, name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! NEX_UTest
    name = "Fortran argument not present in C test"
    failMsg = "Argument was indicated as present when not present"
    call ESMC_Present_test (present_val=tf_c)
    tf = tf_c == ESMF_TRUE
    call ESMF_Test(.not. tf, name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

    ! no exhaustive tests (yet)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------


#endif

    call ESMF_TestEnd(ESMF_SRCLINE)

    end program FeatureTest
    
