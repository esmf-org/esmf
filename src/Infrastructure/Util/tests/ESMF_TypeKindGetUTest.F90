! $Id: ESMF_TypeKindGetUTest.F90,v 1.7.2.4 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!

    program ESMF_TypeKindGetUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_TypeKindGetUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 TypeKindGet unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod         ! test methods
      use ESMF_Mod             ! the ESMF Framework

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_TypeKindGetUTest.F90,v 1.7.2.4 2009/01/21 21:25:24 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

    ! local variables
    integer :: vint
#ifndef ESMF_NO_INTEGER_1_BYTE
    integer(ESMF_KIND_I1) :: vint1(5)
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
    integer(ESMF_KIND_I2) :: vint2
#endif
    integer(ESMF_KIND_I4) :: vint4
    integer(ESMF_KIND_I8) :: vint8

    real                :: vreal
    real (ESMF_KIND_R4) :: vreal4
    real (ESMF_KIND_R8) :: vreal8
    type(ESMF_TypeKind) :: typekind

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

!Test Integer Data TypeKind Values
!=================================
    !NEX_UTest
    ! Default integer
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) " Getting ESMF_TypeKind parameter of default integer"
    typekind=ESMF_TypeKindGet(vint, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "The typekind of default integer is", typekind

    ! ESMF_KIND_I1 integer
    !------------------------------------------------------------------------
    !NEX_UTest
#ifndef ESMF_NO_INTEGER_1_BYTE
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) " Getting ESMF_TypeKind parameter of ESMF_KIND_I1 integer"
    typekind=ESMF_TypeKindGet(vint1(1), rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(name, *) "Dummy test to keep number of tests correct"
    call ESMF_Test((ESMF_SUCCESS.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    !------------------------------------------------------------------------
    !NEX_UTest
#ifndef ESMF_NO_INTEGER_1_BYTE
     write(failMsg, *) "Returned wrong ESMF_TypeKind"
     write(name, *) "Checking ESMF_TypeKind parameter for ESMF_KIND_I1 integer"
    call ESMF_Test((typekind%dkind .eq. ESMF_TYPEKIND_I1%dkind), &
                    name, failMsg, result, ESMF_SRCLINE)
#else
    write(name, *) "Dummy test to keep number of tests correct"
    call ESMF_Test((ESMF_SUCCESS.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

    ! ESMF_KIND_I2 integer
    !------------------------------------------------------------------------
    !NEX_UTest
#ifndef ESMF_NO_INTEGER_2_BYTE
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) " Getting ESMF_TypeKind parameter of ESMF_KIND_I2 integer"
    typekind=ESMF_TypeKindGet(vint2, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(name, *) "Dummy test to keep number of tests correct"
    call ESMF_Test((ESMF_SUCCESS.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    !------------------------------------------------------------------------
    !NEX_UTest
#ifndef ESMF_NO_INTEGER_2_BYTE
     write(failMsg, *) "Returned wrong ESMF_TypeKind"
     write(name, *) "Checking ESMF_TypeKind parameter for ESMF_KIND_I2 integer"
    call ESMF_Test((typekind%dkind .eq. ESMF_TYPEKIND_I2%dkind), &
                    name, failMsg, result, ESMF_SRCLINE)
#else
    write(name, *) "Dummy test to keep number of tests correct"
    call ESMF_Test((ESMF_SUCCESS.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

    ! ESMF_KIND_I4 integer
    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) " Getting ESMF_TypeKind parameter of ESMF_KIND_I4 integer"
    typekind=ESMF_TypeKindGet(vint4, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------
    !NEX_UTest
     write(failMsg, *) "Returned wrong ESMF_TypeKind"
     write(name, *) "Checking ESMF_TypeKind parameter for ESMF_KIND_I4 integer"
    call ESMF_Test((typekind%dkind .eq. ESMF_TYPEKIND_I4%dkind), &
                    name, failMsg, result, ESMF_SRCLINE)

    ! ESMF_KIND_I8 integer
    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting ESMF_TypeKind parameter of ESMF_KIND_I8 integer"
    typekind=ESMF_TypeKindGet(vint8, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------
    !NEX_UTest
     write(failMsg, *) "Returned wrong ESMF_TypeKind"
     write(name, *) "Checking ESMF_TypeKind parameter for ESMF_KIND_I8 integer"
    call ESMF_Test((typekind%dkind .eq. ESMF_TYPEKIND_I8%dkind), &
                    name, failMsg, result, ESMF_SRCLINE)


!Test Real Data TypeKind Values
!=================================

    !------------------------------------------------------------------------
    !NEX_UTest
    !  Default real
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting ESMF_TypeKind parameter of  real"
    typekind=ESMF_TypeKindGet(vreal, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    print *, "The typekind of real is", typekind

    ! ESMF_KIND_R4 real
    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting ESMF_TypeKind parameter of ESMF_KIND_R4 real"
    typekind=ESMF_TypeKindGet(vreal4, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------
    !NEX_UTest
     write(failMsg, *) "Returned wrong ESMF_TypeKind"
     write(name, *) "Checking ESMF_TypeKind parameter for ESMF_KIND_R4 real"
    call ESMF_Test((typekind%dkind .eq. ESMF_TYPEKIND_R4%dkind), &
                    name, failMsg, result, ESMF_SRCLINE)

    ! ESMF_KIND_R8 real
    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting ESMF_TypeKind parameter of ESMF_KIND_R8 real"
    typekind=ESMF_TypeKindGet(vreal8, rc)
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !------------------------------------------------------------------------
    !NEX_UTest
     write(failMsg, *) "Returned wrong ESMF_TypeKind"
     write(name, *) "Checking ESMF_TypeKind parameter for ESMF_KIND_R8 real"
    call ESMF_Test((typekind%dkind .eq. ESMF_TYPEKIND_R8%dkind), &
                    name, failMsg, result, ESMF_SRCLINE)

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_TypeKindGetUTest

