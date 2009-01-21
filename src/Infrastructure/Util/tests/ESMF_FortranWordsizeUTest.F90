! $Id: ESMF_FortranWordsizeUTest.F90,v 1.7.2.4 2009/01/21 21:25:24 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!


    program ESMF_FortranWordsizeUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_FortranWordsizeUTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 FortranWordsize unit tests.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod         ! test methods
      use ESMF_Mod             ! the ESMF Framework

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_FortranWordsizeUTest.F90,v 1.7.2.4 2009/01/21 21:25:24 cdeluca Exp $'
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
      integer(ESMF_KIND_I1) :: vint1
#endif
#ifndef ESMF_NO_INTEGER_2_BYTE
      integer(ESMF_KIND_I2) :: vint2
#endif
      integer(ESMF_KIND_I4) :: vint4
      integer(ESMF_KIND_I8) :: vint8

      real                :: vreal
      real (ESMF_KIND_R4) :: vreal4
      real (ESMF_KIND_R8) :: vreal8
      
      integer :: datasize

!-------------------------------------------------------------------------------
!  The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!  always run. When the environment variable, EXHAUSTIVE, is set to ON then
!  the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!  to OFF, then only the sanity unit tests.
!  Special strings (Non-exhaustive and exhaustive) have been
!  added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

!Test Integer Data Sizes
!=======================
    !NEX_UTest
    ! Default integer
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting size of default integer"
    datasize=ESMF_FortranWordsize(vint, rc )
    print *, "Size of default integer: ", datasize, " bytes"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    ! ESMF_KIND_I1 Integer size
#ifndef ESMF_NO_INTEGER_1_BYTE
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting size of ESMF_KIND_I1 integer"
    datasize=ESMF_FortranWordsize(vint1, rc )
    print *, "Size of ESMF_KIND_I1 integer: ", datasize, " bytes"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(name, *) "Dummy test to keep number of tests correct"
    call ESMF_Test((ESMF_SUCCESS.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    !------------------------------------------------------------------------
    !NEX_UTest
    !ESMF_KIND_I2 Integer size
#ifndef ESMF_NO_INTEGER_2_BYTE
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting size of ESMF_KIND_I2 integer"
    datasize=ESMF_FortranWordsize(vint2, rc )
    print *, "Size of ESMF_KIND_I2 integer: ", datasize, " bytes"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
    write(name, *) "Dummy test to keep number of tests correct"
    call ESMF_Test((ESMF_SUCCESS.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif

    !------------------------------------------------------------------------
    !NEX_UTest
    !ESMF_KIND_I4 Integer size
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting size of ESMF_KIND_I4 integer"
    datasize=ESMF_FortranWordsize(vint4, rc )
    print *, "Size of ESMF_KIND_I4 integer: ", datasize, " bytes"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    !ESMF_KIND_I8 Integer size
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting size of ESMF_KIND_I8 integer"
    datasize=ESMF_FortranWordsize(vint8, rc )
    print *, "Size of ESMF_KIND_I8 integer: ", datasize, " bytes"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!Test Real Data Sizes
!=======================
    !NEX_UTest
    ! Default real
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting size of default real"
    datasize=ESMF_FortranWordsize(vreal, rc )
    print *, "Size of default real: ", datasize, " bytes"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    !ESMF_KIND_R4 Real size
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting size of ESMF_KIND_R4 real"
    datasize=ESMF_FortranWordsize(vreal4, rc )
    print *, "Size of ESMF_KIND_R4 real: ", datasize, " bytes"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    !------------------------------------------------------------------------
    !NEX_UTest
    !ESMF_KIND_R8 Real size
    write(failMsg, *) "Did not RETURN ESMF_SUCCESS"
    write(name, *) "Getting size of ESMF_KIND_R8 real"
    datasize=ESMF_FortranWordsize(vreal8, rc )
    print *, "Size of ESMF_KIND_R8 real: ", datasize, " bytes"
    call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program ESMF_FortranWordsizeUTest

