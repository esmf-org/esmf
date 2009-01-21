! $Id: ESMF_IOSpecUTest.F90,v 1.7.2.4 2009/01/21 21:25:21 cdeluca Exp $
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
      program ESMF_IOSpecUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_IOSpecTest - Tests the IOSpec Set and Get subroutines
!
! !DESCRIPTION:
!
! The code in this file drives F90 IOSpec unit tests.
! The companion file ESMF\_IOSpec.F90 contains the definitions for the
! IOSpec methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_IOSpecUTest.F90,v 1.7.2.4 2009/01/21 21:25:21 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      type(ESMF_IOSpec) :: iospec
      character(ESMF_MAXSTR) :: fname
      type (ESMF_IOFileFormat) :: fformat


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test IOSpec Set
      write(failMsg, *) "Did not return ESMF_SUCCESSS"
      write(name, *) "IOSpec Set Test"
      call ESMF_IOSpecSet(iospec, filename='testFilename', &
                      iofileformat=ESMF_IO_FILEFORMAT_UNSPECIFIED, &
                      rc=rc)

      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test IOSpec Get
      write(failMsg, *) "Did not return ESMF_SUCCESSS"
      write(name, *) "Get IOSpec File Name Test"
      call ESMF_IOSpecGet(iospec, filename=fname,  rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      !NEX_UTest
      ! Verify file name
      write(failMsg, *) "Returned wrong file name"
      write(name, *) "Verify IOSpec Filename Test"
      call ESMF_Test((fname.eq."testFilename"), name, failMsg, result, ESMF_SRCLINE)
      print *, " File name = ", fname

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test IOSpec Get
      write(failMsg, *) "Did not return ESMF_SUCCESSS"
      write(name, *) "Get IOSpec File Format Test"
      call ESMF_IOSpecGet(iospec, iofileformat=fformat,  rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      !NEX_UTest
      ! Verify file name
      write(failMsg, *) "Returned wrong file name"
      write(name, *) "Verify IOSpec File Format  Test"
      call ESMF_Test((fformat.eq.ESMF_IO_FILEFORMAT_UNSPECIFIED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
#ifdef ESMF_TESTEXHAUSTIVE

      ! add more tests here

#endif
      !------------------------------------------------------------------------

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_IOSpecUTest
