! $Id: ESMF_IOSpecUTest.F90,v 1.3 2004/10/05 16:07:11 svasquez Exp $
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
      program ESMF_IOSpecUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

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
      '$Id: ESMF_IOSpecUTest.F90,v 1.3 2004/10/05 16:07:11 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1, npets

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      type(ESMF_IOSpec) :: iospec
      character(ESMF_MAXSTR) :: fname
      type (ESMF_IOFileFormat) :: fformat
      type(ESMF_VM):: vm


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      print *, "Starting job"

      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      print '(/, a, i3)' , "NUMBER_OF_PROCESSORS", npets

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


      call ESMF_Finalize(rc)

      print *, "******  End of IOSpecUTest  ******"

      end program ESMF_IOSpecUTest
