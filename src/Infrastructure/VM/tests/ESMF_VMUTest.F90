! $Id: ESMF_VMUTest.F90,v 1.6 2004/10/05 16:31:27 svasquez Exp $
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
      program ESMF_VMUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 VM unit tests.
! The companion file ESMF\_VM.F90 contains the definitions for the
! VM methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_VMUTest.F90,v 1.6 2004/10/05 16:31:27 svasquez Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc = 1

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

!     !LOCAL VARIABLES:
      type(ESMF_VM):: vm
      integer:: localPet, npets
      integer, allocatable:: array1(:)
      integer, dimension (:, :), allocatable:: array2
      integer::  func_results, myresults
      integer:: nsize, i, j


!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then 
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------- 
      print *, "Starting job"

      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "ESMF_Initialize Test"
      call ESMF_Initialize(vm=vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_VMGet(vm, petCount=npets, rc=rc)
      print '(/, a, i3)' , "NUMBER_OF_PROCESSORS", npets
      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Get Global Test"
      call ESMF_VMGetGlobal(vm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM print Test"
      call ESMF_VMPrint(vm, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM Get Test"
      call ESMF_VMGet(vm, localPet, petCount=npets, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest

      ! allocate data arrays
      nsize = 2
      allocate(array1(nsize))

      ! prepare data array1
      do i=1, nsize
        array1(i) = localPet * 100 + i
      enddo


      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_SUM Test"
      call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=func_results, count=nsize, &
      reduceflag=ESMF_SUM, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! Populate array2
      allocate(array2(nsize,npets))
      do j=1, npets 
      	do i=1, nsize
        	array2(i,j) = (j-1) * 100 + i
      	enddo
      enddo

      ! print the scatter result
      myresults = SUM(array2)
      print *, 'Global sum:'
      print *, localPet,' func_results: ', func_results, ' myresults:', myresults

      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_SUM Results Test"
      call ESMF_Test((func_results.eq.myresults), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_MIN Test"
      call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=func_results, count=nsize, &
      reduceflag=ESMF_MIN, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      myresults = MINVAL(array2)
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_MIN Results Test"
      call ESMF_Test((func_results.eq.myresults), name, failMsg, result, ESMF_SRCLINE)
      print *, localPet,' func_results: ', func_results, ' myresults:', myresults


      !------------------------------------------------------------------------
      !EX_UTest
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "VM All Full Reduce ESMF_MAX Test"
      call ESMF_VMAllFullReduce(vm, sendData=array1, recvData=func_results, count=nsize, &
      reduceflag=ESMF_MAX, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      myresults = MAXVAL(array2)
      write(failMsg, *) "Returned wrong results"
      write(name, *) "Verify Reduce ESMF_MAX Results Test"
      call ESMF_Test((func_results.eq.myresults), name, failMsg, result, ESMF_SRCLINE)
      print *, localPet,' func_results: ', func_results, ' myresults:', myresults



#endif
      call ESMF_Finalize(rc)

      print *, "******  End of VMUTest  ******"

      end program ESMF_VMUTest
