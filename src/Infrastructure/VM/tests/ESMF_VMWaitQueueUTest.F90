! $Id: ESMF_VMWaitQueueUTest.F90,v 1.1 2005/10/18 15:48:55 svasquez Exp $
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
      program ESMF_VMWaitQueueUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMWailtQueueUTest - Unit test for VM WaitQueue  VM WtimePrec Functions
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM  WaitQueue tests.  The VM
! All WaitQueue  function is complex enough to require a separate test file.
!   It runs on multiple processors.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_VMWaitQueueUTest.F90,v 1.1 2005/10/18 15:48:55 svasquez Exp $'
!------------------------------------------------------------------------------
      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0


      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      integer::  rc, loop_count
      type(ESMF_VM):: vm
      integer:: localPet, petCount
      integer:: nlen, nsize, i
      character(8) :: my_todays_date, my_time
      real(ESMF_KIND_R8), allocatable:: array2(:), array1(:)
      real(ESMF_KIND_R8):: comparevalue

!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      ! get global vm information
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)

      ! allocate data arrays
      nsize = 1
      nlen = nsize * petCount
      allocate(array1(nlen))
      allocate(array2(nsize))

      ! prepare data array1
      do i=1, nlen
        array1(i) = i * 100000
      enddo

      ! Calculate loop_count for each Pet
      ! Making loop_count different for each PET so that they
      ! spend different amounts of time going thru the loop.
      loop_count =((localPet + 10 )*8) * 1000
      do i=1, loop_count
	   ! This call put here to waste time
           call date_and_time(date=my_todays_date, time=my_time)
      enddo
      !------------------------------------------------------------------------
      !NEX_UTest
      ! WAit until everyone is finished
      write(name, *) "WaitQueue Test"
      write(failMsg, *) "Did not retuirn ESMF_SUCCESS."
      call ESMF_VMWaitQueue(vm, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !==============================

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Get the VMWtimePrec
      write(name, *) "VMWtimePrec Test"
      write(failMsg, *) "Did not retuirn ESMF_SUCCESS."
      ! save precTime in array2
      do i=1, nlen
        call ESMF_VMWtimePrec(array2(i), rc )
      enddo
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test that we get a non-zero value for WtimePrec
      write(name, *) "VMWtimePrec test for non-zero value"
      write(failMsg, *) "Returned zero for VMWtimePrec"
      ! save precTime in array2
      call ESMF_VMWtimePrec(array2(1), rc )
      call ESMF_Test((array2(1).ne.0), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Gather the VMWtimePrec
      write(name, *) "AllGather Test"
      write(failMsg, *) "Did not retuirn ESMF_SUCCESS."
      call ESMF_VMAllGather(vm, sendData=array2, recvData=array1, count=nsize, rc=rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      print *, 'contents after allgather (time numbers should be equal)'
        do i=1,nlen
          print *, localPet,' array1: ', array1(i)
        end do

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Check the Gathered VMWtimePrec value
      write(name, *) "Verify VMWtimePrec Values Test"
      write(failMsg, *) "VMWtimePrec values not equal."
      ! Get compare value from array1(1)
      comparevalue = array1(1)     
      rc = ESMF_SUCCESS
      do i=1,nlen
          if (array1(i).ne.comparevalue) rc=ESMF_FAILURE
       enddo

      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_VMWaitQueueUTest
