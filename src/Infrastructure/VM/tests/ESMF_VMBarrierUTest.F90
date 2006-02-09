! $Id: ESMF_VMBarrierUTest.F90,v 1.7 2006/02/09 07:27:40 theurich Exp $
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
      program ESMF_VMBarrierUTest

!------------------------------------------------------------------------------
 
#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_VMBarrierUTest - Unit test for VM Barrier Function
!
! !DESCRIPTION:
!
! The code in this file drives the F90 VM Barrier test.  The VM
!   Barrier function is complex enough to require a separate test file.
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
      '$Id: ESMF_VMBarrierUTest.F90,v 1.7 2006/02/09 07:27:40 theurich Exp $'
!------------------------------------------------------------------------------
      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0


      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name

      ! local variables
      integer:: i, rc, loop_rc
      type(ESMF_VM):: vm
      integer:: localPet, petCount
      real(ESMF_KIND_R8)  t_a, t0, t1, t_b, dt, max_time, delay, dt_prec
      real(ESMF_KIND_R8), allocatable:: delay_time(:)


!------------------------------------------------------------------------------
!   The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
!   always run. When the environment variable, EXHAUSTIVE, is set to ON then
!   the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
!   Special strings (Non-exhaustive and exhaustive) have been
!   added to allow a script to count the number and types of unit tests.
!------------------------------------------------------------------------------


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      ! Get count of PETs and which PET number we are
      call ESMF_VMGetGlobal(vm, rc=rc)
      call ESMF_VMGet(vm, localPet, petCount=petCount, rc=rc)

      ! Allocate localData
      allocate(delay_time(0:petCount-1))
      
      ! Determine the precision of the timer function
      dt_prec = 0.
      do i=1, 10
        t_a = get_time()
        t_b = t_a
        do while (t_b == t_a)
          t_b = get_time()
        end do
        if (t_b - t_a > dt_prec) dt_prec = t_b - t_a
      end do
      print *, "timing precision is: ", dt_prec

      !Loop to test delaying each of the processors in turn
      ! Preset loop_rc to ESMF_SUCCESS
      loop_rc=ESMF_SUCCESS 
      do i=0,petCount-1
        delay_time=0.
        delay_time(i)=1.
        !t_a=starting time in each processor
        t_a = get_time()

        call ESMF_VMBarrier(vm, rc)

        !Delay each process for dt=delay_time seconds.
        t0 = get_time()
        t1 = t0
        dt = delay_time(localPet)

        Do While (t1-t0 < dt) 
          t1 = get_time()
          if (t1-t0 < -100.) t1 = t1 + get_time_max()
        End do

        call ESMF_VMBarrier(vm, rc)

        !t_b=ending time at each processor
        t_b = get_time()

        max_time = 1.
        !In case count_max was reached in the system_clock routine...
        if (t_b-t_a < -100.) t_b=t_b+get_time_max()
      !----------------------------------------------------------------
      ! Test Barrier method 
        write(failMsg, *) "Barrier did not hold. Slow PET=", i
        write(name, *) "Barrier Test"

        if ((t_b - t_a) < (max_time - dt_prec)) then
          loop_rc=ESMF_FAILURE
        end if


           delay= t_b - t_a
           print*, 'slow Pet=', i,'Pet=', localPet,'time delay = ', &
         &         delay,'[  -- should be >= ',max_time-dt_prec,'--  ]'
      end do   !Loop over i -- "slow" Pet's

      !NEX_UTest
      !Verify loop test results
      call ESMF_Test((loop_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TestEnd(result, ESMF_SRCLINE)


      contains

      function get_time()
      implicit none
      real(ESMF_KIND_R8) get_time
      integer count,count_rate,count_max
      count=0;count_rate=0;count_max=0
      call system_clock(count, count_rate, count_max)
      get_time = real(count)/real(count_rate)
      end function get_time

      function get_time_max()
      implicit none
      real(ESMF_KIND_R8) get_time_max
      integer count,count_rate,count_max
      count=0;count_rate=0;count_max=0
      call system_clock(count, count_rate, count_max)
      get_time_max = real(count_max)/real(count_rate)
      end function get_time_max

      end program ESMF_VMBarrierUTest
