! $Id: ESMF_VMBarrierUTest.F90,v 1.11.2.3 2009/01/21 21:25:24 cdeluca Exp $
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
      program ESMF_VMBarrierUTest

!------------------------------------------------------------------------------
 
#include "ESMF_Macros.inc"

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
      '$Id: ESMF_VMBarrierUTest.F90,v 1.11.2.3 2009/01/21 21:25:24 cdeluca Exp $'
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
      real(ESMF_KIND_R8):: t_a, t_b, dt, delay, dt_prec
      real(ESMF_KIND_R8):: dt_prec_max(1), dt_prec_local(1)
      real(ESMF_KIND_R8), parameter:: delay_time = 1.0 ! 1 second delay

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
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)

      ! Determine the precision of the timer function
      call ESMF_VMWTimePrec(dt_prec_local(1))
      print *, "timing precision is: ", dt_prec_local(1)
      
      ! Find the maximum dt_prec across all PETs
      call ESMF_VMAllReduce(vm, dt_prec_local, dt_prec_max, 1, ESMF_MAX, rc=rc)
      dt_prec = dt_prec_max(1)
      print *, "max timing precision is: ", dt_prec
      
      !NEX_UTest
      ! delay_time must at least be one order of magnitude longer than precis.
      write(name, *) "Timer Precision test"
      write(failMsg, *) "Timer precision is too low!"
      call ESMF_Test((delay_time >= 10. * dt_prec), &
        name, failMsg, result, ESMF_SRCLINE)

      ! Loop to test delaying each of the processors in turn
      ! Preset loop_rc to ESMF_SUCCESS
      loop_rc=ESMF_SUCCESS 
      do i=0,petCount-1
        call ESMF_VMWTime(t_a)  ! t_a is start time for each PET

        ! double barrier construct
        call ESMF_VMBarrier(vm, rc)
        if (localPet==i) then
          ! delay PET i by delay_time
          dt = delay_time + 4*dt_prec ! 4*dt_prec compensates for 4x taking time
          call ESMF_VMWTimeDelay(dt)
        endif
        call ESMF_VMBarrier(vm, rc)

        call ESMF_VMWTime(t_b)  ! t_b is end time for each PET

        ! test that measured delay is at least as long as delay_time
        delay = t_b - t_a
        if (delay < delay_time) then
          loop_rc=ESMF_FAILURE
        end if

        print*, 'slow Pet=', i,'Pet=', localPet,'time delay = ', &
          delay,'[  -- should be >= ', delay_time,'--  ]'
      end do   !Loop over i -- "slow" Pet's

      !----------------------------------------------------------------
      ! Test Barrier method 
      !NEX_UTest
      !Verify loop test results
      write(name, *) "Barrier Test"
      write(failMsg, *) "Barrier did not hold!"
      call ESMF_Test((loop_rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      call ESMF_TestEnd(result, ESMF_SRCLINE)

      end program ESMF_VMBarrierUTest
