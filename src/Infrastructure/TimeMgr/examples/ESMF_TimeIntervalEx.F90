! $Id: ESMF_TimeIntervalEx.F90,v 1.24 2004/06/05 00:17:33 eschwab Exp $
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
      program ESMF_TimeIntervalEx

!------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_TimeIntervalEx - Time Interval initialization and manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of Time Interval initialization and manipulation
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! instantiate some time intervals
      type(ESMF_TimeInterval) :: timeinterval1, timeinterval2, timeinterval3

      ! local variables
      integer :: d, h, m, s
      double precision :: quotient, divisor, multiplier 
      type(ESMF_TimeInterval) :: remainder

      ! return code
      integer:: rc
!EOC

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Time Interval Initialization}

! This example shows how to initialize two {\tt ESMF\_Time}s.
!EOE

!BOC
      ! initialize time interval1 to 1 day, 1800 seconds (0.5 hour)
      call ESMF_TimeIntervalSet(timeinterval1, d=1, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! initialize time interval2 to 4 days, 5400 seconds (1.5 hours)
      call ESMF_TimeIntervalSet(timeinterval2, d=4, s=5400, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_TimeIntervalGet(timeinterval1, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Time Interval1 = ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval1, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_TimeIntervalGet(timeinterval2, d=d, h=h, m=m, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Time Interval2 = ", d, " days, ", h, " hours, ", m, " minutes."
      call ESMF_TimeIntervalPrint(timeinterval2, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! difference
      timeinterval3 = timeinterval2 - timeinterval1
      call ESMF_TimeIntervalGet(timeinterval3, d=d, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Difference between TimeInterval2 and TimeInterval1 = ", &
               d, " days, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! sum
      timeinterval3 = timeinterval2 + timeinterval1
      call ESMF_TimeIntervalGet(timeinterval3, d=d, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Sum of TimeInterval1 and TimeInterval2 = ", d, " days, ", &
               s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! divide
      quotient = timeinterval2 / timeinterval1
      print *, "TimeInterval2 divided by TimeInterval1 = ", quotient

      ! modulo
      remainder = MOD(timeinterval2, timeinterval1)
      print *, "TimeInterval2 modulo TimeInterval1 = " 
      call ESMF_TimeIntervalPrint(remainder, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! divide by integer
      timeinterval3 = timeinterval2 / 2
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "TimeInterval2 divided by 2 = ", d, " days, ", h, " hours, ", &
               m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! divide by double precision real
      divisor = 1.5
      timeinterval3 = timeinterval2 / divisor
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "TimeInterval2 divided by 1.5 = ", d, " days, ", h, " hours, ", &
               m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! multiply by integer
      timeinterval3 = timeinterval1 * 3
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "TimeInterval1 multiplied by 3 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! multiply by double precision real; use commutative variant of
      !   (*) operator
      multiplier = 2.25
      timeinterval3 = multiplier * timeinterval1
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "TimeInterval1 multiplied by 2.25 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! change time interval 1 to negative value
      timeinterval1 = timeinterval1 * (-1)
      call ESMF_TimeIntervalGet(timeinterval1, d=d, h=h, m=m, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Time Interval1 changed to ", d, " days, ", h, " hours, ", &
               m, " minutes."
      call ESMF_TimeIntervalPrint(timeinterval1, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! absolute value
      timeinterval3 = ESMF_TimeIntervalAbsValue(timeinterval1)
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Absolute value of TimeInterval1 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! negative absolute value of time interval 1
      timeinterval3 = ESMF_TimeIntervalNegAbsValue(timeinterval1)
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Negative absolute value of TimeInterval1 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)

!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! negative absolute value of time interval 2
      timeinterval3 = ESMF_TimeIntervalNegAbsValue(timeinterval2)
      call ESMF_TimeIntervalGet(timeinterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      print *, "Negative absolute value of TimeInterval2 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeinterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! comparison
      if (timeinterval2 > timeinterval1) then
        print *, "TimeInterval2 is larger than TimeInterval1"
      else if (timeinterval2 .lt. timeinterval1) then
        print *, "TimeInterval2 is smaller than TimeInterval1"
      else if (timeinterval2 == timeinterval1) then
        print *, "TimeInterval2 is equal to TimeInterval1"
      end if

      ! finalize ESMF framework
      call ESMF_Finalize(rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
         print *, "PASS: ESMF_TimeIntervalEx.F90"
      else
         print *, "FAIL: ESMF_TimeIntervalEx.F90"
      end if

!BOC
      end program ESMF_TimeIntervalEx
!EOC
