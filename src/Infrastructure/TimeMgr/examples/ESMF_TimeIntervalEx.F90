! $Id: ESMF_TimeIntervalEx.F90,v 1.22 2004/04/09 20:13:38 eschwab Exp $
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
      use ESMF_Mod
      implicit none
      type(ESMF_TimeInterval) :: timeInterval1, timeInterval2, timeInterval3

      ! temp variables
      integer :: d, h, m, s, rc
      integer(ESMF_KIND_I8) :: dl, sl
      double precision :: quotient, divisor, multiplier 
      type(ESMF_TimeInterval) :: remainder
!EOC

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! set default time manager calendar to be Gregorian
      call ESMF_CalendarSetDefault(ESMF_CAL_GREGORIAN, rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! initialize time interval1 to 1 day, 1800 seconds (0.5 hour)
      call ESMF_TimeIntervalSet(timeInterval1, d=1, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! initialize time interval2 to 4 days, 5400 seconds (1.5 hours)
      call ESMF_TimeIntervalSet(timeInterval2, d=4, s=5400, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      call ESMF_TimeIntervalGet(timeInterval1, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Time Interval1 = ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval1, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      call ESMF_TimeIntervalGet(timeInterval2, d=d, h=h, m=m, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Time Interval2 = ", d, " days, ", h, " hours, ", m, " minutes."
      call ESMF_TimeIntervalPrint(timeInterval2, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! difference
      timeInterval3 = timeInterval2 - timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, d=d, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Difference between TimeInterval2 and TimeInterval1 = ", &
               d, " days, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! sum
      timeInterval3 = timeInterval2 + timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, d=d, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Sum of TimeInterval1 and TimeInterval2 = ", d, " days, ", &
               s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! divide
      quotient = timeInterval2 / timeInterval1
      print *, "TimeInterval2 divided by TimeInterval1 = ", quotient

      ! modulo
      remainder = MOD(timeInterval2, timeInterval1)
      print *, "TimeInterval2 modulo TimeInterval1 = " 
      call ESMF_TimeIntervalPrint(remainder, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! divide by integer
      timeInterval3 = timeInterval2 / 2
      call ESMF_TimeIntervalGet(timeInterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "TimeInterval2 divided by 2 = ", d, " days, ", h, " hours, ", &
               m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! divide by double precision real
      divisor = 1.5
      timeInterval3 = timeInterval2 / divisor
      call ESMF_TimeIntervalGet(timeInterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "TimeInterval2 divided by 1.5 = ", d, " days, ", h, " hours, ", &
               m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! multiply by integer
      timeInterval3 = timeInterval1 * 3
      call ESMF_TimeIntervalGet(timeInterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "TimeInterval1 multiplied by 3 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! multiply by double precision real; use commutative variant of
      !   (*) operator
      multiplier = 2.25
      timeInterval3 = multiplier * timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "TimeInterval1 multiplied by 2.25 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! change time interval 1 to negative value
      timeInterval1 = timeInterval1 * (-1)
      call ESMF_TimeIntervalGet(timeInterval1, d=d, h=h, m=m, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Time Interval1 changed to ", d, " days, ", h, " hours, ", &
               m, " minutes."
      call ESMF_TimeIntervalPrint(timeInterval1, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! absolute value
      timeInterval3 = ESMF_TimeIntervalAbsValue(timeInterval1)
      call ESMF_TimeIntervalGet(timeInterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Absolute value of TimeInterval1 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! negative absolute value of time interval 1
      timeInterval3 = ESMF_TimeIntervalNegAbsValue(timeInterval1)
      call ESMF_TimeIntervalGet(timeInterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Negative absolute value of TimeInterval1 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

!EOC
      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! negative absolute value of time interval 2
      timeInterval3 = ESMF_TimeIntervalNegAbsValue(timeInterval2)
      call ESMF_TimeIntervalGet(timeInterval3, d=d, h=h, m=m, s=s, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      print *, "Negative absolute value of TimeInterval2 = ", d, " days, ", h, &
               " hours, ", m, " minutes, ", s, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOC
      ! comparison
      if (timeInterval2 > timeInterval1) then
        print *, "TimeInterval2 is larger than TimeInterval1"
      else if (timeInterval2 .lt. timeInterval1) then
        print *, "TimeInterval2 is smaller than TimeInterval1"
      else if (timeInterval2 == timeInterval1) then
        print *, "TimeInterval2 is equal to TimeInterval1"
      end if
!EOC

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_TimeIntervalEx.F90"
     else
        print *, "FAIL: ESMF_TimeIntervalEx.F90"
     end if

!BOC
      end program ESMF_TimeIntervalEx
!EOC
