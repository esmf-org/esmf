! $Id: ESMF_TimeIntervalEx.F90,v 1.4 2003/06/07 00:41:59 eschwab Exp $
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
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_TimeIntervalEx - Time Interval initialization and manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of Time Interval initialization and manipulation
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_TimeIntervalEx.F90,v 1.4 2003/06/07 00:41:59 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate some time intervals
      type(ESMF_TimeInterval) :: timeInterval1, timeInterval2, timeInterval3

      ! temp variables
      integer :: D, H, M, S
      integer(ESMF_IKIND_I8) :: Dl, Sl
      double precision quotient, divisor, multiplier

      ! result code
      integer :: rc

      ! initialize time interval1 to 1 day, 1800 seconds (0.5 hour)
      call ESMF_TimeIntervalSet(timeInterval1, D=1, &
                                S=1800, rc=rc)

      ! initialize time interval2 to 4 days, 5400 seconds (1.5 hours)
      call ESMF_TimeIntervalSet(timeInterval2, D=4, &
                                S=5400, rc=rc)

      call ESMF_TimeIntervalGet(timeInterval1, D=D, H=H, M=M, rc=rc)
      print *, "Time Interval1 = ", D, " days, ", H, " hours, ", M, " minutes."
      call ESMF_TimeIntervalPrint(timeInterval1, "string", rc)

      call ESMF_TimeIntervalGet(timeInterval2, D=D, H=H, M=M, rc=rc)
      print *, "Time Interval2 = ", D, " days, ", H, " hours, ", M, " minutes."
      call ESMF_TimeIntervalPrint(timeInterval2, "string", rc)

      ! difference
      timeInterval3 = timeInterval2 - timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, D=D, S=S, rc=rc)
      print *, "Difference between TimeInterval2 and TimeInterval1 = ", &
               D, " days, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! sum
      timeInterval3 = timeInterval2 + timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, D=D, S=S, rc=rc)
      print *, "Sum of TimeInterval1 and TimeInterval2 = ", D, " days, ", &
               S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! divide
      quotient = timeInterval2 / timeInterval1
      print *, "TimeInterval2 divided by TimeInterval1 = ", quotient

      ! divide by integer
      timeInterval3 = timeInterval2 / 2
      call ESMF_TimeIntervalGet(timeInterval3, D=D, H=H, M=M, S=S, rc=rc)
      print *, "TimeInterval2 divided by 2 = ", D, " days, ", H, " hours, ", &
               M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! divide by double precision real
      divisor = 1.5
      timeInterval3 = timeInterval2 / divisor
      call ESMF_TimeIntervalGet(timeInterval3, D=D, H=H, M=M, S=S, rc=rc)
      print *, "TimeInterval2 divided by 1.5 = ", D, " days, ", H, " hours, ", &
               M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! multiply by integer
      timeInterval3 = timeInterval1 * 3
      call ESMF_TimeIntervalGet(timeInterval3, D=D, H=H, M=M, S=S, rc=rc)
      print *, "TimeInterval1 multiplied by 3 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! multiply by double precision real
      multiplier = 2.25
      timeInterval3 = timeInterval1 * multiplier
      call ESMF_TimeIntervalGet(timeInterval3, D=D, H=H, M=M, S=S, rc=rc)
      print *, "TimeInterval1 multiplied by 2.25 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! change time interval 1 to negative value
      timeInterval1 = timeInterval1 * (-1)
      call ESMF_TimeIntervalGet(timeInterval1, D=D, H=H, M=M, rc=rc)
      print *, "Time Interval1 changed to ", D, " days, ", H, " hours, ", &
               M, " minutes."
      call ESMF_TimeIntervalPrint(timeInterval1, "string", rc)

      ! absolute value
      timeInterval3 = ESMF_TimeIntervalAbsValue(timeInterval1)
      call ESMF_TimeIntervalGet(timeInterval3, D=D, H=H, M=M, S=S, rc=rc)
      print *, "Absolute value of TimeInterval1 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! negative absolute value of time interval 1
      timeInterval3 = ESMF_TimeIntervalNegAbsValue(timeInterval1)
      call ESMF_TimeIntervalGet(timeInterval3, D=D, H=H, M=M, S=S, rc=rc)
      print *, "Negative absolute value of TimeInterval1 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! negative absolute value of time interval 2
      timeInterval3 = ESMF_TimeIntervalNegAbsValue(timeInterval2)
      call ESMF_TimeIntervalGet(timeInterval3, D=D, H=H, M=M, S=S, rc=rc)
      print *, "Negative absolute value of TimeInterval2 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

      ! comparison
      if (timeInterval2 > timeInterval1) then
        print *, "TimeInterval2 is larger than TimeInterval1"
      else if (timeInterval2 .lt. timeInterval1) then
        print *, "TimeInterval2 is smaller than TimeInterval1"
      else if (timeInterval2 == timeInterval1) then
        print *, "TimeInterval2 is equal to TimeInterval1"
      end if

      end program ESMF_TimeIntervalEx
