! $Id: ESMF_TimeIntervalEx.F90,v 1.16 2004/01/06 16:55:49 svasquez Exp $
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

!==============================================================================
!BOP
!\begin{verbatim}
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
      integer :: D, H, M, S, rc
      integer(ESMF_KIND_I8) :: Dl, Sl
      double precision :: quotient, divisor, multiplier 
      type(ESMF_TimeInterval) :: remainder
!\end{verbatim}
!EOP

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOP
!\begin{verbatim}
      ! initialize time interval1 to 1 day, 1800 seconds (0.5 hour)
      call ESMF_TimeIntervalSet(timeInterval1, d=1, s=1800, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize time interval2 to 4 days, 5400 seconds (1.5 hours)
      call ESMF_TimeIntervalSet(timeInterval2, d=4, s=5400, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeIntervalGet(timeInterval1, d=D, h=H, m=M, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Interval1 = ", D, " days, ", H, " hours, ", M, " minutes."
      call ESMF_TimeIntervalPrint(timeInterval1, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeIntervalGet(timeInterval2, d=D, h=H, m=M, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Interval2 = ", D, " days, ", H, " hours, ", M, " minutes."
      call ESMF_TimeIntervalPrint(timeInterval2, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! difference
      timeInterval3 = timeInterval2 - timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, d=D, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Difference between TimeInterval2 and TimeInterval1 = ", &
               D, " days, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! sum
      timeInterval3 = timeInterval2 + timeInterval1
      call ESMF_TimeIntervalGet(timeInterval3, d=D, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Sum of TimeInterval1 and TimeInterval2 = ", D, " days, ", &
               S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! divide
      quotient = timeInterval2 / timeInterval1
      print *, "TimeInterval2 divided by TimeInterval1 = ", quotient

      ! modulo
      remainder = MOD(timeInterval2, timeInterval1)
      print *, "TimeInterval2 modulo TimeInterval1 = " 
      call ESMF_TimeIntervalPrint(remainder, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! divide by integer
      timeInterval3 = timeInterval2 / 2
      call ESMF_TimeIntervalGet(timeInterval3, d=D, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "TimeInterval2 divided by 2 = ", D, " days, ", H, " hours, ", &
               M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! divide by double precision real
      divisor = 1.5
      timeInterval3 = timeInterval2 / divisor
      call ESMF_TimeIntervalGet(timeInterval3, d=D, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "TimeInterval2 divided by 1.5 = ", D, " days, ", H, " hours, ", &
               M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! multiply by integer
      timeInterval3 = timeInterval1 * 3
      call ESMF_TimeIntervalGet(timeInterval3, d=D, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "TimeInterval1 multiplied by 3 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! multiply by double precision real
      multiplier = 2.25
      timeInterval3 = timeInterval1 * multiplier
      call ESMF_TimeIntervalGet(timeInterval3, d=D, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "TimeInterval1 multiplied by 2.25 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! change time interval 1 to negative value
      timeInterval1 = timeInterval1 * (-1)
      call ESMF_TimeIntervalGet(timeInterval1, d=D, h=H, m=M, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Interval1 changed to ", D, " days, ", H, " hours, ", &
               M, " minutes."
      call ESMF_TimeIntervalPrint(timeInterval1, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! absolute value
      timeInterval3 = ESMF_TimeIntervalAbsValue(timeInterval1)
      call ESMF_TimeIntervalGet(timeInterval3, d=D, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Absolute value of TimeInterval1 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! negative absolute value of time interval 1
      timeInterval3 = ESMF_TimeIntervalNegAbsValue(timeInterval1)
      call ESMF_TimeIntervalGet(timeInterval3, d=D, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Negative absolute value of TimeInterval1 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)

!\end{verbatim}
!EOP
      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! negative absolute value of time interval 2
      timeInterval3 = ESMF_TimeIntervalNegAbsValue(timeInterval2)
      call ESMF_TimeIntervalGet(timeInterval3, d=D, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Negative absolute value of TimeInterval2 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."
      call ESMF_TimeIntervalPrint(timeInterval3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! comparison
      if (timeInterval2 > timeInterval1) then
        print *, "TimeInterval2 is larger than TimeInterval1"
      else if (timeInterval2 .lt. timeInterval1) then
        print *, "TimeInterval2 is smaller than TimeInterval1"
      else if (timeInterval2 == timeInterval1) then
        print *, "TimeInterval2 is equal to TimeInterval1"
      end if
!\end{verbatim}
!EOP

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_TimeIntervalEx.F90"
     else
        print *, "FAIL: ESMF_TimeIntervalEx.F90"
     end if

!BOP
!\begin{verbatim}
      end program ESMF_TimeIntervalEx
!\end{verbatim}
!EOP
