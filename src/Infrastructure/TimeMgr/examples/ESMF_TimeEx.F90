! $Id: ESMF_TimeEx.F90,v 1.25 2004/04/09 20:13:38 eschwab Exp $
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
      program ESMF_TimeEx

!------------------------------------------------------------------------------
!EXAMPLE	String used by test script to count examples.
!==============================================================================
!BOP
!\begin{verbatim}
! !PROGRAM: ESMF_TimeEx - Time initialization and manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of Time initialization and manipulation
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF_Mod
      implicit none

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate some times
      type(ESMF_Time) :: time1, time2, time3
      type(ESMF_Time) :: midMonth
      type(ESMF_Time) :: wallClock

      ! instantiate time interval
      type(ESMF_TimeInterval) :: timeInterval1, timeInterval2

      ! temp variables for Get functions
      integer :: YY, MM, DD, D, H, M, S, rc
      integer(ESMF_KIND_I8) :: YYl, Dl, Sl
      type(ESMF_Calendar) :: cal
      integer :: dayOfYear, dayOfWeek, dayOfMonth
      real(ESMF_KIND_R8) :: dayOfYear_real
      type(ESMF_TimeInterval) :: dayOfYear_interval
      character (len=ESMF_MAXSTR) :: tString
!\end{verbatim}
!EOP

      ! result code
      integer :: finalrc
      finalrc = ESMF_SUCCESS

!BOP
!\begin{verbatim}
      ! set default time manager calendar to be Gregorian
      call ESMF_CalendarSetDefault(ESMF_CAL_GREGORIAN, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize time1 to 2/28/2000 2:24:45
      call ESMF_TimeSet(time1, yy=2000, &
                        mm=2, dd=28, h=2, m=24, s=45, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize time2 to 3/1/2000 3:26:01
      call ESMF_TimeSet(time2, yy=2000, &
                        mm=3, dd=1, h=3, m=26, s=1, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! initialize time interval1 to 2 days, 1800 seconds (0.5 hour)
      call ESMF_TimeIntervalSet(timeInterval1, d=2, &
                                s=1800, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time1 = "
      call ESMF_TimePrint(time1, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time2 = "
      call ESMF_TimePrint(time2, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeIntervalGet(timeInterval1, d=D, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Interval1 = ", D, " days, ", S, " seconds."
      print *

      ! calculate difference between time2 and time1
      timeInterval2 = time2 - time1
      call ESMF_TimeIntervalGet(timeInterval2, d=D, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Difference between time2 and time1 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."

      ! add time interval1 to time1
      time3 = time1 + timeInterval1
      print *, "Time1 plus TimeInterval1 = "
      call ESMF_TimePrint(time3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! subtract time interval1 from time2
      time3 = time2 - timeInterval1
      print *, "Time2 minus TimeInterval1 = "
      call ESMF_TimePrint(time3, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! compare time1 and time2
      if (time1 .lt. time2) then
        print *, "time1 is less than time2."
      end if

      call ESMF_TimeGet(time1, calendar=cal, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "time1 calendar = "
      call ESMF_CalendarPrint(cal, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(time1, dayOfYear=dayOfYear, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "time1 day of the year (integer) = ", dayOfYear

      call ESMF_TimeGet(time1, dayOfYear_r8=dayOfYear_real, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "time1 day of the year (real) = ", dayOfYear_real

      call ESMF_TimeGet(time1, dayOfWeek=dayOfWeek, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "time1 day of the week = ", dayOfWeek

      call ESMF_TimeGet(time1, midMonth=midMonth, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *
      print *, "time1 middle of the month = "
      call ESMF_TimePrint(midMonth, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! get time in string format
      call ESMF_TimeGet(time1, timeString=tString, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "time1 in string format = ", tString

      ! get wall clock time
      call ESMF_TimeSet(wallClock, calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeSyncToRealTime(wallClock, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Wall Clock Time = "
      call ESMF_TimePrint(wallClock, "string", rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
     call ESMF_CalendarDestroy(gregorianCalendar, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_TimeEx.F90"
     else
        print *, "FAIL: ESMF_TimeEx.F90"
     end if

!BOP
!\begin{verbatim}
      end program ESMF_TimeEx
!\end{verbatim}
!EOP
