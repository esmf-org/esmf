! $Id: ESMF_TimeEx.F90,v 1.14 2003/10/20 20:13:57 cdeluca Exp $
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
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_TimeEx - Time initialization and manipulation examples
!
! !DESCRIPTION:
!
! This program shows examples of Time initialization and manipulation
!EOP
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_TimeEx.F90,v 1.14 2003/10/20 20:13:57 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate some times
      type(ESMF_Time) :: time1, time2, time3
      type(ESMF_Time) :: midMonth
      type(ESMF_Time) :: wallClock

      ! instantiate time interval
      type(ESMF_TimeInterval) :: timeInterval1, timeInterval2

      ! temp variables for Get functions
      integer :: YR, MM, DD, D, H, M, S
      integer(ESMF_KIND_I8) :: YRl, Dl, Sl
      type(ESMF_Calendar) :: cal
      integer :: dayOfYear, dayOfWeek, dayOfMonth

      ! result code
      integer :: rc

      ! initialize ESMF framework
      call ESMF_Initialize(rc)

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time1 to 2/28/2000 2:24:45
      call ESMF_TimeSet(time1, yr=2000, &
                        mm=2, dd=28, h=2, m=24, s=45, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize time2 to 3/1/2000 3:26:01
      call ESMF_TimeSet(time2, yr=2000, &
                        mm=3, dd=1, h=3, m=26, s=1, &
                        calendar=gregorianCalendar, rc=rc)

      ! initialize time interval1 to 2 days, 1800 seconds (0.5 hour)
      call ESMF_TimeIntervalSet(timeInterval1, d=2, &
                                s=1800, rc=rc)

      print *, "Time1 = "
      call ESMF_TimePrint(time1, "string", rc)

      print *, "Time2 = "
      call ESMF_TimePrint(time2, "string", rc)

      call ESMF_TimeIntervalGet(timeInterval1, d=D, s=S, rc=rc)
      print *, "Time Interval1 = ", D, " days, ", S, " seconds."
      print *

      ! calculate difference between time2 and time1
      timeInterval2 = time2 - time1
      call ESMF_TimeIntervalGet(timeInterval2, d=D, h=H, m=M, s=S, rc=rc)
      print *, "Difference between time2 and time1 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."

      ! add time interval1 to time1
      time3 = time1 + timeInterval1
      print *, "Time1 plus TimeInterval1 = "
      call ESMF_TimePrint(time3, "string", rc)

      ! subtract time interval1 from time2
      time3 = time2 - timeInterval1
      print *, "Time2 minus TimeInterval1 = "
      call ESMF_TimePrint(time3, "string", rc)

      ! compare time1 and time2
      if (time1 .lt. time2) then
        print *, "time1 is less than time2."
      end if

      call ESMF_TimeGet(time1, calendar=cal, rc=rc)
      print *, "time1 calendar = "
      call ESMF_CalendarPrint(cal, rc=rc)

      call ESMF_TimeGet(time1, dayOfYear=dayOfYear, rc=rc)
      print *, "time1 day of the year = ", dayOfYear

      call ESMF_TimeGet(time1, dayOfMonth=dayOfMonth, rc=rc)
      print *, "time1 day of the month = ", dayOfMonth

      call ESMF_TimeGet(time1, dayOfWeek=dayOfWeek, rc=rc)
      print *, "time1 day of the week = ", dayOfWeek

      call ESMF_TimeGet(time1, midMonth=midMonth, rc=rc)
      print *
      print *, "time1 middle of the month = "
      call ESMF_TimePrint(midMonth, "string", rc)

      ! get wall clock time
      call ESMF_TimeSet(wallClock, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSyncToRealTime(wallClock, rc)
      print *, "Wall Clock Time = "
      call ESMF_TimePrint(wallClock, "string", rc)

      ! finalize ESMF framework
      call ESMF_Finalize(rc)

      end program ESMF_TimeEx
