! $Id: ESMF_TimeEx.F90,v 1.6 2003/05/07 17:37:00 eschwab Exp $
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
      '$Id: ESMF_TimeEx.F90,v 1.6 2003/05/07 17:37:00 eschwab Exp $'
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
      integer :: MM, DD, H, M
      integer(ESMF_IKIND_I8) :: YR, D, S
      type(ESMF_Calendar) :: cal
      integer :: dayOfYear, dayOfWeek, dayOfMonth

      ! result code
      integer :: rc

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time1 to 2/28/2000 2:24:45
      call ESMF_TimeInit(time1, YR=int(2000,kind=ESMF_IKIND_I8), &
                       MM=2, DD=28, H=2, M=24, S=int(45,kind=ESMF_IKIND_I8), &
                       cal=gregorianCalendar, rc=rc)

      ! initialize time2 to 3/1/2000 3:26:01
      call ESMF_TimeInit(time2, YR=int(2000,kind=ESMF_IKIND_I8), &
                       MM=3, DD=1, H=3, M=26, S=int(1,kind=ESMF_IKIND_I8), &
                       cal=gregorianCalendar, rc=rc)

      ! initialize time interval1 to 2 days, 1800 seconds (0.5 hour)
      call ESMF_TimeIntervalInit(timeInterval1, D=int(2,kind=ESMF_IKIND_I8), &
                                 S=int(1800,kind=ESMF_IKIND_I8), rc=rc)

      print *, "Time1 = "
      call ESMF_TimePrint(time1, "string", rc)

      print *, "Time2 = "
      call ESMF_TimePrint(time2, "string", rc)

      call ESMF_TimeIntervalGet(timeInterval1, D=D, S=S, rc=rc)
      print *, "Time Interval1 = ", D, " days, ", S, " seconds."
      print *

      ! calculate difference between time2 and time1
      timeInterval2 = time2 - time1
      call ESMF_TimeIntervalGet(timeInterval2, D=D, H=H, M=M, S=S, rc=rc)
      print *, "Difference between time2 ane time1 = ", D, " days, ", H, &
               " hours, ", M, " minutes, ", S, " seconds."

      ! add time interval1 to time1
      time3 = time1 + timeInterval1
      print *, "Time1 plus TimeInterval1 = "
      call ESMF_TimePrint(time3, "string", rc)

      ! subtract time interval1 from time2
      time3 = time2 - timeInterval1
      print *, "Time2 minus TimeInterval1 = "
      call ESMF_TimePrint(time3, "string", rc)

      call ESMF_TimeGetCalendar(time1, cal, rc)
      print *, "time1 calendar = "
      call ESMF_CalendarPrint(cal, rc=rc)

      call ESMF_TimeGetDayOfYear(time1, dayOfYear, rc)
      print *, "time1 day of the year = ", dayOfYear

      call ESMF_TimeGetDayOfMonth(time1, dayOfMonth, rc)
      print *, "time1 day of the month = ", dayOfMonth

      call ESMF_TimeGetDayOfWeek(time1, dayOfWeek, rc)
      print *, "time1 day of the week = ", dayOfWeek

      call ESMF_TimeGetMidMonth(time1, midMonth, rc)
      print *
      print *, "time1 middle of the month = "
      call ESMF_TimePrint(midMonth, "string", rc)

      ! get wall clock time
      call ESMF_TimeInit(wallClock, cal=gregorianCalendar, rc=rc)
      call ESMF_TimeGetRealTime(wallClock, rc)
      print *, "Wall Clock Time = "
      call ESMF_TimePrint(wallClock, "string", rc)

      end program ESMF_TimeEx
