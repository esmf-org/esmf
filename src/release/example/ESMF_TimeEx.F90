! $Id: ESMF_TimeEx.F90,v 1.5 2004/01/27 20:54:16 eschwab Exp $
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
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------

      ! Instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! Instantiate some times
      type(ESMF_Time) :: time
      type(ESMF_Time) :: midMonth
      type(ESMF_Time) :: wallClock

      ! Temp variables for Get functions
      integer :: mm, dd, h, m
      integer(ESMF_KIND_I8) :: s
      type(ESMF_Calendar) :: cal
      character (len=ESMF_MAXSTR) :: tS
      integer :: dayOfYear, dayOfWeek, dayOfMonth

      ! Result code
      integer :: rc

      ! Initialize calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! Initialize time to 5/12/2003 2:24:45
      call ESMF_TimeSet(time, yy=2003, &
                       mm=5, dd=12, h=2, m=24, s=45, &
                       calendar=gregorianCalendar, rc=rc)

      call ESMF_TimePrint(time, "string", rc)

      call ESMF_TimeGet(time, calendar=cal, rc=rc)
      call ESMF_CalendarPrint(cal, rc=rc)

      call ESMF_TimeGet(time, dayOfYear=dayOfYear, rc=rc)
      print *, "Day of the year = ", dayOfYear

      call ESMF_TimeGet(time, dd=dayOfMonth, rc=rc)
      print *, "Day of the month = ", dayOfMonth

      call ESMF_TimeGet(time, dayOfWeek=dayOfWeek, rc=rc)
      print *, "Day of the week = ", dayOfWeek

      call ESMF_TimeGet(time, midMonth=midMonth, rc=rc)
      print *
      print *, "Middle of the month = "
      call ESMF_TimePrint(midMonth, "string", rc)

      call ESMF_TimeGet(time, timeString=tS, rc=rc)
      print *, "Time in string format = ", tS

      ! Get wall clock time
      call ESMF_TimeSet(wallClock, calendar=gregorianCalendar, rc=rc)
      call ESMF_TimeSyncToRealTime(wallClock, rc)
      print *, "Wall Clock Time = "
      call ESMF_TimePrint(wallClock, "string", rc)

      print *, "ESMF_Time Example completed successfully"

      end program ESMF_TimeEx
