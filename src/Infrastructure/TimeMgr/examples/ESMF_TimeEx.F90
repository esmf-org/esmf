! $Id: ESMF_TimeEx.F90,v 1.3 2003/04/30 07:49:33 eschwab Exp $
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
      use ESMF_BaseMod
      use ESMF_TimeIntervalMod
      use ESMF_TimeMod
      use ESMF_CalendarMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_TimeEx.F90,v 1.3 2003/04/30 07:49:33 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate some times
      type(ESMF_Time) :: Time
      type(ESMF_Time) :: midMonth
      type(ESMF_Time) :: wallClock

      ! temp variables for Get functions
      integer :: MM, DD, H, M
      integer(ESMF_IKIND_I8) :: S
      type(ESMF_Calendar) :: cal
      character, dimension(ESMF_MAXSTR) :: Ts
      integer :: dayOfYear, dayOfWeek, dayOfMonth

      ! result code
      integer :: rc

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time to 5/12/2003 2:24:45
      call ESMF_TimeInit(Time, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=5, DD=12, H=2, M=24, S=int(45,kind=ESMF_IKIND_I8), &
                         cal=gregorianCalendar, rc=rc)

      call ESMF_TimeGetCalendar(Time, cal, rc)
      call ESMF_CalendarPrint(cal, rc=rc)

      call ESMF_TimePrint(Time, "string", rc)

      call ESMF_TimeGetDayOfYear(Time, dayOfYear, rc)
      print *, "Day of the year = ", dayOfYear

      call ESMF_TimeGetMidMonth(Time, midMonth, rc)
      print *, "Middle of the month = "
      call ESMF_TimePrint(midMonth, "string", rc)

      call ESMF_TimeGetDayOfWeek(Time, dayOfWeek, rc)
      print *, "Day of the week = ", dayOfWeek

      call ESMF_TimeGetDayOfMonth(Time, dayOfMonth, rc)
      print *, "Day of the month = ", dayOfMonth

      ! get wall clock time
      call ESMF_TimeInit(wallClock, cal=gregorianCalendar, rc=rc)
      call ESMF_TimeGetRealTime(wallClock, rc)
      print *, "Wall Clock Time = "
      call ESMF_TimePrint(wallClock, "string", rc)

      end program ESMF_TimeEx
