! $Id: ESMF_TimeEx.F90,v 1.3 2003/09/04 18:57:57 cdeluca Exp $
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
      type(ESMF_Time) :: Time
      type(ESMF_Time) :: midMonth
      type(ESMF_Time) :: wallClock

      ! Temp variables for Get functions
      integer :: MM, DD, H, M
      integer(ESMF_KIND_I8) :: S
      type(ESMF_Calendar) :: cal
      character, dimension(ESMF_MAXSTR) :: Ts
      integer :: dayOfYear, dayOfWeek, dayOfMonth

      ! Result code
      integer :: rc

      ! Initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! Initialize time to 5/12/2003 2:24:45
      call ESMF_TimeInit(Time, YR=int(2003,kind=ESMF_KIND_I8), &
                       MM=5, DD=12, H=2, M=24, S=int(45,kind=ESMF_KIND_I8), &
                       cal=gregorianCalendar, rc=rc)

      call ESMF_TimePrint(Time, "string", rc)

      call ESMF_TimeGetCalendar(Time, cal, rc)
      call ESMF_CalendarPrint(cal, rc=rc)

      call ESMF_TimeGetDayOfYear(Time, dayOfYear, rc)
      print *, "Day of the year = ", dayOfYear

      call ESMF_TimeGetDayOfMonth(Time, dayOfMonth, rc)
      print *, "Day of the month = ", dayOfMonth

      call ESMF_TimeGetDayOfWeek(Time, dayOfWeek, rc)
      print *, "Day of the week = ", dayOfWeek

      call ESMF_TimeGetMidMonth(Time, midMonth, rc)
      print *
      print *, "Middle of the month = "
      call ESMF_TimePrint(midMonth, "string", rc)

      ! Get wall clock time
      call ESMF_TimeInit(wallClock, cal=gregorianCalendar, rc=rc)
      call ESMF_TimeGetRealTime(wallClock, rc)
      print *, "Wall Clock Time = "
      call ESMF_TimePrint(wallClock, "string", rc)

      print *, "ESMF_Time Example completed successfully"

      end program ESMF_TimeEx
