! $Id: ESMF_TimeEx.F90,v 1.1 2003/04/24 05:32:03 eschwab Exp $
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
      '$Id: ESMF_TimeEx.F90,v 1.1 2003/04/24 05:32:03 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate a time
      type(ESMF_Time) :: Time

      ! temp variables for Get functions
      integer :: MM, DD, H, M, S
      type(ESMF_Calendar) :: cal

      ! result code
      integer :: rc

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time to 4/1/2003 2:24:00
      call ESMF_TimeInit(Time, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=4, DD=1, H=2, M=24, cal=gregorianCalendar, rc=rc)

      call ESMF_TimeGetCalendar(Time, cal, rc)
      call ESMF_CalendarPrint(cal, rc=rc)

      end program ESMF_TimeEx
