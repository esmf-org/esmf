! $Id: ESMF_ClockEx.F90,v 1.1 2003/03/28 00:51:28 eschwab Exp $
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
      program ESMF_ClockEx

!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_ClockEx - Clock initialization and time-stepping
!
! !DESCRIPTION:
!
! This program shows an example of how to set-up a clock
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_TimeIntervalMod
      use ESMF_TimeMod
      use ESMF_CalendarMod
      use ESMF_ClockMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ClockEx.F90,v 1.1 2003/03/28 00:51:28 eschwab Exp $'
!------------------------------------------------------------------------------

      ! result code
      integer :: rc

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate timestep, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime

      integer(ESMF_IKIND_I8) :: advanceCount

      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 1 hour
      call ESMF_TimeIntervalInit(timeStep, H=1, rc=rc)

      ! initialize start time to 3/27/2003
      call ESMF_TimeInit(startTime, YR=2003, MM=3, DD=27, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 3/29/2003
      call ESMF_TimeInit(stopTime, YR=2003, MM=3, DD=29, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)

      ! time step clock from start time to stop time
      do while (.not.ESMF_ClockIsStopTime(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
      end do

      ! get the number of times the clock was advanced
      call ESMF_ClockGetAdvanceCount(clock, advanceCount, rc)

      print *, "The clock was advanced ", advanceCount, " times."
  
      end program ESMF_ClockEx
