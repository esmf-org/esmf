! $Id: ESMF_ClockEx.F90,v 1.4 2003/04/08 20:04:44 eschwab Exp $
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
! This program shows an example of how to set-up, run, and examine a basic clock
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
      '$Id: ESMF_ClockEx.F90,v 1.4 2003/04/08 20:04:44 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate a clock 
      type(ESMF_Clock) :: clock

      ! instantiate a calendar
      type(ESMF_Calendar) :: gregorianCalendar

      ! instantiate time_step, start and stop times
      type(ESMF_TimeInterval) :: timeStep
      type(ESMF_Time) :: startTime
      type(ESMF_Time) :: stopTime

      ! temp variables for Get functions
      integer :: D
      double precision :: d_
      integer(ESMF_IKIND_I8) :: S
      type(ESMF_TimeInterval) :: time_step
      integer(ESMF_IKIND_I8) :: advanceCount

      ! result code
      integer :: rc


      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 2 days, 4 hours (6 timesteps in 13 days)
      call ESMF_TimeIntervalInit(timeStep, D=2, H=4, rc=rc)

      ! initialize start time to 4/1/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeInit(startTime, YR=2003, MM=4, DD=1, H=2, M=24, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 4/14/2003 2:24:00 ( 1/10 of a day )
      call ESMF_TimeInit(stopTime, YR=2003, MM=4, DD=14, H=2, M=24, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)

      !
      ! time step clock from start time to stop time
      !
      do while (.not.ESMF_ClockIsStopTime(clock, rc))
        call ESMF_ClockAdvance(clock, rc=rc)
      end do

      ! get clock's time_step
      call ESMF_ClockGetTimeStep(clock, time_step, rc)

      ! get time step in integer days and seconds
      call ESMF_TimeIntervalGet(time_step, D=D, S=S, rc=rc)
      print *, "Clock's timestep = ", D, " integer days, ", &
                S, " integer seconds."

      ! get time step in floating point days
      call ESMF_TimeIntervalGet(time_step, d_=d_, rc=rc)
      print *, "Clock's timestep = ", d_, " floating point days."

      ! get start time's day of the year
      call ESMF_TimeGetDayOfYear(startTime, d_, rc=rc)
      print *, "Start time's day of the year = ", d_

      ! get stop time's day of the year
      call ESMF_TimeGetDayOfYear(stopTime, d_, rc=rc)
      print *, "Stop time's day of the year = ", d_

      ! get the number of times the clock was advanced
      call ESMF_ClockGetAdvanceCount(clock, advanceCount, rc)
      print *, "The clock was advanced ", advanceCount, " times."
  
      end program ESMF_ClockEx
