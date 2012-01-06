! $Id: ESMF_CalendarEx.F90,v 1.50 2012/01/06 20:18:06 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_CalendarEx

!------------------------------------------------------------------------------
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOC
! !PROGRAM: ESMF_CalendarEx - Calendar creation examples
!
! !DESCRIPTION:
!
! This program shows examples of how to create different calendar kinds
!-----------------------------------------------------------------------------

      ! ESMF Framework module
      use ESMF
      implicit none

      ! instantiate calendars
      type(ESMF_Calendar) :: gregorianCalendar
      type(ESMF_Calendar) :: julianDayCalendar
      type(ESMF_Calendar) :: marsCalendar

      ! local variables for Get methods
      integer :: sols
      integer(ESMF_KIND_I8) :: dl
      type(ESMF_Time) :: time, marsTime
      type(ESMF_TimeInterval) :: marsTimeStep

      ! return code
      integer:: rc
!EOC

      ! result code
      integer ::  finalrc
      finalrc = ESMF_SUCCESS

!BOC
      ! initialize ESMF framework
      call ESMF_Initialize(defaultlogfilename="CalendarEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Calendar creation}

! This example shows how to create three {\tt ESMF\_Calendars}.
!EOE

!BOC
      ! create a Gregorian calendar
      gregorianCalendar = ESMF_CalendarCreate(ESMF_CALKIND_GREGORIAN, &
                                              name="Gregorian", rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! create a Julian Day calendar
      julianDayCalendar = ESMF_CalendarCreate(ESMF_CALKIND_JULIANDAY, &
                                              name="JulianDay", rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! create a Custom calendar for the planet Mars
      ! 1 Mars solar day = 24 hours, 39 minutes, 35 seconds = 88775 seconds
      ! 1 Mars solar year = 668.5921 Mars solar days = 668 5921/10000 sols/year
      ! http://www.giss.nasa.gov/research/briefs/allison_02
      ! http://www.giss.nasa.gov/tools/mars24/help/notes.html
      marsCalendar = ESMF_CalendarCreate(secondsPerDay=88775, &
                                         daysPerYear=668, &
                                         daysPerYearDn=5921, &
                                         daysPerYearDd=10000, &
                                         name="MarsCalendar", rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Calendar comparison}

! This example shows how to compare an {\tt ESMF\_Calendar} with a known
! calendar kind.
!EOE

!BOC
      ! compare calendar kind against a known type
      if (gregorianCalendar == ESMF_CALKIND_GREGORIAN) then
        print *, "gregorianCalendar is of type ESMF_CALKIND_GREGORIAN."
      else
        print *, "gregorianCalendar is not of type ESMF_CALKIND_GREGORIAN."
      end if
!EOC

!BOE
!\subsubsection{Time conversion between Calendars}

! This example shows how to convert a time from one {\tt ESMF\_Calendar}
! to another.
!EOE

!BOC
      call ESMF_TimeSet(time, yy=2004, mm=4, dd=17, &
                        calendar=gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! switch time's calendar to perform conversion
      call ESMF_TimeSet(time, calendar=julianDayCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_TimeGet(time, d_i8=dl, rc=rc)
      print *, "Gregorian date 2004/4/17 is ", dl, &
               " days in the Julian Day calendar."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Add a time interval to a time on a Calendar}

! This example shows how to increment a time using a custom {\tt ESMF\_Calendar}.
!EOE

!BOC
      ! Set a time to Mars solar year 3, sol 100
      call ESMF_TimeSet(marsTime, yy=3, d=100, &
                        calendar=marsCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! Set a 1 solar year time step
      call ESMF_TimeIntervalSet(marsTimeStep, yy=1, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! Perform the increment
      marsTime = marsTime + marsTimeStep
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! Get the result in sols (2774 = (3+1)*668.5921 + 100)
      call ESMF_TimeGet(marsTime, d=sols, rc=rc)
      print *, "For Mars, 3 solar years, 100 sols + 1 solar year = ", sols, "sols."
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
!\subsubsection{Calendar destruction}

! This example shows how to destroy three {\tt ESMF\_Calendars}.
!EOE

!BOC
      call ESMF_CalendarDestroy(julianDayCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_CalendarDestroy(gregorianCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      call ESMF_CalendarDestroy(marsCalendar, rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOC
      ! finalize ESMF framework
      call ESMF_Finalize(rc=rc)
!EOC

      if (rc.NE.ESMF_SUCCESS) finalrc = ESMF_FAILURE

      if (finalrc.EQ.ESMF_SUCCESS) then
         print *, "PASS: ESMF_CalendarEx.F90"
      else
         print *, "FAIL: ESMF_CalendarEx.F90"
      end if

!BOC
      end program ESMF_CalendarEx
!EOC
