! $Id: ESMF_CalendarEx.F90,v 1.23 2004/02/11 06:51:54 eschwab Exp $
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
      program ESMF_CalendarEx

!------------------------------------------------------------------------------
!EXAMPLE        String used by test script to count examples.
!==============================================================================
!BOP
!\begin{verbatim}
! !PROGRAM: ESMF_CalendarEx - Calendar examples
!
! !DESCRIPTION:
!
! This program shows examples of how to create different calendar types.
!-----------------------------------------------------------------------------
      use ESMF_Mod
      implicit none

      ! instantiate calendars
      type(ESMF_Calendar) :: gregorianCalendar
      type(ESMF_Calendar) :: noLeapCalendar
      type(ESMF_Calendar) :: day360Calendar
      type(ESMF_Calendar) :: julianDayCalendar

      ! temp variables for Get functions
      integer(ESMF_KIND_I8) :: yyl, dl, sl
      integer :: yy, mm, dd, d, h, m, s, rc
      type(ESMF_Time) :: timeZero
      type(ESMF_Time) :: checkTime
      type(ESMF_Time) :: yearOne
      type(ESMF_CalendarType) :: calType
!\end{verbatim}
!EOP

      ! result code
      integer ::  finalrc
      finalrc = ESMF_SUCCESS

!BOP
!\begin{verbatim}
      !
      ! Julian Day Calendar
      !
      ! initialize calendar to be Julian type
      julianDayCalendar = ESMF_CalendarCreate("JulianDay", &
                                              ESMF_CAL_JULIANDAY, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      !
      ! Gregorian Calendar
      !
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
      call ESMF_CalendarGet(gregorianCalendar, calendarType=calType, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! demonstrate ESMF_CalendarType (==) overloaded operator
      if (calType == ESMF_CAL_GREGORIAN) then
          print *, "gregorianCalendar is of type ESMF_CAL_GREGORIAN."
      else
          print *, "gregorianCalendar is not of type ESMF_CAL_GREGORIAN."
      end if

      ! demonstrate ESMF_Calendar (==) overloaded operator
      if (julianDayCalendar == gregorianCalendar) then
          print *, "julianDayCalendar is the same as gregorianCalendar."
      else
          print *, "julianDayCalendar is not the same as gregorianCalendar."
      end if

      !
      ! Gregorian Calendar examples
      !
      call ESMF_TimeSet(timeZero, s=0, sN=0, sD=1, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(timeZero, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero Gregorian = ", &
               yy, "/", mm, "/", dd, " ", h, ":", m, ":", s

      yy = -4713
      call ESMF_TimeSet(timeZero, yy=yy, mm=11, dd=24, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero Gregorian -4713/11/24 = " 
      call ESMF_TimePrint(timeZero, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      yy = 2003
      call ESMF_TimeSet(checkTime, yy=yy, mm=4, dd=17, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimePrint(checkTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(checkTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time1 Gregorian = ", &
               yy, "/", mm, "/", dd, " ", h, ":", m, ":", s

      ! oldest date for which Fliegel algorithm works
      yy = -4800
      call ESMF_TimeSet(checkTime, yy=yy, mm=3, dd=1, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimePrint(checkTime, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!\end{verbatim}
!EOP
      call ESMF_TimeGet(checkTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time2 Gregorian = ", &
               yy, "/", mm, "/", dd, " ", h, ":", m, ":", s

      ! Fliegel algorithm breaks at 2/29/-4800
      yy = -4800 
      call ESMF_TimeSet(checkTime, yy=yy, mm=2, dd=29, &
                        calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_FAILURE) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimePrint(checkTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(checkTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time3 Gregorian = ", &
               yy, "/", mm, "/", dd, " ", h, ":", m, ":", s

      ! Farthest future date for which Fliegel algorithm works,
      !   Julian day 106,751,991,167,300
      dl = 1067519911  ! break up initialization,
      dl = dl * 100000  !   since F90 constants
      dl = dl + 67300   !     are 32-bit
      call ESMF_TimeSet(checkTime, d_i8=dl, calendar=julianDayCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimePrint(checkTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! switch calendar to perform conversion
      call ESMF_TimeSet(checkTime, calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(checkTime, yy_i8=yyl, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
!\end{verbatim}
!EOP


      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time4 Gregorian = ", &
               yyl, "/", mm, "/", dd, " ", h, ":", m, ":", s

      ! Fliegel algorithm breaks at Julian day 106,751,991,167,301
      dl = 1067519911  ! break up initialization,
      dl = dl * 100000  !   since F90 constants
      dl = dl + 67301   !     are 32-bit
      call ESMF_TimeSet(checkTime, d_i8=dl, calendar=julianDayCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimePrint(checkTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      ! switch calendar to perform conversion
      call ESMF_TimeSet(checkTime, calendar=gregorianCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(checkTime, yy_i8=yyl, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time5 Gregorian = ", &
               yyl, "/", mm, "/", dd, " ", h, ":", m, ":", s

      !
      ! No Leap Calendar examples
      !
      noLeapCalendar = ESMF_CalendarCreate("NoLeap", ESMF_CAL_NOLEAP, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeSet(timeZero, s=0, sN=0, sD=1, &
                        calendar=noLeapCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(timeZero, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero No Leap = ", &
               yy, "/", mm, "/", dd, " ", h, ":", m, ":", s

      yy = -4713
      call ESMF_TimeSet(timeZero, yy=yy, mm=11, dd=24, &
                        calendar=noLeapCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero No Leap -4713/11/24 = " 
      call ESMF_TimePrint(timeZero, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if


!BOP
!\begin{verbatim}
      yy = 2004
      call ESMF_TimeSet(checkTime, yy=yy, mm=5, dd=18, &
                        calendar=noLeapCalendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimePrint(checkTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(checkTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time1 No Leap = ", &
               yy, "/", mm, "/", dd, " ", h, ":", m, ":", s

      !
      ! 360 Day Calendar examples
      !
      day360Calendar = ESMF_CalendarCreate("360Day", ESMF_CAL_360DAY, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeSet(timeZero, s=0, sN=0, sD=1, &
                        calendar=day360Calendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(timeZero, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero 360 Day = ", &
                yy, "/", mm, "/", dd, " ", h, ":", m, ":", s

      yy = -4713
      call ESMF_TimeSet(timeZero, yy=yy, mm=11, dd=24, &
                        calendar=day360Calendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero 360 Day -4713/11/24 = " 
      call ESMF_TimePrint(timeZero, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      yy = 2005
      call ESMF_TimeSet(checkTime, yy=yy, mm=6, dd=19, &
                        calendar=day360Calendar, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimePrint(checkTime, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      call ESMF_TimeGet(checkTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)

      print *, "Check Time1 360 Day = ", &
               yy, "/", mm, "/", dd, " ", h, ":", m, ":", s
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
     call ESMF_CalendarDestroy(julianDayCalendar, rc)
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

!BOP
!\begin{verbatim}
     call ESMF_CalendarDestroy(noLeapCalendar, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
     call ESMF_CalendarDestroy(day360Calendar, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

     if (finalrc.EQ.ESMF_SUCCESS) then
        print *, "PASS: ESMF_CalendarEx.F90"
     else
        print *, "FAIL: ESMF_CalendarEx.F90"
     end if

!BOP
!\begin{verbatim}
      end program ESMF_CalendarEx
!\end{verbatim}
!EOP
