! $Id: ESMF_CalendarEx.F90,v 1.18 2004/01/06 16:57:26 svasquez Exp $
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
!
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
      type(ESMF_Calendar) :: julianCalendar

      ! temp variables for Get functions
      integer(ESMF_KIND_I8) :: YRl, Dl, Sl
      integer :: YR, MM, DD, D, H, M, S, rc
      type(ESMF_Time) :: timeZero
      type(ESMF_Time) :: checkTime
      type(ESMF_Time) :: yearOne
!\end{verbatim}
!EOP

      ! result code
      integer ::  finalrc
      finalrc = ESMF_SUCCESS

!BOP
!\begin{verbatim}
      !
      ! Julian Calendar
      !
      ! initialize calendar to be Julian type
      call ESMF_CalendarSet(JulianCalendar, ESMF_CAL_JULIANDAY, rc)
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
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
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
      call ESMF_TimeGet(timeZero, yr=YR, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      YR = -4713
      call ESMF_TimeSet(timeZero, yr=YR, mm=11, dd=24, &
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
      YR = 2003
      call ESMF_TimeSet(checkTime, yr=YR, mm=4, dd=17, &
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
      call ESMF_TimeGet(checkTime, yr=YR, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time1 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! oldest date for which Fliegel algorithm works
      YR = -4800
      call ESMF_TimeSet(checkTime, yr=YR, mm=3, dd=1, &
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
      call ESMF_TimeGet(checkTime, yr=YR, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time2 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! Fliegel algorithm breaks at 2/29/-4800
      YR = -4800 
      call ESMF_TimeSet(checkTime, yr=YR, mm=2, dd=29, &
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
      call ESMF_TimeGet(checkTime, yr=YR, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time3 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! Farthest future date for which Fliegel algorithm works,
      !   Julian day 106,751,991,167,300
      Dl = 1067519911  ! break up initialization,
      Dl = Dl * 100000  !   since F90 constants
      Dl = Dl + 67300   !     are 32-bit
      call ESMF_TimeSet(checkTime, d_i8=Dl, calendar=julianCalendar, rc=rc)
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
      call ESMF_TimeGet(checkTime, yr_i8=YRl, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP


      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time4 Gregorian = ", &
               YRl, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! Fliegel algorithm breaks at Julian day 106,751,991,167,301
      Dl = 1067519911  ! break up initialization,
      Dl = Dl * 100000  !   since F90 constants
      Dl = Dl + 67301   !     are 32-bit
      call ESMF_TimeSet(checkTime, d_i8=Dl, calendar=julianCalendar, rc=rc)
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
      call ESMF_TimeGet(checkTime, yr_i8=YRl, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time5 Gregorian = ", &
               YRl, "/", MM, "/", DD, " ", H, ":", M, ":", S

      !
      ! No Leap Calendar examples
      !
      call ESMF_CalendarSet(noLeapCalendar, ESMF_CAL_NOLEAP, rc)
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
      call ESMF_TimeGet(timeZero, yr=YR, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero No Leap = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      YR = -4713
      call ESMF_TimeSet(timeZero, yr=YR, mm=11, dd=24, &
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
      YR = 2004
      call ESMF_TimeSet(checkTime, yr=YR, mm=5, dd=18, &
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
      call ESMF_TimeGet(checkTime, yr=YR, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Check Time1 No Leap = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      !
      ! 360 Day Calendar examples
      !
      call ESMF_CalendarSet(day360Calendar, ESMF_CAL_360DAY, rc)
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
      call ESMF_TimeGet(timeZero, yr=YR, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)
!\end{verbatim}
!EOP

      if (rc.NE.ESMF_SUCCESS) then
          finalrc = ESMF_FAILURE
      end if

!BOP
!\begin{verbatim}
      print *, "Time Zero 360 Day = ", &
                YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      YR = -4713
      call ESMF_TimeSet(timeZero, yr=YR, mm=11, dd=24, &
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
      YR = 2005
      call ESMF_TimeSet(checkTime, yr=YR, mm=6, dd=19, &
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
      call ESMF_TimeGet(checkTime, yr=YR, mm=MM, dd=DD, h=H, m=M, s=S, rc=rc)

      print *, "Check Time1 360 Day = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S
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
