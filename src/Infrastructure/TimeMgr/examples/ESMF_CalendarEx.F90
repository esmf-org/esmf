! $Id: ESMF_CalendarEx.F90,v 1.1 2003/04/19 02:03:34 eschwab Exp $
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
! !PROGRAM: ESMF_CalendarEx - Calendar examples
!
! !DESCRIPTION:
!
! This program shows an example of how to set-up, run, and examine a basic clock
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_TimeMod
      use ESMF_CalendarMod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_CalendarEx.F90,v 1.1 2003/04/19 02:03:34 eschwab Exp $'
!------------------------------------------------------------------------------

      ! instantiate calendars
      type(ESMF_Calendar) :: gregorianCalendar
      type(ESMF_Calendar) :: noLeapCalendar
      type(ESMF_Calendar) :: day360Calendar
      type(ESMF_Calendar) :: julianCalendar

      ! temp variables for Get functions
      integer :: YR, MM, DD, D, H, M, S
      type(ESMF_Time) :: timeZero
      type(ESMF_Time) :: checkTime
      type(ESMF_Time) :: yearOne

      ! result code
      integer :: rc

      !
      ! Julian Calendar
      !
      ! initialize calendar to be Julian type
      call ESMF_CalendarInit(JulianCalendar, ESMF_CAL_JULIAN, rc)

      !
      ! Gregorian Calendar
      !
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      call ESMF_TimeInit(yearOne, YR=-2, MM=1, DD=1, &
                         cal=gregorianCalendar, rc=rc)
      print *, "Year One -2/1/1 = (below) seconds since time zero -4713/11/24" 
      call ESMF_TimePrint(yearOne, rc=rc)

      call ESMF_TimeSet(yearOne, YR=-1, MM=1, DD=1, rc=rc)
      print *, "Year One -1/1/1 = (below) seconds since time zero -4713/11/24" 
      call ESMF_TimePrint(yearOne, rc=rc)

      call ESMF_TimeSet(yearOne, YR=0, MM=1, DD=1, rc=rc)
      print *, "Year One 0/1/1 = (below) seconds since time zero -4713/11/24" 
      call ESMF_TimePrint(yearOne, rc=rc)

      call ESMF_TimeSet(yearOne, YR=1, MM=1, DD=1, rc=rc)
      print *, "Year One 1/1/1 = (below) seconds since time zero -4713/11/24" 
      call ESMF_TimePrint(yearOne, rc=rc)

      call ESMF_TimeSet(yearOne, YR=2, MM=1, DD=1, rc=rc)
      print *, "Year One 2/1/1 = (below) seconds since time zero -4713/11/24" 
      call ESMF_TimePrint(yearOne, rc=rc)

      !
      ! Gregorian Calendar
      !
      call ESMF_TimeInit(timeZero, S=0, Sn=0, Sd=1, &
                         cal=gregorianCalendar, rc=rc)
      call ESMF_TimeGet(timeZero, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Time Zero Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      call ESMF_TimeSet(timeZero, YR=-4713, MM=11, DD=24, rc=rc)
      print *, "Time Zero Gregorian -4713/11/24 = " 
      call ESMF_TimePrint(timeZero, rc=rc)

      call ESMF_TimeInit(checkTime, YR=2003, MM=4, DD=17, &
                         cal=gregorianCalendar, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time1 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      call ESMF_TimeSet(checkTime, YR=-1, MM=1, DD=1, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time2 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! oldest date for which Fliegel algorithm works
      call ESMF_TimeSet(checkTime, YR=-4900, MM=3, DD=1, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time3 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! Fliegel algorithm breaks at 2/29/-4900
      call ESMF_TimeSet(checkTime, YR=-4900, MM=2, DD=29, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time4 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! Fliegel algorithm breaks at 2/28/-4900
      call ESMF_TimeSet(checkTime, YR=-4900, MM=2, DD=28, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time5 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! Farthest future date for which Fliegel algorithm works, day 536,803,342
      call ESMF_TimeInit(checkTime, D=536802342, &
                         cal=julianCalendar, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeSetCalendar(checkTime, gregorianCalendar, rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time6 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      ! Fliegel algorithm breaks at Julian day 536,802,343
      call ESMF_TimeInit(checkTime, D=536802343, &
                         cal=julianCalendar, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeSetCalendar(checkTime, gregorianCalendar, rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time7 Gregorian = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      !
      ! No Leap Calendar
      !
      call ESMF_CalendarInit(noLeapCalendar, ESMF_CAL_NOLEAP, rc)
      call ESMF_TimeInit(timeZero, S=0, Sn=0, Sd=1, &
                         cal=noLeapCalendar, rc=rc)
      call ESMF_TimeGet(timeZero, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Time Zero No Leap = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      call ESMF_TimeSet(timeZero, YR=-4713, MM=11, DD=24, rc=rc)
      print *, "Time Zero No Leap -4713/11/24 = " 
      call ESMF_TimePrint(timeZero, rc=rc)

      call ESMF_TimeInit(checkTime, YR=-98765, MM=1, DD=19, &
                         cal=noLeapCalendar, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time1 No Leap = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      call ESMF_TimeSet(checkTime, YR=2004, MM=5, DD=18, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time2 No Leap = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      call ESMF_TimeSet(checkTime, YR=-1, MM=1, DD=1, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time3 No Leap = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      !
      ! 360 Day Calendar
      !
      call ESMF_CalendarInit(day360Calendar, ESMF_CAL_360DAY, rc)
      call ESMF_TimeInit(timeZero, S=0, Sn=0, Sd=1, &
                         cal=day360Calendar, rc=rc)
      call ESMF_TimeGet(timeZero, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Time Zero 360 Day = ", &
                YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      call ESMF_TimeSet(timeZero, YR=-4713, MM=11, DD=24, rc=rc)
      print *, "Time Zero 360 Day -4713/11/24 = " 
      call ESMF_TimePrint(timeZero, rc=rc)

      call ESMF_TimeInit(checkTime, YR=-98765, MM=1, DD=19, &
                         cal=day360Calendar, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time1 360 Day = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      call ESMF_TimeSet(checkTime, YR=2005, MM=6, DD=19, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time2 360 Day = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      call ESMF_TimeSet(checkTime, YR=-1, MM=1, DD=1, rc=rc)
      call ESMF_TimePrint(checkTime, rc=rc)
      call ESMF_TimeGet(checkTime, YR=YR, MM=MM, DD=DD, H=H, M=M, S=S, rc=rc)
      print *, "Check Time3 360 Day = ", &
               YR, "/", MM, "/", DD, " ", H, ":", M, ":", S

      end program ESMF_CalendarEx
