! $Id: ESMF_AlarmEx.f,v 1.1 2002/11/15 17:01:21 jwolfe Exp $
      !\begin{verbatim}
!===============================================================================
! ESMF_Alarm F90 Unit Tests and Examples
!===============================================================================

      program main
 
      use ESMF_TimeMgmtMod
      use ESMF_AppMod

      implicit none

      integer, parameter :: START_DATE=20011128, START_SECS=43200
      integer, parameter :: STOP_DATE=20011203, STOP_SECS=1200
      integer, parameter :: STEP_DAYS=1, STEP_SECS=43200
      integer, parameter :: NUM_ITS=3

      type(ESMF_Time) :: stepSize
      type(ESMF_TimeMgr) :: timeMgrNoBase
      type(ESMF_Date) :: startDate, stopDate, currDate, prevDate      
      type(ESMF_Date) :: baseDate
      type(ESMF_Alarm) :: alarm
  
      type(ESMF_App) :: app

      logical test, isLast
      integer i, nsteps
      integer retCalDate, retDays, retSecs
      character(60) str    


      app = ESMF_AppNew()

      print *, "=================================================="
      print *, "ESMF_Alarm F90 Unit Tests and Examples"
      print *, "=================================================="

      stepSize = ESMF_TimeInit(STEP_DAYS, STEP_SECS)
      startDate = ESMF_DateInit(ESMF_GREGORIAN, START_DATE, START_SECS)
      stopDate = ESMF_DateInit(ESMF_GREGORIAN, STOP_DATE, STOP_SECS)

      alarm = ESMF_AlarmInitMonthly()

      timeMgrNoBase = ESMF_TimeMgrInit(stepSize, startDate, stopDate)

      do while (.NOT. ESMF_TimeMgrLastStep(timeMgrNoBase))
         call ESMF_TimeMgrAdvance(timeMgrNoBase)
         if(ESMF_AlarmIsOn(alarm, timeMgrNoBase)) then
           print *, "alarmIsOn"
         end if
         call ESMF_AlarmSet(alarm, .FALSE.)
      end do

      call ESMF_AppDelete(app)

      end program main !\end{verbatim}
