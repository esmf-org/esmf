! $Id: ESMF_TimeMgrEx.f,v 1.1 2002/11/15 17:02:41 jwolfe Exp $
      !\begin{verbatim}
!===============================================================================
! ESMF_TimeMgr F90 Unit Tests and Examples
!===============================================================================

      program main
 
      use ESMF_TimeMgmtMod
      use ESMF_AppMod

      implicit none

      integer, parameter :: START_DATE=20011128, START_SECS=43200
      integer, parameter :: STOP_DATE=20011203, STOP_SECS=1200
      integer, parameter :: STEP_DAYS=1, STEP_SECS=43200
      integer, parameter :: NUM_ITS=3

      integer, parameter :: LONG_STARTD=19690113
      integer, parameter :: LONG_STOPD=20020214
      integer, parameter :: LONG_STEPD=0, LONG_STEPS=12*3600

      integer, parameter :: EXH_LONG_STARTD=113
      integer, parameter :: EXH_LONG_STOPD=20020214
      integer, parameter :: EXH_LONG_STEPD=0, EXH_LONG_STEPS=0.5*3600

      integer :: startd, stopd, endd, stepd, steps

      type(ESMF_Time) :: stepSize, diffSize
      type(ESMF_TimeMgr) :: timeMgrNoBase
      type(ESMF_TimeMgr) :: timeMgrNoBase_Reconstruct
      type(ESMF_Date) :: startDate, stopDate, currDate, prevDate      
      type(ESMF_Date) :: baseDate

      type(ESMF_TimeMgr) :: timeMgrG, timeMgrN
      type(ESMF_Alarm) :: printAlarmG, printAlarmN

      type(ESMF_App) :: app

      logical :: isLater
      logical test, isLast, done, bigtest
      integer i, nsteps
      integer retCalDate, retDays, retSecs
      character(60) str    

      integer calType
      integer nstep
      integer stepDays, stepSec
      integer startYYMMDD, startSec
      integer stopYYMMDD, stopSec
      integer baseYYMMDD, baseSec
      integer currYYMMDD, currSec

      integer calType1
      integer nstep1
      integer stepDays1, stepSec1
      integer startYYMMDD1, startSec1
      integer stopYYMMDD1, stopSec1
      integer baseYYMMDD1, baseSec1
      integer currYYMMDD1, currSec1

      real :: rnum

      logical exhaustive_tests
      character(80) :: test_type

      app = ESMF_AppNew()


      print *, "=================================================="
      print *, "ESMF_TimeMgr F90 Unit Tests and Examples"
      print *, "=================================================="

      call getenv('ESMF_EXHTEST', test_type)

      print *, "test_type=", test_type
      if (test_type .eq. "on") then
        exhaustive_tests = .true.
      else
        exhaustive_tests = .false.
      endif


      stepSize = ESMF_TimeInit(STEP_DAYS, STEP_SECS)
      startDate = ESMF_DateInit(ESMF_GREGORIAN, START_DATE, START_SECS)
      stopDate = ESMF_DateInit(ESMF_GREGORIAN, STOP_DATE, STOP_SECS)

!===============================================================================
! Advance, LastStep
!===============================================================================
      timeMgrNoBase = ESMF_TimeMgrInit(stepSize, startDate, stopDate)

      do while (.NOT. ESMF_TimeMgrLastStep(timeMgrNoBase))
         call ESMF_TimeMgrAdvance(timeMgrNoBase)
      end do

      currDate = ESMF_TimeMgrGetCurrDate(timeMgrNoBase)
      call ESMF_DateGet(currDate, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==20011203) .AND. (retSecs==0))
      str = "ESMF_TimeMgrGetCurrDate:  get current date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! GetPrevDate
!===============================================================================
      prevDate = ESMF_TimeMgrGetPrevDate(timeMgrNoBase)
      call ESMF_DateGet(prevDate, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==20011201) .AND. (retSecs==43200))
      str = "ESMF_TimeMgrGetPrevDate:  get previous date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! GetStartDate
!===============================================================================
      startDate = ESMF_TimeMgrGetStartDate(timeMgrNoBase)
      call ESMF_DateGet(startDate, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==START_DATE) .AND. (retSecs==START_SECS))
      str = "ESMF_TimeMgrGetStartDate: get starting date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! GetStopDate
!===============================================================================
      stopDate = ESMF_TimeMgrGetStopDate(timeMgrNoBase)
      call ESMF_DateGet(stopDate, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==STOP_DATE) .AND. (retSecs==STOP_SECS))
      str = "ESMF_TimeMgrGetStopDate: get stop date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! GetBaseDate
!===============================================================================
      baseDate = ESMF_TimeMgrGetBaseDate(timeMgrNoBase)
      call ESMF_DateGet(baseDate, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==START_DATE) .AND. (retSecs==START_SECS))
      str = "ESMF_TimeMgrGetBaseDate: get base date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! GetNStep
!===============================================================================
      nsteps = ESMF_TimeMgrGetNStep(timeMgrNoBase)
      print *, "Ret nsteps", nsteps
      test = (nsteps == 3)
      str = "ESMF_TimeMgrGetNStep:  get number of timesteps"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Set/GetStepSize
!===============================================================================
      call ESMF_TimeMgrSetStepSize(timeMgrNoBase, 4, 3200)
      call ESMF_TimeMgrGetStepSize(timeMgrNoBase, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", retSecs 
      test = ((retDays==4) .AND. (retSecs==3200))
      str = "ESMF_TimeMgrGetStepSize, ESMF_TimeMgrSetStepSize: test"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! SetCurrDate (checks to make sure that previous date is correctly set)
!===============================================================================
      call ESMF_TimeMgrSetCurrDate(timeMgrNoBase,
     &	   START_DATE, START_SECS)
      call ESMF_TimeMgrGetStepSize(timeMgrNoBase, stepSize)
      prevDate = ESMF_TimeMgrGetPrevDate(timeMgrNoBase)
      prevDate = ESMF_DateIncrement(prevDate, stepSize)
      call ESMF_DateGet(prevDate, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==START_DATE) .AND. (retSecs==START_SECS))
      str = "ESMF_TimeMgrSetCurrDate:  set curr date"
      call ESMF_ErrorTest(test, str)

      
!===============================================================================
! RestartWrite/Read
!===============================================================================
      call ESMF_TimeMgrRestartWriteIs(timeMgrNoBase,
     &     calType,
     &     nstep,
     &     stepDays, stepSec,
     &     startYYMMDD, startSec,
     &     stopYYMMDD, stopSec,
     &     baseYYMMDD, baseSec,
     &     currYYMMDD, currSec)

      print *, "Original TimeMgr:"
      print *, "CalType ", calType
      print *, "nstep = ", nstep
      print *, "stepDays ", stepDays, "stepSec", stepSec
      print *, "startYYMMDD ", startYYMMDD, "startSec", startSec
      print *, "stopYYMMDD ", stopYYMMDD, "stopSec", stopSec
      print *, "baseYYMMDD ", baseYYMMDD, "baseSec", baseSec
      print *, "currYYMMDD ", currYYMMDD, "currSec", currSec


      timeMgrNoBase_Reconstruct =
     &     ESMF_TimeMgrRestartReadIs(
     &     calType,
     &     nstep,     
     &     stepDays, stepSec,
     &     startYYMMDD, startSec,
     &     stopYYMMDD, stopSec,
     &     baseYYMMDD, baseSec,
     &     currYYMMDD, currSec)

      call ESMF_TimeMgrRestartWriteIs(timeMgrNoBase_Reconstruct,
     &     calType1,
     &     nstep1,
     &     stepDays1, stepSec1,
     &     startYYMMDD1, startSec1,
     &     stopYYMMDD1, stopSec1,
     &     baseYYMMDD1, baseSec1,
     &     currYYMMDD1, currSec1)

      print *, "Reconstructed TimeMgr:"
      print *, "CalType ", calType1
      print *, "nstep = ", nstep1
      print *, "stepDays ", stepDays1, "stepSec", stepSec1
      print *, "startYYMMDD ", startYYMMDD1, "startSec", startSec1
      print *, "stopYYMMDD ", stopYYMMDD1, "stopSec", stopSec1
      print *, "baseYYMMDD ", baseYYMMDD1, "baseSec", baseSec1
      print *, "currYYMMDD ", currYYMMDD1, "currSec", currSec1

      test = (calType==calType1 )
     &     .and. (nstep==nstep1)
     &     .and. (stepDays==stepDays1) .and. (stepSec==stepSec1)
     &     .and. (startYYMMDD==startYYMMDD1)
     &	   .and. (startSec==startSec1)
     &     .and. (stopYYMMDD==stopYYMMDD1) .and. (stopSec==stopSec1)
     &     .and. (baseYYMMDD==baseYYMMDD1) .and. (baseSec==baseSec1)
     &     .and. (currYYMMDD==currYYMMDD1) .and. (currSec==currSec1)
      str = "ESMF_TimeMgrRestartWrite, ESMF_TimeMgrRestartRead: test"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Exhaustive test.
! Set start to to a very early date.
! Set stop date to a very late date.
! This test creates a Gregorian and NO-Leap calendar and advances each
! by a small time-step.  At each step the current and previous date
! is calculated, and the timestep is retrieved.  The current and previous
! are diff'ed and the result is tested against the timestep (to which it
! should equate).  For variety, the timestep is randomly adjusted at 
! each step.
!===============================================================================

      if (exhaustive_tests) then
        startd = EXH_LONG_STARTD
        stopd  = EXH_LONG_STOPD
        stepd  = EXH_LONG_STEPD
        steps  = EXH_LONG_STEPS
      else
        startd = LONG_STARTD
        stopd  = LONG_STOPD
        stepd  = LONG_STEPD
        steps  = LONG_STEPS
      endif

      printAlarmG = ESMF_AlarmInitYearly()
      printAlarmN = ESMF_AlarmInitYearly()

      stepSize = ESMF_TimeInit(stepd, steps)

! Gregorian time manager
      startDate = ESMF_DateInit(ESMF_GREGORIAN, startd, 0)
      stopDate = ESMF_DateInit(ESMF_GREGORIAN, stopd, 0)
      timeMgrG = ESMF_TimeMgrInit(stepSize, startDate, stopDate)

! No-Leap time manager
      startDate = ESMF_DateInit(ESMF_NO_LEAP, startd, 0)
      stopDate = ESMF_DateInit(ESMF_NO_LEAP, stopd, 0)
      timeMgrN = ESMF_TimeMgrInit(stepSize, startDate, stopDate)

      done = .false.
      bigtest = .true.

      do while(.not. done)


        if (.NOT. ESMF_TimeMgrLastStep(timeMgrG)) then
          call ESMF_TimeMgrAdvance(timeMgrG)
          currDate = ESMF_TimeMgrGetCurrDate(timeMgrG)
          prevDate = ESMF_TimeMgrGetPrevDate(timeMgrG)
          call ESMF_TimeMgrGetStepSize(timeMgrG, stepSize)
          call ESMF_DateDiff(prevDate, currDate, diffSize, isLater)
          call ESMF_TimeGet(diffSize, stepDays, stepSec)
          call ESMF_TimeGet(stepSize, stepDays1, stepSec1)
          if ((stepDays.ne.stepDays1) .or. (stepSec.ne.stepSec1)) then
             print *, "FAIL: Gregorian stepsize != prev-curr"
             bigtest = .false.
          endif
          call ESMF_DateGet(currDate, retCalDate, retSecs)
          if (ESMF_AlarmIsOn(printAlarmG, timeMgrG)) then
            print *, "Gregorian date:", retCalDate
          endif

          call random_number(rnum)
          rnum = rnum*5
          stepDays = rnum
          call random_number(rnum)
          rnum = rnum*24*3600
          stepSec = rnum
          call ESMF_TimeSet(stepSize, stepDays, stepSec)
          call ESMF_TimeMgrSetStepSize(timeMgrG, stepSize)
        endif 

        if (.NOT. ESMF_TimeMgrLastStep(timeMgrN)) then
          call ESMF_TimeMgrAdvance(timeMgrN)
          currDate = ESMF_TimeMgrGetCurrDate(timeMgrN)
          prevDate = ESMF_TimeMgrGetPrevDate(timeMgrN)
          call ESMF_TimeMgrGetStepSize(timeMgrN, stepSize)
          call ESMF_DateDiff(prevDate, currDate, diffSize, isLater)
          call ESMF_TimeGet(diffSize, stepDays, stepSec)
          call ESMF_TimeGet(stepSize, stepDays1, stepSec1)
          if ((stepDays.ne.stepDays1) .or. (stepSec.ne.stepSec1)) then
             print *, "FAIL: No-Leap stepsize != prev-curr"
             bigtest = .false.
          endif
          call ESMF_DateGet(currDate, retCalDate, retSecs)
          if (ESMF_AlarmIsOn(printAlarmN, timeMgrN)) then
            print *, "No-Leap date:", retCalDate
          endif

          call random_number(rnum)
          rnum = rnum*5
          stepDays = rnum
          call random_number(rnum)
          rnum = rnum*24*3600
          stepSec = rnum
          call ESMF_TimeSet(stepSize, stepDays, stepSec)
          call ESMF_TimeMgrSetStepSize(timeMgrN, stepSize)
        endif

      done = (ESMF_TimeMgrLastStep(timeMgrG) .and.
     & ESMF_TimeMgrLastStep(timeMgrN))
      end do

      if (bigtest) then
        print *, "PASS: Long TimeMgr run."
      else
        print *, "FAIL: Long TimeMgr run."
      endif

      call ESMF_AppDelete(app)
      
      end program main !\end{verbatim}


