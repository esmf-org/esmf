! $Id: ESMF_DateEx.f,v 1.1 2002/11/15 17:01:47 jwolfe Exp $
      !\begin{verbatim}
!===============================================================================
! ESMF_Date F90 Unit Tests and Examples
!===============================================================================
      program main
 
      use ESMF_TimeMgmtMod
      use ESMF_AppMod
      implicit none

      integer, parameter :: START_DATE=20011128, START_SECS=43200
      integer, parameter :: STOP_DATE=20041201, STOP_SECS=1200
      integer, parameter :: DAY_INC=(3*365)+5, SEC_INC=86400+43200+1
      integer, parameter :: MAX_IS_LATER_IT=3000

      logical exhaustive_tests

      integer, parameter :: LONG_STARTD=19690113
      integer, parameter :: LONG_ENDY=2001
      integer, parameter :: LONG_HOURINC=23   ! Must be <= 24 so  all days are covered.

! Parameters for exhaustive tests
      integer, parameter :: EXH_LONG_STARTD=-47130113
      integer, parameter :: EXH_LONG_ENDY=214000
      integer, parameter :: EXH_LONG_HOURINC=6   ! Must be <= 24 so  all days are covered.
 
      integer startd, endy, hourinc

      integer :: retCalDate, retDays, retSecs
      integer :: retCalDate1, retDays1, retSecs1
      integer :: numIter, numLeap, lastLeap, curYear
      integer :: dayOfYear
      logical :: test, isLater, isLater1, done, bigtest
      type(ESMF_Date) :: startDateG, stopDateG, retDateG, origDateG
      type(ESMF_Date) :: startDateN, stopDateN, retDateN, origDateN
      type(ESMF_Date) :: copy
      type(ESMF_Time) :: incTime, retTime, retTime1
      type(ESMF_Date) :: calDayTester
      type(ESMF_App) :: app
      real(8) :: floatDay, floatTest
      character(60) :: str
      character(80) :: test_type

      app = ESMF_AppNew()

      print *, "=================================================="
      print *, "ESMF_Date F90 Unit Tests and Examples"
      print *, "=================================================="

      call getenv('ESMF_EXHTEST', test_type)

      print *, "test_type=", test_type
      if (test_type .eq. "on") then
        exhaustive_tests = .true.
      else
        exhaustive_tests = .false.
      endif

!===============================================================================
! Initializations
!===============================================================================
      retTime = ESMF_TimeInit()
      incTime = ESMF_TimeInit()
      retDateG = ESMF_DateInit() 
      retDateN = ESMF_DateInit() 

!===============================================================================
! Test DateInit, DateGet, DateCopyInit: Gregorian
!===============================================================================
      startDateG = ESMF_DateInit(ESMF_GREGORIAN,
     &		 START_DATE, START_SECS) 
      stopDateG = ESMF_DateInit(ESMF_GREGORIAN, STOP_DATE, STOP_SECS)
      copy = ESMF_DateCopyInit(stopDateG)
      call ESMF_DateGet(startDateG, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==START_DATE) .AND. (retSecs==START_SECS))
      str = "ESMF_DateInit, ESMF_DateGet:  init date and get attr"
      call ESMF_ErrorTest(test, str)
      call ESMF_DateGet(copy, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==STOP_DATE) .AND. (retSecs==STOP_SECS))
      str = "ESMF_DateCopyInit(Gregorian):  init date and get attr"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateInit, DateGet, DateCopyInit: No Leap
!===============================================================================
      startDateN = ESMF_DateInit(ESMF_NO_LEAP,
     &		 START_DATE, START_SECS) 
      stopDateN = ESMF_DateInit(ESMF_NO_LEAP, STOP_DATE, STOP_SECS)
      copy = ESMF_DateCopyInit(stopDateN)
      call ESMF_DateGet(startDateN, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==START_DATE) .AND. (retSecs==START_SECS))
      str = "ESMF_DateInit, ESMF_DateGet:  init date and get attr"
      call ESMF_ErrorTest(test, str)
      call ESMF_DateGet(copy, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==STOP_DATE) .AND. (retSecs==STOP_SECS))
      str = "ESMF_DateCopyInit(No Leap):  init date and get attr"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateCopy: Gregorian
!===============================================================================
      call ESMF_DateSet(stopDateG, ESMF_GREGORIAN, STOP_DATE, STOP_SECS)
      copy = ESMF_DateInit()
      call ESMF_DateCopy(copy, stopDateG)
      call ESMF_DateGet(copy, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==STOP_DATE) .AND. (retSecs==STOP_SECS))
      str = "ESMF_DateCopy(Gregorian):  Copy date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateCopy: No Leap
!===============================================================================
      call ESMF_DateSet(stopDateN, ESMF_NO_LEAP, STOP_DATE, STOP_SECS)
      copy = ESMF_DateInit()
      call ESMF_DateCopy(copy, stopDateN)
      call ESMF_DateGet(copy, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==STOP_DATE) .AND. (retSecs==STOP_SECS))
      str = "ESMF_DateCopy(No Leap):  Copy date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateIncrementSec: Gregorian
!===============================================================================
      call ESMF_DateSet(startDateG,
     &  ESMF_GREGORIAN, START_DATE, START_SECS)
      retDateG = ESMF_DateIncrementSec(startDateG, SEC_INC)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==20011130) .AND. (retSecs==1))
      str = 
     &  "ESMF_DateIncrementSec(Gregorian):  increment date by seconds"  
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateIncrementSec: No Leap
!===============================================================================
      call ESMF_DateSet(startDateN,
     &  ESMF_NO_LEAP, START_DATE, START_SECS)
      retDateN = ESMF_DateIncrementSec(startDateN, SEC_INC)
      call ESMF_DateGet(retDateN, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==20011130) .AND. (retSecs==1))
      str = 
     &  "ESMF_DateIncrementSec(No Leap):  increment date by seconds"  
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateIncrementDay: Gregorian
!===============================================================================
      call ESMF_DateSet(startDateG,
     &  ESMF_GREGORIAN, START_DATE, START_SECS)
      retDateG = ESMF_DateIncrementDay(startDateG, DAY_INC)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==20041202) .AND. (retSecs==START_SECS))
      str = 
     &  "ESMF_DateIncrementDay(Gregorian):  increment date by days"  
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateIncrementDay: No Leap
!===============================================================================
      call ESMF_DateSet(startDateN,
     &  ESMF_NO_LEAP, START_DATE, START_SECS)
      retDateN = ESMF_DateIncrementDay(startDateN, DAY_INC)
      call ESMF_DateGet(retDateN, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs 
      test = ((retCalDate==20041203) .AND. (retSecs==START_SECS))
      str = 
     &  "ESMF_DateIncrementDay(No Leap):  increment date by days"  
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateIncrement: Gregorian
!===============================================================================
      incTime = ESMF_TimeInit(DAY_INC, SEC_INC) 
      call ESMF_DateSet(startDateG,
     &  ESMF_GREGORIAN, START_DATE, START_SECS)
      retDateG = ESMF_DateIncrement(startDateG, incTime)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs
      test = ((retCalDate==20041204) .AND. (retSecs==1))
      str = "ESMF_DateIncrement(Gregorian):  increment date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateIncrement: No Leap
!===============================================================================
      incTime = ESMF_TimeInit(DAY_INC, SEC_INC) 
      call ESMF_DateSet(startDateN,
     &  ESMF_NO_LEAP, START_DATE, START_SECS)
      retDateN = ESMF_DateIncrement(startDateN, incTime)
      call ESMF_DateGet(retDateN, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs
      test = ((retCalDate==20041205) .AND. (retSecs==1))
      str = "ESMF_DateIncrement(No Leap):  increment date"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateDecrement: Gregorian
!===============================================================================
      incTime = ESMF_TimeInit(DAY_INC, SEC_INC) 
      call ESMF_DateSet(startDateG,
     &  ESMF_GREGORIAN, START_DATE, START_SECS)
      retDateG = ESMF_DateDecrement(startDateG, incTime)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs
      test = ((retCalDate==19981122) .AND. (retSecs==86399))
      str = "ESMF_DateDecrement:  decrement date in place"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateDecrement: No Leap
!===============================================================================
      incTime = ESMF_TimeInit(DAY_INC, SEC_INC) 
      call ESMF_DateSet(startDateN,
     &  ESMF_NO_LEAP, START_DATE, START_SECS)
      retDateN = ESMF_DateDecrement(startDateN, incTime)
      call ESMF_DateGet(retDateN, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs
      test = ((retCalDate==19981121) .AND. (retSecs==86399))
      str = "ESMF_DateDecrement(No Leap):  decrement date in place"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateDiff: Gregorian
!===============================================================================
      call ESMF_DateSet(startDateG,
     &  ESMF_GREGORIAN, START_DATE, START_SECS)
      call ESMF_DateSet(stopDateG,
     &  ESMF_GREGORIAN, STOP_DATE, STOP_SECS)
      call ESMF_DateDiff(startDateG, stopDateG, retTime, isLater)
      call ESMF_TimeGet(retTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", 
     &         retSecs, "isLater", isLater 
      test = ((retDays==1098) .AND. (retSecs==44400) 
     &       .AND. (isLater))
      str = "ESMF_DateDiff(Gregorian):  diff of two dates"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Test DateDiff: No Leap
!===============================================================================
      call ESMF_DateSet(startDateN,
     &  ESMF_NO_LEAP, START_DATE, START_SECS)
      call ESMF_DateSet(stopDateN,
     &  ESMF_NO_LEAP, STOP_DATE, STOP_SECS)
      call ESMF_DateDiff(startDateN, stopDateN, retTime, isLater)
      call ESMF_TimeGet(retTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", 
     &         retSecs, "isLater", isLater 
      test = ((retDays==1097) .AND. (retSecs==44400) 
     &       .AND. (isLater))
      str = "ESMF_DateDiff(No Leap):  diff of two dates"
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Date Is Later: Gregorian
!   Creates a Date at stop time, then decrements this in a loop
!   until it reaches start time.
!===============================================================================
      call ESMF_DateSet(startDateG,
     &  ESMF_GREGORIAN, START_DATE, START_SECS)
      call ESMF_DateSet(stopDateG,
     &  ESMF_GREGORIAN, STOP_DATE, 0)
      incTime = ESMF_TimeInit(0, 43200)
      call ESMF_DateIsLater(startDateG, stopDateG, isLater)
      numIter = 0
      do while(isLater)
        stopDateG = ESMF_DateDecrement(stopDateG, incTime)
        call ESMF_DateGet(stopDateG, retCalDate, retSecs)
!        print *, "Ret date ", retCalDate, " Ret secs ", retSecs
        call ESMF_DateIsLater(startDateG, stopDateG, isLater)

! Keep this test from Looping forever.
        numIter = numIter + 1
        if (numIter .gt. MAX_IS_LATER_IT) then 
           isLater = .false.
        endif       
      end do

      call ESMF_DateGet(stopDateG, retCalDate, retSecs)
      print *, "AT END: Ret date ", retCalDate, " Ret secs ", retSecs
      if (numIter .le. MAX_IS_LATER_IT) then
        test = ((retCalDate==START_DATE).and.(retSecs==START_SECS))
        str = "ESMF_DateIsLater(Gregorian):  (true result)"
      else
        test = .false.
        str =
     &  "(numIter),ESMF_DateIsLater(Gregorian): (true result)"
      endif
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Date Is Later: No Leap
!   Creates a Date at stop time, then decrements this in a loop
!   until it reaches start time.
!===============================================================================
      call ESMF_DateSet(startDateN,
     &  ESMF_NO_LEAP, START_DATE, START_SECS)
      call ESMF_DateSet(stopDateN,
     &  ESMF_NO_LEAP, STOP_DATE, 0)
      incTime = ESMF_TimeInit(0, 43200)
      call ESMF_DateIsLater(startDateN, stopDateN, isLater)
      numIter = 0
      do while(isLater)
        stopDateN = ESMF_DateDecrement(stopDateN, incTime)
        call ESMF_DateGet(stopDateN, retCalDate, retSecs)
!        print *, "Ret date ", retCalDate, " Ret secs ", retSecs
        call ESMF_DateIsLater(startDateN, stopDateN, isLater)

! Keep this test from Looping forever.
        numIter = numIter + 1
        if (numIter .gt. MAX_IS_LATER_IT) then 
           isLater = .false.
        endif       
      end do

      call ESMF_DateGet(stopDateN, retCalDate, retSecs)
      print *, "AT END: Ret date ", retCalDate, " Ret secs ", retSecs
      if (numIter .le. MAX_IS_LATER_IT) then
        test = ((retCalDate==START_DATE).and.(retSecs==START_SECS))
        str = "ESMF_DateIsLater(No Leap):  (true result)"
      else
        test = .false.
        str =
     &  "(numIter),ESMF_DateIsLater(No Leap): (true result)"
      endif
      call ESMF_ErrorTest(test, str)


!===============================================================================
! Test Increment Month, invalid day: Gregorian
!===============================================================================
      call ESMF_DateSet(startDateG, ESMF_GREGORIAN, 20011031, 0)

      retDateG = ESMF_DateIncrementMonth(startDateG, 4)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs
      test = ((retCalDate==20020228) .AND. (retSecs==0))
      str = "ESMF_DateIncrementMonth(Gregorian):  handle invalid day"
      call ESMF_ErrorTest(test, str)      

!===============================================================================
! Test Increment Month, invalid day: No Leap
!===============================================================================
      call ESMF_DateSet(startDateN, ESMF_NO_LEAP, 20011031, 0)

      retDateG = ESMF_DateIncrementMonth(startDateN, 4)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs
      test = ((retCalDate==20020228) .AND. (retSecs==0))
      str = "ESMF_DateIncrementMonth(No Leap):  handle invalid day"
      call ESMF_ErrorTest(test, str) 

!===============================================================================
! Test Increment Year, invalid day: Gregorian
!===============================================================================
      call ESMF_DateSet(startDateG, ESMF_GREGORIAN, 20040229, 0)
      retDateG = ESMF_DateIncrementYear(startDateG, 1)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Ret date ", retCalDate, " Ret secs ", retSecs
      test = ((retCalDate==20050228) .AND. (retSecs==0))
      str = "ESMF_DateIncrementYear(Gregorian):  handle invalid day"
      call ESMF_ErrorTest(test, str)      

!===============================================================================
! Test Increment Year, invalid day: No Leap 
! This test does not apply to No Leap calendar.
!===============================================================================


!===============================================================================
! GetFltDayOfYear: Gregorian
!===============================================================================
      calDayTester = ESMF_DateInit(ESMF_GREGORIAN,
     &	START_DATE,START_SECS)
      print *, "Calendar Day Test"
      call ESMF_DatePrint(calDayTester)
      floatDay = ESMF_DateGetFltDayOfYear(calDayTester)
      call ESMF_DateGet(calDayTester, retCalDate, retSecs)
      retDays = ESMF_DateGetDayOfYear(calDayTester)
      print *, "float days = ", floatDay
      print *, "floor(floatDay) = ", floor(floatDay)
      print *, "secs=", (floatDay - floor(floatDay))
      floatTest = retDays + (retSecs / 86400.)
      print *, "Local calculation got:", floatTest
      print *, "Difference: ", (floatTest - floatDay)
      test = (abs(floatTest - floatDay) < 0.0000001)
      str = "ESMF_DateFltDayOfYear(Gregorian):  return days.seconds"
      call ESMF_ErrorTest(test, str)      

!===============================================================================
! GetFltDayOfYear: No Leap
!===============================================================================
      calDayTester = ESMF_DateInit(ESMF_NO_LEAP,
     &	START_DATE,START_SECS)
      print *, "Calendar Day Test"
      call ESMF_DatePrint(calDayTester)
      floatDay = ESMF_DateGetFltDayOfYear(calDayTester)
      call ESMF_DateGet(calDayTester, retCalDate, retSecs)
      retDays = ESMF_DateGetDayOfYear(calDayTester)
      print *, "float days = ", floatDay
      print *, "floor(floatDay) = ", floor(floatDay)
      print *, "secs=", (floatDay - floor(floatDay))
      floatTest = retDays + (retSecs / 86400.)
      print *, "Local calculation got:", floatTest
      print *, "Difference: ", (floatTest - floatDay)
      test = (abs(floatTest - floatDay) < 0.0000001)
      str = "ESMF_DateFltDayOfYear(No Leap):  return days.seconds"
      call ESMF_ErrorTest(test, str)      

!===============================================================================
! ESMF_DatePrint
!===============================================================================
      call ESMF_DatePrint(startDateG)


!===============================================================================
! Limit checking.
!===============================================================================

!===============================================================================
! Verify Lower Gregorian calendar limits.
!===============================================================================     

      call ESMF_DateSet(retDateG, ESMF_GREGORIAN, -47131125, 0)
      incTime = ESMF_TimeInit(0, 24*3600)
      retDateG = ESMF_DateIncrement(retDateG, incTime)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Lower Limit, retCalDate=", retCalDate
      print *, "Lower Limit, retSecs=", retSecs
      test = ((retCalDate==-47131126).and.(retSecs==0))      
      str = "Date Lower Limits(Gregorian): Handle lower limit."
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Verify Upper Gregorian calendar limits.
!===============================================================================     

      call ESMF_DateSet(retDateG, ESMF_GREGORIAN, 2147481231, 0)
      incTime = ESMF_TimeInit(0, 24*3600)
      retDateG = ESMF_DateDecrement(retDateG, incTime)
      call ESMF_DateGet(retDateG, retCalDate, retSecs)
      print *, "Upper Limit, retCalDate=", retCalDate
      print *, "Upper Limit, retSecs=", retSecs
      test = ((retCalDate==2147481230).and.(retSecs==0))      
      str = "Date Upper Limits(Gregorian): Handle upper limit."
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Verify Lower No Leap calendar limits.
!===============================================================================     

      call ESMF_DateSet(retDateN, ESMF_NO_LEAP, -2147480101, 0)
      incTime = ESMF_TimeInit(0, 24*3600)
      retDateN = ESMF_DateIncrement(retDateN, incTime)
      call ESMF_DateGet(retDateN, retCalDate, retSecs)
      print *, "Lower Limit, retCalDate=", retCalDate
      print *, "Lower Limit, retSecs=", retSecs
      test = ((retCalDate==-2147480102).and.(retSecs==0))      
      str = "Date Lower Limits(No Leap): Handle lower limit."
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Verify Upper No Leap calendar limits.
!===============================================================================     

      call ESMF_DateSet(retDateN, ESMF_NO_LEAP, 2147481231, 0)
      incTime = ESMF_TimeInit(0, 24*3600)
      retDateN = ESMF_DateDecrement(retDateN, incTime)
      call ESMF_DateGet(retDateN, retCalDate, retSecs)
      print *, "Upper Limit, retCalDate=", retCalDate
      print *, "Upper Limit, retSecs=", retSecs
      test = ((retCalDate==2147481230).and.(retSecs==0))      
      str = "Date Upper Limits(No Leap): Handle upper limit."
      call ESMF_ErrorTest(test, str)

!===============================================================================
! Extended test
!
! We start with a Gregorian and a No-Leap calendar at the same date.
! Next, we begin adding twelve hours to the date.  At each interval we 
! take the difference between the current time and the start time 
! for both calendars.  We also check that the yymmdd and seconds agree
! for both calendars.  This will be the case until we hit a leap year.
! When we hit a leap year, in particular Feb 29 on a leap year, we
! skip the Gregorian calendar through it by adding one day.  This makes
! the yymmdd agree again, but we must compensate for the diff by adding
! numLeap years (encountered) into the diff from the start date for the
! NO-LEAP calendar.
!===============================================================================

      if (exhaustive_tests) then
        startd = EXH_LONG_STARTD
        endy = EXH_LONG_ENDY
        hourinc = EXH_LONG_HOURINC
      else
        startd = LONG_STARTD
        endy = LONG_ENDY
        hourinc = LONG_HOURINC
      endif

      call ESMF_DateSet(startDateG, ESMF_GREGORIAN, startd, 0)
      call ESMF_DateSet(startDateN, ESMF_NO_LEAP, startd, 0)

      incTime = ESMF_TimeInit(0, hourinc*3600)
      call ESMF_DateCopy(retDateG, startDateG)
      call ESMF_DateCopy(retDateN, startDateN)
      done = .false.
      bigtest = .true.
      numLeap = 0
      lastLeap = 1969 ! Should not be a valid leap year
      do while(.not. done)
        call ESMF_DateGet(calDayTester, retCalDate, retSecs)
        retDateN = ESMF_DateIncrement(retDateN, incTime)
        retDateG = ESMF_DateIncrement(retDateG, incTime)

! Difference the dates from start.  Should be the same.
        call ESMF_DateDiff(startDateG, retDateG, retTime, isLater)
        call ESMF_DateDiff(startDateN, retDateN, retTime1, isLater)
        call ESMF_TimeGetIS(retTime, retDays, retSecs)
        call ESMF_TimeGetIS(retTime1, retDays1, retSecs1)
        if ( ((retDays1+numLeap).ne.retDays) .or.
     & (retSecs.ne.retSecs1)) then
          print *, "Date diff is wrong"
          bigtest = .false.
        endif

! Decrement each date by time passed.  This should be start date
        origDateG = ESMF_DateDecrement(retDateG, retTime)
        origDateN = ESMF_DateDecrement(retDateN, retTime1)

        call ESMF_DateGet(origDateG, retCalDate, retSecs)
        if ((retCalDate.ne.startd) .or. (retSecs.ne.0)) then
          print *, "Decrement Gregorian Failed"
          bigtest = .false.
        endif
        call ESMF_DateGet(origDateN, retCalDate, retSecs)
        if ((retCalDate.ne.startd) .or. (retSecs.ne.0)) then
          print *, "Decrement NO-LEAP Failed"
          bigtest = .false.
          print *, "retCalDate:", retCalDate, "retSecs:", retSecs
          call ESMF_DateGet(retDateN, retCalDate1, retSecs1)
          print *, "Ret dateN ", retCalDate1, " Ret secs ", retSecs1
          print *, "N: retDays1:", retDays1, "retSecs1:", retSecs1
        endif
        

! Get the cal Dates.  Adjust for leap years passed and compare.
        call ESMF_DateGet(retDateG, retCalDate, retSecs)
        curYear = retCalDate / 10000
        if (
     &     ((mod((curYear), 4)==0) .and. (mod(curYear,100).ne.0))
     &     .or.
     &    ((mod((curYear), 4)==0) .and. (mod(curYear,400)==0))
     &    ) then
           dayOfYear = ESMF_DateGetDayOfYear(retDateG)
           if (dayOfYear .eq. 60) then ! Skip through the leap day
             retDateG = ESMF_DateIncrementDay(retDateG, 1) 
             call ESMF_DateGet(retDateG, retCalDate, retSecs)
             numLeap = numLeap + 1
           endif 
           if (curYear .ne. lastLeap) then ! Leading edge
             lastLeap = curYear
             print *, "Leap Year:", retCalDate, "numLeap:", numLeap
           endif
        endif
        call ESMF_DateGet(retDateN, retCalDate1, retSecs1)

        if ((retCalDate1.ne.retCalDate) .or. (retSecs1.ne.retSecs)) then
          print *, "Dates no longer agree"
          bigtest = .false.        
        endif

      if (curYear .ge. endy) done = .true.

      end do

      if (bigtest) then
        print *, "PASS: Long date test."
      else
        print *, "FAIL: Long date test."
      endif

      call ESMF_AppDelete(app)

      end program main !\end{verbatim}
