! $Id: ESMF_TimeEx.f,v 1.1 2002/11/15 17:02:14 jwolfe Exp $
      !\begin{verbatim}
!===============================================================================
! ESMC_Time F90 Unit Tests and Examples
!===============================================================================
      program main
 
      use ESMF_TimeMgmtMod
      use ESMF_AppMod
      implicit none

      integer, parameter :: START_DAYS=5, START_SECS=43200
      integer, parameter :: STOP_DAYS=12, STOP_SECS=1200
      integer, parameter :: DAY_INC=2, SEC_INC=1000
      integer, parameter :: SEC_OVER_SID=100000

      integer :: rc
      integer :: retDays, retSecs
      logical :: test, isLater
      real(8) :: retRealDays
      character(60) :: str

      type(ESMF_Time) :: startTime, stopTime, retTime
      type(ESMF_App) :: app

      app = ESMF_AppNew()

      print *, "=================================================="
      print *, "ESMF_TimeMgr F90 Unit Tests and Examples"
      print *, "=================================================="

      startTime = ESMF_TimeInit(START_DAYS, START_SECS) 

      stopTime = ESMF_TimeInit(STOP_DAYS, STOP_SECS) 
      retTime = ESMF_TimeInit() 

      call ESMF_TimeGet(startTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", retSecs 
      test = ((retDays == START_DAYS) .AND. (retSecs == START_SECS))
      str = "ESMF_TimeInit, ESMF_TimeGet:  init time and get attr"
      call ESMF_ErrorTest(test, str)

      retRealDays = ESMF_TimeGetDays(startTime)
      print *, "Ret real days ", retRealDays 
      test = (abs(retRealDays - 5.5) < epsilon(5.5) )
      str = "ESMF_TimeGetDays:  get time value as real days"
      call ESMF_ErrorTest(test, str)

      retTime = ESMF_TimeIncrement(startTime, DAY_INC, SEC_INC)
      call ESMF_TimeGet(retTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", retSecs 
      test = ((retDays == 7) .AND. (retSecs == 44200))
      str = "ESMF_TimeIncrement:  increment time, sec inc < 86400"  
      call ESMF_ErrorTest(test, str)

      call ESMF_TimeDiff(startTime, stopTime, retTime, isLater)
      call ESMF_TimeGet(retTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", retSecs 
      test = ((retDays == 6) .AND. (retSecs == 44400) .AND. 
     &        (isLater)) 
      str = "ESMF_TimeDiff:  take time difference, isLater is true" 
      call ESMF_ErrorTest(test, str)

      call ESMF_TimeDiff(stopTime, startTime, retTime, isLater)
      call ESMF_TimeGet(retTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", retSecs 
      test = ((retDays == 6) .AND. (retSecs == 44400) .AND. 
     &        (.NOT. isLater))
      str = "ESMF_TimeDiff:  take time difference, isLater is false" 
      call ESMF_ErrorTest(test, str)

      call ESMF_TimeSet(retTime, DAY_INC, SEC_OVER_SID)
      call ESMF_TimeGet(retTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", retSecs 
      test = ((retDays == 3) .AND. (retSecs == 13600))
      str = "ESMF_TimeSet:  set time, sec > 86400"  
      call ESMF_ErrorTest(test, str)

      retTime = ESMF_TimeDecrement(stopTime, DAY_INC, 
     &          SEC_OVER_SID)
      call ESMF_TimeGet(retTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", retSecs 
      test = ((retDays == 8) .AND. (retSecs == 74000))
      str = "ESMF_TimeDecrement:  decrement time, sec > 86400"  
      call ESMF_ErrorTest(test, str)

      call ESMF_TimeSet(retTime, 0, 3600)
      retTime = ESMF_TimeDecrement(retTime, 0, 3600)
      call ESMF_TimeGet(retTime, retDays, retSecs)
      print *, "Ret days ", retDays, " Ret secs ", retSecs 
      test = ((retDays == 0) .AND. (retSecs == 0))
      str = "ESMF_TimeDecrement:  decrement time down to 0"  
      call ESMF_ErrorTest(test, str)

!      print *, "Test Error Handler:"
!      call ESMF_ErrHandlerSetType(ESMF_ERR_RETURN)
!      call ESMF_TimeSet(retTime, -2, SEC_OVER_SID, rc)
!      call ESMF_ErrPrint(rc)

!      print *, "Test Error Handler:"
!      call ESMF_ErrHandlerSetType(ESMF_ERR_EXIT)
!      call ESMF_TimeSet(retTime, -2, SEC_OVER_SID, rc)
!      call ESMF_ErrPrint(rc)

      print *, "Test Print Method:"
      call ESMF_TimePrint(startTime)

      call ESMF_AppDelete(app)

      end program main ! \end{verbatim}









