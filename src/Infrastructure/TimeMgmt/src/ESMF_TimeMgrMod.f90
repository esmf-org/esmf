! $Id: ESMF_TimeMgrMod.f90,v 1.3 2002/12/11 16:06:37 nscollins Exp $
      module ESMF_TimeMgrMod
!===============================================================================
!BOP
! !MODULE: ESMF_TimeMgrMod 
!
! !USES:
	use ESMF_DateMod
! nsc - the return codes are in the following mod now.
        use ESMF_TimeMgmtMod
!
! !PUBLIC TYPES:
      implicit none

      type ESMF_TimeMgr
        private
        sequence
        integer(8) nstep
        type(ESMF_Time) stepSize
        type(ESMF_Date) startDate
        type(ESMF_Date) stopDate
        type(ESMF_Date) baseDate
        type(ESMF_Date) currDate
        type(ESMF_Date) prevDate
      end type ESMF_TimeMgr
!
! !PUBLIC MEMBER FUNCTIONS:
!     ESMF_TimeMgrInit (Interface only)
!     ESMF_TimeMgrSetStepSize (Interface only)
!     ESMF_TimeMgrGetStepSize (Interface only)
!     ESMF_TimeMgrRestartGet (Interface only)
!     ESMF_TimeMgrRestartSet (Interface only)
!     ESMF_TimeMgrAdvance
!     ESMF_TimeMgrGetNStep
!     ESMF_TimeMgrSetNStep
!     ESMF_TimeMgrGetStartDate
!     ESMF_TimeMgrGetStopDate
!     ESMF_TimeMgrGetBaseDate
!     ESMF_TimeMgrGetCurrDate 
!     ESMF_TimeMgrSetCurrDate (Interface only)
!     ESMF_TimeMgrGetPrevDate
!
! !DESCRIPTION:
! The Time Manager class is part of the Modeling Framework Time Management  
! module ({\tt ESMF\_TimeMgmt}).  A Time Manager object stores the step size, start, 
! stop and base dates of a model run.  It advances the model date with each 
! completed timestep.
!EOP
!===============================================================================

!===============================================================================
! ESMF_TimeMgrOverload.F
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrInit
!
! !INTERFACE:
      interface ESMF_TimeMgrInit
        module procedure ESMF_TimeMgrInitStd, &
                         ESMF_TimeMgrInitNoBaseStd, &
                         ESMF_TimeMgrInitIS, &
                         ESMF_TimeMgrInitNoBaseIS
      end interface

! !DESCRIPTION:
!     Initializes a new {\tt ESMF\_TimeMgr} object. \\
!     See the explicit interfaces: 
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeMgrInitStd}       & general interface to init a time manager \\
!     {\tt ESMF\_TimeMgrInitNoBaseStd} & general interface to init a time manager,
!                                       with base date == start date \\
!     {\tt ESMF\_TimeMgrInitIS}        & init a time manager with integer second 
!                                       discretization \\
!     {\tt ESMF\_TimeMgrInitNoBaseIS}  & init a time manager with integer second
!                                       discretization, base date == start date \\
!     \end{tabular}  
!
! SEE ALSO:
!   {\tt ESMF\_TimeMgrInitStd, ESMF\_TimeMgrInitNoBaseStd, ESMF\_TimeMgrInitIS, 
!   ESMF\_TimeMgrInitNoBaseIS}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrGetStepSize
!
! !INTERFACE:
      interface ESMF_TimeMgrGetStepSize
        module procedure ESMF_TimeMgrGetStepSizeStd, &
                         ESMF_TimeMgrGetStepSizeIS
      end interface

! !DESCRIPTION:
!     Gets a step size. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeMgrGetStepSizeStd} & get step size as time type \\
!     {\tt ESMF\_TimeMgrGetStepSizeIS}  & get step size as days and seconds \\
!     \end{tabular}  
!
! SEE ALSO:
!   ESMF\_TimeMgrGetStepSizeStd, ESMF\_TimeMgrGetStepSizeIS
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrSetStepSize
!
! !INTERFACE:
      interface ESMF_TimeMgrSetStepSize
        module procedure ESMF_TimeMgrSetStepSizeStd, &
                         ESMF_TimeMgrSetStepSizeIS
      end interface

! !DESCRIPTION:
!     Sets a step size. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeMgrSetStepSizeStd}  & set step size with time type \\
!     {\tt ESMF\_TimeMgrSetStepSizeIS}   & set step size with days and seconds \\
!     \end{tabular}  
!
! SEE ALSO:
!   {\tt ESMF\_TimeMgrSetStepSizeStd, ESMF\_TimeMgrSetStepSizeIS}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrRestartWrite
!
! !INTERFACE:
      interface ESMF_TimeMgrRestartWrite
        module procedure ESMF_TimeMgrRestartWriteIS
      end interface

! !DESCRIPTION:
!     Returns the data needed for a restart. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeMgrRestartWriteIS}   & return restart data \\
!     \end{tabular}  
!
! SEE ALSO:
!   {\tt ESMF\_TimeMgrRestartWrite}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrRestartRead
!
! !INTERFACE:
      interface ESMF_TimeMgrRestartRead
        module procedure ESMF_TimeMgrRestartReadIS
      end interface

! !DESCRIPTION:
!     Retrieves the restart data and returns a TimeMgr. \\
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeMgrRestartReadIS}   & constructs restart TimeMgr (int) seconds  \\
!     \end{tabular}  
!
! SEE ALSO:
!   {\tt ESMF\_TimeMgrRestartRead}
!
!EOP
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrSetCurrDate
!
! !INTERFACE:
      interface ESMF_TimeMgrSetCurrDate
        module procedure ESMF_TimeMgrSetCurrDateIS
      end interface
! !DESCRIPTION:
!     Sets the current date (updates prev date).
!     See the explicit interfaces:
!
!     \begin{tabular}{p{2in}p{3.5in}}
!     {\tt ESMF\_TimeMgrSetCurrDateIS}   & set curr date \\
!     \end{tabular}  
!
! SEE ALSO:
!   {\tt ESMF\_TimeMgrGetCurrDate}
!
!EOP
!===============================================================================
	contains
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrInitStd
!
! !INTERFACE:
      function ESMF_TimeMgrInitStd(stepSize, startDate,  &
                                  stopDate, baseDate, rc)

! !RETURN VALUE:
      type(ESMF_TimeMgr) :: ESMF_TimeMgrInitStd    ! return a new time manager

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: stepSize      ! step size   
      type(ESMF_Date), intent(in) :: startDate     ! start date
      type(ESMF_Date), intent(in) :: stopDate      ! stop date
      type(ESMF_Date), intent(in) :: baseDate      ! base date
      integer, intent(out), optional :: rc         ! return code

! !DESCRIPTION:
!     General initialization for a new time manager object.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrInit(ESMF_TimeMgrInitStd, stepSize,  &
                             startDate, stopDate, baseDate, stub)
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrInitStd

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrInitIS
!
! !INTERFACE:
      function ESMF_TimeMgrInitIS(stepDays, stepSecs,  &
                                 startCalendarDate, startTOD,  &
                                 stopCalendarDate, stopTOD,  &
                                 baseCalendarDate, baseTOD,  &
                                 type, rc)     

! !RETURN VALUE:
      type(ESMF_TimeMgr) :: ESMF_TimeMgrInitIS   ! new time manager with 
				                 !   integer second granularity

! !PARAMETERS:
      integer, intent(in) :: stepDays            ! days in time step   
      integer, intent(in) :: stepSecs            ! seconds in time step
      integer, intent(in) :: startCalendarDate   ! start date
      integer, intent(in) :: startTOD            ! start time of day
      integer, intent(in) :: stopCalendarDate    ! stop date
      integer, intent(in) :: stopTOD             ! stop time of day
      integer, intent(in) :: baseCalendarDate    ! base date
      integer, intent(in) :: baseTOD             ! base time of day
      integer, intent(in) :: type                ! calendar type
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Initializes a new time manager for climate-type models with 
!     integer second temporal discretization.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrInitIS(ESMF_TimeMgrInitIS, &
                    stepDays, stepSecs, startCalendarDate, &
                    startTOD, stopCalendarDate, stopTOD, &
                    baseCalendarDate, baseTOD, type, stub)
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrInitIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrInitNoBaseStd
!
! !INTERFACE:
      function ESMF_TimeMgrInitNoBaseStd(stepSize, startDate, &
                                         stopDate, rc) 
 
! !RETURN VALUE:
      type(ESMF_TimeMgr) :: ESMF_TimeMgrInitNoBaseStd ! return a new time manager

! !PARAMETERS:
      type(ESMF_Time), intent(in) :: stepSize         ! step size   
      type(ESMF_Date), intent(in) :: startDate        ! start date
      type(ESMF_Date), intent(in) :: stopDate         ! stop date
      integer, intent(out), optional :: rc            ! return code

! !DESCRIPTION:
!     General initialization for a new time manager object.  No base
!     date is supplied (it is set by default to the start date).
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrInitNoBase(ESMF_TimeMgrInitNoBaseStd, stepSize, &
                                  startDate, stopDate, stub)
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrInitNoBaseStd

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrInitNoBaseIS
!
! !INTERFACE:
      function ESMF_TimeMgrInitNoBaseIS(stepDays, stepSecs, &
                                        startCalendarDate, startTOD, &
                                        stopCalendarDate, stopTOD, &
                                        type, rc)     

! !RETURN VALUE:
      type(ESMF_TimeMgr) :: ESMF_TimeMgrInitNoBaseIS  ! new time manager with 
				                      !   integer second granularity

! !PARAMETERS:
      integer, intent(in) :: stepDays            ! days in time step   
      integer, intent(in) :: stepSecs            ! seconds in time step
      integer, intent(in) :: startCalendarDate   ! start date
      integer, intent(in) :: startTOD            ! start time of day
      integer, intent(in) :: stopCalendarDate    ! stop date
      integer, intent(in) :: stopTOD             ! stop time of day
      integer, intent(in) :: type                ! calendar type
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Initializes a new time manager for climate-type models with 
!     integer second discretization.  No base date is supplied (it is 
!     set by default to the start date).
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrInitNoBaseIS(ESMF_TimeMgrInitNoBaseIS, &
                     stepDays, stepSecs, startCalendarDate, &
                     startTOD, stopCalendarDate, stopTOD, type, stub)
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrInitNoBaseIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrLastStep
!
! !INTERFACE:
      function ESMF_TimeMgrLastStep(timeMgr, rc) 

! !RETURN VALUE:
      logical      :: ESMF_TimeMgrLastStep        ! return true if last step

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr   ! time manager
      integer, intent(out), optional :: rc        ! return code

! !DESCRIPTION:
!     Returns true if the time manager's current date plus time step 
!     exceeds the stop date.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrLastStep(timeMgr, ESMF_TimeMgrLastStep, stub)
      if (present(rc)) rc = stub
  
      end function ESMF_TimeMgrLastStep

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrAdvance
!
! !INTERFACE: 
      subroutine ESMF_TimeMgrAdvance(timeMgr, rc)

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(inout) :: timeMgr  ! time manager 
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Advances time manager by one time step.  Resets current date and
!     previous date.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrAdvance(timeMgr, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_TimeMgrAdvance

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrSetStepSizeStd
!
! !INTERFACE:
      subroutine ESMF_TimeMgrSetStepSizeStd(timeMgr, stepSize, rc)

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(inout) :: timeMgr  ! time manager
      type(ESMF_Time), intent(in) :: stepSize       ! step size
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Resets the step size in the time manager to stepSize.
!
!EOP
!-------------------------------------------------------------------------------
 
      integer stub

      call ESMC_TimeMgrSetStepSizeStd(timeMgr, stepSize, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_TimeMgrSetStepSizeStd

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrSetStepSizeIS
!
! !INTERFACE:
      subroutine ESMF_TimeMgrSetStepSizeIS(timeMgr, days, seconds, rc)

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(inout) :: timeMgr  ! time manager
      integer, intent(in) :: days                   ! days
      integer, intent(in) :: seconds                ! seconds
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Resets the step size in the time manager using {\tt days} and 
!     {\tt seconds}.
!
!EOP
!-------------------------------------------------------------------------------
 
      integer stub

      call ESMC_TimeMgrSetStepSizeIS(timeMgr, days, seconds, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_TimeMgrSetStepSizeIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrGetNStep
!
! !INTERFACE:
      function ESMF_TimeMgrGetNStep(timeMgr, rc) 

! !RETURN VALUE:
      integer :: ESMF_TimeMgrGetNStep            ! returned number of time steps

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr  ! time manager 
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Gets the number of time steps in the time manager.
!
!EOP
!-------------------------------------------------------------------------------
 
      integer stub

      call ESMC_TimeMgrGetNStep(timeMgr, ESMF_TimeMgrGetNStep, stub)
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrGetNStep

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrSetNStep
!
! !INTERFACE:
      subroutine ESMF_TimeMgrSetNStep(timeMgr, nstep, rc) 

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr  ! time manager 
      integer, intent(in) :: nstep               ! New nstep
      integer, intent(out), optional :: rc       ! return code

! !DESCRIPTION:
!     Sets the number of time steps in the time manager.
!
!EOP
!-------------------------------------------------------------------------------
 
      integer stub

      call ESMC_TimeMgrSetNStep(timeMgr, nstep, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_TimeMgrSetNStep

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrGetStepSizeIS
!
! !INTERFACE:
      subroutine ESMF_TimeMgrGetStepSizeIS(timeMgr, days, seconds, rc)

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr   ! time manager
      integer, intent(out) :: days                ! days
      integer, intent(out) :: seconds             ! seconds
      integer, intent(out), optional :: rc        ! return code

! !DESCRIPTION:
!     Returns the step size.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrGetStepSizeIS(timeMgr, days, seconds, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_TimeMgrGetStepSizeIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrGetStepSizeStd
!
! !INTERFACE:
      subroutine ESMF_TimeMgrGetStepSizeStd(timeMgr, stepSize, rc)

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr   ! time manager
      type(ESMF_Time), intent(out) :: stepSize    ! step size
      integer, intent(out), optional :: rc        ! return code

! !DESCRIPTION:
!     Returns the step size.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrGetStepSizeStd(timeMgr, stepsize, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_TimeMgrGetStepSizeStd

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrGetStartDate
!
! !INTERFACE: 
      function ESMF_TimeMgrGetStartDate(timeMgr, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_TimeMgrGetStartDate   ! start date    

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in)  :: timeMgr    ! time manager
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Returns the start date.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_TimeMgrGetStartDate, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_TimeMgrGetStartDate(timeMgr, ESMF_TimeMgrGetStartDate, &
         stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrGetStartDate

!===============================================================================
!BOP
!
! !IROUTINE: ESMF_TimeMgrGetStopDate
!
! !INTERFACE:
      function ESMF_TimeMgrGetStopDate(timeMgr, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_TimeMgrGetStopDate  ! stop date   

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr   ! time manager
      integer, intent(out), optional :: rc        ! return code

! !DESCRIPTION:
!     Returns the stop date.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_TimeMgrGetStopDate, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_TimeMgrGetStopDate(timeMgr, ESMF_TimeMgrGetStopDate, &
          stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrGetStopDate

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrGetBaseDate
!
! !INTERFACE:
      function ESMF_TimeMgrGetBaseDate(timeMgr, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_TimeMgrGetBaseDate  ! base date

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr   ! time manager
      integer, intent(out), optional :: rc        ! return code

! !DESCRIPTION:
!     Returns the base date.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_TimeMgrGetBaseDate, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_TimeMgrGetBaseDate(timeMgr, ESMF_TimeMgrGetBaseDate, &
         stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrGetBaseDate

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrGetCurrDate
!
! !INTERFACE:
      function ESMF_TimeMgrGetCurrDate(timeMgr, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_TimeMgrGetCurrDate   ! current date

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr    ! time manager
      integer, intent(out), optional :: rc         ! return code

! !DESCRIPTION:
!     Returns the current date.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_TimeMgrGetCurrDate, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_TimeMgrGetCurrDate(timeMgr, ESMF_TimeMgrGetCurrDate, &
          stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrGetCurrDate

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrSetCurrDateIS
!
! !INTERFACE:
      subroutine ESMF_TimeMgrSetCurrDateIS(timeMgr, dateYYMMDD, tod, rc)

! !RETURN VALUE:

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr    ! time manager
      integer, intent(in) :: dateYYMMDD ! date
      integer, intent(in) :: tod ! seconds into day
      integer, intent(out), optional :: rc         ! return code

! !DESCRIPTION:
!     Sets the current date.  Also sets the previous date based on 
!     the current date and the time step.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrSetCurrDateIS(timeMgr, &
           dateYYMMDD, &
           tod, &
           stub)
      
      if (present(rc)) rc = stub
      
      end subroutine ESMF_TimeMgrSetCurrDateIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrGetPrevDate
!
! !INTERFACE:
      function ESMF_TimeMgrGetPrevDate(timeMgr, rc)

! !RETURN VALUE:
      type(ESMF_Date) :: ESMF_TimeMgrGetPrevDate    ! previous date

! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr     ! time manager
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Returns the previous timestep.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_DateInitUndefined(ESMF_TimeMgrGetPrevDate, stub)
      if (stub == ESMF_SUCCESS) then
        call ESMC_TimeMgrGetPrevDate(timeMgr, ESMF_TimeMgrGetPrevDate, &
         stub)
      end if
      if (present(rc)) rc = stub

      end function ESMF_TimeMgrGetPrevDate

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrRestartWriteIS
!
! !INTERFACE:
      subroutine ESMF_TimeMgrRestartWriteIS(timeMgr, &
          type,  &
          nstep,  &
          stepDays, stepSec,  &
          startYYMMDD, startSec,  &
          stopYYMMDD, stopSec,  &
          baseYYMMDD, baseSec,  &
          currYYMMDD, currSec,  &
          rc)
! !PARAMETERS:
      type(ESMF_TimeMgr), intent(in) :: timeMgr ! time Manager
      integer, intent(out) :: type ! Calendar type
      integer, intent(out) :: nstep ! step number
      integer, intent(out) :: stepDays ! days in step size
      integer, intent(out) :: stepSec ! seconds in step size
      integer, intent(out) :: startYYMMDD ! start day
      integer, intent(out) :: startSec ! start time
      integer, intent(out) :: stopYYMMDD ! stop day
      integer, intent(out) :: stopSec ! stop time
      integer, intent(out) :: baseYYMMDD ! base day
      integer, intent(out) :: baseSec ! base time
      integer, intent(out) :: currYYMMDD ! curr day
      integer, intent(out) :: currSec ! curr time      
      integer, intent(out), optional :: rc ! return code
! !DESCRIPTION:
!     Returns all the data necessary to reconstruct an identical
!     TimeMgr after a restart.
!     
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrRestartWriteIS(timeMgr, &
          type, &
          nstep, &
          stepDays, stepSec, &
          startYYMMDD, startSec, &
          stopYYMMDD, stopSec, &
          baseYYMMDD, baseSec, &
          currYYMMDD, currSec, &
          stub) 

      if (present(rc)) rc = stub

      end subroutine ESMF_TimeMgrRestartWriteIS

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_TimeMgrRestartReadIS
!
! !INTERFACE:
      function ESMF_TimeMgrRestartReadIS( &
            type, &
            nstep, &
            stepDays, stepSec, &
            startYYMMDD, startSec, &
            stopYYMMDD, stopSec, &
            baseYYMMDD, baseSec, &
            currYYMMDD, currSec, &
            rc)
! !RETURN VALUE:
      type(ESMF_TimeMgr) :: ESMF_TimeMgrRestartReadIS ! previous date
! !PARAMETERS:
      integer, intent(in) :: stepDays ! days in step
      integer, intent(in) :: type ! calendar type
      integer, intent(in) :: nstep ! step number
      integer, intent(in) :: stepSec ! seconds in step
      integer, intent(in) :: startYYMMDD ! start day
      integer, intent(in) :: startSec ! start time
      integer, intent(in) :: stopYYMMDD ! stop day
      integer, intent(in) :: stopSec ! stop time
      integer, intent(in) :: baseYYMMDD ! base day
      integer, intent(in) :: baseSec ! base time
      integer, intent(in) :: currYYMMDD ! curr day
      integer, intent(in) :: currSec ! curr time      
      integer, intent(out), optional :: rc ! return code
! !DESCRIPTION:
!     Reconstructs a time manager based on the data given.  This data
!     will typically come from a {\tt MFM\_TimeMgrRestartWrite}. 
!     The value of {\tt nstep} may be set to any positive integer.
!     
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_TimeMgrRestartReadIS( &
          ESMF_TimeMgrRestartReadIS, &
          type, &
          nstep, &
          stepDays, stepSec, &
          startYYMMDD, startSec, &
          stopYYMMDD, stopSec, &
          baseYYMMDD, baseSec, &
          currYYMMDD, currSec, &
          stub)

      if (present(rc)) rc = stub

      end function ESMF_TimeMgrRestartReadIS

!===============================================================================
	end module









