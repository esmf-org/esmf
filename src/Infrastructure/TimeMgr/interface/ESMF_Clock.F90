
! $Id: ESMF_Clock.F90,v 1.77.2.4 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Clock.F90"
!==============================================================================
!
!     ESMF Clock Module
      module ESMF_ClockMod
!     
!==============================================================================
!     
! This file contains the Clock class definition and all Clock class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_TimeMgr.inc"

!==============================================================================
!BOPI
! !MODULE: ESMF_ClockMod
!     
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation.
!     
! See {\tt ../include/ESMC\_Clock.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_UtilTypesMod
      use ESMF_BaseMod
      use ESMF_InitMacrosMod

      ! for ReadRestart()/WriteRestart()
      use ESMF_IOSpecMod

      ! associated derived types
      use ESMF_CalendarMod
      use ESMF_TimeIntervalMod
      use ESMF_TimeIntervalTypeMod
      use ESMF_TimeMod
      use ESMF_TimeTypeMod
      use ESMF_AlarmTypeMod

      ! type definition for this module
      use ESMF_ClockTypeMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Clock definition in ESMF_ClockTypeMod to resolve mutual
!     ! method dependency with ESMF_Alarm

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     This type is defined in ESMF_ClockTypeMod and progagated up from here.
      public ESMF_Clock
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public operator(==)
      public operator(/=)
      public ESMF_ClockAdvance
      public ESMF_ClockCreate
      public ESMF_ClockDestroy 
      public ESMF_ClockGet     
      public ESMF_ClockGetAlarm
      public ESMF_ClockGetAlarmList
      public ESMF_ClockGetNextTime
      public ESMF_ClockIsDone
      public ESMF_ClockIsReverse
      public ESMF_ClockIsStopTime
      public ESMF_ClockIsStopTimeEnabled
      public ESMF_ClockPrint
      public ESMF_ClockReadRestart
      public ESMF_ClockSet
      public ESMF_ClockStopTimeDisable     
      public ESMF_ClockStopTimeEnable     
      public ESMF_ClockSyncToRealTime
      public ESMF_ClockValidate
      public ESMF_ClockWriteRestart
!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_ClockEQ
      private ESMF_ClockNE
      private ESMF_ClockCreateNew
      private ESMF_ClockCreateCopy

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Clock.F90,v 1.77.2.4 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE:  ESMF_ClockOperator(==) - Test if Clock 1 is equal to Clock 2
!
! !INTERFACE:
      interface operator(==)
!     if (clock1 == clock2) then ... endif
!                  OR
!     result = (clock1 == clock2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Clock), intent(in) :: clock1
!     type(ESMF_Clock), intent(in) :: clock2
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Clock} class.
!     Compare two clocks for equality; return true if equal,
!     false otherwise.  Comparison is based on IDs, which are distinct
!     for newly created clocks and identical for clocks created as copies.
!
!     The arguments are:
!     \begin{description}
!     \item[clock1]
!          The first {\tt ESMF\_Clock} in comparison.
!     \item[clock2]
!          The second {\tt ESMF\_Clock} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_ClockEQ
!
! !REQUIREMENTS:
!     TMGx.x.x

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ClockOperator(/=) - Test if Clock 1 is not equal to Clock 2
!
! !INTERFACE:
      interface operator(/=)
!     if (clock1 /= clock2) then ... endif
!                  OR
!     result = (clock1 /= clock2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Clock), intent(in) :: clock1
!     type(ESMF_Clock), intent(in) :: clock2
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Clock} class.
!     Compare two clocks for inequality; return true if not equal,
!     false otherwise.  Comparison is based on IDs, which are distinct
!     for newly created clocks and identical for clocks created as copies.
!
!     The arguments are:
!     \begin{description}
!     \item[clock1]
!          The first {\tt ESMF\_Clock} in comparison.
!     \item[clock2]
!          The second {\tt ESMF\_Clock} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_ClockNE
!
! !REQUIREMENTS:
!     TMGx.x.x

      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ClockCreate - Create an ESMF Clock
!
! !INTERFACE:
      interface ESMF_ClockCreate

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_ClockCreateNew
      module procedure ESMF_ClockCreateCopy

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Clock} Create
!     methods. 
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockAdvance()"
!BOP
! !IROUTINE: ESMF_ClockAdvance - Advance a Clock's current time by one time step

! !INTERFACE:
      subroutine ESMF_ClockAdvance(clock, timeStep, ringingAlarmList, &
                                   ringingAlarmCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock),               intent(inout)         :: clock
      type(ESMF_TimeInterval),        intent(inout),  optional :: timeStep
      type(ESMF_Alarm), dimension(:), intent(out), optional :: ringingAlarmList
      integer,                        intent(out), optional :: ringingAlarmCount
      integer,                        intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Advances the {\tt clock}'s current time by one time step:  either the
!     {\tt clock}'s, or the passed-in {\tt timeStep} (see below).  When the
!     {\tt clock} is in {\tt ESMF\_MODE\_FORWARD} (default), this method adds
!     the {\tt timeStep} to the {\tt clock}'s current time.
!     In {\tt ESMF\_MODE\_REVERSE}, {\tt timeStep} is subtracted from the
!     current time.  In either case, {\tt timeStep} can be positive or negative.
!     See the "direction" argument in method {\tt ESMF\_ClockSet()}.
!     {\tt ESMF\_ClockAdvance()} optionally returns a list and number of ringing
!     {\tt ESMF\_Alarm}s.  See also method {\tt ESMF\_ClockGetRingingAlarms()}.
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance.
!     \item[{[timeStep]}]
!          Time step is performed with given timeStep, instead of
!          the {\tt ESMF\_Clock}'s.  Does not replace the {\tt ESMF\_Clock}'s 
!          timeStep; use {\tt ESMF\_ClockSet(clock, timeStep, ...)} for
!          this purpose.  Supports applications with variable time steps.
!          timeStep can be positive or negative.
!     \item[{[ringingAlarmList]}]
!          Returns the array of alarms that are ringing after the
!          time step.
!     \item[{[ringingAlarmCount]}]
!          The number of alarms ringing after the time step.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
!EOP
! !REQUIREMENTS:
!     TMG3.4.1


      ! initialize list size to zero for not-present list
      integer :: sizeofRingingAlarmList,i
      type(ESMF_Pointer), allocatable :: ringingAlarmPtrList(:)      
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,timestep)

      sizeofRingingAlarmList = 0

      ! get size of given ringing alarm list for C++ validation
      if (present(ringingAlarmList)) then
        sizeofRingingAlarmList = size(ringingAlarmList)
 
        ! for Init Macros
        allocate(ringingAlarmPtrList(sizeofRingingAlarmList))
      end if

      ! invoke C to C++ entry point

      if (present(ringingAlarmList) .and. sizeofRingingAlarmList > 1) then
        ! pass address of 2nd element for C++ to calculate array step size
        call c_ESMC_ClockAdvance2(clock, timeStep, &
                     ringingAlarmPtrList(1), ringingAlarmPtrList(2), &
                     sizeofRingingAlarmList, ringingAlarmCount, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) then
          ! Not bail out until deallocation
          write(*,*)"c_ESMC_ClockAdvance2 fails"
        endif
      else if (sizeofRingingAlarmList == 1) then
        ! array has only one element
        call c_ESMC_ClockAdvance1(clock, timeStep, ringingAlarmPtrList(1), &
                        sizeofRingingAlarmList, ringingAlarmCount, localrc)
        ! Not bail out until deallocation
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) then
          ! Not bail out until deallocation
          write(*,*)"c_ESMC_ClockAdvance1 fails"
        endif 
      else
        ! array is not present
        call c_ESMC_ClockAdvance0(clock, timeStep, &
                    sizeofRingingAlarmList, ringingAlarmCount, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) then
          ! Not bail out until deallocation
          write(*,*)"c_ESMC_ClockAdvance0 fails"
        endif
      endif

      ! postprocess ringing alarm list
      if (present(ringingAlarmList)) then

         ! postprocess ringing ringingAlarm list
         do i=1,sizeofRingingAlarmList
            call ESMF_AlarmSetThis(ringingAlarmList(i),ringingAlarmPtrList(i))
            call ESMF_AlarmSetInitCreated(ringingAlarmList(i))
         enddo

        ! Get rid of list
        deallocate(ringingAlarmPtrList)
     end if

     ! Return success
     if (present(rc)) rc = localrc
    
     end subroutine ESMF_ClockAdvance

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockCreateNew()"
!BOP
! !IROUTINE: ESMF_ClockCreate - Create a new ESMF Clock

! !INTERFACE:
      ! Private name; call using ESMF_ClockCreate()
      function ESMF_ClockCreateNew(name, timeStep, startTime, stopTime, &
                                   runDuration, runTimeStepCount, refTime, rc)

! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockCreateNew

! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(ESMF_TimeInterval), intent(in)            :: timeStep
      type(ESMF_Time),         intent(in)            :: startTime
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: runDuration
      integer,                 intent(in),  optional :: runTimeStepCount
      type(ESMF_Time),         intent(in),  optional :: refTime
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Creates and sets the initial values in a new {\tt ESMF\_Clock}.    
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_ClockCreate()}.
!     
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]     
!          The name for the newly created clock.  If not specified, a
!          default unique name will be generated: "ClockNNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[timeStep]
!          The {\tt ESMF\_Clock}'s time step interval, which can be
!          positive or negative.
!     \item[startTime]
!          The {\tt ESMF\_Clock}'s starting time.  Can be less than or
!          or greater than stopTime, depending on a positive or negative
!          timeStep, respectively, and whether a stopTime is specified;
!          see below.
!     \item[{[stopTime]}]
!          The {\tt ESMF\_Clock}'s stopping time.  Can be greater than or
!          less than the startTime, depending on a positive or negative
!          timeStep, respectively.  If neither stopTime, runDuration, nor
!          runTimeStepCount is specified, clock runs "forever"; user must
!          use other means to know when to stop (e.g. ESMF\_Alarm or
!          ESMF\_ClockGet(clock, currTime)).  Mutually exclusive with
!          runDuration and runTimeStepCount.
!     \item[{[runDuration]}]
!          Alternative way to specify {\tt ESMF\_Clock}'s stopping time;
!             stopTime = startTime + runDuration.
!          Can be positive or negative, consistent with the timeStep's sign.
!          Mutually exclusive with stopTime and runTimeStepCount.
!     \item[{[runTimeStepCount]}]
!          Alternative way to specify {\tt ESMF\_Clock}'s stopping time;
!             stopTime = startTime + (runTimeStepCount * timeStep).
!          stopTime can be before startTime if timeStep is negative.
!          Mutually exclusive with stopTime and runDuration.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.  Provides reference point
!          for simulation time (see currSimTime in ESMF\_ClockGet() below).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4

      ! initialize name length to zero for non-existent name
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

!     invoke C to C++ entry point to allocate and initialize new clock
      call c_ESMC_ClockCreateNew(ESMF_ClockCreateNew, nameLen, name, &
                                 timeStep, startTime, stopTime, runDuration, &
                                 runTimeStepCount, refTime, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_ClockSetInitCreated(ESMF_ClockCreateNew)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockCreateNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockCreateCopy()"
!BOP
! !IROUTINE: ESMF_ClockCreate - Create a copy of an existing ESMF Clock

! !INTERFACE:
      ! Private name; call using ESMF_ClockCreate()
      function ESMF_ClockCreateCopy(clock, rc)

! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockCreateCopy

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      integer,          intent(out), optional :: rc
    
! !DESCRIPTION:
!     Creates a copy of a given {\tt ESMF\_Clock}.    
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_ClockCreate()}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!        The {\tt ESMF\_Clock} to copy.
!     \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

!     invoke C to C++ entry point to copy clock
      call c_ESMC_ClockCreateCopy(ESMF_ClockCreateCopy, clock, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_ClockSetInitCreated(ESMF_ClockCreateCopy)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockDestroy()"
!BOP
! !IROUTINE: ESMF_ClockDestroy - Free all resources associated with a Clock
!
! !INTERFACE:
      subroutine ESMF_ClockDestroy(clock, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock) :: clock
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!       Destroy contents of this {\tt ESMF\_Clock}.
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockDestroy(clock, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) then 
        ! Don't bail out until Delete
        write(*,*)" c_ESMC_ClockDestroy fails"
      endif

      call ESMF_ClockSetInitDeleted(clock)

      ! Return success
      if (present(rc)) rc = localrc
      end subroutine ESMF_ClockDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockGet()"
!BOP
! !IROUTINE: ESMF_ClockGet - Get a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockGet(clock, name, timeStep, startTime, stopTime, &
                               runDuration, runTimeStepCount, refTime, &
                               currTime, prevTime, currSimTime, prevSimTime, &
                               calendar, calendarType, timeZone, advanceCount, &
                               alarmCount, direction, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)            :: clock
      character (len=*),       intent(out), optional :: name
      type(ESMF_TimeInterval), intent(out), optional :: timeStep
      type(ESMF_Time),         intent(out), optional :: startTime
      type(ESMF_Time),         intent(out), optional :: stopTime
      type(ESMF_TimeInterval), intent(out), optional :: runDuration
      real(ESMF_KIND_R8),      intent(out), optional :: runTimeStepCount
      type(ESMF_Time),         intent(out), optional :: refTime
      type(ESMF_Time),         intent(out), optional :: currTime
      type(ESMF_Time),         intent(out), optional :: prevTime
      type(ESMF_TimeInterval), intent(out), optional :: currSimTime
      type(ESMF_TimeInterval), intent(out), optional :: prevSimTime
      type(ESMF_Calendar),     intent(out), optional :: calendar
      type(ESMF_CalendarType), intent(out), optional :: calendarType
      integer,                 intent(out), optional :: timeZone
      integer(ESMF_KIND_I8),   intent(out), optional :: advanceCount
      integer,                 intent(out), optional :: alarmCount
      type(ESMF_Direction),    intent(out), optional :: direction
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Gets one or more of the properties of an {\tt ESMF\_Clock}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to query.
!     \item[{[name]}]
!          The name of this clock.
!     \item[{[timeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[{[startTime]}]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[{[stopTime]}]
!          The {\tt ESMF\_Clock}'s stopping time.
!     \item[{[runDuration]}]
!          Alternative way to get {\tt ESMF\_Clock}'s stopping time;
!             runDuration = stopTime - startTime.
!     \item[{[runTimeStepCount]}]
!          Alternative way to get {\tt ESMF\_Clock}'s stopping time;
!             runTimeStepCount = (stopTime - startTime) / timeStep.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.
!     \item[{[currTime]}]
!          The {\tt ESMF\_Clock}'s current time.
!     \item[{[prevTime]}]
!          The {\tt ESMF\_Clock}'s previous time.  Equals currTime at
!          the previous time step.
!     \item[{[currSimTime]}]
!          The current simulation time (currTime - refTime).
!     \item[{[prevSimTime]}]
!          The previous simulation time.  Equals currSimTime at
!          the previous time step.
!     \item[{[calendar]}]
!          The {\tt Calendar} on which all the {\tt Clock}'s times are defined.
!     \item[{[calendarType]}]
!          The {\tt CalendarType} on which all the {\tt Clock}'s times are
!          defined.
!     \item[{[timeZone]}]
!          The timezone within which all the {\tt Clock}'s times are defined.
!     \item[{[advanceCount]}]
!          The number of times the {\tt ESMF\_Clock} has been advanced.
!          Increments in {\tt ESMF\_MODE\_FORWARD} and decrements in
!          {\tt ESMF\_MODE\_REVERSE}; see "direction" argument below and in
!          {\tt ESMF\_ClockSet()}.
!     \item[{[alarmCount]}]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list.
!     \item[{[direction]}]
!          The {\tt ESMF\_Clock}'s time stepping direction.  See also
!          {\tt ESMF\_ClockIsReverse()}, an alternative for convenient use in
!          "if" and "do while" constructs.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG3.5.1 - TMG3.5.5

      ! temp name for C++ to fill
      character (len=ESMF_MAXSTR) :: tempName

      ! initialize name lengths to zero for non-existent name
      integer :: nameLen, tempNameLen
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0
      tempNameLen = 0

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,stopTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,refTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,currTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,prevTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,startTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,runDuration)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,timestep)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,currSimTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,prevSimTime)


      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

!     invoke C to C++ entry point
      call c_ESMC_ClockGet(clock, nameLen, tempNameLen, tempName, &
                           timeStep, startTime, stopTime, &
                           runDuration, runTimeStepCount, refTime, &
                           currTime, prevTime, currSimTime, prevSimTime, &
                           calendar, calendarType, timeZone, advanceCount, &
                           alarmCount, direction, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(calendar)) call ESMF_CalendarSetInitCreated(calendar)

      ! copy temp name back to given name to restore native Fortran
      !   storage style
      if (present(name)) then
        name = tempName(1:tempNameLen)
      endif
    
      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockGetAlarm()"
!BOP
! !IROUTINE: ESMF_ClockGetAlarm - Get an Alarm in a Clock's Alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetAlarm(clock, name, alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(inout)         :: clock
      character (len=*), intent(in)            :: name
      type(ESMF_Alarm),  intent(out)           :: alarm
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the {\tt alarm} whose name is the value of name in the {\tt clock}'s
!     {\tt ESMF\_Alarm} list.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt ESMF\_Alarm} from.
!     \item[name]
!          The name of the desired {\tt ESMF\_Alarm}.
!     \item[alarm]
!          The desired alarm.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMGx.x

      ! get length of given name for C++ validation
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = len_trim(name)

!     invoke C to C++ entry point
      call c_ESMC_ClockGetAlarm(clock, nameLen, name, alarm, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
   
      call ESMF_AlarmSetInitCreated(alarm)
 
      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGetAlarm

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockGetAlarmList()"
!BOP
! !IROUTINE: ESMF_ClockGetAlarmList - Get a list of Alarms from a Clock

! !INTERFACE:
      subroutine ESMF_ClockGetAlarmList(clock, alarmListType, &
                                        alarmList, alarmCount, timeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock),               intent(in)         :: clock
      type(ESMF_AlarmListType),       intent(in)            :: alarmListType
      type(ESMF_Alarm), dimension(:), intent(out)           :: alarmList
      integer,                        intent(out)           :: alarmCount
      type(ESMF_TimeInterval),        intent(in),  optional :: timeStep
      integer,                        intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Gets the {\tt clock}'s list of alarms.
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance from which to get an {\tt ESMF\_Alarm} list.
!     \item[alarmListType]
!          The type of list to get:
!            {\tt ESMF\_ALARMLIST\_ALL} :
!                Returns the {\tt ESMF\_Clock}'s entire list of alarms.
!
!            {\tt ESMF\_ALARMLIST\_NEXTRINGING} :
!                Return only those alarms that will ring upon the next
!                {\tt clock} time step.  Can optionally specify argument
!                timeStep (see below) to use instead of the {\tt clock}'s.
!                See also method {\tt ESMF\_AlarmWillRingNext()} for checking a
!                single alarm.
!
!            {\tt ESMF\_ALARMLIST\_PREVRINGING} :
!                Return only those alarms that were ringing on the previous
!                {\tt ESMF\_Clock} time step.  See also method
!                {\tt ESMF\_AlarmWasPrevRinging()} for checking a single alarm.
!
!            {\tt ESMF\_ALARMLIST\_RINGING} :
!                Returns only those {\tt clock} alarms that are currently
!                ringing.  See also method {\tt ESMF\_ClockAdvance()} for
!                getting the list of ringing alarms subsequent to a time step.
!                See also method {\tt ESMF\_AlarmIsRinging()} for checking a
!                single alarm.
!     \item[alarmList]
!          The array of returned alarms. 
!     \item[alarmCount]
!          The number of {\tt ESMF\_Alarm}s in the returned list.
!     \item[{[timeStep]}]
!          Optional time step to be used instead of the {\tt clock}'s.
!          Only used with {\tt ESMF\_ALARMLIST\_NEXTRINGING alarmListType}
!          (see above); ignored if specified with other {\tt alarmListTypes}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
!EOP
! !REQUIREMENTS:
!     TMG4.3, 4.8

      ! get size of given alarm list for C++ validation
      integer :: sizeofAlarmList,i
      type(ESMF_Pointer), allocatable :: alarmPtrList(:)      
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      sizeofAlarmList = size(alarmList)
      
      ! for Init Macros
      allocate(alarmPtrList(sizeofAlarmList))

      ! invoke C to C++ entry point
      if (sizeofAlarmList > 1) then
        ! pass address of 2nd element for C++ to calculate array step size
        call c_ESMC_ClockGetAlarmList2(clock, alarmListType, &
                           alarmPtrList(1), alarmPtrList(2), &
                           sizeofAlarmList, alarmCount, timeStep, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) then
          ! Not bail out until deallocation
          write(*,*)"c_ESMC_ClockGetAlarmList2 fails"
        endif
      else
        ! array has only one element
        call c_ESMC_ClockGetAlarmList1(clock, alarmListType, &
                           alarmPtrList(1), &
                           sizeofAlarmList, alarmCount, timeStep, localrc)
        if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) then
          ! Not bail out until deallocation
          write(*,*)"c_ESMC_ClockGetAlarmList1 fails"
        endif
      endif

    
      ! postprocess ringing alarm list
      do i=1,sizeofAlarmList
         call ESMF_AlarmSetThis(alarmList(i),alarmPtrList(i))
         call ESMF_AlarmSetInitCreated(alarmList(i))
      enddo

      ! Get rid of list
      deallocate(alarmPtrList)

      ! Return success
      if (present(rc)) rc = localrc
      end subroutine ESMF_ClockGetAlarmList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockGetNextTime()"
!BOP
! !IROUTINE: ESMF_ClockGetNextTime - Calculate a Clock's next time

! !INTERFACE:
      subroutine ESMF_ClockGetNextTime(clock, nextTime, timeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)         :: clock
      type(ESMF_Time),         intent(out)           :: nextTime
      type(ESMF_TimeInterval), intent(inout),  optional :: timeStep
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Calculates what the next time of the {\tt clock} will be, based on
!     the {\tt clock}'s current time step or an optionally passed-in
!     {\tt timeStep}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance for which to get the next time.
!     \item[nextTime]
!          The resulting {\tt ESMF\_Clock}'s next time.
!     \item[{[timeStep]}]
!          The time step interval to use instead of the clock's.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMGx.x, CCSM
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,nextTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,timeStep)

!     invoke C to C++ entry point
      call c_ESMC_ClockGetNextTime(clock, nextTime, timeStep, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGetNextTime

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockIsDone()"
!BOP
! !IROUTINE: ESMF_ClockIsDone - Based on its direction, test if the Clock has reached or exceeded its stop time or start time

! !INTERFACE:
      function ESMF_ClockIsDone(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsDone

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)         :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if currentTime is greater than or equal to stopTime
!     in {\tt ESMF\_MODE\_FORWARD}, or if currentTime is less than or equal to
!     startTime in {\tt ESMF\_MODE\_REVERSE}.  It returns false otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG3.5.7
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockIsDone(clock, ESMF_ClockIsDone, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockIsDone

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockIsReverse()"
!BOP
! !IROUTINE: ESMF_ClockIsReverse - Test if the Clock is in reverse mode

! !INTERFACE:
      function ESMF_ClockIsReverse(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsReverse

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)         :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if clock is in {\tt ESMF\_MODE\_REVERSE}, and false if in
!     {\tt ESMF\_MODE\_FORWARD}.  Allows convenient use in "if" and "do while"
!     constructs.  Alternative to {\tt ESMF\_ClockGet(...direction=...)}.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG3.4.6
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockIsReverse(clock, ESMF_ClockIsReverse, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockIsReverse

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockIsStopTime()"
!BOP
! !IROUTINE: ESMF_ClockIsStopTime - Test if the Clock has reached or exceeded its stop time

! !INTERFACE:
      function ESMF_ClockIsStopTime(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTime

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)         :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if the {\tt clock} has reached or exceeded its stop time,
!     and false otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG3.5.6
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockIsStopTime(clock, ESMF_ClockIsStopTime, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockIsStopTime

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockIsStopTimeEnabled()"
!BOP
! !IROUTINE: ESMF_ClockIsStopTimeEnabled - Test if the Clock's stop time is enabled

! !INTERFACE:
      function ESMF_ClockIsStopTimeEnabled(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTimeEnabled

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)         :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if the {\tt clock}'s stop time is set and enabled,
!     and false otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGx.x, WRF
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockIsStopTimeEnabled(clock, ESMF_ClockIsStopTimeEnabled, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockIsStopTimeEnabled

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockPrint()"
!BOP
! !IROUTINE:  ESMF_ClockPrint - Print the contents of a Clock

! !INTERFACE:
      subroutine ESMF_ClockPrint(clock, options, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)         :: clock
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Prints out an {\tt ESMF\_Clock}'s properties to {\tt stdout}, in
!     support of testing and debugging.  The options control the type of
!     information and level of detail.
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all {\tt clock} property
!          values.\\
!          "advanceCount" - print the number of times the clock has been
!                           advanced. \\
!          "alarmCount"   - print the number of alarms in the clock's list. \\
!          "alarmList"    - print the clock's alarm list. \\
!          "currTime"     - print the current clock time. \\
!          "direction"    - print the clock's timestep direction. \\
!          "name"         - print the clock's name. \\
!          "prevTime"     - print the previous clock time. \\
!          "refTime"      - print the clock's reference time. \\
!          "startTime"    - print the clock's start time. \\
!          "stopTime"     - print the clock's stop time. \\
!          "timeStep"     - print the clock's time step. \\
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
      
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockPrint(clock, options, localrc)   
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockReadRestart()"
!BOPI
! !IROUTINE: ESMF_ClockReadRestart - Restore the contents of a Clock (not implemented)

! !INTERFACE:
      function ESMF_ClockReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockReadRestart
!
! !ARGUMENTS:
      character (len=*), intent(in)            :: name
      type(ESMF_IOSpec), intent(in),  optional :: iospec
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Clock} object from the last call to
!     {\tt ESMF\_ClockWriteRestart()}.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          The name of the object instance to restore.
!     \item[{[iospec]}]      
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}  
!
!EOPI
! !REQUIREMENTS:

      ! get length of given name for C++ validation
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = len_trim(name)

!     invoke C to C++ entry point to allocate and restore clock
      call c_ESMC_ClockReadRestart(ESMF_ClockReadRestart, nameLen, name, &
                                   iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_ClockSetInitCreated(ESMF_ClockReadRestart)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockSet()"
!BOP
! !IROUTINE: ESMF_ClockSet - Set one or more properties of a Clock

! !INTERFACE:
      subroutine ESMF_ClockSet(clock, name, timeStep, startTime, stopTime, &
                               runDuration, runTimeStepCount, refTime, &
                               currTime, advanceCount, direction, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(inout)         :: clock
      character (len=*),       intent(in),  optional :: name
      type(ESMF_TimeInterval), intent(inout),  optional :: timeStep
      type(ESMF_Time),         intent(inout),  optional :: startTime
      type(ESMF_Time),         intent(inout),  optional :: stopTime
      type(ESMF_TimeInterval), intent(inout),  optional :: runDuration
      integer,                 intent(in),  optional :: runTimeStepCount
      type(ESMF_Time),         intent(inout),  optional :: refTime
      type(ESMF_Time),         intent(inout),  optional :: currTime
      integer(ESMF_KIND_I8),   intent(in),  optional :: advanceCount
      type(ESMF_Direction),    intent(in),  optional :: direction
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Sets/resets one or more of the properties of an {\tt ESMF\_Clock} that
!     was previously initialized via {\tt ESMF\_ClockCreate()}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set.
!     \item[{[name]}]
!          The new name for this clock.
!     \item[{[timeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval, which can be positive or
!          negative.  This is used to change a clock's timestep property for
!          those applications that need variable timesteps.  See
!          {\tt ESMF\_ClockAdvance()} below for specifying variable timesteps
!          that are NOT saved as the clock's internal time step property.
!          See "direction" argument below for behavior with
!          {\\t ESMF\_MODE\_REVERSE} direction.
!     \item[{[startTime]}]
!          The {\tt ESMF\_Clock}'s starting time.  Can be less than or
!          or greater than stopTime, depending on a positive or negative
!          timeStep, respectively, and whether a stopTime is specified;
!          see below.
!     \item[{[stopTime]}]
!          The {\tt ESMF\_Clock}'s stopping time.  Can be greater than or
!          less than the startTime, depending on a positive or negative
!          timeStep, respectively.  If neither stopTime, runDuration, nor
!          runTimeStepCount is specified, clock runs "forever"; user must
!          use other means to know when to stop (e.g. ESMF\_Alarm or
!          ESMF\_ClockGet(clock, currTime)).
!          Mutually exclusive with runDuration and runTimeStepCount.
!     \item[{[runDuration]}]
!          Alternative way to specify {\tt ESMF\_Clock}'s stopping time;
!             stopTime = startTime + runDuration.
!          Can be positive or negative, consistent with the timeStep's sign.
!          Mutually exclusive with stopTime and runTimeStepCount.
!     \item[{[runTimeStepCount]}]
!          Alternative way to specify {\tt ESMF\_Clock}'s stopping time;
!             stopTime = startTime + (runTimeStepCount * timeStep).
!          stopTime can be before startTime if timeStep is negative.
!          Mutually exclusive with stopTime and runDuration.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.
!          See description in {\tt ESMF\_ClockCreate()} above.
!     \item[{[currTime]}]
!          The current time.
!     \item[{[advanceCount]}]
!          The number of times the clock has been timestepped.
!     \item[{[direction]}]
!          Sets the clock's time-stepping direction.  If called with
!          {\tt ESMF\_MODE\_REVERSE}, sets the clock in "reverse" mode,
!          causing it to timestep back towards its startTime.  If called
!          with {\tt ESMF\_MODE\_FORWARD}, sets the clock in normal,
!          "forward" mode, causing it to timestep in the direction of its
!          startTime to stopTime.  This holds true for negative timestep
!          clocks as well, which are initialized (created) with
!          stopTime < startTime.  The default mode is
!          {\tt ESMF\_MODE\_FORWARD}, established at {\tt ESMF\_ClockCreate()}.
!          timeStep can also be specified as an argument at the same time,
!          which allows for a change in magnitude and/or sign of the clock's
!          timeStep.  If not specified with {\tt ESMF\_MODE\_REVERSE}, the
!          clock's current timeStep is effectively negated.  If timeStep is
!          specified, its sign is used as specified; it is not negated
!          internally.  E.g., if the specified timeStep is negative and the
!          clock is placed in {\tt ESMF\_MODE\_REVERSE}, subsequent calls to
!          {\tt ESMF\_ClockAdvance()} will cause the clock's current time to
!          be decremented by the new timeStep's magnitude.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4

      ! initialize name length to zero for non-existent name
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,timeStep)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,refTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,currTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,startTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,stopTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,runDuration)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

!     invoke C to C++ entry point
      call c_ESMC_ClockSet(clock, nameLen, name, timeStep, startTime, &
                           stopTime, runDuration, runTimeStepCount, &
                           refTime, currTime, advanceCount, direction, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockStopTimeDisable()"
!BOP
! !IROUTINE: ESMF_ClockStopTimeDisable - Disable a Clock's stop time

! !INTERFACE:
      subroutine ESMF_ClockStopTimeDisable(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Disables a {\tt ESMF\_Clock}'s stop time; {\tt ESMF\_ClockIsStopTime()}
!     will always return false, allowing a clock to run past its stopTime.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance whose stop time to disable.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGx.x, WRF
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL 

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockStopTimeDisable(clock, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockStopTimeDisable

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockStopTimeEnable()"
!BOP
! !IROUTINE: ESMF_ClockStopTimeEnable - Enable an Clock's stop time

! !INTERFACE:
      subroutine ESMF_ClockStopTimeEnable(clock, stopTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      type(ESMF_Time),  intent(in),  optional :: stopTime
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Enables a {\tt ESMF\_Clock}'s stop time, allowing
!     {\tt ESMF\_ClockIsStopTime()} to respect the stopTime.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance whose stop time to enable.
!     \item[{[stopTime]}]
!          The stop time to set or reset.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGx.x, WRF
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockStopTimeEnable(clock, stopTime, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockStopTimeEnable

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockSyncToRealTime()"
!BOP
! !IROUTINE: ESMF_ClockSyncToRealTime - Set Clock's current time to wall clock time

! !INTERFACE:
      subroutine ESMF_ClockSyncToRealTime(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      integer,          intent(out), optional :: rc
    
! !DESCRIPTION:
!     Sets a {\tt clock}'s current time to the wall clock time.  It is
!     accurate to the nearest second.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to be synchronized with wall clock time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMG3.4.5
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockSyncToRealTime(clock, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS 
      end subroutine ESMF_ClockSyncToRealTime

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockValidate()"
!BOP
! !IROUTINE:  ESMF_ClockValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockValidate(clock, options, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(inout)         :: clock
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Checks whether a {\tt clock} is valid.  
!     Must have a valid startTime and timeStep.  If {\tt clock} has a
!     stopTime, its currTime must be within startTime to stopTime, inclusive;
!     also startTime's and stopTime's calendars must be the same.
!
!     The arguments are:  
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to be validated.
!     \item[{[options]}]
!          Validation options are not yet supported.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
 
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockValidate(clock, options, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS 
      end subroutine ESMF_ClockValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockWriteRestart()"
!BOPI
! !IROUTINE: ESMF_ClockWriteRestart - Save the contents of a Clock (not implemented)

! !INTERFACE:
      subroutine ESMF_ClockWriteRestart(clock, iospec, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(inout)         :: clock
      type(ESMF_IOSpec), intent(in),  optional :: iospec
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Clock} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to save.
!     \item[{[iospec]}]
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

!     invoke C to C++ entry point
      call c_ESMC_ClockWriteRestart(clock, iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockWriteRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockEQ()"
!BOPI
! !IROUTINE:  ESMF_ClockEQ - Compare two Clocks for equality
!
! !INTERFACE:
      function ESMF_ClockEQ(clock1, clock2)
!
! !RETURN VALUE:
      logical :: ESMF_ClockEQ

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock1
      type(ESMF_Clock), intent(in) :: clock2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Clock}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_ClockEQ(clock1, clock2, ESMF_ClockEQ)

      end function ESMF_ClockEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockNE()"
!BOPI
! !IROUTINE:  ESMF_ClockNE - Compare two Clocks for inequality
!
! !INTERFACE:
      function ESMF_ClockNE(clock1, clock2)
!
! !RETURN VALUE:
      logical :: ESMF_ClockNE

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock1
      type(ESMF_Clock), intent(in) :: clock2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Clock}
!     class.  See "interface operator(/=)" above for complete description.
!
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_ClockNE(clock1, clock2, ESMF_ClockNE)

      end function ESMF_ClockNE

!------------------------------------------------------------------------------

      end module ESMF_ClockMod
