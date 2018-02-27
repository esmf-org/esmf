! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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
      use ESMF_BaseMod
      use ESMF_UtilTypesMod
      use ESMF_InitMacrosMod
      use ESMF_LogErrMod
      use ESMF_IOUtilMod

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
!     This type is defined in ESMF_ClockTypeMod and propagated up from here.
      public ESMF_Clock
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
      public operator(==)
      public operator(/=)
      public ESMF_ClockAdvance
      public ESMF_ClockCreate
      public ESMF_ClockDestroy
      public ESMF_ClockGet
      public ESMF_ClockGetAlarm
      public ESMF_ClockGetAlarmList
      public ESMF_ClockGetNextTime
      public ESMF_ClockIsCreated
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

! - ESMF-internal methods:
      public ESMF_ClockGetInit
      public ESMF_ClockSetInitCreated
      public ESMF_ClockSetInitDeleted
      public ESMF_ClockGetThis
      public ESMF_ClockSetThis

!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_ClockEQ
      private ESMF_ClockNE
      private ESMF_ClockCreateNew
      private ESMF_ClockCreateCopy

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE:  ESMF_ClockAssignment(=) - Assign a Clock to another Clock
!
! !INTERFACE:
!     interface assignment(=)
!     clock1 = clock2
!
! !ARGUMENTS:
!     type(ESMF_Clock) :: clock1
!     type(ESMF_Clock) :: clock2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Assign {\tt clock1} as an alias to the same {\tt ESMF\_Clock} object in
!     memory as {\tt clock2}. If {\tt clock2} is invalid, then {\tt clock1}
!     will be equally invalid after the assignment.
!
!     The arguments are:
!     \begin{description}
!     \item[clock1]
!          The {\tt ESMF\_Clock} object on the left hand side of the
!          assignment.
!     \item[clock2]
!          The {\tt ESMF\_Clock} object on the right hand side of the
!          assignment.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
!     None, documentation only, to describe the behavior of the default
!     Fortran assignment(=).
!
! !REQUIREMENTS:
!     API review 11/2010.
!
!     end interface
!
!------------------------------------------------------------------------------
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
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Clock} class.
!     Compare two clocks for equality; return {\tt .true.} if equal,
!     {\tt .false.} otherwise. Comparison is based on IDs, which are distinct
!     for newly created clocks and identical for clocks created as copies.
!
!     If either side of the equality test is not in the
!     {\tt ESMF\_INIT\_CREATED} status an error will be logged. However, this
!     does not affect the return value, which is {\tt .true.} when both
!     sides are in the {\em same} status, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[clock1]
!          The {\tt ESMF\_Clock} object on the left hand side of the equality
!          operation.
!     \item[clock2]
!          The {\tt ESMF\_Clock} object on the right hand side of the equality
!          operation.
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
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Clock} class.
!     Compare two clocks for inequality; return {\tt .true.} if not equal,
!     {\tt .false.} otherwise. Comparison is based on IDs, which are distinct
!     for newly created clocks and identical for clocks created as copies.
!
!     If either side of the equality test is not in the
!     {\tt ESMF\_INIT\_CREATED} status an error will be logged. However, this
!     does not affect the return value, which is {\tt .true.} when both sides
!     are {\em not} in the {\em same} status, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[clock1]
!          The {\tt ESMF\_Clock} object on the left hand side of the
!          non-equality operation.
!     \item[clock2]
!          The {\tt ESMF\_Clock} object on the right hand side of the
!          non-equality operation.
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
      subroutine ESMF_ClockAdvance(clock, keywordEnforcer, &
        timeStep, ringingAlarmList, ringingAlarmCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(inout)         :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      type(ESMF_Alarm),        intent(out), optional :: ringingAlarmList(:)
      integer,                 intent(out), optional :: ringingAlarmCount
      integer,                 intent(out), optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Advances the {\tt clock}'s current time by one time step:  either the
!     {\tt clock}'s, or the passed-in {\tt timeStep} (see below).  When the
!     {\tt clock} is in {\tt ESMF\_DIRECTION\_FORWARD} (default), this method
!     adds the {\tt timeStep} to the {\tt clock}'s current time.
!     In {\tt ESMF\_DIRECTION\_REVERSE}, {\tt timeStep} is subtracted from the
!     current time.  In either case, {\tt timeStep} can be positive or negative.
!     See the "direction" argument in method {\tt ESMF\_ClockSet()}.
!     {\tt ESMF\_ClockAdvance()} optionally returns a list and number of ringing
!     {\tt ESMF\_Alarm}s.  See also method {\tt ESMF\_ClockGetRingingAlarms()}.
!     \end{sloppypar}
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

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeStep,rc)

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
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) then
          ! Not bail out until deallocation
          write(*,*)"c_ESMC_ClockAdvance2 fails"
        endif
      else if (sizeofRingingAlarmList == 1) then
        ! array has only one element
        call c_ESMC_ClockAdvance1(clock, timeStep, ringingAlarmPtrList(1), &
                        sizeofRingingAlarmList, ringingAlarmCount, localrc)
        ! Not bail out until deallocation
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) then
          ! Not bail out until deallocation
          write(*,*)"c_ESMC_ClockAdvance1 fails"
        endif
      else
        ! array is not present
        call c_ESMC_ClockAdvance0(clock, timeStep, &
                    sizeofRingingAlarmList, ringingAlarmCount, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) then
          ! Not bail out until deallocation
          write(*,*)"c_ESMC_ClockAdvance0 fails"
        endif
      endif

      ! post-process ringing alarm list
      if (present(ringingAlarmList)) then
         do i=1,sizeofRingingAlarmList
            call ESMF_AlarmSetThis(ringingAlarmList(i),ringingAlarmPtrList(i))
            ! mark output as successfully initialized
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
      function ESMF_ClockCreateNew(timeStep, startTime, keywordEnforcer, &
        stopTime, runDuration, runTimeStepCount, refTime, name, rc)

! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockCreateNew

! !ARGUMENTS:
      type(ESMF_TimeInterval), intent(in)            :: timeStep
      type(ESMF_Time),         intent(in)            :: startTime
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: runDuration
      integer,                 intent(in),  optional :: runTimeStepCount
      type(ESMF_Time),         intent(in),  optional :: refTime
      character (len=*),       intent(in),  optional :: name
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Creates and sets the initial values in a new {\tt ESMF\_Clock}.
!
!     The arguments are:
!     \begin{description}
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
!     \item[{[name]}]
!          The name for the newly created clock.  If not specified, a
!          default unique name will be generated: "ClockNNN" where NNN
!          is a unique sequence number from 001 to 999.
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

      ! check inputs
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeStep,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,startTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,stopTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,runDuration,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,refTime,rc)

      nameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

      ! invoke C to C++ entry point to allocate and initialize new clock
      call c_ESMC_ClockCreateNew(ESMF_ClockCreateNew, nameLen, name, &
                                 timeStep, startTime, stopTime, runDuration, &
                                 runTimeStepCount, refTime, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
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
      function ESMF_ClockCreateCopy(clock, keywordEnforcer, rc)

! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockCreateCopy

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Creates a deep copy of a given {\tt ESMF\_Clock}, but does not copy its
!     list of {\tt ESMF\_Alarm}s (pointers), since an {\tt ESMF\_Alarm} can only
!     be associated with one {\tt ESMF\_Clock}.  Hence, the returned
!     {\tt ESMF\_Clock} copy has no associated {\tt ESMF\_Alarm}s, the same as
!     with a newly created {\tt ESMF\_Clock}.  If desired, new
!     {\tt ESMF\_Alarm}s must be created and associated with this copied
!     {\tt ESMF\_Clock} via {\tt ESMF\_AlarmCreate()}, or existing
!     {\tt ESMF\_Alarm}s must be re-associated with this copied
!     {\tt ESMF\_Clock} via {\tt ESMF\_AlarmSet(...clock=...)}.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point to copy clock
      call c_ESMC_ClockCreateCopy(ESMF_ClockCreateCopy, clock, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_ClockSetInitCreated(ESMF_ClockCreateCopy)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockDestroy()"
!BOP
! !IROUTINE: ESMF_ClockDestroy - Release resources associated with a Clock
!
! !INTERFACE:
      subroutine ESMF_ClockDestroy(clock, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)          :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out),  optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Releases resources associated with this {\tt ESMF\_Clock}.  This releases
!     the list of associated {\tt ESMF\_Alarm}s (pointers), but not the
!     {\tt ESMF\_Alarm}s themselves; the user must explicitly call
!     {\tt ESMF\_AlarmDestroy()} on each {\tt ESMF\_Alarm} to release its
!     resources.  {\tt ESMF\_ClockDestroy()} and corresponding
!     {\tt ESMF\_AlarmDestroy()}s can be called in either order.
!     \end{sloppypar}
!
!     \begin{sloppypar}
!     If {\tt ESMF\_ClockDestroy()} is called before {\tt ESMF\_AlarmDestroy()},
!     any {\tt ESMF\_Alarm}s that were in the {\tt ESMF\_Clock}'s list will
!     no longer be associated with any {\tt ESMF\_Clock}.  If desired,
!     these "orphaned" {\tt ESMF\_Alarm}s can be associated with a different
!     {\tt ESMF\_Clock} via a call to {\tt ESMF\_AlarmSet(...clock=...)}.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!       Release resources associated with this {\tt ESMF\_Clock} and mark the
!       object as invalid.  It is an error to pass this object into any other
!       routines after being destroyed.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockDestroy(clock, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) then
        ! Don't bail out until Delete
        write(*,*)" c_ESMC_ClockDestroy fails"
      endif

      ! mark output as successfully deleted
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
      subroutine ESMF_ClockGet(clock, keywordEnforcer, &
        timeStep, startTime, stopTime, &
        runDuration, runTimeStepCount, refTime, currTime, prevTime, &
        currSimTime, prevSimTime, calendar, calkindflag, timeZone, &
        advanceCount, alarmCount, direction, name, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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
      type(ESMF_CalKind_Flag), intent(out), optional :: calkindflag
      integer,                 intent(out), optional :: timeZone
      integer(ESMF_KIND_I8),   intent(out), optional :: advanceCount
      integer,                 intent(out), optional :: alarmCount
      type(ESMF_Direction_Flag),    intent(out), optional :: direction
      character (len=*),       intent(out), optional :: name
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets one or more of the properties of an {\tt ESMF\_Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to query.
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
!     \item[{[calkindflag]}]
!          The {\tt CalKind\_Flag} on which all the {\tt Clock}'s times are
!          defined.
!     \item[{[timeZone]}]
!          The timezone within which all the {\tt Clock}'s times are defined.
!     \item[{[advanceCount]}]
!          \begin{sloppypar}
!          The number of times the {\tt ESMF\_Clock} has been advanced.
!          Increments in {\tt ESMF\_DIRECTION\_FORWARD} and decrements in
!          {\tt ESMF\_DIRECTION\_REVERSE}; see "direction" argument below and
!          in {\tt ESMF\_ClockSet()}.
!          \end{sloppypar}
!     \item[{[alarmCount]}]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list.
!     \item[{[direction]}]
!          The {\tt ESMF\_Clock}'s time stepping direction.  See also
!          {\tt ESMF\_ClockIsReverse()}, an alternative for convenient use in
!          "if" and "do while" constructs.
!     \item[{[name]}]
!          The name of this clock.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

      ! invoke C to C++ entry point
      call c_ESMC_ClockGet(clock, nameLen, tempNameLen, tempName, &
                           timeStep, startTime, stopTime, &
                           runDuration, runTimeStepCount, refTime, &
                           currTime, prevTime, currSimTime, prevSimTime, &
                           calendar, calkindflag, timeZone, advanceCount, &
                           alarmCount, direction, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! copy temp name back to given name to restore native Fortran
      !   storage style
      if (present(name)) then
        name = tempName(1:tempNameLen)
      endif

      ! mark outputs as successfully initialized
      call ESMF_TimeIntervalInit(timeStep)
      call ESMF_TimeInit(startTime)
      call ESMF_TimeInit(stopTime)
      call ESMF_TimeIntervalInit(runDuration)
      call ESMF_TimeInit(refTime)
      call ESMF_TimeInit(currTime)
      call ESMF_TimeInit(prevTime)
      call ESMF_TimeIntervalInit(currSimTime)
      call ESMF_TimeIntervalInit(prevSimTime)
      call ESMF_CalendarSetInitCreated(calendar)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockGetAlarm()"
!BOP
! !IROUTINE: ESMF_ClockGetAlarm - Get an Alarm in a Clock's Alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetAlarm(clock, alarmname, alarm, &
        keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      character (len=*), intent(in)            :: alarmname
      type(ESMF_Alarm),  intent(out)           :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets the {\tt alarm} whose name is the value of alarmname in the
!     {\tt clock}'s {\tt ESMF\_Alarm} list.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt ESMF\_Alarm} from.
!     \item[alarmname]
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
      integer :: alarmnameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      alarmnameLen = len_trim(alarmname)

      ! invoke C to C++ entry point
      call c_ESMC_ClockGetAlarm(clock, alarmnameLen, alarmname, alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
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
      subroutine ESMF_ClockGetAlarmList(clock, alarmlistflag, &
        keywordEnforcer, timeStep, alarmList, alarmCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock),          intent(in)            :: clock
      type(ESMF_AlarmList_Flag), intent(in)            :: alarmlistflag
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_TimeInterval),   intent(in),  optional :: timeStep
      type(ESMF_Alarm),          intent(out), optional :: alarmList(:)
      integer,                   intent(out), optional :: alarmCount
      integer,                   intent(out), optional :: rc
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets the {\tt clock}'s list of alarms and/or number of alarms.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance from which to get an {\tt ESMF\_Alarm} list
!          and/or count of {\tt ESMF\_Alarm}s.
!     \item[alarmlistflag]
!          The kind of list to get:
!
!            {\tt ESMF\_ALARMLIST\_ALL} :
!                Returns the {\tt ESMF\_Clock}'s entire list of alarms.
!
!            {\tt ESMF\_ALARMLIST\_NEXTRINGING} :
!                Return only those alarms that will ring upon the next
!                {\tt clock} time step.  Can optionally specify argument
!                {\tt timeStep} (see below) to use instead of the {\tt clock}'s.
!                See also method {\tt ESMF\_AlarmWillRingNext()} for checking a
!                single alarm.
!
!            {\tt ESMF\_ALARMLIST\_PREVRINGING} :
!                \begin{sloppypar}
!                Return only those alarms that were ringing on the previous
!                {\tt ESMF\_Clock} time step.  See also method
!                {\tt ESMF\_AlarmWasPrevRinging()} for checking a single alarm.
!                \end{sloppypar}
!
!            {\tt ESMF\_ALARMLIST\_RINGING} :
!                Returns only those {\tt clock} alarms that are currently
!                ringing.  See also method {\tt ESMF\_ClockAdvance()} for
!                getting the list of ringing alarms subsequent to a time step.
!                See also method {\tt ESMF\_AlarmIsRinging()} for checking a
!                single alarm.
!     \item[{[timeStep]}]
!          \begin{sloppypar}
!          Optional time step to be used instead of the {\tt clock}'s.
!          Only used with {\tt ESMF\_ALARMLIST\_NEXTRINGING alarmlistflag}
!          (see above); ignored if specified with other {\tt alarmlistflags}.
!          \end{sloppypar}
!     \item[{[alarmList]}]
!          The array of returned alarms.  If given, the array must be large
!          enough to hold the number of alarms of the specified
!          {\tt alarmlistflag} in the specified {\tt clock}.
!     \item[{[alarmCount]}]
!          If specified, returns the number of {\tt ESMF\_Alarm}s of the
!          specified {\tt alarmlistflag} in the specified {\tt clock}.
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

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeStep,rc)

      if (.not.present(alarmList)) then
        ! only get alarmCount if specified
        sizeofAlarmList = 0
        ! invoke C to C++ entry point
        call c_ESMC_ClockGetAlarmList3(clock, alarmlistflag, &
                           sizeofAlarmList, alarmCount, timeStep, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! get alarmList and alarmCount if specified

        sizeofAlarmList = size(alarmList)

        ! for init macros
        allocate(alarmPtrList(sizeofAlarmList))

        ! invoke C to C++ entry point
        if (sizeofAlarmList > 1) then
          ! pass address of 2nd element for C++ to calculate array step size
          call c_ESMC_ClockGetAlarmList2(clock, alarmlistflag, &
                             alarmPtrList(1), alarmPtrList(2), &
                             sizeofAlarmList, alarmCount, timeStep, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) then
            ! do not bail out until deallocation
            write(*,*)"c_ESMC_ClockGetAlarmList2 fails"
          endif
        else
          ! array has only one element
          call c_ESMC_ClockGetAlarmList1(clock, alarmlistflag, &
                             alarmPtrList(1), &
                             sizeofAlarmList, alarmCount, timeStep, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) then
            ! do not bail out until deallocation
            write(*,*)"c_ESMC_ClockGetAlarmList1 fails"
          endif
        endif

        ! post-process alarm list
        do i=1,sizeofAlarmList
           call ESMF_AlarmSetThis(alarmList(i),alarmPtrList(i))
           ! mark output as successfully initialized
           call ESMF_AlarmSetInitCreated(alarmList(i))
        enddo

        ! Get rid of temporary list
        deallocate(alarmPtrList)

      endif

      ! Return success
      if (present(rc)) rc = localrc
      end subroutine ESMF_ClockGetAlarmList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockGetNextTime()"
!BOP
! !IROUTINE: ESMF_ClockGetNextTime - Calculate a Clock's next time

! !INTERFACE:
      subroutine ESMF_ClockGetNextTime(clock, nextTime, keywordEnforcer, &
        timeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)            :: clock
      type(ESMF_Time),         intent(out)           :: nextTime
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeStep,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockGetNextTime(clock, nextTime, timeStep, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_TimeInit(nextTime)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockGetNextTime


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockIsCreated()"
!BOP
! !IROUTINE: ESMF_ClockIsCreated - Check whether a Clock object has been created

! !INTERFACE:
  function ESMF_ClockIsCreated(clock, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_ClockIsCreated
!
! !ARGUMENTS:
    type(ESMF_Clock), intent(in)            :: clock
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt clock} has been created. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[clock]
!     {\tt ESMF\_Clock} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ESMF_ClockIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_ClockGetInit(clock)==ESMF_INIT_CREATED) &
      ESMF_ClockIsCreated = .true.
  end function
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockIsDone()"
!BOP
! !IROUTINE: ESMF_ClockIsDone - Based on its direction, test if the Clock has reached or exceeded its stop time or start time

! !INTERFACE:
      function ESMF_ClockIsDone(clock, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsDone

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Returns true if currentTime is greater than or equal to stopTime
!     in {\tt ESMF\_DIRECTION\_FORWARD}, or if currentTime is less than or
!     equal to startTime in {\tt ESMF\_DIRECTION\_REVERSE}.  It returns false
!     otherwise.
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

      ! Initialize output value in case of error
      ESMF_ClockIsDone = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockIsDone(clock, ESMF_ClockIsDone, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      function ESMF_ClockIsReverse(clock, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsReverse

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Returns true if clock is in {\tt ESMF\_DIRECTION\_REVERSE}, and false if
!     in {\tt ESMF\_DIRECTION\_FORWARD}.  Allows convenient use in "if" and
!     "do while" constructs.  Alternative to
!     {\tt ESMF\_ClockGet(...direction=...)}.
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

      ! Initialize output value in case of error
      ESMF_ClockIsReverse = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockIsReverse(clock, ESMF_ClockIsReverse, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      function ESMF_ClockIsStopTime(clock, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTime

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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

      ! Initialize output value in case of error
      ESMF_ClockIsStopTime = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockIsStopTime(clock, ESMF_ClockIsStopTime, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      function ESMF_ClockIsStopTimeEnabled(clock, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTimeEnabled

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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

      ! Initialize output value in case of error
      ESMF_ClockIsStopTimeEnabled = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockIsStopTimeEnabled(clock, ESMF_ClockIsStopTimeEnabled, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_ClockIsStopTimeEnabled

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockPrint()"
!BOP
! !IROUTINE:  ESMF_ClockPrint - Print Clock information

! !INTERFACE:
      subroutine ESMF_ClockPrint(clock, keywordEnforcer, options, preString, unit, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      character (len=*), intent(in),  optional :: options
      character(*),      intent(in),  optional :: preString
      character(*),      intent(out), optional :: unit
      integer,           intent(out), optional :: rc

!
! !DESCRIPTION:
!     Prints out an {\tt ESMF\_Clock}'s properties to {\tt stdout}, in
!     support of testing and debugging.  The options control the type of
!     information and level of detail. \\
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
!     \item[{[preString]}]
!          Optionally prepended string. Default to empty string.
!     \item[{[unit]}]
!          Internal unit, i.e. a string. Default to printing to stdout.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code
      character(len=80)       :: optionsOpt
      type(ESMF_Time)         :: time

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      if (present(unit).or.present(preString)) then
        ! simple, single line print format

        if (present(options)) then
          optionsOpt=trim(options)
        else
          optionsOpt="currTime"
        endif

        if (trim(optionsOpt)=="currTime") then
          call ESMF_ClockGet(clock, currTime=time, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          call ESMF_TimePrint(time, preString=preString, unit=unit, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else if (trim(optionsOpt)=="startTime") then
          call ESMF_ClockGet(clock, startTime=time, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          call ESMF_TimePrint(time, preString=preString, unit=unit, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else if (trim(optionsOpt)=="stopTime") then
          call ESMF_ClockGet(clock, stopTime=time, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          call ESMF_TimePrint(time, preString=preString, unit=unit, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Unknown selection requested.", &
            ESMF_CONTEXT, rcToReturn=rc)
          return
        endif

      else
        ! print to STDOUT

        ! invoke C to C++ entry point
        call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

        call c_ESMC_ClockPrint(clock, options, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_ClockPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ClockReadRestart()"
!BOPI
! !IROUTINE: ESMF_ClockReadRestart - Restore the contents of a Clock (not implemented)

! !INTERFACE:
      function ESMF_ClockReadRestart(name, keywordEnforcer, rc)
!
! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockReadRestart
!
! !ARGUMENTS:
      character (len=*), intent(in)            :: name
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Clock} object from the last call to
!     {\tt ESMF\_ClockWriteRestart()}.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          The name of the object instance to restore.
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

      ! invoke C to C++ entry point to allocate and restore clock
      call c_ESMC_ClockReadRestart(ESMF_ClockReadRestart, nameLen, name, &
                                   localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
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
      subroutine ESMF_ClockSet(clock, keywordEnforcer, &
        timeStep, startTime, stopTime, &
        runDuration, runTimeStepCount, refTime, currTime, advanceCount, &
        direction, name, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(inout)         :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      type(ESMF_Time),         intent(in),  optional :: startTime
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: runDuration
      integer,                 intent(in),  optional :: runTimeStepCount
      type(ESMF_Time),         intent(in),  optional :: refTime
      type(ESMF_Time),         intent(in),  optional :: currTime
      integer(ESMF_KIND_I8),   intent(in),  optional :: advanceCount
      type(ESMF_Direction_Flag),    intent(in),  optional :: direction
      character (len=*),       intent(in),  optional :: name
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     \begin{sloppypar}
!     Sets/resets one or more of the properties of an {\tt ESMF\_Clock} that
!     was previously initialized via {\tt ESMF\_ClockCreate()}.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set.
!     \item[{[timeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval, which can be positive or
!          negative.  This is used to change a clock's timestep property for
!          those applications that need variable timesteps.  See
!          {\tt ESMF\_ClockAdvance()} below for specifying variable timesteps
!          that are NOT saved as the clock's internal time step property.
!          See "direction" argument below for behavior with
!          {\\t ESMF\_DIRECTION\_REVERSE} direction.
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
!          {\tt ESMF\_DIRECTION\_REVERSE}, sets the clock in "reverse" mode,
!          causing it to timestep back towards its startTime.  If called
!          with {\tt ESMF\_DIRECTION\_FORWARD}, sets the clock in normal,
!          "forward" mode, causing it to timestep in the direction of its
!          startTime to stopTime.  This holds true for negative timestep
!          clocks as well, which are initialized (created) with
!          stopTime < startTime.  The default mode is
!          {\tt ESMF\_DIRECTION\_FORWARD}, established at
!          {\tt ESMF\_ClockCreate()}.  timeStep can also be specified as an
!          argument at the same time, which allows for a change in magnitude
!          and/or sign of the clock's timeStep.  If not specified with
!          {\tt ESMF\_DIRECTION\_REVERSE}, the clock's current timeStep is
!          effectively negated.  If timeStep is specified, its sign is used as
!          specified; it is not negated internally.  E.g., if the specified
!          timeStep is negative and the clock is placed in
!          {\tt ESMF\_DIRECTION\_REVERSE}, subsequent calls to
!          {\tt ESMF\_ClockAdvance()} will cause the clock's current time to
!          be decremented by the new timeStep's magnitude.
!     \item[{[name]}]
!          The new name for this clock.
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

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeStep,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,startTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,stopTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,runDuration,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,refTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,currTime,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

      ! invoke C to C++ entry point
      call c_ESMC_ClockSet(clock, nameLen, name, timeStep, startTime, &
                           stopTime, runDuration, runTimeStepCount, &
                           refTime, currTime, advanceCount, direction, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_ClockStopTimeDisable(clock, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockStopTimeDisable(clock, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_ClockStopTimeEnable(clock, keywordEnforcer, stopTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Time),  intent(in),  optional :: stopTime
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,stopTime,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockStopTimeEnable(clock, stopTime, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_ClockSyncToRealTime(clock, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockSyncToRealTime(clock, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_ClockValidate(clock, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code
      character :: options ! dummy options

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockValidate(clock, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_ClockWriteRestart(clock, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Clock} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to save.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)

      ! invoke C to C++ entry point
      call c_ESMC_ClockWriteRestart(clock, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      ESMF_INIT_TYPE clockinit1, clockinit2
      integer :: localrc1, localrc2
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      clockinit1 = ESMF_ClockGetInit(clock1)
      clockinit2 = ESMF_ClockGetInit(clock2)

      if (clockinit1.eq.ESMF_INIT_CREATED.and. &
          clockinit2.eq.ESMF_INIT_CREATED) then
        ! invoke C to C++ entry point
        call c_ESMC_ClockEQ(clock1, clock2, ESMF_ClockEQ)
      else
        ! log error, convert to return code, and compare
        lval1 = ESMF_IMErr(clockinit1, ESMF_CONTEXT, rc=localrc1)
        lval2 = ESMF_IMErr(clockinit2, ESMF_CONTEXT, rc=localrc2)
        ESMF_ClockEQ = localrc1.eq.localrc2
      endif

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
      ESMF_INIT_TYPE clockinit1, clockinit2
      integer :: localrc1, localrc2
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      clockinit1 = ESMF_ClockGetInit(clock1)
      clockinit2 = ESMF_ClockGetInit(clock2)

      if (clockinit1.eq.ESMF_INIT_CREATED.and. &
          clockinit2.eq.ESMF_INIT_CREATED) then
        ! invoke C to C++ entry point
        call c_ESMC_ClockNE(clock1, clock2, ESMF_ClockNE)
      else
        ! log error, convert to return code, and compare
        lval1 = ESMF_IMErr(clockinit1, ESMF_CONTEXT, rc=localrc1)
        lval2 = ESMF_IMErr(clockinit2, ESMF_CONTEXT, rc=localrc2)
        ESMF_ClockNE = localrc1.ne.localrc2
      endif

      end function ESMF_ClockNE

!------------------------------------------------------------------------------

      end module ESMF_ClockMod
