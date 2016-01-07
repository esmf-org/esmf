! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Alarm.F90"
!==============================================================================
!
!     ESMF Alarm Module
      module ESMF_AlarmMod
!
!==============================================================================
!
! This file contains the Alarm class definition and all Alarm class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_TimeMgr.inc"
#include "ESMF.h"

!===============================================================================
!BOPI
!
! !MODULE: ESMF_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}.
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description.
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
      use ESMF_TimeIntervalMod
      use ESMF_TimeIntervalTypeMod
      use ESMF_TimeMod
      use ESMF_TimeTypeMod
      use ESMF_ClockTypeMod

      ! type definition for this module
      use ESMF_AlarmTypeMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Alarm definition in ESMF_AlarmTypeMod to resolve mutual
!     ! method dependency with ESMF_Clock
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     These types are defined in ESMF_AlarmTypeMod and propagated up from here.
!
      public ESMF_AlarmList_Flag
      public ESMF_ALARMLIST_ALL, ESMF_ALARMLIST_RINGING, &
             ESMF_ALARMLIST_NEXTRINGING, ESMF_ALARMLIST_PREVRINGING
      public ESMF_Alarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
      public operator(==)
      public operator(/=)
      public ESMF_AlarmCreate
      public ESMF_AlarmDestroy
      public ESMF_AlarmDisable
      public ESMF_AlarmEnable
      public ESMF_AlarmGet
      public ESMF_AlarmIsCreated
      public ESMF_AlarmIsEnabled
      public ESMF_AlarmIsRinging
      public ESMF_AlarmIsSticky
      public ESMF_AlarmNotSticky
      public ESMF_AlarmPrint
      public ESMF_AlarmReadRestart
      public ESMF_AlarmRingerOff
      public ESMF_AlarmRingerOn
      public ESMF_AlarmSet
      public ESMF_AlarmSticky
      public ESMF_AlarmValidate
      public ESMF_AlarmWasPrevRinging
      public ESMF_AlarmWillRingNext
      public ESMF_AlarmWriteRestart

! - ESMF-internal methods:
      public ESMF_AlarmGetInit
      public ESMF_AlarmSetInitCreated
      public ESMF_AlarmSetInitDeleted
      public ESMF_AlarmGetThis
      public ESMF_AlarmSetThis

!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_AlarmEQ
      private ESMF_AlarmNE
      private ESMF_AlarmCreateNew
      private ESMF_AlarmCreateCopy

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
! !IROUTINE:  ESMF_AlarmAssignment(=) - Assign an Alarm to another Alarm
!
! !INTERFACE:
!     interface assignment(=)
!     alarm1 = alarm2
!
! !ARGUMENTS:
!     type(ESMF_Alarm) :: alarm1
!     type(ESMF_Alarm) :: alarm2
! 
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Assign {\tt alarm1} as an alias to the same {\tt ESMF\_Alarm} object in 
!     memory as {\tt alarm2}. If {\tt alarm2} is invalid, then {\tt alarm1} 
!     will be equally invalid after the assignment.
!
!     The arguments are:
!     \begin{description} 
!     \item[alarm1] 
!          The {\tt ESMF\_Alarm} object on the left hand side of the 
!          assignment.
!     \item[alarm2] 
!          The {\tt ESMF\_Alarm} object on the right hand side of the 
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
! !IROUTINE:  ESMF_AlarmOperator(==) - Test if Alarm 1 is equal to Alarm 2
!
! !INTERFACE:
      interface operator(==)
!     if (alarm1 == alarm2) then ... endif
!                  OR
!     result = (alarm1 == alarm2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Alarm), intent(in) :: alarm1
!     type(ESMF_Alarm), intent(in) :: alarm2
!
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Alarm} class.
!     Compare two alarms for equality; return {\tt .true.} if equal,
!     {\tt .false.} otherwise. Comparison is based on IDs, which are distinct
!     for newly created alarms and identical for alarms created as copies.
!
!     If either side of the equality test is not in the
!     {\tt ESMF\_INIT\_CREATED} status an error will be logged. However, this
!     does not affect the return value, which is {\tt .true.} when both
!     sides are in the {\em same} status, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The {\tt ESMF\_Alarm} object on the left hand side of the equality
!          operation.
!     \item[alarm2]
!          The {\tt ESMF\_Alarm} object on the right hand side of the equality
!          operation.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmEQ
!
! !REQUIREMENTS:
!     TMGx.x.x

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmOperator(/=) - Test if Alarm 1 is not equal to Alarm 2
!
! !INTERFACE:
      interface operator(/=)
!     if (alarm1 /= alarm2) then ... endif
!                  OR
!     result = (alarm1 /= alarm2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Alarm), intent(in) :: alarm1
!     type(ESMF_Alarm), intent(in) :: alarm2
!
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Alarm} class.
!     Compare two alarms for inequality; return {\tt .true.} if not equal,
!     {\tt .false.} otherwise. Comparison is based on IDs, which are distinct
!     for newly created alarms and identical for alarms created as copies.
!
!     If either side of the equality test is not in the
!     {\tt ESMF\_INIT\_CREATED} status an error will be logged. However, this
!     does not affect the return value, which is {\tt .true.} when both sides
!     are {\em not} in the {\em same} status, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The {\tt ESMF\_Alarm} object on the left hand side of the 
!          non-equality operation.
!     \item[alarm2]
!          The {\tt ESMF\_Alarm} object on the right hand side of the 
!          non-equality operation.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmNE
!
! !REQUIREMENTS:
!     TMGx.x.x

      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_AlarmCreate - Create an ESMF Alarm
!
! !INTERFACE:
      interface ESMF_AlarmCreate    

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmCreateNew
      module procedure ESMF_AlarmCreateCopy

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Alarm} Create
!     methods.
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmCreateNew()"
!BOP
! !IROUTINE: ESMF_AlarmCreate - Create a new ESMF Alarm

! !INTERFACE:
      ! Private name; call using ESMF_AlarmCreate()
      function ESMF_AlarmCreateNew(clock, keywordEnforcer, &
        ringTime, ringInterval, stopTime, ringDuration, ringTimeStepCount, &
        refTime, enabled, sticky, name, rc)

! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmCreateNew

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)            :: clock
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: ringTimeStepCount
      type(ESMF_Time),         intent(in),  optional :: refTime
      logical,                 intent(in),  optional :: enabled
      logical,                 intent(in),  optional :: sticky
      character (len=*),       intent(in),  optional :: name
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Creates and sets the initial values in a new {\tt ESMF\_Alarm}.
!
!     In {\tt ESMF\_DIRECTION\_REVERSE} (see Section~\ref{sec:Clock}), alarms 
!     ring in reverse, i.e., they begin ringing when they originally ended, 
!     and end ringing when they originally began.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The clock with which to associate this newly created alarm.
!     \item[{[ringTime]}]
!          The ring time for a one-shot alarm or the first ring time for a 
!          repeating (interval) alarm.  Must specify at least one of ringTime
!          or ringInterval.
!     \item[{[ringInterval]}]
!          The ring interval for repeating (interval) alarms.  If
!          {\tt ringTime} is not also specified (first ring time), it will be
!          calculated as the {\tt clock}'s current time plus {\tt ringInterval}.
!          Must specify at least one of ringTime or ringInterval.
!     \item[{[stopTime]}]
!          The stop time for repeating (interval) alarms.  If not
!          specified, an interval alarm will repeat forever.
!     \item[{[ringDuration]}]
!          The absolute ring duration.  If not sticky (see argument below),
!          alarms rings for ringDuration, then turns itself off.  Default is
!          zero (unused).  Mutually exclusive with ringTimeStepCount (below);
!          used only if set to a non-zero duration and ringTimeStepCount is 1
!          (see below).
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[ringTimeStepCount]}]
!          The relative ring duration.  If not sticky (see argument below),
!          alarms rings for ringTimeStepCount, then turns itself off.
!          Default is 1: a non-sticky alarm will ring for one clock time step.
!          Mutually exclusive with ringDuration (above); used if
!          ringTimeStepCount > 1.  If ringTimeStepCount is 1 (default) and
!          ringDuration is non-zero, ringDuration is used (see above), otherwise
!          ringTimeStepCount is used.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[refTime]}]
!          The reference (i.e. base) time for an interval alarm.
!     \item[{[enabled]}]
!          Sets the enabled state; default is on (true).  If disabled,
!          an alarm will not function at all.
!          See also {\tt ESMF\_AlarmEnable()}, {\tt ESMF\_AlarmDisable()}.
!     \item[{[sticky]}]
!          Sets the sticky state; default is on (true).  If sticky,
!          once an alarm is ringing, it will remain ringing until turned off 
!          manually via a user call to {\tt ESMF\_AlarmRingerOff()}.
!          If not sticky, an alarm will turn itself off after a certain
!          ring duration specified by either ringDuration or
!          ringTimeStepCount (see above).  There is an implicit limitation
!          that in order to properly reverse timestep through a ring end
!          time in {\tt ESMF\_DIRECTION\_REVERSE}, that time must have already
!          been traversed in the forward direction.  This is due to the fact
!          that the Time Manager cannot predict when user code will call
!          {\tt ESMF\_AlarmRingerOff()}.  An error message will be logged
!          when this limitation is not satisfied.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[name]}]
!          The name for the newly created alarm.  If not specified,
!          a default unique name will be generated: "AlarmNNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7

      ! initialize name length to zero for non-existent name
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ringTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ringInterval,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,stopTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ringDuration,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,refTime,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

      ! invoke C to C++ entry point to allocate and initialize new alarm
      call c_ESMC_AlarmCreateNew(ESMF_AlarmCreateNew, nameLen, name, clock, &
                                 ringTime, ringInterval, stopTime, &
                                 ringDuration, ringTimeStepCount, refTime, &
                                 enabled, sticky, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_AlarmSetInitCreated(ESMF_AlarmCreateNew)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmCreateNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmCreateCopy()"
!BOP
! !IROUTINE: ESMF_AlarmCreate - Create a copy of an existing ESMF Alarm

! !INTERFACE:
      ! Private name; call using ESMF_AlarmCreate()
      function ESMF_AlarmCreateCopy(alarm, keywordEnforcer, rc)

! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmCreateCopy

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Creates a complete (deep) copy of a given {\tt ESMF\_Alarm}.
!     The returned {\tt ESMF\_Alarm} copy is associated with the same
!     {\tt ESMF\_Clock} as the original {\tt ESMF\_Alarm}.  If desired, use
!     {\tt ESMF\_AlarmSet(...clock=...)} to re-associate the 
!     {\tt ESMF\_Alarm} copy with a different {\tt ESMF\_Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!        The {\tt ESMF\_Alarm} to copy.
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
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point to copy alarm
      call c_ESMC_AlarmCreateCopy(ESMF_AlarmCreateCopy, alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_AlarmSetInitCreated(ESMF_AlarmCreateCopy)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmDestroy()"
!BOP
! !IROUTINE: ESMF_AlarmDestroy - Release resources associated with an Alarm
!
! !INTERFACE:
      subroutine ESMF_AlarmDestroy(alarm, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)          :: alarm
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
!     Releases resources associated with this {\tt ESMF\_Alarm}.  Also
!     removes this {\tt ESMF\_Alarm} from its associated {\tt ESMF\_Clock}'s
!     list of {\tt ESMF\_Alarm}s (removes the {\tt ESMF\_Alarm} pointer from
!     the list).
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!       Release resources associated with this {\tt ESMF\_Alarm} and mark the
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
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmDestroy(alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully deleted
      call ESMF_AlarmSetInitDeleted(alarm)

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmDisable()"
!BOP
! !IROUTINE: ESMF_AlarmDisable - Disable an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmDisable(alarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Disables an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to disable.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.5.3
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
    
      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmDisable(alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmDisable

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmEnable()"
!BOP
! !IROUTINE: ESMF_AlarmEnable - Enable an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmEnable(alarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Enables an {\tt ESMF\_Alarm} to function.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to enable.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.5.3
      integer :: localrc                      ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmEnable(alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmEnable

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmGet()"
!BOP
! !IROUTINE: ESMF_AlarmGet - Get Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmGet(alarm, keywordEnforcer, &
        clock, ringTime, prevRingTime, ringInterval, stopTime, ringDuration, &
        ringTimeStepCount, timeStepRingingCount, ringBegin, ringEnd, &
        refTime, ringing, ringingOnPrevTimeStep, enabled, sticky, name, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(in)            :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Clock),        intent(out), optional :: clock
      type(ESMF_Time),         intent(out), optional :: ringTime
      type(ESMF_Time),         intent(out), optional :: prevRingTime
      type(ESMF_TimeInterval), intent(out), optional :: ringInterval
      type(ESMF_Time),         intent(out), optional :: stopTime
      type(ESMF_TimeInterval), intent(out), optional :: ringDuration
      integer,                 intent(out), optional :: ringTimeStepCount
      integer,                 intent(out), optional :: timeStepRingingCount
      type(ESMF_Time),         intent(out), optional :: ringBegin
      type(ESMF_Time),         intent(out), optional :: ringEnd
      type(ESMF_Time),         intent(out), optional :: refTime
      logical,                 intent(out), optional :: ringing
      logical,                 intent(out), optional :: ringingOnPrevTimeStep
      logical,                 intent(out), optional :: enabled
      logical,                 intent(out), optional :: sticky
      character (len=*),       intent(out), optional :: name
      integer,                 intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Gets one or more of an {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to query.
!     \item[{[clock]}]
!          The associated clock.
!     \item[{[ringTime]}]
!          The ring time for a one-shot alarm or the next repeating alarm.
!     \item[{[prevRingTime]}]
!          The previous ring time.
!     \item[{[ringInterval]}]
!          The ring interval for repeating (interval) alarms.
!     \item[{[stopTime]}]
!          The stop time for repeating (interval) alarms.
!     \item[{[ringDuration]}]
!          The ring duration.  Mutually exclusive with 
!          ringTimeStepCount (see below).
!     \item[{[ringTimeStepCount]}]
!          The number of time steps comprising the ring duration.  Mutually
!          exclusive with ringDuration (see above).
!     \item[{[timeStepRingingCount]}]
!          The number of time steps for which the alarm has been ringing thus
!          far.  Used internally for tracking ringTimeStepCount ring 
!          durations (see above).  Mutually exclusive with ringBegin
!          (see below).  Increments in {\tt ESMF\_DIRECTION\_FORWARD} and 
!          decrements in {\tt ESMF\_DIRECTION\_REVERSE}; 
!          see Section~\ref{sec:Clock}.
!     \item[{[ringBegin]}]
!          The time when the alarm began ringing.  Used internally for tracking
!          ringDuration (see above).  Mutually exclusive with
!          timeStepRingingCount (see above).
!     \item[{[ringEnd]}]
!	   \begin{sloppypar}
!          The time when the alarm ended ringing.  Used internally for
!          re-ringing alarm in {\tt ESMF\_DIRECTION\_REVERSE}.
!	   \end{sloppypar}
!     \item[{[refTime]}]
!          The reference (i.e. base) time for an interval alarm.
!     \item[{[ringing]}]
!          The current ringing state.
!          See also {\tt ESMF\_AlarmRingerOn()}, {\tt ESMF\_AlarmRingerOff()}.
!     \item[{[ringingOnPrevTimeStep]}]
!          \begin{sloppypar}
!          The ringing state upon the previous time step. Same as
!          {\tt ESMF\_AlarmWasPrevRinging()}.
!          \end{sloppypar}
!     \item[{[enabled]}]
!          The enabled state.
!          See also {\tt ESMF\_AlarmEnable()}, {\tt ESMF\_AlarmDisable()}.
!     \item[{[sticky]}]
!          The sticky state. 
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[name]}]
!          The name of this alarm.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7

      ! temp name for C++ to fill
      character (len=ESMF_MAXSTR) :: tempName

      ! initialize name lengths to zero for non-existent name
      integer :: nameLen
      integer :: tempNameLen
      integer :: localrc             ! local return code
      nameLen = 0
      tempNameLen = 0

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

      ! invoke C to C++ entry point
      call c_ESMC_AlarmGet(alarm, nameLen, tempNameLen, tempName, clock, &
                    ringTime, prevRingTime, ringInterval, stopTime, &
                    ringDuration, ringTimeStepCount, &
                    timeStepRingingCount, ringBegin, ringEnd, refTime, &
                    ringing, ringingOnPrevTimeStep, enabled, sticky, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! copy temp name back to given name to restore native Fortran
      !   storage style
      if (present(name)) then
        name = tempName(1:tempNameLen)
      endif

      !  mark outputs as successfully initialized
      call ESMF_ClockSetInitCreated(clock)
      call ESMF_TimeInit(ringTime)
      call ESMF_TimeInit(prevRingTime)
      call ESMF_TimeIntervalInit(ringInterval)
      call ESMF_TimeInit(stopTime)
      call ESMF_TimeIntervalInit(ringDuration)
      call ESMF_TimeInit(ringBegin)
      call ESMF_TimeInit(ringEnd)
      call ESMF_TimeInit(refTime)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmGet



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmIsCreated()"
!BOP
! !IROUTINE: ESMF_AlarmIsCreated - Check whether a Alarm object has been created

! !INTERFACE:
  function ESMF_AlarmIsCreated(alarm, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_AlarmIsCreated
!
! !ARGUMENTS:
    type(ESMF_Alarm), intent(in)            :: alarm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt alarm} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[alarm]
!     {\tt ESMF\_Alarm} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_AlarmIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_AlarmGetInit(alarm)==ESMF_INIT_CREATED) &
      ESMF_AlarmIsCreated = .true.
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmIsEnabled()"
!BOP
! !IROUTINE:  ESMF_AlarmIsEnabled - Check if Alarm is enabled

! !INTERFACE:
      function ESMF_AlarmIsEnabled(alarm, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsEnabled

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is enabled.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for enabled state.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! Initialize output value in case of error
      ESMF_AlarmIsEnabled = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmIsEnabled(alarm, ESMF_AlarmIsEnabled, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmIsEnabled

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmIsRinging()"
!BOP
! !IROUTINE:  ESMF_AlarmIsRinging - Check if Alarm is ringing

! !INTERFACE:
      function ESMF_AlarmIsRinging(alarm, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is ringing.
!
!     See also method
!           {\tt ESMF\_ClockGetAlarmList(clock, ESMF\_ALARMLIST\_RINGING, ...)}
!     to get a list of all ringing alarms belonging to an {\tt ESMF\_Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The alarm to check for ringing state.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.4
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! Initialize output value in case of error
      ESMF_AlarmIsRinging = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
    
      ! invoke C to C++ entry point
      call c_ESMC_AlarmIsRinging(alarm, ESMF_AlarmIsRinging, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmIsRinging

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmIsSticky()"
!BOP
! !IROUTINE:  ESMF_AlarmIsSticky - Check if Alarm is sticky

! !INTERFACE:
      function ESMF_AlarmIsSticky(alarm, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsSticky

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Check if {\tt alarm} is sticky.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for sticky state.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! Initialize output value in case of error
      ESMF_AlarmIsSticky = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
    
      ! invoke C to C++ entry point
      call c_ESMC_AlarmIsSticky(alarm, ESMF_AlarmIsSticky, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmIsSticky

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmNotSticky()"
!BOP
! !IROUTINE:  ESMF_AlarmNotSticky - Unset an Alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmNotSticky(alarm, keywordEnforcer, &
        ringDuration, ringTimeStepCount, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: ringTimeStepCount
      integer,                 intent(out), optional :: rc
    
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Unset an {\tt ESMF\_Alarm}'s sticky flag; once alarm is ringing,
!     it turns itself off after ringDuration.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to unset sticky.
!     \item[{[ringDuration]}]
!          If not sticky, alarms rings for ringDuration, then turns itself off.
!          Mutually exclusive with ringTimeStepCount (see below and full
!          description in method {\tt ESMF\_AlarmCreate()} or
!          {\tt ESMF\_AlarmSet()}).
!     \item[{[ringTimeStepCount]}]
!          \begin{sloppypar}
!          If not sticky, alarms rings for ringTimeStepCount, then turns
!          itself off.  Mutually exclusive with ringDuration (see above and
!          full description in method {\tt ESMF\_AlarmCreate()} or
!          {\tt ESMF\_AlarmSet()}).
!          \end{sloppypar}
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

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ringDuration,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmNotSticky(alarm, ringDuration, &
                                 ringTimeStepCount, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmNotSticky

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmPrint()"
!BOP
! !IROUTINE:  ESMF_AlarmPrint - Print Alarm information

! !INTERFACE:
      subroutine ESMF_AlarmPrint(alarm, options, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),  intent(in)            :: alarm
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

!
! !DESCRIPTION:
!     Prints out an {\tt ESMF\_Alarm}'s properties to {\tt stdout}, in support
!     of testing and debugging.  The options control the type of information
!     and level of detail. \\
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all {\tt alarm} property values.\\
!          "clock"        - print the associated clock's name. \\
!          "enabled"      - print the alarm's ability to ring. \\
!          "name"         - print the alarm's name. \\
!          "prevRingTime" - print the alarm's previous ring time. \\
!          "ringBegin"    - print time when the alarm actually begins to ring.\\
!          "ringDuration" - print how long this alarm is to remain ringing. \\
!          "ringEnd"      - print time when the alarm actually ends ringing.\\
!          "ringing"                - print the alarm's current ringing state.\\
!          "ringingOnPrevTimeStep"  - print whether the alarm was ringing 
!                                     immediately after the previous clock
!                                     time step. \\
!          "ringInterval" - print the alarm's periodic ring interval. \\
!          "ringTime"     - print the alarm's next time to ring. \\
!          "ringTimeStepCount" - print how long this alarm is to remain
!                                ringing, in terms of a number of clock time
!                                steps. \\
!          "refTime"      - print the alarm's interval reference (base) time. \\
!          "sticky"       - print whether the alarm must be turned off
!                           manually. \\
!          "stopTime"     - print when alarm intervals end. \\
!          "timeStepRingingCount"   - print the number of time steps the
!                                     alarm has been ringing thus far. \\
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
      
      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call c_ESMC_AlarmPrint(alarm, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmReadRestart()"
!BOPI
! !IROUTINE: ESMF_AlarmReadRestart - Restore the contents of an Alarm (not implemented)

! !INTERFACE:
      function ESMF_AlarmReadRestart(name, keywordEnforcer, rc)
!
! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmReadRestart
!
! !ARGUMENTS:
      character (len=*), intent(in)            :: name
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Alarm} object from the last call to
!     {\tt ESMF\_AlarmWriteRestart()}.  (Not implemented yet).
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

      ! invoke C to C++ entry point to allocate and restore alarm
      call c_ESMC_AlarmReadRestart(ESMF_AlarmReadRestart, nameLen, name, &
                                   localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_AlarmSetInitCreated(ESMF_AlarmReadRestart) 

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmRingerOff()"
!BOP
! !IROUTINE:  ESMF_AlarmRingerOff - Turn off an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmRingerOff(alarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc
    
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Turn off an {\tt ESMF\_Alarm}; unsets ringing state.  For a sticky
!     alarm, this method must be called to turn off its ringing state.
!     This is true for either {\tt ESMF\_DIRECTION\_FORWARD} (default) or
!     {\tt ESMF\_DIRECTION\_REVERSE}.  See Section~\ref{sec:Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn off.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.6
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmRingerOff(alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmRingerOff

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmRingerOn()"
!BOP
! !IROUTINE:  ESMF_AlarmRingerOn - Turn on an Alarm


! !INTERFACE:
      subroutine ESMF_AlarmRingerOn(alarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc
    
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Turn on an {\tt ESMF\_Alarm}; sets ringing state.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn on.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.6
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmRingerOn(alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmRingerOn

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmSet()"
!BOP
! !IROUTINE: ESMF_AlarmSet - Set Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmSet(alarm, keywordEnforcer, &
        clock, ringTime, ringInterval, stopTime, ringDuration, &
        ringTimeStepCount, refTime, ringing, enabled, sticky, name, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      type(ESMF_Clock),        intent(in),  optional :: clock
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: ringTimeStepCount
      type(ESMF_Time),         intent(in),  optional :: refTime
      logical,                 intent(in),  optional :: ringing
      logical,                 intent(in),  optional :: enabled
      logical,                 intent(in),  optional :: sticky
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
!     Sets/resets one or more of the properties of an {\tt ESMF\_Alarm} that
!     was previously initialized via {\tt ESMF\_AlarmCreate()}.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set.
!     \item[{[clock]}]
!          Re-associates this alarm with a different clock.
!     \item[{[ringTime]}]
!          The next ring time for a one-shot alarm or a repeating (interval)
!          alarm.
!     \item[{[ringInterval]}]
!          The ring interval for repeating (interval) alarms.
!     \item[{[stopTime]}]
!          The stop time for repeating (interval) alarms.
!     \item[{[ringDuration]}]
!          The absolute ring duration.  If not sticky (see argument below),
!          alarms rings for ringDuration, then turns itself off.  Default is
!          zero (unused).  Mutually exclusive with ringTimeStepCount (below);
!          used only if set to a non-zero duration and ringTimeStepCount is 1
!          (see below).
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[ringTimeStepCount]}]
!          The relative ring duration.  If not sticky (see argument below),
!          alarms rings for ringTimeStepCount, then turns itself off.
!          Default is 1: a non-sticky alarm will ring for one clock time step.
!          Mutually exclusive with ringDuration (above); used if
!          ringTimeStepCount > 1.  If ringTimeStepCount is 1 (default) and
!          ringDuration is non-zero, ringDuration is used (see above), otherwise
!          ringTimeStepCount is used.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[refTime]}]
!          The reference (i.e. base) time for an interval alarm.
!     \item[{[ringing]}]
!          Sets the ringing state.
!          See also {\tt ESMF\_AlarmRingerOn()}, {\tt ESMF\_AlarmRingerOff()}.
!     \item[{[enabled]}]
!          Sets the enabled state.  If disabled, an alarm will not function
!          at all.
!          See also {\tt ESMF\_AlarmEnable()}, {\tt ESMF\_AlarmDisable()}.
!     \item[{[sticky]}]
!          Sets the sticky state.  If sticky, once an alarm is ringing, it
!          will remain ringing until turned off manually via a user call to
!          {\tt ESMF\_AlarmRingerOff()}.  If not sticky, an alarm will turn
!          itself off after a certain ring duration specified by either
!          ringDuration or ringTimeStepCount (see above).
!          There is an implicit limitation that in order to properly reverse
!          timestep through a ring end time in {\tt ESMF\_DIRECTION\_REVERSE},
!          that time must have already been traversed in the forward direction.
!          This is due to the fact that the Time Manager cannot predict when
!          user code will call {\tt ESMF\_AlarmRingerOff()}.  An error message
!          will be logged when this limitation is not satisfied.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[name]}]
!          The new name for this alarm.  
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7

      ! initialize name length to zero for non-existent name
      integer :: nameLen, localrc

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      nameLen = 0

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ringTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ringInterval,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,stopTime,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ringDuration,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,refTime,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

      ! invoke C to C++ entry point
      call c_ESMC_AlarmSet(alarm, nameLen, name, clock, ringTime, &
                           ringInterval, stopTime, ringDuration, &
                           ringTimeStepCount, refTime, ringing, &
                           enabled, sticky, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmSticky()"
!BOP
! !IROUTINE:  ESMF_AlarmSticky - Set an Alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmSticky(alarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc
    
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s sticky flag; once alarm is ringing,
!     it remains ringing until {\tt ESMF\_AlarmRingerOff()} is called.
!     There is an implicit limitation that in order to properly reverse
!     timestep through a ring end time in {\tt ESMF\_DIRECTION\_REVERSE}, that
!     time must have already been traversed in the forward direction.
!     This is due to the fact that an {\tt ESMF\_Alarm} cannot predict when
!     user code will call {\tt ESMF\_AlarmRingerOff()}.  An error message
!     will be logged when this limitation is not satisfied.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to be set sticky.
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

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmSticky(alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmSticky

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmValidate()"
!BOP
! !IROUTINE:  ESMF_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmValidate(alarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),  intent(in)            :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Performs a validation check on an {\tt ESMF\_Alarm}'s properties.
!     Must have a valid ringTime, set either directly or indirectly via
!     ringInterval.  See {\tt ESMF\_AlarmCreate()}.
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to be validated.
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
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmValidate(alarm, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmWasPrevRinging()"
!BOP
! !IROUTINE:  ESMF_AlarmWasPrevRinging - Check if Alarm was ringing on the previous Clock timestep

! !INTERFACE:
      function ESMF_AlarmWasPrevRinging(alarm, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmWasPrevRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} was ringing on the previous clock timestep.
!
!     See also method
!       {\tt ESMF\_ClockGetAlarmList(clock, ESMF\_ALARMLIST\_PREVRINGING, ...)}
!     get a list of all alarms belonging to a {\tt ESMF\_Clock} that were
!     ringing on the previous time step.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for previous ringing state.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.4
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! Initialize output value in case of error
      ESMF_AlarmWasPrevRinging = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
    
      ! invoke C to C++ entry point
      call c_ESMC_AlarmWasPrevRinging(alarm, ESMF_AlarmWasPrevRinging, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmWasPrevRinging

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmWillRingNext()"
!BOP
! !IROUTINE:  ESMF_AlarmWillRingNext - Check if Alarm will ring upon the next Clock timestep

! !INTERFACE:
      function ESMF_AlarmWillRingNext(alarm, keywordEnforcer, timeStep, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmWillRingNext

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(in)            :: alarm
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
!     Check if {\tt ESMF\_Alarm} will ring on the next clock timestep, either
!     the current clock timestep or a passed-in timestep.
!
!     See also method
!       {\tt ESMF\_ClockGetAlarmList(clock, ESMF\_ALARMLIST\_NEXTRINGING, ...)}
!     to get a list of all alarms belonging to a {\tt ESMF\_Clock} that will
!     ring on the next time step.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The alarm to check for next ringing state.
!     \item[{[timeStep]}]
!          Optional timestep to use instead of the clock's.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.4
      integer :: localrc                        ! local return code

      ! Assume failure until success
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
    
      ! Initialize output value in case of error
      ESMF_AlarmWillRingNext = .false.

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeStep,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmWillRingNext(alarm, timeStep,  &
                   ESMF_AlarmWillRingNext, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmWillRingNext

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmWriteRestart()"
!BOPI
! !IROUTINE: ESMF_AlarmWriteRestart - Save the contents of an Alarm (not implemented)

! !INTERFACE:
      subroutine ESMF_AlarmWriteRestart(alarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),  intent(in)            :: alarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Alarm} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
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
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_AlarmWriteRestart(alarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmWriteRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmEQ()"
!BOPI
! !IROUTINE:  ESMF_AlarmEQ - Compare two Alarms for equality
!
! !INTERFACE:
      function ESMF_AlarmEQ(alarm1, alarm2)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmEQ

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm1
      type(ESMF_Alarm), intent(in) :: alarm2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Alarm}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI
      ESMF_INIT_TYPE alarminit1, alarminit2
      integer :: localrc1, localrc2                 ! local return codes
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      alarminit1 = ESMF_AlarmGetInit(alarm1)
      alarminit2 = ESMF_AlarmGetInit(alarm2)

      if (alarminit1.eq.ESMF_INIT_CREATED.and. &
          alarminit2.eq.ESMF_INIT_CREATED) then
        ! invoke C to C++ entry point
        call c_ESMC_AlarmEQ(alarm1, alarm2, ESMF_AlarmEQ)
      else
        ! log error, convert to return code, and compare
        lval1 = ESMF_IMErr(alarminit1, ESMF_CONTEXT, rc=localrc1)
        lval2 = ESMF_IMErr(alarminit2, ESMF_CONTEXT, rc=localrc2)
        ESMF_AlarmEQ = localrc1.eq.localrc2
      endif

      end function ESMF_AlarmEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmNE()"
!BOPI
! !IROUTINE:  ESMF_AlarmNE - Compare two Alarms for inequality
!
! !INTERFACE:
      function ESMF_AlarmNE(alarm1, alarm2)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmNE

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm1
      type(ESMF_Alarm), intent(in) :: alarm2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Alarm}
!     class.  See "interface operator(/=)" above for complete description.
!
!EOPI
      ESMF_INIT_TYPE alarminit1, alarminit2
      integer :: localrc1, localrc2                 ! local return codes
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      alarminit1 = ESMF_AlarmGetInit(alarm1)
      alarminit2 = ESMF_AlarmGetInit(alarm2)

      if (alarminit1.eq.ESMF_INIT_CREATED.and. &
          alarminit2.eq.ESMF_INIT_CREATED) then
        ! invoke C to C++ entry point
        call c_ESMC_AlarmNE(alarm1, alarm2, ESMF_AlarmNE)
      else
        ! log error, convert to return code, and compare
        lval1 = ESMF_IMErr(alarminit1, ESMF_CONTEXT, rc=localrc1)
        lval2 = ESMF_IMErr(alarminit2, ESMF_CONTEXT, rc=localrc2)
        ESMF_AlarmNE = localrc1.ne.localrc2
      endif

      end function ESMF_AlarmNE

!------------------------------------------------------------------------------

      end module ESMF_AlarmMod
