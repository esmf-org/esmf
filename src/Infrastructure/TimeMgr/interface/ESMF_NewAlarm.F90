! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_NewAlarm.F90"
!==============================================================================
!
!     ESMF NewAlarm Module
      module ESMF_NewAlarmMod
!
!==============================================================================
!
! This file contains the NewAlarm class definition and all NewAlarm class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_TimeMgr.inc"
#include "ESMF.h"

!===============================================================================
!BOPI
!
! !MODULE: ESMF_NewAlarmMod
!
! !DESCRIPTION:
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMC\_NewAlarm}.
!
! See {\tt ../include/ESMC\_NewAlarm.h} for complete description.
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
      use ESMF_NewAlarmTypeMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_NewAlarm definition in ESMF_NewAlarmTypeMod to resolve mutual
!     ! method dependency with ESMF_Clock
!
!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     These types are defined in ESMF_NewAlarmTypeMod and propagated up from here.
!
      public ESMF_NewAlarmList_Flag
      public ESMF_NEWALARMLIST_ALL, ESMF_NEWALARMLIST_RINGING, &
             ESMF_NEWALARMLIST_NEXTRINGING, ESMF_NEWALARMLIST_PREVRINGING
      public ESMF_NewAlarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
      public operator(==)
      public operator(/=)
      public ESMF_NewAlarmCreate
      public ESMF_NewAlarmDestroy
      public ESMF_NewAlarmDisable
      public ESMF_NewAlarmEnable
      public ESMF_NewAlarmGet
      public ESMF_NewAlarmIsCreated
      public ESMF_NewAlarmIsEnabled
      public ESMF_NewAlarmIsRinging
      public ESMF_NewAlarmIsSticky
      public ESMF_NewAlarmNotSticky
      public ESMF_NewAlarmPrint
      public ESMF_NewAlarmReadRestart
      public ESMF_NewAlarmRingerOff
      public ESMF_NewAlarmRingerOn
      public ESMF_NewAlarmSet
      public ESMF_NewAlarmSticky
      public ESMF_NewAlarmValidate
      public ESMF_NewAlarmWasPrevRinging
      public ESMF_NewAlarmWillRingNext
      public ESMF_NewAlarmWriteRestart

! - ESMF-internal methods:
      public ESMF_NewAlarmGetInit
      public ESMF_NewAlarmSetInitCreated
      public ESMF_NewAlarmSetInitDeleted
      public ESMF_NewAlarmGetThis
      public ESMF_NewAlarmSetThis

!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_NewAlarmEQ
      private ESMF_NewAlarmNE
      private ESMF_NewAlarmCreateNew
      private ESMF_NewAlarmCreateCopy

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
! !IROUTINE:  ESMF_NewAlarmAssignment(=) - Assign an NewAlarm to another NewAlarm
!
! !INTERFACE:
!     interface assignment(=)
!     newalarm1 = newalarm2
!
! !ARGUMENTS:
!     type(ESMF_NewAlarm) :: newalarm1
!     type(ESMF_NewAlarm) :: newalarm2
!
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Assign {\tt newalarm1} as an alias to the same {\tt ESMF\_NewAlarm} object in
!     memory as {\tt newalarm2}. If {\tt newalarm2} is invalid, then {\tt newalarm1}
!     will be equally invalid after the assignment.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm1]
!          The {\tt ESMF\_NewAlarm} object on the left hand side of the
!          assignment.
!     \item[newalarm2]
!          The {\tt ESMF\_NewAlarm} object on the right hand side of the
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
! !IROUTINE:  ESMF_NewAlarmOperator(==) - Test if NewAlarm 1 is equal to NewAlarm 2
!
! !INTERFACE:
      interface operator(==)
!     if (newalarm1 == newalarm2) then ... endif
!                  OR
!     result = (newalarm1 == newalarm2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_NewAlarm), intent(in) :: newalarm1
!     type(ESMF_NewAlarm), intent(in) :: newalarm2
!
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_NewAlarm} class.
!     Compare two newalarms for equality; return {\tt .true.} if equal,
!     {\tt .false.} otherwise. Comparison is based on IDs, which are distinct
!     for newly created newalarms and identical for newalarms created as copies.
!
!     If either side of the equality test is not in the
!     {\tt ESMF\_INIT\_CREATED} status an error will be logged. However, this
!     does not affect the return value, which is {\tt .true.} when both
!     sides are in the {\em same} status, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm1]
!          The {\tt ESMF\_NewAlarm} object on the left hand side of the equality
!          operation.
!     \item[newalarm2]
!          The {\tt ESMF\_NewAlarm} object on the right hand side of the equality
!          operation.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_NewAlarmEQ
!
! !REQUIREMENTS:
!     TMGx.x.x

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_NewAlarmOperator(/=) - Test if NewAlarm 1 is not equal to NewAlarm 2
!
! !INTERFACE:
      interface operator(/=)
!     if (newalarm1 /= newalarm2) then ... endif
!                  OR
!     result = (newalarm1 /= newalarm2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_NewAlarm), intent(in) :: newalarm1
!     type(ESMF_NewAlarm), intent(in) :: newalarm2
!
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_NewAlarm} class.
!     Compare two newalarms for inequality; return {\tt .true.} if not equal,
!     {\tt .false.} otherwise. Comparison is based on IDs, which are distinct
!     for newly created newalarms and identical for newalarms created as copies.
!
!     If either side of the equality test is not in the
!     {\tt ESMF\_INIT\_CREATED} status an error will be logged. However, this
!     does not affect the return value, which is {\tt .true.} when both sides
!     are {\em not} in the {\em same} status, and {\tt .false.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm1]
!          The {\tt ESMF\_NewAlarm} object on the left hand side of the
!          non-equality operation.
!     \item[newalarm2]
!          The {\tt ESMF\_NewAlarm} object on the right hand side of the
!          non-equality operation.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_NewAlarmNE
!
! !REQUIREMENTS:
!     TMGx.x.x

      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_NewAlarmCreate - Create an ESMF NewAlarm
!
! !INTERFACE:
      interface ESMF_NewAlarmCreate

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_NewAlarmCreateNew
      module procedure ESMF_NewAlarmCreateCopy

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_NewAlarm} Create
!     methods.
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmCreateNew()"
!BOP
! !IROUTINE: ESMF_NewAlarmCreate - Create a new ESMF NewAlarm

! !INTERFACE:
      ! Private name; call using ESMF_NewAlarmCreate()
      function ESMF_NewAlarmCreateNew(clock, keywordEnforcer, &
        ringTime, ringInterval, stopTime, ringDuration, ringTimeStepCount, &
        refTime, enabled, sticky, name, rc)

! !RETURN VALUE:
      type(ESMF_NewAlarm) :: ESMF_NewAlarmCreateNew

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
!     Creates and sets the initial values in a new {\tt ESMF\_NewAlarm}.
!
!     In {\tt ESMF\_DIRECTION\_REVERSE} (see Section~\ref{sec:Clock}), newalarms
!     ring in reverse, i.e., they begin ringing when they originally ended,
!     and end ringing when they originally began.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The clock with which to associate this newly created newalarm.
!     \item[{[ringTime]}]
!          The ring time for a one-shot newalarm or the first ring time for a
!          repeating (interval) newalarm.  Must specify at least one of ringTime
!          or ringInterval.
!     \item[{[ringInterval]}]
!          The ring interval for repeating (interval) newalarms.  If
!          {\tt ringTime} is not also specified (first ring time), it will be
!          calculated as the {\tt clock}'s current time plus {\tt ringInterval}.
!          Must specify at least one of ringTime or ringInterval.
!     \item[{[stopTime]}]
!          The stop time for repeating (interval) newalarms.  If not
!          specified, an interval newalarm will repeat forever.
!     \item[{[ringDuration]}]
!          The absolute ring duration.  If not sticky (see argument below),
!          newalarms rings for ringDuration, then turns itself off.  Default is
!          zero (unused).  Mutually exclusive with ringTimeStepCount (below);
!          used only if set to a non-zero duration and ringTimeStepCount is 1
!          (see below).
!          See also {\tt ESMF\_NewAlarmSticky()}, {\tt ESMF\_NewAlarmNotSticky()}.
!     \item[{[ringTimeStepCount]}]
!          The relative ring duration.  If not sticky (see argument below),
!          newalarms rings for ringTimeStepCount, then turns itself off.
!          Default is 1: a non-sticky newalarm will ring for one clock time step.
!          Mutually exclusive with ringDuration (above); used if
!          ringTimeStepCount > 1.  If ringTimeStepCount is 1 (default) and
!          ringDuration is non-zero, ringDuration is used (see above), otherwise
!          ringTimeStepCount is used.
!          See also {\tt ESMF\_NewAlarmSticky()}, {\tt ESMF\_NewAlarmNotSticky()}.
!     \item[{[refTime]}]
!          The reference (i.e. base) time for an interval newalarm.
!     \item[{[enabled]}]
!          Sets the enabled state; default is on (true).  If disabled,
!          an newalarm will not function at all.
!          See also {\tt ESMF\_NewAlarmEnable()}, {\tt ESMF\_NewAlarmDisable()}.
!     \item[{[sticky]}]
!          Sets the sticky state; default is on (true).  If sticky,
!          once an newalarm is ringing, it will remain ringing until turned off
!          manually via a user call to {\tt ESMF\_NewAlarmRingerOff()}.
!          If not sticky, an newalarm will turn itself off after a certain
!          ring duration specified by either ringDuration or
!          ringTimeStepCount (see above).  There is an implicit limitation
!          that in order to properly reverse timestep through a ring end
!          time in {\tt ESMF\_DIRECTION\_REVERSE}, that time must have already
!          been traversed in the forward direction.  This is due to the fact
!          that the Time Manager cannot predict when user code will call
!          {\tt ESMF\_NewAlarmRingerOff()}.  An error message will be logged
!          when this limitation is not satisfied.
!          See also {\tt ESMF\_NewAlarmSticky()}, {\tt ESMF\_NewAlarmNotSticky()}.
!     \item[{[name]}]
!          The name for the newly created newalarm.  If not specified,
!          a default unique name will be generated: "NewAlarmNNN" where NNN
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

      ! invoke C to C++ entry point to allocate and initialize new newalarm
      call c_ESMC_NewAlarmCreateNew(ESMF_NewAlarmCreateNew, nameLen, name, clock, &
                                 ringTime, ringInterval, stopTime, &
                                 ringDuration, ringTimeStepCount, refTime, &
                                 enabled, sticky, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_NewAlarmSetInitCreated(ESMF_NewAlarmCreateNew)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_NewAlarmCreateNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmCreateCopy()"
!BOP
! !IROUTINE: ESMF_NewAlarmCreate - Create a copy of an existing ESMF NewAlarm

! !INTERFACE:
      ! Private name; call using ESMF_NewAlarmCreate()
      function ESMF_NewAlarmCreateCopy(newalarm, keywordEnforcer, rc)

! !RETURN VALUE:
      type(ESMF_NewAlarm) :: ESMF_NewAlarmCreateCopy

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(in)            :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Creates a complete (deep) copy of a given {\tt ESMF\_NewAlarm}.
!     The returned {\tt ESMF\_NewAlarm} copy is associated with the same
!     {\tt ESMF\_Clock} as the original {\tt ESMF\_NewAlarm}.  If desired, use
!     {\tt ESMF\_NewAlarmSet(...clock=...)} to re-associate the
!     {\tt ESMF\_NewAlarm} copy with a different {\tt ESMF\_Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!        The {\tt ESMF\_NewAlarm} to copy.
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point to copy newalarm
      call c_ESMC_NewAlarmCreateCopy(ESMF_NewAlarmCreateCopy, newalarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_NewAlarmSetInitCreated(ESMF_NewAlarmCreateCopy)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_NewAlarmCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmDestroy()"
!BOP
! !IROUTINE: ESMF_NewAlarmDestroy - Release resources associated with an NewAlarm
!
! !INTERFACE:
      subroutine ESMF_NewAlarmDestroy(newalarm, keywordEnforcer, rc)
!
! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(inout)          :: newalarm
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
!     Releases resources associated with this {\tt ESMF\_NewAlarm}.  Also
!     removes this {\tt ESMF\_NewAlarm} from its associated {\tt ESMF\_Clock}'s
!     list of {\tt ESMF\_NewAlarm}s (removes the {\tt ESMF\_NewAlarm} pointer from
!     the list).
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!       Release resources associated with this {\tt ESMF\_NewAlarm} and mark the
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmDestroy(newalarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully deleted
      call ESMF_NewAlarmSetInitDeleted(newalarm)

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmDisable()"
!BOP
! !IROUTINE: ESMF_NewAlarmDisable - Disable an NewAlarm

! !INTERFACE:
      subroutine ESMF_NewAlarmDisable(newalarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(inout)         :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Disables an {\tt ESMF\_NewAlarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_NewAlarmDisable(newalarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmDisable

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmEnable()"
!BOP
! !IROUTINE: ESMF_NewAlarmEnable - Enable an NewAlarm

! !INTERFACE:
      subroutine ESMF_NewAlarmEnable(newalarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(inout)         :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Enables an {\tt ESMF\_NewAlarm} to function.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmEnable(newalarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmEnable

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmGet()"
!BOP
! !IROUTINE: ESMF_NewAlarmGet - Get NewAlarm properties

! !INTERFACE:
      subroutine ESMF_NewAlarmGet(newalarm, keywordEnforcer, &
        clock, ringTime, prevRingTime, ringInterval, stopTime, ringDuration, &
        ringTimeStepCount, timeStepRingingCount, ringBegin, ringEnd, &
        refTime, ringing, ringingOnPrevTimeStep, enabled, sticky, name, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm),        intent(in)            :: newalarm
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
!     Gets one or more of an {\tt ESMF\_NewAlarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          The object instance to query.
!     \item[{[clock]}]
!          The associated clock.
!     \item[{[ringTime]}]
!          The ring time for a one-shot newalarm or the next repeating newalarm.
!     \item[{[prevRingTime]}]
!          The previous ring time.
!     \item[{[ringInterval]}]
!          The ring interval for repeating (interval) newalarms.
!     \item[{[stopTime]}]
!          The stop time for repeating (interval) newalarms.
!     \item[{[ringDuration]}]
!          The ring duration.  Mutually exclusive with
!          ringTimeStepCount (see below).
!     \item[{[ringTimeStepCount]}]
!          The number of time steps comprising the ring duration.  Mutually
!          exclusive with ringDuration (see above).
!     \item[{[timeStepRingingCount]}]
!          The number of time steps for which the newalarm has been ringing thus
!          far.  Used internally for tracking ringTimeStepCount ring
!          durations (see above).  Mutually exclusive with ringBegin
!          (see below).  Increments in {\tt ESMF\_DIRECTION\_FORWARD} and
!          decrements in {\tt ESMF\_DIRECTION\_REVERSE};
!          see Section~\ref{sec:Clock}.
!     \item[{[ringBegin]}]
!          The time when the newalarm began ringing.  Used internally for tracking
!          ringDuration (see above).  Mutually exclusive with
!          timeStepRingingCount (see above).
!     \item[{[ringEnd]}]
!          \begin{sloppypar}
!          The time when the newalarm ended ringing.  Used internally for
!          re-ringing newalarm in {\tt ESMF\_DIRECTION\_REVERSE}.
!          \end{sloppypar}
!     \item[{[refTime]}]
!          The reference (i.e. base) time for an interval newalarm.
!     \item[{[ringing]}]
!          The current ringing state.
!          See also {\tt ESMF\_NewAlarmRingerOn()}, {\tt ESMF\_NewAlarmRingerOff()}.
!     \item[{[ringingOnPrevTimeStep]}]
!          \begin{sloppypar}
!          The ringing state upon the previous time step. Same as
!          {\tt ESMF\_NewAlarmWasPrevRinging()}.
!          \end{sloppypar}
!     \item[{[enabled]}]
!          The enabled state.
!          See also {\tt ESMF\_NewAlarmEnable()}, {\tt ESMF\_NewAlarmDisable()}.
!     \item[{[sticky]}]
!          The sticky state.
!          See also {\tt ESMF\_NewAlarmSticky()}, {\tt ESMF\_NewAlarmNotSticky()}.
!     \item[{[name]}]
!          The name of this newalarm.
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmGet(newalarm, nameLen, tempNameLen, tempName, clock, &
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
      end subroutine ESMF_NewAlarmGet



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmIsCreated()"
!BOP
! !IROUTINE: ESMF_NewAlarmIsCreated - Check whether a NewAlarm object has been created

! !INTERFACE:
  function ESMF_NewAlarmIsCreated(newalarm, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_NewAlarmIsCreated
!
! !ARGUMENTS:
    type(ESMF_NewAlarm), intent(in)            :: newalarm
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt newalarm} has been created. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[newalarm]
!     {\tt ESMF\_NewAlarm} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------
    ESMF_NewAlarmIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_NewAlarmGetInit(newalarm)==ESMF_INIT_CREATED) &
      ESMF_NewAlarmIsCreated = .true.
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmIsEnabled()"
!BOP
! !IROUTINE:  ESMF_NewAlarmIsEnabled - Check if NewAlarm is enabled

! !INTERFACE:
      function ESMF_NewAlarmIsEnabled(newalarm, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_NewAlarmIsEnabled

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(in)            :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Check if {\tt ESMF\_NewAlarm} is enabled.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_NewAlarmIsEnabled = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmIsEnabled(newalarm, ESMF_NewAlarmIsEnabled, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_NewAlarmIsEnabled

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmIsRinging()"
!BOP
! !IROUTINE:  ESMF_NewAlarmIsRinging - Check if NewAlarm is ringing

! !INTERFACE:
      function ESMF_NewAlarmIsRinging(newalarm, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_NewAlarmIsRinging

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(in)            :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Check if {\tt ESMF\_NewAlarm} is ringing.
!
!     See also method
!           {\tt ESMF\_ClockGetNewAlarmList(clock, ESMF\_NEWALARMLIST\_RINGING, ...)}
!     to get a list of all ringing newalarms belonging to an {\tt ESMF\_Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          The newalarm to check for ringing state.
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
      ESMF_NewAlarmIsRinging = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmIsRinging(newalarm, ESMF_NewAlarmIsRinging, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_NewAlarmIsRinging

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmIsSticky()"
!BOP
! !IROUTINE:  ESMF_NewAlarmIsSticky - Check if NewAlarm is sticky

! !INTERFACE:
      function ESMF_NewAlarmIsSticky(newalarm, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_NewAlarmIsSticky

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(in)            :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Check if {\tt newalarm} is sticky.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_NewAlarmIsSticky = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmIsSticky(newalarm, ESMF_NewAlarmIsSticky, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_NewAlarmIsSticky

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmNotSticky()"
!BOP
! !IROUTINE:  ESMF_NewAlarmNotSticky - Unset an NewAlarm's sticky flag


! !INTERFACE:
      subroutine ESMF_NewAlarmNotSticky(newalarm, keywordEnforcer, &
        ringDuration, ringTimeStepCount, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm),        intent(inout)         :: newalarm
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
!     Unset an {\tt ESMF\_NewAlarm}'s sticky flag; once newalarm is ringing,
!     it turns itself off after ringDuration.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          The object instance to unset sticky.
!     \item[{[ringDuration]}]
!          If not sticky, newalarms rings for ringDuration, then turns itself off.
!          Mutually exclusive with ringTimeStepCount (see below and full
!          description in method {\tt ESMF\_NewAlarmCreate()} or
!          {\tt ESMF\_NewAlarmSet()}).
!     \item[{[ringTimeStepCount]}]
!          \begin{sloppypar}
!          If not sticky, newalarms rings for ringTimeStepCount, then turns
!          itself off.  Mutually exclusive with ringDuration (see above and
!          full description in method {\tt ESMF\_NewAlarmCreate()} or
!          {\tt ESMF\_NewAlarmSet()}).
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ringDuration,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmNotSticky(newalarm, ringDuration, &
                                 ringTimeStepCount, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmNotSticky

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmPrint()"
!BOP
! !IROUTINE:  ESMF_NewAlarmPrint - Print NewAlarm information

! !INTERFACE:
      subroutine ESMF_NewAlarmPrint(newalarm, options, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm),  intent(in)            :: newalarm
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

!
! !DESCRIPTION:
!     Prints out an {\tt ESMF\_NewAlarm}'s properties to {\tt stdout}, in support
!     of testing and debugging.  The options control the type of information
!     and level of detail. \\
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          {\tt ESMF\_NewAlarm} to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all {\tt newalarm} property values.\\
!          "clock"        - print the associated clock's name. \\
!          "enabled"      - print the newalarm's ability to ring. \\
!          "name"         - print the newalarm's name. \\
!          "prevRingTime" - print the newalarm's previous ring time. \\
!          "ringBegin"    - print time when the newalarm actually begins to ring.\\
!          "ringDuration" - print how long this newalarm is to remain ringing. \\
!          "ringEnd"      - print time when the newalarm actually ends ringing.\\
!          "ringing"                - print the newalarm's current ringing state.\\
!          "ringingOnPrevTimeStep"  - print whether the newalarm was ringing
!                                     immediately after the previous clock
!                                     time step. \\
!          "ringInterval" - print the newalarm's periodic ring interval. \\
!          "ringTime"     - print the newalarm's next time to ring. \\
!          "ringTimeStepCount" - print how long this newalarm is to remain
!                                ringing, in terms of a number of clock time
!                                steps. \\
!          "refTime"      - print the newalarm's interval reference (base) time. \\
!          "sticky"       - print whether the newalarm must be turned off
!                           manually. \\
!          "stopTime"     - print when newalarm intervals end. \\
!          "timeStepRingingCount"   - print the number of time steps the
!                                     newalarm has been ringing thus far. \\
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call c_ESMC_NewAlarmPrint(newalarm, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmReadRestart()"
!BOPI
! !IROUTINE: ESMF_NewAlarmReadRestart - Restore the contents of an NewAlarm (not implemented)

! !INTERFACE:
      function ESMF_NewAlarmReadRestart(name, keywordEnforcer, rc)
!
! !RETURN VALUE:
      type(ESMF_NewAlarm) :: ESMF_NewAlarmReadRestart
!
! !ARGUMENTS:
      character (len=*), intent(in)            :: name
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_NewAlarm} object from the last call to
!     {\tt ESMF\_NewAlarmWriteRestart()}.  (Not implemented yet).
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

      ! invoke C to C++ entry point to allocate and restore newalarm
      call c_ESMC_NewAlarmReadRestart(ESMF_NewAlarmReadRestart, nameLen, name, &
                                   localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! mark output as successfully initialized
      call ESMF_NewAlarmSetInitCreated(ESMF_NewAlarmReadRestart)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_NewAlarmReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmRingerOff()"
!BOP
! !IROUTINE:  ESMF_NewAlarmRingerOff - Turn off an NewAlarm

! !INTERFACE:
      subroutine ESMF_NewAlarmRingerOff(newalarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(inout)         :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Turn off an {\tt ESMF\_NewAlarm}; unsets ringing state.  For a sticky
!     newalarm, this method must be called to turn off its ringing state.
!     This is true for either {\tt ESMF\_DIRECTION\_FORWARD} (default) or
!     {\tt ESMF\_DIRECTION\_REVERSE}.  See Section~\ref{sec:Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmRingerOff(newalarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmRingerOff

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmRingerOn()"
!BOP
! !IROUTINE:  ESMF_NewAlarmRingerOn - Turn on an NewAlarm


! !INTERFACE:
      subroutine ESMF_NewAlarmRingerOn(newalarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(inout)         :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Turn on an {\tt ESMF\_NewAlarm}; sets ringing state.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmRingerOn(newalarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmRingerOn

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmSet()"
!BOP
! !IROUTINE: ESMF_NewAlarmSet - Set NewAlarm properties

! !INTERFACE:
      subroutine ESMF_NewAlarmSet(newalarm, keywordEnforcer, &
        clock, ringTime, ringInterval, stopTime, ringDuration, &
        ringTimeStepCount, refTime, ringing, enabled, sticky, name, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm),        intent(inout)         :: newalarm
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
!     Sets/resets one or more of the properties of an {\tt ESMF\_NewAlarm} that
!     was previously initialized via {\tt ESMF\_NewAlarmCreate()}.
!     \end{sloppypar}
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          The object instance to set.
!     \item[{[clock]}]
!          Re-associates this newalarm with a different clock.
!     \item[{[ringTime]}]
!          The next ring time for a one-shot newalarm or a repeating (interval)
!          newalarm.
!     \item[{[ringInterval]}]
!          The ring interval for repeating (interval) newalarms.
!     \item[{[stopTime]}]
!          The stop time for repeating (interval) newalarms.
!     \item[{[ringDuration]}]
!          The absolute ring duration.  If not sticky (see argument below),
!          newalarms rings for ringDuration, then turns itself off.  Default is
!          zero (unused).  Mutually exclusive with ringTimeStepCount (below);
!          used only if set to a non-zero duration and ringTimeStepCount is 1
!          (see below).
!          See also {\tt ESMF\_NewAlarmSticky()}, {\tt ESMF\_NewAlarmNotSticky()}.
!     \item[{[ringTimeStepCount]}]
!          The relative ring duration.  If not sticky (see argument below),
!          newalarms rings for ringTimeStepCount, then turns itself off.
!          Default is 1: a non-sticky newalarm will ring for one clock time step.
!          Mutually exclusive with ringDuration (above); used if
!          ringTimeStepCount > 1.  If ringTimeStepCount is 1 (default) and
!          ringDuration is non-zero, ringDuration is used (see above), otherwise
!          ringTimeStepCount is used.
!          See also {\tt ESMF\_NewAlarmSticky()}, {\tt ESMF\_NewAlarmNotSticky()}.
!     \item[{[refTime]}]
!          The reference (i.e. base) time for an interval newalarm.
!     \item[{[ringing]}]
!          Sets the ringing state.
!          See also {\tt ESMF\_NewAlarmRingerOn()}, {\tt ESMF\_NewAlarmRingerOff()}.
!     \item[{[enabled]}]
!          Sets the enabled state.  If disabled, an newalarm will not function
!          at all.
!          See also {\tt ESMF\_NewAlarmEnable()}, {\tt ESMF\_NewAlarmDisable()}.
!     \item[{[sticky]}]
!          Sets the sticky state.  If sticky, once an newalarm is ringing, it
!          will remain ringing until turned off manually via a user call to
!          {\tt ESMF\_NewAlarmRingerOff()}.  If not sticky, an newalarm will turn
!          itself off after a certain ring duration specified by either
!          ringDuration or ringTimeStepCount (see above).
!          There is an implicit limitation that in order to properly reverse
!          timestep through a ring end time in {\tt ESMF\_DIRECTION\_REVERSE},
!          that time must have already been traversed in the forward direction.
!          This is due to the fact that the Time Manager cannot predict when
!          user code will call {\tt ESMF\_NewAlarmRingerOff()}.  An error message
!          will be logged when this limitation is not satisfied.
!          See also {\tt ESMF\_NewAlarmSticky()}, {\tt ESMF\_NewAlarmNotSticky()}.
!     \item[{[name]}]
!          The new name for this newalarm.
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)
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
      call c_ESMC_NewAlarmSet(newalarm, nameLen, name, clock, ringTime, &
                           ringInterval, stopTime, ringDuration, &
                           ringTimeStepCount, refTime, ringing, &
                           enabled, sticky, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmSticky()"
!BOP
! !IROUTINE:  ESMF_NewAlarmSticky - Set an NewAlarm's sticky flag


! !INTERFACE:
      subroutine ESMF_NewAlarmSticky(newalarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(inout)         :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Set an {\tt ESMF\_NewAlarm}'s sticky flag; once newalarm is ringing,
!     it remains ringing until {\tt ESMF\_NewAlarmRingerOff()} is called.
!     There is an implicit limitation that in order to properly reverse
!     timestep through a ring end time in {\tt ESMF\_DIRECTION\_REVERSE}, that
!     time must have already been traversed in the forward direction.
!     This is due to the fact that an {\tt ESMF\_NewAlarm} cannot predict when
!     user code will call {\tt ESMF\_NewAlarmRingerOff()}.  An error message
!     will be logged when this limitation is not satisfied.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmSticky(newalarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmSticky

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmValidate()"
!BOP
! !IROUTINE:  ESMF_NewAlarmValidate - Validate an NewAlarm's properties

! !INTERFACE:
      subroutine ESMF_NewAlarmValidate(newalarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm),  intent(in)            :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Performs a validation check on an {\tt ESMF\_NewAlarm}'s properties.
!     Must have a valid ringTime, set either directly or indirectly via
!     ringInterval.  See {\tt ESMF\_NewAlarmCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          {\tt ESMF\_NewAlarm} to be validated.
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmValidate(newalarm, options, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmWasPrevRinging()"
!BOP
! !IROUTINE:  ESMF_NewAlarmWasPrevRinging - Check if NewAlarm was ringing on the previous Clock timestep

! !INTERFACE:
      function ESMF_NewAlarmWasPrevRinging(newalarm, keywordEnforcer, rc)
!
! !RETURN VALUE:
      logical :: ESMF_NewAlarmWasPrevRinging

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(in)            :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,          intent(out), optional :: rc

!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!     Check if {\tt ESMF\_NewAlarm} was ringing on the previous clock timestep.
!
!     See also method
!       {\tt ESMF\_ClockGetNewAlarmList(clock, ESMF\_NEWALARMLIST\_PREVRINGING, ...)}
!     get a list of all newalarms belonging to a {\tt ESMF\_Clock} that were
!     ringing on the previous time step.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_NewAlarmWasPrevRinging = .false.

      ! check input
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmWasPrevRinging(newalarm, ESMF_NewAlarmWasPrevRinging, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_NewAlarmWasPrevRinging

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmWillRingNext()"
!BOP
! !IROUTINE:  ESMF_NewAlarmWillRingNext - Check if NewAlarm will ring upon the next Clock timestep

! !INTERFACE:
      function ESMF_NewAlarmWillRingNext(newalarm, keywordEnforcer, timeStep, rc)
!
! !RETURN VALUE:
      logical :: ESMF_NewAlarmWillRingNext

! !ARGUMENTS:
      type(ESMF_NewAlarm),        intent(in)            :: newalarm
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
!     Check if {\tt ESMF\_NewAlarm} will ring on the next clock timestep, either
!     the current clock timestep or a passed-in timestep.
!
!     See also method
!       {\tt ESMF\_ClockGetNewAlarmList(clock, ESMF\_NEWALARMLIST\_NEXTRINGING, ...)}
!     to get a list of all newalarms belonging to a {\tt ESMF\_Clock} that will
!     ring on the next time step.
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
!          The newalarm to check for next ringing state.
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
      ESMF_NewAlarmWillRingNext = .false.

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,timeStep,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmWillRingNext(newalarm, timeStep,  &
                   ESMF_NewAlarmWillRingNext, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_NewAlarmWillRingNext

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmWriteRestart()"
!BOPI
! !IROUTINE: ESMF_NewAlarmWriteRestart - Save the contents of an NewAlarm (not implemented)

! !INTERFACE:
      subroutine ESMF_NewAlarmWriteRestart(newalarm, keywordEnforcer, rc)

! !ARGUMENTS:
      type(ESMF_NewAlarm),  intent(in)            :: newalarm
      type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_NewAlarm} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[newalarm]
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
      ESMF_INIT_CHECK_DEEP(ESMF_NewAlarmGetInit,newalarm,rc)

      ! invoke C to C++ entry point
      call c_ESMC_NewAlarmWriteRestart(newalarm, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_NewAlarmWriteRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmEQ()"
!BOPI
! !IROUTINE:  ESMF_NewAlarmEQ - Compare two NewAlarms for equality
!
! !INTERFACE:
      function ESMF_NewAlarmEQ(newalarm1, newalarm2)
!
! !RETURN VALUE:
      logical :: ESMF_NewAlarmEQ

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(in) :: newalarm1
      type(ESMF_NewAlarm), intent(in) :: newalarm2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_NewAlarm}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI
      ESMF_INIT_TYPE newalarminit1, newalarminit2
      integer :: localrc1, localrc2                 ! local return codes
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      newalarminit1 = ESMF_NewAlarmGetInit(newalarm1)
      newalarminit2 = ESMF_NewAlarmGetInit(newalarm2)

      if (newalarminit1.eq.ESMF_INIT_CREATED.and. &
          newalarminit2.eq.ESMF_INIT_CREATED) then
        ! invoke C to C++ entry point
        call c_ESMC_NewAlarmEQ(newalarm1, newalarm2, ESMF_NewAlarmEQ)
      else
        ! log error, convert to return code, and compare
        lval1 = ESMF_IMErr(newalarminit1, ESMF_CONTEXT, rc=localrc1)
        lval2 = ESMF_IMErr(newalarminit2, ESMF_CONTEXT, rc=localrc2)
        ESMF_NewAlarmEQ = localrc1.eq.localrc2
      endif

      end function ESMF_NewAlarmEQ

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NewAlarmNE()"
!BOPI
! !IROUTINE:  ESMF_NewAlarmNE - Compare two NewAlarms for inequality
!
! !INTERFACE:
      function ESMF_NewAlarmNE(newalarm1, newalarm2)
!
! !RETURN VALUE:
      logical :: ESMF_NewAlarmNE

! !ARGUMENTS:
      type(ESMF_NewAlarm), intent(in) :: newalarm1
      type(ESMF_NewAlarm), intent(in) :: newalarm2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_NewAlarm}
!     class.  See "interface operator(/=)" above for complete description.
!
!EOPI
      ESMF_INIT_TYPE newalarminit1, newalarminit2
      integer :: localrc1, localrc2                 ! local return codes
      logical :: lval1, lval2

      ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain
      ! init checks on both args, and in the case where both are uninitialized,
      ! to distinguish equality based on uninitialized type (uncreated,
      ! deleted).

      ! TODO: Consider moving this logic to C++: use Base class? status?
      !       Or replicate logic for C interface also.

      ! check inputs
      newalarminit1 = ESMF_NewAlarmGetInit(newalarm1)
      newalarminit2 = ESMF_NewAlarmGetInit(newalarm2)

      if (newalarminit1.eq.ESMF_INIT_CREATED.and. &
          newalarminit2.eq.ESMF_INIT_CREATED) then
        ! invoke C to C++ entry point
        call c_ESMC_NewAlarmNE(newalarm1, newalarm2, ESMF_NewAlarmNE)
      else
        ! log error, convert to return code, and compare
        lval1 = ESMF_IMErr(newalarminit1, ESMF_CONTEXT, rc=localrc1)
        lval2 = ESMF_IMErr(newalarminit2, ESMF_CONTEXT, rc=localrc2)
        ESMF_NewAlarmNE = localrc1.ne.localrc2
      endif

      end function ESMF_NewAlarmNE

!------------------------------------------------------------------------------

      end module ESMF_NewAlarmMod
