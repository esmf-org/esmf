! $Id: ESMF_Alarm.F90,v 1.72.2.4 2009/01/21 21:25:23 cdeluca Exp $
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
! Part of Time Manager Fortran API wrapper of C++ implemenation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}.
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_UtilTypesMod
      use ESMF_BaseMod

      ! for ReadRestart()/WriteRestart()
      use ESMF_IOSpecMod

      ! associated derived types
      use ESMF_TimeIntervalMod
      use ESMF_TimeIntervalTypeMod
      use ESMF_TimeMod
      use ESMF_TimeTypeMod
      use ESMF_ClockTypeMod

      ! type definition for this module
      use ESMF_AlarmTypeMod

      use ESMF_InitMacrosMod

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
!     These types are defined in ESMF_AlarmTypeMod and progagated up from here.
!
      public ESMF_AlarmListType
      public ESMF_ALARMLIST_ALL, ESMF_ALARMLIST_RINGING, &
             ESMF_ALARMLIST_NEXTRINGING, ESMF_ALARMLIST_PREVRINGING
      public ESMF_Alarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public operator(==)
      public operator(/=)
      public ESMF_AlarmCreate
      public ESMF_AlarmDestroy
      public ESMF_AlarmDisable
      public ESMF_AlarmEnable
      public ESMF_AlarmGet
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
!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_AlarmEQ
      private ESMF_AlarmNE
      private ESMF_AlarmCreateNew
      private ESMF_AlarmCreateCopy

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Alarm.F90,v 1.72.2.4 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
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
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Alarm} class.
!     Compare two alarms for equality; return true if equal,
!     false otherwise.  Comparison is based on IDs, which are distinct
!     for newly created alarms and identical for alarms created as copies.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt ESMF\_Alarm} in comparison.
!     \item[alarm2]
!          The second {\tt ESMF\_Alarm} in comparison.
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
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Alarm} class.
!     Compare two alarms for inequality; return true if not equal,
!     false otherwise.  Comparison is based on IDs, which are distinct
!     for newly created alarms and identical for alarms created as copies.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt ESMF\_Alarm} in comparison.
!     \item[alarm2]
!          The second {\tt ESMF\_Alarm} in comparison.
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
      function ESMF_AlarmCreateNew(name, clock, ringTime, ringInterval, &
                                   stopTime, ringDuration, &
                                   ringTimeStepCount, &
                                   refTime, enabled, sticky, rc)

! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmCreateNew

! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(ESMF_Clock),        intent(in)            :: clock
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: ringTimeStepCount
      type(ESMF_Time),         intent(in),  optional :: refTime
      logical,                 intent(in),  optional :: enabled
      logical,                 intent(in),  optional :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Creates and sets the initial values in a new {\tt ESMF\_Alarm}.
!
!     In {\tt ESMF\_MODE\_REVERSE} (see Section~\ref{sec:Clock}), alarms ring
!     in reverse, i.e., they begin ringing when they originally ended, and end
!     ringing when they originally began.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_AlarmCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          The name for the newly created alarm.  If not specified,
!          a default unique name will be generated: "AlarmNNN" where NNN
!          is a unique sequence number from 001 to 999.
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
!          alarms rings for ringDuration, then turns itself off.  Mutually 
!          exclusive with ringTimeStepCount (below); used only if
!          ringTimeStepCount is zero.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[ringTimeStepCount]}]
!          The relative ring duration.  If not sticky (see argument below),
!          alarms rings for ringTimeStepCount, then turns itself off.
!          Mutually exclusive with ringDuration (above); used if non-zero,
!          otherwise ringDuration is used.
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
!          time in {\tt ESMF\_MODE\_REVERSE}, that time must have already
!          been traversed in the forward direction.  This is due to the fact
!          that the Time Manager cannot predict when user code will call
!          {\tt ESMF\_AlarmRingerOff()}.  An error message will be logged
!          when this limitation is not satisfied.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
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

      ESMF_INIT_CHECK_DEEP(ESMF_ClockGetInit,clock,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,ringTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,stopTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,refTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,ringInterval)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,ringDuration)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

!     invoke C to C++ entry point to allocate and initialize new alarm
      call c_ESMC_AlarmCreateNew(ESMF_AlarmCreateNew, nameLen, name, clock, &
                                 ringTime, ringInterval, stopTime, &
                                 ringDuration, ringTimeStepCount, refTime, &
                                 enabled, sticky, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

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
      function ESMF_AlarmCreateCopy(alarm, rc)

! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmCreateCopy

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Creates a copy of a given {\tt ESMF\_Alarm}.
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_AlarmCreate()}.
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

      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point to copy alarm
      call c_ESMC_AlarmCreateCopy(ESMF_AlarmCreateCopy, alarm, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_AlarmSetInitCreated(ESMF_AlarmCreateCopy)

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmDestroy()"
!BOP
! !IROUTINE: ESMF_AlarmDestroy - Free all resources associated with an Alarm
!
! !INTERFACE:
      subroutine ESMF_AlarmDestroy(alarm, rc)
!
! !ARGUMENTS:
      type(ESMF_Alarm) :: alarm
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!       Destroy contents of this {\tt ESMF\_Alarm}.
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

      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmDestroy(alarm, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_AlarmSetInitDeleted(alarm)

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmDisable()"
!BOP
! !IROUTINE: ESMF_AlarmDisable - Disable an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc

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
    
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmDisable(alarm, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmDisable

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmEnable()"
!BOP
! !IROUTINE: ESMF_AlarmEnable - Enable an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc

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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmEnable(alarm, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmEnable

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmGet()"
!BOP
! !IROUTINE: ESMF_AlarmGet - Get Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmGet(alarm, name, clock, ringTime, prevRingTime, &
                               ringInterval, stopTime, ringDuration, &
                               ringTimeStepCount, timeStepRingingCount, &
                               ringBegin, ringEnd, refTime, ringing, &
                               ringingOnPrevTimeStep, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      character (len=*),       intent(out), optional :: name
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
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets one or more of an {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to query.
!     \item[{[name]}]
!          The name of this alarm.
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
!          (see below).  Increments in {\tt ESMF\_MODE\_FORWARD} and decrements
!          in {\tt ESMF\_MODE\_REVERSE}; see Section~\ref{sec:Clock}.
!     \item[{[ringBegin]}]
!          The time when the alarm began ringing.  Used internally for tracking
!          ringDuration (see above).  Mutually exclusive with
!          timeStepRingingCount (see above).
!     \item[{[ringEnd]}]
!          The time when the alarm ended ringing.  Used internally for
!          re-ringing alarm in {\tt ESMF\_MODE\_REVERSE}.
!     \item[{[refTime]}]
!          The reference (i.e. base) time for an interval alarm.
!     \item[{[ringing]}]
!          The current ringing state.
!          See also {\tt ESMF\_AlarmRingerOn()}, {\tt ESMF\_AlarmRingerOff()}.
!     \item[{[ringingOnPrevTimeStep]}]
!          The ringing state upon the previous time step. Same as
!          {\tt ESMF\_AlarmWasPrevRinging()}.
!     \item[{[enabled]}]
!          The enabled state.
!          See also {\tt ESMF\_AlarmEnable()}, {\tt ESMF\_AlarmDisable()}.
!     \item[{[sticky]}]
!          The sticky state. 
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,ringTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,prevRingTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,ringInterval)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,stopTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,refTime)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,ringBegin)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeGetInit,ESMF_TimeInit,ringEnd)
      ESMF_INIT_CHECK_SHALLOW(ESMF_TimeIntervalGetInit,ESMF_TimeIntervalInit,ringDuration)


      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

!     invoke C to C++ entry point
      call c_ESMC_AlarmGet(alarm, nameLen, tempNameLen, tempName, clock, &
                    ringTime, prevRingTime, ringInterval, stopTime, &
                    ringDuration, ringTimeStepCount, &
                    timeStepRingingCount, ringBegin, ringEnd, refTime, &
                    ringing, ringingOnPrevTimeStep, enabled, sticky, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! make work with Init. Stand.
      if (present(clock)) call ESMF_ClockSetInitCreated(clock)

      ! copy temp name back to given name to restore native Fortran
      !   storage style
      if (present(name)) then
        name = tempName(1:tempNameLen)
      endif

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmIsEnabled()"
!BOP
! !IROUTINE:  ESMF_AlarmIsEnabled - Check if Alarm is enabled

! !INTERFACE:
      function ESMF_AlarmIsEnabled(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsEnabled

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc

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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmIsEnabled(alarm, ESMF_AlarmIsEnabled, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      function ESMF_AlarmIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)            :: alarm
      integer,          intent(out), optional :: rc

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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmIsRinging(alarm, ESMF_AlarmIsRinging, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      function ESMF_AlarmIsSticky(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsSticky

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc

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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmIsSticky(alarm, ESMF_AlarmIsSticky, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_AlarmNotSticky(alarm, ringDuration, &
                                     ringTimeStepCount, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: ringTimeStepCount
      integer,                 intent(out), optional :: rc
    
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
!     \item[{[ringTimeStepCount]}]
!          If not sticky, alarms rings for ringTimeStepCount, then turns
!          itself off.
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
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmNotSticky(alarm, ringDuration, &
                                 ringTimeStepCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_AlarmNotSticky

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmPrint()"
!BOP
! !IROUTINE:  ESMF_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmPrint(alarm, options, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),  intent(inout)         :: alarm
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Prints out an {\tt ESMF\_Alarm}'s properties to {\tt stdout}, in support
!     of testing and debugging.  The options control the type of information
!     and level of detail.
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
      
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmPrint(alarm, options, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      function ESMF_AlarmReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmReadRestart
!
! !ARGUMENTS:
      character (len=*), intent(in)            :: name
      type(ESMF_IOSpec), intent(in),  optional :: iospec
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Alarm} object from the last call to
!     {\tt ESMF\_AlarmWriteRestart()}.  (Not implemented yet).
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

!     invoke C to C++ entry point to allocate and restore alarm
      call c_ESMC_AlarmReadRestart(ESMF_AlarmReadRestart, nameLen, name, &
                                   iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Return success
      if (present(rc)) rc = ESMF_SUCCESS
      end function ESMF_AlarmReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_AlarmRingerOff()"
!BOP
! !IROUTINE:  ESMF_AlarmRingerOff - Turn off an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmRingerOff(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an {\tt ESMF\_Alarm}; unsets ringing state.  For a sticky
!     alarm, this method must be called to turn off its ringing state.
!     This is true for either {\tt ESMF\_MODE\_FORWARD} (default) or
!     {\tt ESMF\_MODE\_REVERSE}.  See Section~\ref{sec:Clock}.
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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmRingerOff(alarm, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_AlarmRingerOn(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc
    
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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmRingerOn(alarm, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_AlarmSet(alarm, name, clock, ringTime, ringInterval, &
                               stopTime, ringDuration, ringTimeStepCount, &
                               refTime, ringing, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      character (len=*),       intent(in),  optional :: name
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
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Sets/resets one or more of the properties of an {\tt ESMF\_Alarm} that
!     was previously initialized via {\tt ESMF\_AlarmCreate()}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set.
!     \item[{[name]}]
!          The new name for this alarm.  
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
!          alarms rings for ringDuration, then turns itself off.  Mutually 
!          exclusive with ringTimeStepCount (below); used only if
!          ringTimeStepCount is zero.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[ringTimeStepCount]}]
!          The relative ring duration.  If not sticky (see argument below),
!          alarms rings for ringTimeStepCount, then turns itself off.
!          Mutually exclusive with ringDuration (above); used if non-zero,
!          otherwise ringDuration is used.
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
!          timestep through a ring end time in {\tt ESMF\_MODE\_REVERSE},
!          that time must have already been traversed in the forward direction. !          This is due to the fact that the Time Manager cannot predict when
!          user code will call {\tt ESMF\_AlarmRingerOff()}.  An error message
!          will be logged when this limitation is not satisfied.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

!     invoke C to C++ entry point
      call c_ESMC_AlarmSet(alarm, nameLen, name, clock, ringTime, &
                           ringInterval, stopTime, ringDuration, &
                           ringTimeStepCount, refTime, ringing, &
                           enabled, sticky, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_AlarmSticky(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc
    
! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s sticky flag; once alarm is ringing,
!     it remains ringing until {\tt ESMF\_AlarmRingerOff()} is called.
!     There is an implicit limitation that in order to properly reverse
!     timestep through a ring end time in {\tt ESMF\_MODE\_REVERSE}, that
!     time must have already been traversed in the forward direction.
!     This is due to the fact that the Time Manager cannot predict when
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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmSticky(alarm, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_AlarmValidate(alarm, options, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),  intent(inout)         :: alarm
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Performs a validation check on an {\tt ESMF\_Alarm}'s properties.
!     Must have a valid ringTime, set either directly or indirectly via
!     ringInterval.  See {\tt ESMF\_AlarmCreate()}.
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to be validated.
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
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmValidate(alarm, options, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      function ESMF_AlarmWasPrevRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmWasPrevRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc

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

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmWasPrevRinging(alarm, ESMF_AlarmWasPrevRinging, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      function ESMF_AlarmWillRingNext(alarm, timeStep, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmWillRingNext

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      integer,                 intent(out), optional :: rc

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
    
      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmWillRingNext(alarm, timeStep,  &
                   ESMF_AlarmWillRingNext, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
      subroutine ESMF_AlarmWriteRestart(alarm, iospec, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),  intent(inout)         :: alarm
      type(ESMF_IOSpec), intent(in),  optional :: iospec
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Alarm} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
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
      ESMF_INIT_CHECK_DEEP(ESMF_AlarmGetInit,alarm,rc)

!     invoke C to C++ entry point
      call c_ESMC_AlarmWriteRestart(alarm, iospec, localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!     invoke C to C++ entry point
      call c_ESMC_AlarmEQ(alarm1, alarm2, ESMF_AlarmEQ)

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
!     invoke C to C++ entry point
      call c_ESMC_AlarmNE(alarm1, alarm2, ESMF_AlarmNE)

      end function ESMF_AlarmNE

!------------------------------------------------------------------------------

      end module ESMF_AlarmMod
