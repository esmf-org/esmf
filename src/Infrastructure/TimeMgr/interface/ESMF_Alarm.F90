! $Id: ESMF_Alarm.F90,v 1.33 2003/10/22 01:03:48 eschwab Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
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
#include <ESMF_TimeMgr.inc>
#include "ESMF.h"

!===============================================================================
!BOPI
!
! !MODULE: ESMF_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation.
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}.
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! associated derived types
      use ESMF_BaseMod
      use ESMF_TimeIntervalMod
      use ESMF_TimeMod
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
!     These types are defined in ESMF_AlarmTypeMod and progagated up from here.
!
      public ESMF_AlarmListType
      public ESMF_ALARMLIST_ALL, ESMF_ALARMLIST_RINGING, &
             ESMF_ALARMLIST_NEXTRINGING, ESMF_ALARMLIST_PREVRINGING
      public ESMF_Alarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_AlarmCreate
      public ESMF_AlarmDestroy
      public ESMF_AlarmSet
      public ESMF_AlarmGet

      public ESMF_AlarmEnable
      public ESMF_AlarmDisable
      public ESMF_AlarmIsEnabled

      public ESMF_AlarmRingerOn
      public ESMF_AlarmRingerOff
      public ESMF_AlarmIsRinging
      public ESMF_AlarmWillRingNext
      public ESMF_AlarmWasPrevRinging

      public ESMF_AlarmSticky
      public ESMF_AlarmNotSticky
      public ESMF_AlarmIsSticky

      public operator(==)
 
! Required inherited and overridden ESMF_Base class methods

      public ESMF_AlarmReadRestart
      public ESMF_AlarmWriteRestart
      public ESMF_AlarmValidate
      public ESMF_AlarmPrint
!EOPI

! !PRIVATE MEMBER FUNCTIONS:

      private ESMF_AlarmEQ

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Alarm.F90,v 1.33 2003/10/22 01:03:48 eschwab Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(==)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmEQ
!     pointer version ESMF_pteq defined in base class

! !DESCRIPTION:
!     This interface overloads the == operator for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!==============================================================================

      contains

!==============================================================================
!------------------------------------------------------------------------------
!
! This section includes the Set/Get methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmCreate - Create an Alarm

! !INTERFACE
      function ESMF_AlarmCreate(name, clock, ringTime, ringInterval, &
                                stopTime, ringDuration, &
                                nRingDurationTimeSteps, &
                                refTime, enabled, sticky, rc)

! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmCreate

! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(ESMF_Clock),        intent(in)            :: clock
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: nRingDurationTimeSteps
      type(ESMF_Time),         intent(in),  optional :: refTime
      logical,                 intent(in),  optional :: enabled
      logical,                 intent(in),  optional :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          Optional name for the newly created alarm.  If not specified,
!          a default unique name will be generated: "AlarmNNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[clock]
!          The clock with which to associate this newly created alarm.
!     \item[{[ringTime]}]
!          Optional ring time for a one-shot alarm or the first repeating
!          (interval) alarm.
!     \item[{[ringInterval]}]
!          Optional ring interval for repeating (interval) alarms.
!     \item[{[stopTime]}]
!          Optional stop time for repeating (interval) alarms.  If not
!          specified, an interval alarm will repeat forever.
!     \item[{[ringDuration]}]
!          Optional absolute ring duration:  If not sticky (see argument below),
!          alarms rings for ringDuration, then turns itself off.  Mutually 
!          exclusive with nRingDurationTimeSteps (below); used only if
!          nRingDurationTimeSteps is zero.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[nRingDurationTimeSteps]}]
!          Optional relative ring duration:  If not sticky (see argument below),
!          alarms rings for nRingDurationTimeSteps, then turns itself off.
!          Mutually exclusive with ringDuration (above); used if non-zero,
!          otherwise ringDuration is used.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[refTime]}]
!          Optional reference time for an alarm.
!     \item[{[enabled]}]
!          Optionally set the enabled state; default is on (true).  If disabled,
!          an alarm will not function at all.
!          See also {\tt ESMF\_AlarmEnable()}, {\tt ESMF\_AlarmDisable()}.
!     \item[{[sticky]}]
!          Optionally set the sticky state; default is on (true).  If sticky,
!          once an alarm is ringing, it will remain ringing until turned off 
!          manually via a user call to {\tt ESMF\_AlarmRingerOff()}.
!          If not sticky, an alarm will turn itself off after a certain
!          ring duration specified by either ringDuration or
!          nRingDurationTimeSteps (see above).
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7

      ! initialize name length to zero for non-existent name
      integer :: nameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

!     invoke C to C++ entry point to allocate and initialize new alarm
      call c_ESMC_AlarmCreate(ESMF_AlarmCreate, nameLen, name, clock, &
                              ringTime, ringInterval, stopTime, ringDuration, &
                              nRingDurationTimeSteps, refTime, enabled, &
                              sticky, rc)

      end function ESMF_AlarmCreate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmDestroy
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

!     invoke C to C++ entry point
      call c_ESMC_AlarmDestroy(alarm, rc)

      end subroutine ESMF_AlarmDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSet - Set Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmSet(alarm, name, clock, ringTime, ringInterval, &
                               stopTime, ringDuration, nRingDurationTimeSteps, &
                               refTime, ringing, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      character (len=*),       intent(in),  optional :: name
      type(ESMF_Clock),        intent(in),  optional :: clock
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: nRingDurationTimeSteps
      type(ESMF_Time),         intent(in),  optional :: refTime
      logical,                 intent(in),  optional :: ringing
      logical,                 intent(in),  optional :: enabled
      logical,                 intent(in),  optional :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Sets an {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set.
!     \item[{[name]}]
!          Optional name for this alarm.  If not specified, a default unique
!          name will be generated: "AlarmNNN" where NNN is a unique sequence
!          number from 001 to 999.
!     \item[{[clock]}]
!          Optionally re-associate with a different clock.
!     \item[{[ringTime]}]
!          Optional ring time for a one-shot alarm or the first repeating
!          (interval) alarm.
!     \item[{[ringInterval]}]
!          Optional ring interval for repeating (interval) alarms.
!     \item[{[stopTime]}]
!          Optional stop time for repeating (interval) alarms.  If not
!          specified, an interval alarm will repeat forever.
!     \item[{[ringDuration]}]
!          Optional absolute ring duration:  If not sticky (see argument below),
!          alarms rings for ringDuration, then turns itself off.  Mutually 
!          exclusive with nRingDurationTimeSteps (below); used only if
!          nRingDurationTimeSteps is zero.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[nRingDurationTimeSteps]}]
!          Optional relative ring duration:  If not sticky (see argument below),
!          alarms rings for nRingDurationTimeSteps, then turns itself off.
!          Mutually exclusive with ringDuration (above); used if non-zero,
!          otherwise ringDuration is used.
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[refTime]}]
!          Optional reference time for an alarm.
!     \item[{[ringing]}]
!          Optionally set the ringing state; default is off (false). 
!          See also {\tt ESMF\_AlarmRingerOn()}, {\tt ESMF\_AlarmRingerOff()}.
!     \item[{[enabled]}]
!          Optionally set the enabled state; default is on (true).  If disabled,
!          an alarm will not function at all.
!          See also {\tt ESMF\_AlarmEnable()}, {\tt ESMF\_AlarmDisable()}.
!     \item[{[sticky]}]
!          Optionally set the sticky state; default is on (true).  If sticky,
!          once an alarm is ringing, it will remain ringing until turned off 
!          manually via a user call to {\tt ESMF\_AlarmRingerOff()}.
!          If not sticky, an alarm will turn itself off after a certain
!          ring duration specified by either ringDuration or
!          nRingDurationTimeSteps (see above).
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7

      ! initialize name length to zero for non-existent name
      integer :: nameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

!     invoke C to C++ entry point
      call c_ESMC_AlarmSet(alarm, nameLen, name, clock, ringTime, &
                           ringInterval, stopTime, ringDuration, &
                           nRingDurationTimeSteps, refTime, ringing, &
                           enabled, sticky, rc)

      end subroutine ESMF_AlarmSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmGet - Get Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmGet(alarm, name, clock, ringTime, prevRingTime, &
                               ringInterval, stopTime, ringDuration, &
                               nRingDurationTimeSteps, nTimeStepsRinging, &
                               ringBegin, refTime, ringing, &
                               ringingOnPrevTimeStep, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(in)            :: alarm
      character (len=*),       intent(out), optional :: name
      type(ESMF_Clock),        intent(out), optional :: clock
      type(ESMF_Time),         intent(out), optional :: ringTime
      type(ESMF_Time),         intent(out), optional :: prevRingTime
      type(ESMF_TimeInterval), intent(out), optional :: ringInterval
      type(ESMF_Time),         intent(out), optional :: stopTime
      type(ESMF_TimeInterval), intent(out), optional :: ringDuration
      integer,                 intent(out), optional :: nRingDurationTimeSteps
      integer,                 intent(out), optional :: nTimeStepsRinging
      type(ESMF_Time),         intent(out), optional :: ringBegin
      type(ESMF_Time),         intent(out), optional :: refTime
      logical,                 intent(out), optional :: ringing
      logical,                 intent(out), optional :: ringingOnPrevTimeStep
      logical,                 intent(out), optional :: enabled
      logical,                 intent(out), optional :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets an {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get.
!     \item[{[name]}]
!          Gets the name of this alarm.
!     \item[{[clock]}]
!          Gets the associated clock.
!     \item[{[ringTime]}]
!          Gets the ring time for a one-shot alarm or the next repeating alarm.
!     \item[{[prevRingTime]}]
!          Gets the previous ring time.
!     \item[{[ringInterval]}]
!          Gets the ring interval for repeating (interval) alarms.
!     \item[{[stopTime]}]
!          Gets the stop time for repeating (interval) alarms.
!     \item[{[ringDuration]}]
!          Gets the ringDuration.  Mutually exclusive with 
!          nRingDurationTimeSteps (see below).
!     \item[{[nRingDurationTimeSteps]}]
!          Gets nRingDurationTimeSteps.  Mutually exclusive with ringDuration
!          (see above).
!     \item[{[nTimeStepsRinging]}]
!          Gets the number of timesteps the alarm has been ringing thus far.
!          Used internally for tracking nRingDurationTimeSteps ring 
!          durations (see above).  Mutually exclusive with ringBegin
!          (see below).
!     \item[{[ringBegin]}]
!          Gets the time the alarm began ringing.  Used internally for tracking
!          ringDuration (see above).  Mutually exclusive with nTimeStepsRinging
!          (see above).
!     \item[{[refTime]}]
!          Gets the reference time for an alarm.
!     \item[{[ringing]}]
!          Gets the current ringing state.
!          See also {\tt ESMF\_AlarmRingerOn()}, {\tt ESMF\_AlarmRingerOff()}.
!     \item[{[ringingOnPrevTimeStep]}]
!          Gets the ringing state upon the previous time step. Same as
!          ESMF_AlarmWasPrevRinging().
!     \item[{[enabled]}]
!          Gets the enabled state.
!          See also {\tt ESMF\_AlarmEnable()}, {\tt ESMF\_AlarmDisable()}.
!     \item[{[sticky]}]
!          Gets the sticky state. 
!          See also {\tt ESMF\_AlarmSticky()}, {\tt ESMF\_AlarmNotSticky()}.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7

      ! temp name for C++ to fill
      character (len=ESMF_MAXSTR) :: tempName

      ! initialize name lengths to zero for non-existent name
      integer :: nameLen = 0
      integer :: tempNameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

!     invoke C to C++ entry point
      call c_ESMC_AlarmGet(alarm, nameLen, tempNameLen, tempName, clock, &
                           ringTime, prevRingTime, ringInterval, stopTime, &
                           ringDuration, nRingDurationTimeSteps, &
                           nTimeStepsRinging, ringBegin, refTime, &
                           ringing, ringingOnPrevTimeStep, enabled, sticky, rc)

      ! copy temp name back to given name to restore native F90 storage style
      if (present(name)) then
        name = tempName(1:tempNameLen)
      endif

      end subroutine ESMF_AlarmGet

!------------------------------------------------------------------------------
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

!     invoke C to C++ entry point
      call c_ESMC_AlarmEnable(alarm, rc)

      end subroutine ESMF_AlarmEnable

!------------------------------------------------------------------------------
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
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmDisable(alarm, rc)

      end subroutine ESMF_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmIsEnabled - Check if Alarm is enabled

! !INTERFACE:
      function ESMF_AlarmIsEnabled(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsEnabled

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
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
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmIsEnabled(alarm, ESMF_AlarmIsEnabled, rc)

      end function ESMF_AlarmIsEnabled

!------------------------------------------------------------------------------
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

!     invoke C to C++ entry point
      call c_ESMC_AlarmRingerOn(alarm, rc)

      end subroutine ESMF_AlarmRingerOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmRingerOff - Turn off an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmRingerOff(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout)         :: alarm
      integer,          intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an {\tt ESMF\_Alarm}; unsets ringing state.
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

!     invoke C to C++ entry point
      call c_ESMC_AlarmRingerOff(alarm, rc)

      end subroutine ESMF_AlarmRingerOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmIsRinging - Check if Alarm is ringing

! !INTERFACE:
      function ESMF_AlarmIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is ringing.
!
!     See also method
!           ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_RINGING, ...)
!     to get a list of all ringing alarms belonging to a {\tt ESMF_Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for ringing state.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.4
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmIsRinging(alarm, ESMF_AlarmIsRinging, rc)

      end function ESMF_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmWillRingNext - Check if Alarm will ring upon the next clock timestep

! !INTERFACE:
      function ESMF_AlarmWillRingNext(alarm, timeStep, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmWillRingNext

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(in)            :: alarm
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} will ring on the next clock timestep, either
!     the current clock timestep or a passed-in timestep.
!
!     See also method
!           ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_NEXTRINGING, ...)
!     to get a list of all alarms belonging to a {\tt ESMF_Clock} that will
!     ring on the next time step.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for next ringing state.
!     \item[{[timeStep]}]
!          Optional timestep to use instead of the clock's.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.4
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmWillRingNext(alarm, timeStep, ESMF_AlarmWillRingNext, rc)

      end function ESMF_AlarmWillRingNext

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmWasPrevRinging - Check if Alarm was ringing on the previous clock timestep

! !INTERFACE:
      function ESMF_AlarmWasPrevRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmWasPrevRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} was ringing on the previous clock timestep.
!
!     See also method
!           ESMF_ClockGetAlarmList(clock, ESMF_ALARMLIST_PREVRINGING, ...)
!     get a list of all alarms belonging to a {\tt ESMF_Clock} that were
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
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmWasPrevRinging(alarm, ESMF_AlarmWasPrevRinging, rc)

      end function ESMF_AlarmWasPrevRinging

!------------------------------------------------------------------------------
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
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set sticky.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point
      call c_ESMC_AlarmSticky(alarm, rc)

      end subroutine ESMF_AlarmSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmNotSticky - Unset an Alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmNotSticky(alarm, ringDuration, &
                                     nRingDurationTimeSteps, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: nRingDurationTimeSteps
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
!     \item[{[nRingDurationTimeSteps]}]
!          If not sticky, alarms rings for nRingDurationTimeSteps, then turns
!          itself off.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point
      call c_ESMC_AlarmNotSticky(alarm, ringDuration, &
                                 nRingDurationTimeSteps, rc)

      end subroutine ESMF_AlarmNotSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmIsSticky - Check if Alarm is sticky

! !INTERFACE:
      function ESMF_AlarmIsSticky(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsSticky

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is sticky.
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
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmIsSticky(alarm, ESMF_AlarmIsSticky, rc)

      end function ESMF_AlarmIsSticky

!------------------------------------------------------------------------------
!BOP
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
!     Compare two alarms for equality; return true if equal, false otherwise.
!     Maps to overloaded (==) operator interface function.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt ESMF\_Alarm} to compare.
!     \item[alarm2]
!          The second {\tt ESMF\_Alarm} to compare.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  

!     invoke C to C++ entry point
      call c_ESMC_AlarmEQ(alarm1, alarm2, ESMF_AlarmEQ)

      end function ESMF_AlarmEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmReadRestart - Restore an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmReadRestart(alarm, clock, ringInterval, &
                                       ringDuration, ringTime, prevRingTime, &
                                       stopTime, ringBegin, refTime, &
                                       nRingDurationTimeSteps, &
                                       nTimeStepsRinging, &
                                       ringing, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(out)           :: alarm
      type(ESMF_Clock),        intent(in)            :: clock
      type(ESMF_TimeInterval), intent(in)            :: ringInterval
      type(ESMF_TimeInterval), intent(in)            :: ringDuration
      type(ESMF_Time),         intent(in)            :: ringTime
      type(ESMF_Time),         intent(in)            :: prevRingTime
      type(ESMF_Time),         intent(in)            :: stopTime
      type(ESMF_Time),         intent(in)            :: ringBegin
      type(ESMF_Time),         intent(in)            :: refTime
      integer,                 intent(in)            :: nRingDurationTimeSteps
      integer,                 intent(in)            :: nTimeStepsRinging
      logical,                 intent(in)            :: ringing
      logical,                 intent(in)            :: enabled
      logical,                 intent(in)            :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to restore.
!     \item[clock]
!          The associated clock.
!     \item[ringInterval]
!          The ring interval for repeating alarms.
!     \item[ringDuration]
!          The ring duration.
!     \item[ringTime]
!          Ring time for one-shot or first repeating alarm.
!     \item[prevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time.
!     \item[stopTime]
!          Stop time for repeating alarms.
!     \item[ringBegin]
!          Begin time for ringing alarm.
!     \item[refTime]
!          The {\tt ESMF\_Alarm}'s reference time.
!     \item[{[nRingDurationTimeSteps]}]
!          Alarms can ring for nRingDurationTimeSteps, rather than ringDuration.
!     \item[{[nTimeStepsRinging]}]
!          Number of timesteps the alarm has been ringing thus far.
!     \item[ringing]
!          The {\tt ESMF\_Alarm}'s ringing state.
!     \item[enabled]
!          {\tt ESMF\_Alarm} enabled/disabled.
!     \item[sticky]
!          {\tt ESMF\_Alarm} sticky flag.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_AlarmReadRestart(alarm, clock, ringInterval, ringDuration, &
                                   ringTime, prevRingTime, stopTime, &      
                                   ringBegin, refTime, &
                                   nRingDurationTimeSteps, &
                                   nTimeStepsRinging, &
                                   ringing, enabled, sticky, rc)

      end subroutine ESMF_AlarmReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmWriteRestart - Save an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmWriteRestart(alarm, clock, ringInterval, &
                                        ringDuration, ringTime, prevRingTime, &
                                        stopTime, ringBegin, refTime, &
                                        nRingDurationTimeSteps, &
                                        nTimeStepsRinging, &
                                        ringing, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(in)            :: alarm
      type(ESMF_Clock),        intent(out)           :: clock
      type(ESMF_TimeInterval), intent(out)           :: ringInterval
      type(ESMF_TimeInterval), intent(out)           :: ringDuration
      type(ESMF_Time),         intent(out)           :: ringTime
      type(ESMF_Time),         intent(out)           :: prevRingTime
      type(ESMF_Time),         intent(out)           :: stopTime
      type(ESMF_Time),         intent(out)           :: ringBegin
      type(ESMF_Time),         intent(out)           :: refTime
      integer,                 intent(out)           :: nRingDurationTimeSteps
      integer,                 intent(out)           :: nTimeStepsRinging
      logical,                 intent(out)           :: ringing
      logical,                 intent(out)           :: enabled
      logical,                 intent(out)           :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to save.
!     \item[clock]
!          The associated clock.
!     \item[ringInterval]
!          Ring interval for repeating alarms.
!     \item[ringDuration]
!          The ring duration.
!     \item[ringTime]
!          Ring time for one-shot or first repeating alarm.
!     \item[prevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time.
!     \item[stopTime]
!          Stop time for repeating alarms.
!     \item[ringBegin]
!          Begin time for ringing alarm.
!     \item[refTime]
!          The {\tt ESMF\_Alarm}'s reference time.
!     \item[{[nRingDurationTimeSteps]}]
!          Alarms can ring for nRingDurationTimeSteps, rather than ringDuration.
!     \item[{[nTimeStepsRinging]}]
!          Number of timesteps the alarm has been ringing thus far.
!     \item[ringing]
!          The {\tt ESMF\_Alarm}'s ringing state.
!     \item[enabled]
!          {\tt ESMF\_Alarm} enabled/disabled.
!     \item[sticky]
!          {\tt ESMF\_Alarm} sticky flag.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_AlarmWriteRestart(alarm, clock, ringInterval, ringDuration, &
                                    ringTime, prevRingTime, stopTime, &
                                    ringBegin, refTime, &
                                    nRingDurationTimeSteps, &
                                    nTimeStepsRinging, &
                                    ringing, enabled, sticky, rc)

      end subroutine ESMF_AlarmWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmValidate(alarm, options, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),  intent(in)            :: alarm
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Alarm}'s properties.  The
!     options control the type of validation.
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to validate.
!     \item[{[options]}]
!          Validate options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
!     invoke C to C++ entry point
      call c_ESMC_AlarmValidate(alarm, options, rc)
    
      end subroutine ESMF_AlarmValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmPrint(alarm, options, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),  intent(in)            :: alarm
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Alarm}'s
!     properties.  The options control the type of information and level
!     of detail.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to print out.
!     \item[{[options]}]
!          Print options. If none specified, prints all alarm property values.\\
!          "name"         - print the alarm's name. \\
!          "clock"        - print the associated clock's name. \\
!          "ringinterval" - print the alarm's periodic ring interval. \\
!          "ringduration" - print how long this alarm is to remain ringing. \\
!          "ringTime"     - print the alarm's next time to ring. \\
!          "prevRingTime" - print the alarm's previous ring time. \\
!          "stopTime"     - print when alarm intervals end. \\
!          "ringBegin"    - print time when the alarm actually begins to ring.\\
!          "refTime"      - print the alarm's reference time. \\
!          "nRingDurationTimeSteps" - print for how many time steps the alarm \\
!                                     rings. \\
!          "nTimeStepsRinging"      - print for how many time steps the alarm \\
!                                     has been ringing. \\
!          "ringing"                - print the alarm's current ringing state.\\
!          "ringingOnPrevTimeStep"  - print whether the alarm was ringing \\
!                                     immediately after the previous clock \\
!                                     time step.
!          "enabled"      - print the alarm's ability to ring. \\
!          "sticky"       - print whether the alarm must be turned off \\
!                           manually. \\
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
!     invoke C to C++ entry point
      call c_ESMC_AlarmPrint(alarm, options, rc)   

      end subroutine ESMF_AlarmPrint

!------------------------------------------------------------------------------

      end module ESMF_AlarmMod
