! $Id: ESMF_Alarm.F90,v 1.30 2003/09/11 00:01:06 eschwab Exp $
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
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! associated derived types
      use ESMF_TimeIntervalMod
      use ESMF_TimeMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!     private
!------------------------------------------------------------------------------
!     ! ESMF_Alarm
!
!     ! F90 class type to match C++ Alarm class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_Alarm
      sequence
      private
        type(ESMF_TimeInterval) :: ringInterval
        type(ESMF_TimeInterval) :: ringDuration
        type(ESMF_Time)         :: ringTime
        type(ESMF_Time)         :: prevRingTime
        type(ESMF_Time)         :: stopTime
        type(ESMF_Time)         :: ringBegin
        type(ESMF_Time)         :: refTime
        integer                 :: nRingDurationTimeSteps
        integer                 :: nTimeStepsRinging
        integer                 :: id
#ifndef ESMF_NO_INITIALIZERS
        logical                 :: ringing = .false.
        logical                 :: enabled = .true.
        logical                 :: sticky  = .true.
#else
        logical                 :: ringing
        logical                 :: enabled
        logical                 :: sticky
#endif
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Alarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_AlarmSetup
      public ESMF_AlarmSet
      public ESMF_AlarmGet

      public ESMF_AlarmEnable
      public ESMF_AlarmDisable
      public ESMF_AlarmIsEnabled

      public ESMF_AlarmRingerOn
      public ESMF_AlarmRingerOff
      public ESMF_AlarmIsRinging

      public ESMF_AlarmSticky
      public ESMF_AlarmNotSticky
      public ESMF_AlarmIsSticky

      public operator(.EQ.)
 
! Required inherited and overridden ESMF_Base class methods

      public ESMF_AlarmReadRestart
      public ESMF_AlarmWriteRestart
      public ESMF_AlarmValidate
      public ESMF_AlarmPrint
!EOPI

! !PRIVATE MEMBER FUNCTIONS:

! Object version methods

      private ESMF_AlarmObjSetup
      private ESMF_AlarmObjSet
      private ESMF_AlarmObjGet

      private ESMF_AlarmObjEnable
      private ESMF_AlarmObjDisable
      private ESMF_AlarmObjIsEnabled

      private ESMF_AlarmObjRingerOn
      private ESMF_AlarmObjRingerOff
      private ESMF_AlarmObjIsRinging

      private ESMF_AlarmObjSticky
      private ESMF_AlarmObjNotSticky
      private ESMF_AlarmObjIsSticky

      private ESMF_AlarmObjEQ

      private ESMF_AlarmObjReadRestart
      private ESMF_AlarmObjWriteRestart
      private ESMF_AlarmObjValidate
      private ESMF_AlarmObjPrint

! Pointer version methods

      private ESMF_AlarmPtrSetup
      private ESMF_AlarmPtrSet
      private ESMF_AlarmPtrGet

      private ESMF_AlarmPtrEnable
      private ESMF_AlarmPtrDisable
      private ESMF_AlarmPtrIsEnabled

      private ESMF_AlarmPtrRingerOn
      private ESMF_AlarmPtrRingerOff
      private ESMF_AlarmPtrIsRinging

      private ESMF_AlarmPtrSticky
      private ESMF_AlarmPtrNotSticky
      private ESMF_AlarmPtrIsSticky

      private ESMF_AlarmPtrReadRestart
      private ESMF_AlarmPtrWriteRestart
      private ESMF_AlarmPtrValidate
      private ESMF_AlarmPtrPrint

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Alarm.F90,v 1.30 2003/09/11 00:01:06 eschwab Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_AlarmSetup

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjSetup
      module procedure ESMF_AlarmPtrSetup

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmSetup} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmSet

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjSet
      module procedure ESMF_AlarmPtrSet

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmSet} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmGet

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjGet
      module procedure ESMF_AlarmPtrGet

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmGet} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmEnable

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjEnable
      module procedure ESMF_AlarmPtrEnable

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmEnable} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmDisable

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjDisable
      module procedure ESMF_AlarmPtrDisable

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmDisable} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmIsEnabled

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjIsEnabled
      module procedure ESMF_AlarmPtrIsEnabled

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmIsEnabled} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmRingerOn

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjRingerOn
      module procedure ESMF_AlarmPtrRingerOn

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmRingerOn} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmRingerOff

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjRingerOff
      module procedure ESMF_AlarmPtrRingerOff

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmRingerOff} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmIsRinging

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjIsRinging
      module procedure ESMF_AlarmPtrIsRinging

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmIsRinging} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmSticky

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjSticky
      module procedure ESMF_AlarmPtrSticky

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmSticky} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmNotSticky

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjNotSticky
      module procedure ESMF_AlarmPtrNotSticky

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmNotSticky} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmIsSticky

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjIsSticky
      module procedure ESMF_AlarmPtrIsSticky

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmIsSticky} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.EQ.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjEQ
!     pointer version ESMF_pteq defined in base class

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmReadRestart

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjReadRestart
      module procedure ESMF_AlarmPtrReadRestart

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmReadRestart} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmWriteRestart

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjWriteRestart
      module procedure ESMF_AlarmPtrWriteRestart

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmWriteRestart} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmValidate

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjValidate
      module procedure ESMF_AlarmPtrValidate

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmValidate} method for the 
!     {\tt ESMF\_Alarm} class.  It allows for passing either the direct
!     alarm object or a pointer to it.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AlarmPrint

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_AlarmObjPrint
      module procedure ESMF_AlarmPtrPrint

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_AlarmPrint} method for the 
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
! Object version API
!------------------------------------------------------------------------------
!
! This section includes the Set/Get methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmObjSetup - Initialize Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmObjSetup(alarm, ringTime, ringInterval, stopTime, &
                                    ringDuration, nRingDurationTimeSteps, &
                                    refTime, id, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: nRingDurationTimeSteps
      type(ESMF_Time),         intent(in),  optional :: refTime
      integer,                 intent(in),  optional :: id
      logical,                 intent(in),  optional :: enabled
      logical,                 intent(in),  optional :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set.
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
!     \item[{[id]}]
!          Optional unique identifier for an alarm.
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

!     invoke C to C++ entry point
      call c_ESMC_AlarmObjSetup(alarm, ringTime, ringInterval, &
                                stopTime, ringDuration, &
                                nRingDurationTimeSteps, refTime, id, &
                                enabled, sticky, rc)

      end subroutine ESMF_AlarmObjSetup

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmObjSet - Set Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmObjSet(alarm, ringTime, ringInterval, stopTime, &
                                  ringDuration, nRingDurationTimeSteps, &
                                  refTime, id, ringing, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(inout)         :: alarm
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: nRingDurationTimeSteps
      type(ESMF_Time),         intent(in),  optional :: refTime
      integer,                 intent(in),  optional :: id
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
!     \item[{[id]}]
!          Optional unique identifier for an alarm.
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

!     invoke C to C++ entry point
      call c_ESMC_AlarmObjSet(alarm, ringTime, ringInterval, &
                              stopTime, ringDuration, nRingDurationTimeSteps, &
                              refTime, id, ringing, enabled, sticky, rc)

      end subroutine ESMF_AlarmObjSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmObjGet - Get Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmObjGet(alarm, ringTime, prevRingTime, ringInterval, &
                                  stopTime, ringDuration, &
                                  nRingDurationTimeSteps, nTimeStepsRinging, &
                                  ringBegin, refTime, id, &
                                  ringing, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(in)            :: alarm
      type(ESMF_Time),         intent(out), optional :: ringTime
      type(ESMF_Time),         intent(out), optional :: prevRingTime
      type(ESMF_TimeInterval), intent(out), optional :: ringInterval
      type(ESMF_Time),         intent(out), optional :: stopTime
      type(ESMF_TimeInterval), intent(out), optional :: ringDuration
      integer,                 intent(out), optional :: nRingDurationTimeSteps
      integer,                 intent(out), optional :: nTimeStepsRinging
      type(ESMF_Time),         intent(out), optional :: ringBegin
      type(ESMF_Time),         intent(out), optional :: refTime
      integer,                 intent(out), optional :: id
      logical,                 intent(out), optional :: ringing
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
!     \item[{[id]}]
!          Gets the unique identifier for an alarm.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7

!     invoke C to C++ entry point
      call c_ESMC_AlarmObjGet(alarm, ringTime, prevRingTime, ringInterval, &
                              stopTime, ringDuration, nRingDurationTimeSteps, &
                              nTimeStepsRinging, ringBegin, refTime, id, &
                              ringing, enabled, sticky, rc)

      end subroutine ESMF_AlarmObjGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmObjEnable - Enable an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmObjEnable(alarm, rc)

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
      call c_ESMC_AlarmObjEnable(alarm, rc)

      end subroutine ESMF_AlarmObjEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmObjDisable - Disable an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmObjDisable(alarm, rc)

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
      call c_ESMC_AlarmObjDisable(alarm, rc)

      end subroutine ESMF_AlarmObjDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjIsEnabled - Check if Alarm is enabled

! !INTERFACE:
      function ESMF_AlarmObjIsEnabled(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmObjIsEnabled

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
      call c_ESMC_AlarmObjIsEnabled(alarm, ESMF_AlarmObjIsEnabled, rc)

      end function ESMF_AlarmObjIsEnabled

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjRingerOn - Turn on an Alarm


! !INTERFACE:
      subroutine ESMF_AlarmObjRingerOn(alarm, rc)

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
      call c_ESMC_AlarmObjRingerOn(alarm, rc)

      end subroutine ESMF_AlarmObjRingerOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjRingerOff - Turn off an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmObjRingerOff(alarm, rc)

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
      call c_ESMC_AlarmObjRingerOff(alarm, rc)

      end subroutine ESMF_AlarmObjRingerOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjIsRinging - Check if Alarm is ringing

! !INTERFACE:
      function ESMF_AlarmObjIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmObjIsRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in)            :: alarm
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is ringing.
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
      call c_ESMC_AlarmObjIsRinging(alarm, ESMF_AlarmObjIsRinging, rc)

      end function ESMF_AlarmObjIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjSticky - Set an Alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmObjSticky(alarm, rc)

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
      call c_ESMC_AlarmObjSticky(alarm, rc)

      end subroutine ESMF_AlarmObjSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjNotSticky - Unset an Alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmObjNotSticky(alarm, ringDuration, &
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
      call c_ESMC_AlarmObjNotSticky(alarm, ringDuration, &
                                    nRingDurationTimeSteps, rc)

      end subroutine ESMF_AlarmObjNotSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjIsSticky - Check if Alarm is sticky

! !INTERFACE:
      function ESMF_AlarmObjIsSticky(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmObjIsSticky

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
      call c_ESMC_AlarmObjIsSticky(alarm, ESMF_AlarmObjIsSticky, rc)

      end function ESMF_AlarmObjIsSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjEQ - Compare two Alarms for equality
!
! !INTERFACE:
      function ESMF_AlarmObjEQ(alarm1, alarm2)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmObjEQ

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
      call c_ESMC_AlarmObjEQ(alarm1, alarm2, ESMF_AlarmObjEQ)

      end function ESMF_AlarmObjEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmObjReadRestart - Restore an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmObjReadRestart(alarm, ringInterval, ringDuration, &
                                          ringTime, prevRingTime, stopTime, &
                                          ringBegin, refTime, &
                                          nRingDurationTimeSteps, &
                                          nTimeStepsRinging, &
                                          ringing, enabled, sticky, id, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(out)           :: alarm
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
      integer,                 intent(in)            :: id
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to restore.
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
!     \item[id]
!          The {\tt ESMF\_Alarm}'s ID.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_AlarmObjReadRestart(alarm, ringInterval, ringDuration, &
                                      ringTime, prevRingTime, stopTime, &      
                                      ringBegin, refTime, &
                                      nRingDurationTimeSteps, &
                                      nTimeStepsRinging, &
                                      ringing, enabled, sticky, id, rc)

      end subroutine ESMF_AlarmObjReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmObjWriteRestart - Save an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmObjWriteRestart(alarm, ringInterval, ringDuration, &
                                           ringTime, prevRingTime, stopTime, &
                                           ringBegin, refTime, &
                                           nRingDurationTimeSteps, &
                                           nTimeStepsRinging, &
                                           ringing, enabled, sticky, id, rc)

! !ARGUMENTS:
      type(ESMF_Alarm),        intent(in)            :: alarm
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
      integer,                 intent(out)           :: id
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to save.
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
!     \item[id]
!          The {\tt ESMF\_Alarm}'s ID.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_AlarmObjWriteRestart(alarm, ringInterval, ringDuration, &
                                       ringTime, prevRingTime, stopTime, &
                                       ringBegin, refTime, &
                                       nRingDurationTimeSteps, &
                                       nTimeStepsRinging, &
                                       ringing, enabled, sticky, id, rc)

      end subroutine ESMF_AlarmObjWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmObjValidate(alarm, options, rc)

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
      call c_ESMC_AlarmObjValidate(alarm, options, rc)
    
      end subroutine ESMF_AlarmObjValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmObjPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmObjPrint(alarm, options, rc)

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
!          Print options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
!     invoke C to C++ entry point
      call c_ESMC_AlarmObjPrint(alarm, options, rc)   

      end subroutine ESMF_AlarmObjPrint

!------------------------------------------------------------------------------
! Pointer version API
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmPtrSetup - Initialize Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmPtrSetup(alarm, ringTime, ringInterval, stopTime, &
                                    ringDuration, nRingDurationTimeSteps, &
                                    refTime, id, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Pointer),      intent(inout)         :: alarm
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: nRingDurationTimeSteps
      type(ESMF_Time),         intent(in),  optional :: refTime
      integer,                 intent(in),  optional :: id
      logical,                 intent(in),  optional :: enabled
      logical,                 intent(in),  optional :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to set.
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
!     \item[{[id]}]
!          Optional unique identifier for an alarm.
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

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrSetup(alarm, ringTime, ringInterval, &
                                stopTime, ringDuration, &
                                nRingDurationTimeSteps, refTime, id, &
                                enabled, sticky, rc)

      end subroutine ESMF_AlarmPtrSetup

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmPtrSet - Set Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmPtrSet(alarm, ringTime, ringInterval, stopTime, &
                                  ringDuration, nRingDurationTimeSteps, &
                                  refTime, id, ringing, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Pointer),      intent(inout)         :: alarm
      type(ESMF_Time),         intent(in),  optional :: ringTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringInterval
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: ringDuration
      integer,                 intent(in),  optional :: nRingDurationTimeSteps
      type(ESMF_Time),         intent(in),  optional :: refTime
      integer,                 intent(in),  optional :: id
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
!          Pointer to the object instance to set.
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
!     \item[{[id]}]
!          Optional unique identifier for an alarm.
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

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrSet(alarm, ringTime, ringInterval, &
                              stopTime, ringDuration, nRingDurationTimeSteps, &
                              refTime, id, ringing, enabled, sticky, rc)

      end subroutine ESMF_AlarmPtrSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmPtrGet - Get Alarm properties

! !INTERFACE:
      subroutine ESMF_AlarmPtrGet(alarm, ringTime, prevRingTime, ringInterval, &
                                  stopTime, ringDuration, &
                                  nRingDurationTimeSteps, nTimeStepsRinging, &
                                  ringBegin, refTime, id, &
                                  ringing, enabled, sticky, rc)

! !ARGUMENTS:
      type(ESMF_Pointer),      intent(in)            :: alarm
      type(ESMF_Time),         intent(out), optional :: ringTime
      type(ESMF_Time),         intent(out), optional :: prevRingTime
      type(ESMF_TimeInterval), intent(out), optional :: ringInterval
      type(ESMF_Time),         intent(out), optional :: stopTime
      type(ESMF_TimeInterval), intent(out), optional :: ringDuration
      integer,                 intent(out), optional :: nRingDurationTimeSteps
      integer,                 intent(out), optional :: nTimeStepsRinging
      type(ESMF_Time),         intent(out), optional :: ringBegin
      type(ESMF_Time),         intent(out), optional :: refTime
      integer,                 intent(out), optional :: id
      logical,                 intent(out), optional :: ringing
      logical,                 intent(out), optional :: enabled
      logical,                 intent(out), optional :: sticky
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Gets an {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to get.
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
!     \item[{[id]}]
!          Gets the unique identifier for an alarm.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrGet(alarm, ringTime, prevRingTime, ringInterval, &
                              stopTime, ringDuration, nRingDurationTimeSteps, &
                              nTimeStepsRinging, ringBegin, refTime, id, &
                              ringing, enabled, sticky, rc)

      end subroutine ESMF_AlarmPtrGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmPtrEnable - Enable an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmPtrEnable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Pointer), intent(inout)         :: alarm
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     Enables an {\tt ESMF\_Alarm} to function.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to enable.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.5.3

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrEnable(alarm, rc)

      end subroutine ESMF_AlarmPtrEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmPtrDisable - Disable an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmPtrDisable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Pointer), intent(inout)         :: alarm
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     Disables an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to disable.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.5.3
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrDisable(alarm, rc)

      end subroutine ESMF_AlarmPtrDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrIsEnabled - Check if Alarm is enabled

! !INTERFACE:
      function ESMF_AlarmPtrIsEnabled(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmPtrIsEnabled

! !ARGUMENTS:
      type(ESMF_Pointer), intent(in)            :: alarm
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is enabled.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to check for enabled state.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrIsEnabled(alarm, ESMF_AlarmPtrIsEnabled, rc)

      end function ESMF_AlarmPtrIsEnabled

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrRingerOn - Turn on an Alarm


! !INTERFACE:
      subroutine ESMF_AlarmPtrRingerOn(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Pointer), intent(inout)         :: alarm
      integer,            intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn on an {\tt ESMF\_Alarm}; sets ringing state.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to turn on.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.6

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrRingerOn(alarm, rc)

      end subroutine ESMF_AlarmPtrRingerOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrRingerOff - Turn off an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmPtrRingerOff(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Pointer), intent(inout)         :: alarm
      integer,            intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an {\tt ESMF\_Alarm}; unsets ringing state.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to turn off.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.6

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrRingerOff(alarm, rc)

      end subroutine ESMF_AlarmPtrRingerOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrIsRinging - Check if Alarm is ringing

! !INTERFACE:
      function ESMF_AlarmPtrIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmPtrIsRinging

! !ARGUMENTS:
      type(ESMF_Pointer), intent(in)            :: alarm
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to check for ringing state.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.4
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrIsRinging(alarm, ESMF_AlarmPtrIsRinging, rc)

      end function ESMF_AlarmPtrIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrSticky - Set an Alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmPtrSticky(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Pointer), intent(inout)         :: alarm
      integer,            intent(out), optional :: rc
    
! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s sticky flag; once alarm is ringing,
!     it remains ringing until {\tt ESMF\_AlarmRingerOff()} is called.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to set sticky.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrSticky(alarm, rc)

      end subroutine ESMF_AlarmPtrSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrNotSticky - Unset an Alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmPtrNotSticky(alarm, ringDuration, &
                                        nRingDurationTimeSteps, rc)

! !ARGUMENTS:
      type(ESMF_Pointer),      intent(inout)         :: alarm
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
!          Pointer to the object instance to unset sticky.
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
      call c_ESMC_AlarmPtrNotSticky(alarm, ringDuration, &
                                    nRingDurationTimeSteps, rc)

      end subroutine ESMF_AlarmPtrNotSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrIsSticky - Check if Alarm is sticky

! !INTERFACE:
      function ESMF_AlarmPtrIsSticky(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmPtrIsSticky

! !ARGUMENTS:
      type(ESMF_Pointer), intent(in)            :: alarm
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is sticky.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to check for sticky state.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrIsSticky(alarm, ESMF_AlarmPtrIsSticky, rc)

      end function ESMF_AlarmPtrIsSticky

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmPtrReadRestart - Restore an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmPtrReadRestart(alarm, ringInterval, ringDuration, &
                                          ringTime, prevRingTime, stopTime, &
                                          ringBegin, refTime, &
                                          nRingDurationTimeSteps, &
                                          nTimeStepsRinging, &
                                          ringing, enabled, sticky, id, rc)

! !ARGUMENTS:
      type(ESMF_Pointer),      intent(out)           :: alarm
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
      integer,                 intent(in)            :: id
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to restore.
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
!     \item[id]
!          The {\tt ESMF\_Alarm}'s ID.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrReadRestart(alarm, ringInterval, ringDuration, &
                                      ringTime, prevRingTime, stopTime, &      
                                      ringBegin, refTime, &
                                      nRingDurationTimeSteps, &
                                      nTimeStepsRinging, &
                                      ringing, enabled, sticky, id, rc)

      end subroutine ESMF_AlarmPtrReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmPtrWriteRestart - Save an Alarm

! !INTERFACE:
      subroutine ESMF_AlarmPtrWriteRestart(alarm, ringInterval, ringDuration, &
                                           ringTime, prevRingTime, stopTime, &
                                           ringBegin, refTime, &
                                           nRingDurationTimeSteps, &
                                           nTimeStepsRinging, &
                                           ringing, enabled, sticky, id, rc)

! !ARGUMENTS:
      type(ESMF_Pointer),      intent(in)            :: alarm
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
      integer,                 intent(out)           :: id
      integer,                 intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to the object instance to save.
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
!     \item[id]
!          The {\tt ESMF\_Alarm}'s ID.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrWriteRestart(alarm, ringInterval, ringDuration, &
                                       ringTime, prevRingTime, stopTime, &
                                       ringBegin, refTime, &
                                       nRingDurationTimeSteps, &
                                       nTimeStepsRinging, &
                                       ringing, enabled, sticky, id, rc)

      end subroutine ESMF_AlarmPtrWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmPtrValidate(alarm, options, rc)

! !ARGUMENTS:
      type(ESMF_Pointer), intent(in)            :: alarm
      character (len=*),  intent(in),  optional :: options
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          Pointer to {\tt ESMF\_Alarm} to validate.
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
      call c_ESMC_AlarmPtrValidate(alarm, options, rc)
    
      end subroutine ESMF_AlarmPtrValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPtrPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmPtrPrint(alarm, options, rc)

! !ARGUMENTS:
      type(ESMF_Pointer), intent(in)            :: alarm
      character (len=*),  intent(in),  optional :: options
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Alarm}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Pointer to {\tt ESMF\_Alarm} to print out.
!     \item[{[options]}]
!          Print options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
!     invoke C to C++ entry point
      call c_ESMC_AlarmPtrPrint(alarm, options, rc)   

      end subroutine ESMF_AlarmPtrPrint

!------------------------------------------------------------------------------

      end module ESMF_AlarmMod
