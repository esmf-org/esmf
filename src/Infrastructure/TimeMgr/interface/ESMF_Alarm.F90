! $Id: ESMF_Alarm.F90,v 1.23 2003/07/25 22:57:54 eschwab Exp $
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
        type(ESMF_Time)  :: ringTime
        type(ESMF_Time)  :: prevRingTime
        type(ESMF_Time)  :: stopTime
        type(ESMF_Time)  :: ringBegin
        type(ESMF_Time)  :: refTime
        integer :: id
        integer :: alarmMutex
        logical :: ringing = .false.
        logical :: enabled = .true.
        logical :: sticky  = .true.
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Alarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_AlarmSet
      public ESMF_AlarmGetRingInterval
      public ESMF_AlarmSetRingInterval
      public ESMF_AlarmGetRingDuration
      public ESMF_AlarmSetRingDuration
      public ESMF_AlarmGetRingTime
      public ESMF_AlarmSetRingTime
      public ESMF_AlarmGetPrevRingTime
      public ESMF_AlarmSetPrevRingTime
      public ESMF_AlarmGetStopTime
      public ESMF_AlarmSetStopTime
      public ESMF_AlarmGetRefTime
      public ESMF_AlarmSetRefTime
      public ESMF_AlarmEnable
      public ESMF_AlarmDisable
      public ESMF_AlarmTurnOn
      public ESMF_AlarmTurnOff
      public ESMF_AlarmSticky
      public ESMF_AlarmNotSticky
      public ESMF_AlarmIsRinging
 
! Required inherited and overridden ESMF_Base class methods

      public ESMF_AlarmReadRestart
      public ESMF_AlarmWriteRestart
      public ESMF_AlarmValidate
      public ESMF_AlarmPrint

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_AlarmEQ
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Alarm.F90,v 1.23 2003/07/25 22:57:54 eschwab Exp $'

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

! !DESCRIPTION:
!     This interface overloads the == operator for the {\tt ESMF\_Alarm} class.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSet - Initializes an alarm

! !INTERFACE:
      subroutine ESMF_AlarmSet(alarm, ringTime, ringInterval, &
                               stopTime, enabled, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in), optional :: ringTime
      type(ESMF_TimeInterval), intent(in), optional :: ringInterval
      type(ESMF_Time), intent(in), optional :: stopTime
      logical, intent(in) :: enabled
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Alarm}.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to initialize.
!     \item[{[ringTime]}]
!          Optional ring time for one-shot or first repeating alarm.
!     \item[{[ringInterval]}]
!          Optional ring interval for repeating alarms.
!     \item[{[stopTime]}]
!          Optional stop time for repeating alarms.
!     \item[enabled]
!          Alarm enabled/disabled.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.7
      call c_ESMC_AlarmSet(alarm, ringTime, ringInterval, &
                            stopTime, enabled, rc)

      end subroutine ESMF_AlarmSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmGetRingInterval - Get an alarm's ring interval
!
! !INTERFACE:
      subroutine ESMF_AlarmGetRingInterval(alarm, ringInterval, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_TimeInterval), intent(out) :: ringInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s ring interval.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring interval.
!     \item[ringInterval]
!          The {\tt Alarm}'s ring interval.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG4.7
    
      call c_ESMC_AlarmGetRingInterval(alarm, ringInterval, rc)

      end subroutine ESMF_AlarmGetRingInterval
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSetRingInterval - Set an alarm's ring interval
!
! !INTERFACE:
      subroutine ESMF_AlarmSetRingInterval(alarm, ringInterval, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_TimeInterval), intent(in) :: ringInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s ring interval.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring interval.
!     \item[ringInterval]
!          The {\tt Alarm}'s ring interval.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
    
      call c_ESMC_AlarmSetRingInterval(alarm, ringInterval, rc)

      end subroutine ESMF_AlarmSetRingInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmGetRingDuration - Get an alarm's ring duration
!
! !INTERFACE:
      subroutine ESMF_AlarmGetRingDuration(alarm, ringDuration, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_TimeInterval), intent(out) :: ringDuration
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s ring duration; how long it rings
!     once it begins ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring duration.
!     \item[ringDuration]
!          The {\tt Alarm}'s ring duration.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
      call c_ESMC_AlarmGetRingDuration(alarm, ringDuration, rc)

      end subroutine ESMF_AlarmGetRingDuration
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSetRingDuration - Set an alarm's ring duration
!
! !INTERFACE:
      subroutine ESMF_AlarmSetRingDuration(alarm, ringDuration, &
                                           numTimeSteps, timeStep, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_TimeInterval), intent(in), optional :: ringDuration
      integer, intent(in), optional :: numTimeSteps
      type(ESMF_TimeInterval), intent(in), optional :: timeStep
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s ring duration; how long it rings
!     once it begins ringing. Either specify the time interval ringDuration,
!     or a number of timesteps and the timestep and the ringDuration will
!     be calculated.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring duration.
!     \item[ringDuration]
!          The {\tt Alarm}'s ring duration.
!     \item[numTimeSteps]
!          The number of time steps to set the duration to.
!     \item[timeStep]
!          The time step * number of time steps = ring duration.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
      call c_ESMC_AlarmSetRingDuration(alarm, ringDuration, numTimeSteps, &
                                       timeStep, rc)

      end subroutine ESMF_AlarmSetRingDuration
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetRingTime - Get an alarm's time to ring
!
! !INTERFACE:
      subroutine ESMF_AlarmGetRingTime(alarm, ringTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_Time), intent(out) :: ringTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s time to ring.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring time.
!     \item[ringTime]
!          The {\tt ESMF\_Alarm}'s ring time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.7, TMG4.8

      call c_ESMC_AlarmGetRingTime(alarm, ringTime, rc)

      end subroutine ESMF_AlarmGetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetRingTime - Set an alarm's time to ring
!
! !INTERFACE:
      subroutine ESMF_AlarmSetRingTime(alarm, ringTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in) :: ringTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s time to ring.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring time.
!     \item[ringTime]
!          The {\tt ESMF\_Alarm}'s ring time to set.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.5.1, TMG4.7, TMG4.8
   
      call c_ESMC_AlarmSetRingTime(alarm, ringTime, rc)

      end subroutine ESMF_AlarmSetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetPrevRingTime - Get an alarm's previous ring time
!
! !INTERFACE:
      subroutine ESMF_AlarmGetPrevRingTime(alarm, prevRingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_Time), intent(out) :: prevRingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s previous ring time.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time.
!     \item[prevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.7, TMG4.8

      call c_ESMC_AlarmGetPrevRingTime(alarm, prevRingTime, rc)

      end subroutine ESMF_AlarmGetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetPrevRingTime - Set an alarm's previous ring time
!
! !INTERFACE:
      subroutine ESMF_AlarmSetPrevRingTime(alarm, prevRingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in) :: prevRingTime
      integer, intent(out), optional :: rc
   
! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s previous ring time.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the previous ring time.
!     \item[prevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time to set.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.7, TMG4.8

      call c_ESMC_AlarmSetPrevRingTime(alarm, prevRingTime, rc)

      end subroutine ESMF_AlarmSetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetStopTime - Get an alarm's stop time
!
! !INTERFACE:
      subroutine ESMF_AlarmGetStopTime(alarm, stopTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_Time), intent(out) :: stopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s stop time.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the stop time.
!     \item[stopTime]
!          The {\tt ESMF\_Alarm}'s stop time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
   
      call c_ESMC_AlarmGetStopTime(alarm, stopTime, rc)

      end subroutine ESMF_AlarmGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetStopTime - Set an alarm's stop time
!
! !INTERFACE:
      subroutine ESMF_AlarmSetStopTime(alarm, stopTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in) :: stopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s stop time.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the stop time.
!     \item[stopTime]
!          The {\tt ESMF\_Alarm}'s stop time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7

      call c_ESMC_AlarmSetStopTime(alarm, stopTime, rc)

      end subroutine ESMF_AlarmSetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetRefTime - Get an alarm's reference time
!
! !INTERFACE:
      subroutine ESMF_AlarmGetRefTime(alarm, refTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_Time), intent(out) :: refTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s reference time.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the reference time.
!     \item[refTime]
!          The {\tt ESMF\_Alarm}'s reference time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_AlarmGetRefTime(alarm, refTime, rc)

      end subroutine ESMF_AlarmGetRefTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetRefTime - Set an alarm's reference time
!
! !INTERFACE:
      subroutine ESMF_AlarmSetRefTime(alarm, refTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_Time), intent(in) :: refTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s reference time.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the reference time.
!     \item[refTime]
!          The {\tt ESMF\_Alarm}'s reference time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
   
      call c_ESMC_AlarmSetRefTime(alarm, refTime, rc)

      end subroutine ESMF_AlarmSetRefTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmEnable - Enables an alarm

! !INTERFACE:
      subroutine ESMF_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out), optional :: rc

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

      call c_ESMC_AlarmEnable(alarm, rc)

      end subroutine ESMF_AlarmEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmDisable - Disables an alarm

! !INTERFACE:
      subroutine ESMF_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out), optional :: rc

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
    
      call c_ESMC_AlarmDisable(alarm, rc)

      end subroutine ESMF_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmTurnOn - Turn on an alarm


! !INTERFACE:
      subroutine ESMF_AlarmTurnOn(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out), optional :: rc
    
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

      call c_ESMC_AlarmTurnOn(alarm, rc)

      end subroutine ESMF_AlarmTurnOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmTurnOff - Turn off an alarm

! !INTERFACE:
      subroutine ESMF_AlarmTurnOff(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out), optional :: rc
    
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

      call c_ESMC_AlarmTurnOff(alarm, rc)

      end subroutine ESMF_AlarmTurnOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSticky - Set an alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmSticky(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s sticky flag; once alarm is ringing,
!     it remains ringing until {\tt ESMF\_AlarmTurnOff()} is called.
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

      call c_ESMC_AlarmSticky(alarm, rc)

      end subroutine ESMF_AlarmSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmNotSticky - Unset an alarm's sticky flag


! !INTERFACE:
      subroutine ESMF_AlarmNotSticky(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Unset an {\tt ESMF\_Alarm}'s sticky flag; once alarm is ringing,
!     it turns itself off after ringDuration.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to unset sticky.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n

      call c_ESMC_AlarmNotSticky(alarm, rc)

      end subroutine ESMF_AlarmNotSticky

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmIsRinging - Check if alarm is ringing

! !INTERFACE:
      function ESMF_AlarmIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmIsRinging

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      integer, intent(out), optional :: rc

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
    
      call c_ESMC_AlarmIsRinging(alarm, ESMF_AlarmIsRinging, rc)

      end function ESMF_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmEQ - Compare two alarms for equality
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

      call c_ESMC_AlarmEQ(alarm1, alarm2, ESMF_AlarmEQ)

      end function ESMF_AlarmEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmReadRestart - restores an alarm

! !INTERFACE:
      subroutine ESMF_AlarmReadRestart(alarm, ringInterval, ringDuration, &
                                       ringTime, prevRingTime, stopTime, &
                                       ringBegin, refTime, ringing, enabled, &
                                       sticky, id, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_TimeInterval), intent(in) :: ringInterval
      type(ESMF_TimeInterval), intent(in) :: ringDuration
      type(ESMF_Time), intent(in) :: ringTime
      type(ESMF_Time), intent(in) :: prevRingTime
      type(ESMF_Time), intent(in) :: stopTime
      type(ESMF_Time), intent(in) :: ringBegin
      type(ESMF_Time), intent(in) :: refTime
      logical, intent(in) :: ringing
      logical, intent(in) :: enabled
      logical, intent(in) :: sticky
      integer, intent(in) :: id
      integer, intent(out), optional :: rc

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
      call c_ESMC_AlarmReadRestart(alarm, ringInterval, ringDuration, &
                                   ringTime, prevRingTime, stopTime, &
                                   ringBegin, refTime, ringing, enabled, &
                                   sticky, id, rc)

      end subroutine ESMF_AlarmReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmWriteRestart - saves an alarm

! !INTERFACE:
      subroutine ESMF_AlarmWriteRestart(alarm, ringInterval, ringDuration, &
                                       ringTime, prevRingTime, stopTime, &
                                       ringBegin, refTime, ringing, enabled, &
                                       sticky, id, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_TimeInterval), intent(out) :: ringInterval
      type(ESMF_TimeInterval), intent(out) :: ringDuration
      type(ESMF_Time), intent(out) :: ringTime
      type(ESMF_Time), intent(out) :: prevRingTime
      type(ESMF_Time), intent(out) :: stopTime
      type(ESMF_Time), intent(out) :: ringBegin
      type(ESMF_Time), intent(out) :: refTime
      logical, intent(out) :: ringing
      logical, intent(out) :: enabled
      logical, intent(out) :: sticky
      integer, intent(out) :: id
      integer, intent(out), optional :: rc

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
      call c_ESMC_AlarmWriteRestart(alarm, ringInterval, ringDuration, &
                                    ringTime, prevRingTime, stopTime, &
                                    ringBegin, refTime, ringing, enabled, &
                                    sticky, id, rc)

      end subroutine ESMF_AlarmWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmValidate(alarm, opts, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Alarm}'s properties.
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to validate.
!     \item[{[opts]}]
!          Validate options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
      call c_ESMC_AlarmValidate(alarm, opts, rc)
    
      end subroutine ESMF_AlarmValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine ESMF_AlarmPrint(alarm, opts, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Alarm}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to print out.
!     \item[{[opts]}]
!          Print options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
      call c_ESMC_AlarmPrint(alarm, opts, rc)   

      end subroutine ESMF_AlarmPrint

!------------------------------------------------------------------------------

      end module ESMF_AlarmMod
