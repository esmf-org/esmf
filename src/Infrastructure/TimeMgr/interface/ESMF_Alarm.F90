! $Id: ESMF_Alarm.F90,v 1.22 2003/07/25 05:17:06 eschwab Exp $
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
        type(ESMF_Time)  :: ringTime
        type(ESMF_Time)  :: prevRingTime
        type(ESMF_Time)  :: stopTime
        integer :: id
        integer :: alarmMutex
        logical :: ringing
        logical :: enabled
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Alarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_AlarmSet
      public ESMF_AlarmGetRingInterval
      public ESMF_AlarmSetRingInterval
      public ESMF_AlarmGetRingTime
      public ESMF_AlarmSetRingTime
      public ESMF_AlarmGetPrevRingTime
      public ESMF_AlarmSetPrevRingTime
      public ESMF_AlarmGetStopTime
      public ESMF_AlarmSetStopTime
      public ESMF_AlarmEnable
      public ESMF_AlarmDisable
      public ESMF_AlarmTurnOn
      public ESMF_AlarmTurnOff
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
      '$Id: ESMF_Alarm.F90,v 1.22 2003/07/25 05:17:06 eschwab Exp $'

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
      subroutine ESMF_AlarmReadRestart(alarm, ringInterval, ringTime, &
                                       prevRingTime, stopTime, ringing, &
                                       enabled, id, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(out) :: alarm
      type(ESMF_TimeInterval), intent(in) :: ringInterval
      type(ESMF_Time), intent(in) :: ringTime
      type(ESMF_Time), intent(in) :: prevRingTime
      type(ESMF_Time), intent(in) :: stopTime
      logical, intent(in) :: ringing
      logical, intent(in) :: enabled
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
!     \item[ringTime]
!          Ring time for one-shot or first repeating alarm.
!     \item[prevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time.
!     \item[stopTime]
!          Stop time for repeating alarms.
!     \item[ringing]
!          The {\tt ESMF\_Alarm}'s ringing state.
!     \item[enabled]
!          {\tt ESMF\_Alarm} enabled/disabled.
!     \item[id]
!          The {\tt ESMF\_Alarm}'s ID.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
      call c_ESMC_AlarmReadRestart(alarm, ringInterval, ringTime, &
                                   prevRingTime, stopTime, ringing, &
                                   enabled, id, rc)

      end subroutine ESMF_AlarmReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmWriteRestart - saves an alarm

! !INTERFACE:
      subroutine ESMF_AlarmWriteRestart(alarm, ringInterval, ringTime, &
                                        prevRingTime, stopTime, ringing, &
                                        enabled, id, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(in) :: alarm
      type(ESMF_TimeInterval), intent(out) :: ringInterval
      type(ESMF_Time), intent(out) :: ringTime
      type(ESMF_Time), intent(out) :: prevRingTime
      type(ESMF_Time), intent(out) :: stopTime
      logical, intent(out) :: ringing
      logical, intent(out) :: enabled
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
!     \item[ringTime]
!          Ring time for one-shot or first repeating alarm.
!     \item[prevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time.
!     \item[stopTime]
!          Stop time for repeating alarms.
!     \item[ringing]
!          The {\tt ESMF\_Alarm}'s ringing state.
!     \item[enabled]
!          {\tt ESMF\_Alarm} enabled/disabled.
!     \item[id]
!          The {\tt ESMF\_Alarm}'s ID.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
      call c_ESMC_AlarmWriteRestart(alarm, ringInterval, ringTime, &
                                    prevRingTime, stopTime, ringing, &
                                    enabled, id, rc)

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
