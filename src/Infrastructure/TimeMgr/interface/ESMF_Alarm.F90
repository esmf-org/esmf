! $Id: ESMF_Alarm.F90,v 1.5 2003/03/22 05:46:04 eschwab Exp $
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
!BOP
!
! !MODULE: ESMF_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class ESMC\_Alarm
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! associated derived types
      use ESMF_TimeIntervalMod, only : ESMF_TimeInterval
      use ESMF_TimeMod,         only : ESMF_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
!     private
!------------------------------------------------------------------------------
!     ! ESMF_Alarm
!
!     ! F90 class to match C++ Alarm class in size and sequence

      type ESMF_Alarm
      sequence
      private
        type(ESMF_TimeInterval) :: RingInterval
        type(ESMF_Time)  :: RingTime
        type(ESMF_Time)  :: PrevRingTime
        type(ESMF_Time)  :: StopTime
        logical :: Ringing
        logical :: Enabled
        integer :: ID
        integer :: AlarmMutex
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Alarm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_AlarmInit
      public ESMF_AlarmEnable
      public ESMF_AlarmDisable
      public ESMF_AlarmTurnOn
      public ESMF_AlarmTurnOff
      public ESMF_AlarmIsRinging
      public ESMF_AlarmCheckRingTime
      public ESMF_AlarmGetRingInterval
      public ESMF_AlarmSetRingInterval
      public ESMF_AlarmGetRingTime
      public ESMF_AlarmSetRingTime
      public ESMF_AlarmGetPrevRingTime
      public ESMF_AlarmSetPrevRingTime
      public ESMF_AlarmGetStopTime
      public ESMF_AlarmSetStopTime
 
! Required inherited and overridden ESMF_Base class methods

      public ESMF_BaseValidate
      public ESMF_BasePrint

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_AlarmEQ
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Alarm.F90,v 1.5 2003/03/22 05:46:04 eschwab Exp $'

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
!     This interface overloads the == operator for the {\tt Alarm} class
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
! This section includes the Init methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmInit - Initializes an alarm

! !INTERFACE:
      subroutine ESMF_AlarmInit(alarm, RingInterval, RingTime, &
                                StopTime, Enabled, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_TimeInterval), intent(in), optional :: RingInterval
      type(ESMF_Time), intent(in), optional :: RingTime
      type(ESMF_Time), intent(in), optional :: StopTime
      logical, intent(in) :: Enabled
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to initialize
!     \item[{[RingInterval]}]
!          Optional ring interval for repeating alarms
!     \item[{[RingTime]}]
!          Optional ring time for one-shot or first repeating alarm
!     \item[{[StopTime]}]
!          Optional stop time for repeating alarms
!     \item[Enabled]
!          Alarm enabled/disabled
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.1, TMG4.7
!EOP
      call c_ESMC_AlarmInit(alarm, RingInterval, RingTime, &
                            StopTime, Enabled, rc)

      end subroutine ESMF_AlarmInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmEnable - Enables an alarm

! !INTERFACE:
      subroutine ESMF_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Enables an {\tt Alarm} to function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to enable
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP

      call c_ESMC_AlarmEnable(alarm, rc)

      end subroutine ESMF_AlarmEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmDisable - Disables an alarm

! !INTERFACE:
      subroutine ESMF_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Disables an {\tt Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to disable
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
    
      call c_ESMC_AlarmDisable(alarm, rc)

      end subroutine ESMF_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmTurnOn - Turn on an alarm


! !INTERFACE:
      subroutine ESMF_AlarmTurnOn(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn on an {\tt Alarm}; sets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn on
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.6
!EOP

      call c_ESMC_AlarmTurnOn(alarm, rc)

      end subroutine ESMF_AlarmTurnOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmTurnOff - Turn off an alarm

! !INTERFACE:
      subroutine ESMF_AlarmTurnOff(alarm, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an {\tt Alarm}; unsets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn off   
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.6
!EOP

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
      type(ESMF_Alarm), intent(inout) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt Alarm} is ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for ringing state  
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4
!EOP
    
      call c_ESMC_AlarmIsRinging(alarm, ESMF_AlarmIsRinging, rc)

      end function ESMF_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmCheckRingTime - Method used by a clock to check whether to trigger an alarm
!
! !INTERFACE:
      function ESMF_AlarmCheckRingTime(alarm, ClockCurrTime, positive, rc)
!
! !RETURN VALUE:
      logical :: ESMF_AlarmCheckRingTime
!
! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_Time), intent(in) :: ClockCurrTime
      integer, intent(in) :: positive
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Main method used by a {\tt Clock} to check whether to trigger
!     the {\tt Alarm} 
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check if time to ring   
!     \item[ClockCurrTime]
!          The {\tt Clock}'s current time
!     \item[positive]
!          Whether to check ring time in the positive or negative direction
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4, TMG4.6
!EOP

      call c_ESMC_AlarmCheckRingTime(alarm, ESMF_AlarmCheckRingTime, &
                                     ClockCurrTime, positive, rc)

      end function ESMF_AlarmCheckRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmGetRingInterval - Get an alarm's ring interval
!
! !INTERFACE:
      subroutine ESMF_AlarmGetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_TimeInterval), intent(out) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.7
!EOP
    
      call c_ESMC_AlarmGetRingInterval(alarm, RingInterval, rc)

      end subroutine ESMF_AlarmGetRingInterval
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSetRingInterval - Set an alarm's ring interval
!
! !INTERFACE:
      subroutine ESMF_AlarmSetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_TimeInterval), intent(in) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
    
      call c_ESMC_AlarmSetRingInterval(alarm, RingInterval, rc)

      end subroutine ESMF_AlarmSetRingInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetRingTime - Get an alarm's time to ring
!
! !INTERFACE:
      subroutine ESMF_AlarmGetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_Time), intent(out) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring time
!     \item[RingTime]
!          The {\tt Alarm}'s ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

      call c_ESMC_AlarmGetRingTime(alarm, RingTime, rc)

      end subroutine ESMF_AlarmGetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetRingTime - Set an alarm's time to ring
!
! !INTERFACE:
      subroutine ESMF_AlarmSetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_Time), intent(in) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring time
!     \item[RingTime]
!          The {\tt Alarm}'s ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.1, TMG4.7, TMG4.8
!EOP
   
      call c_ESMC_AlarmSetRingTime(alarm, RingTime, rc)

      end subroutine ESMF_AlarmSetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetPrevRingTime - Get an alarm's previous ring time
!
! !INTERFACE:
      subroutine ESMF_AlarmGetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_Time), intent(out) :: PrevRingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

      call c_ESMC_AlarmGetPrevRingTime(alarm, PrevRingTime, rc)

      end subroutine ESMF_AlarmGetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetPrevRingTime - Set an alarm's previous ring time
!
! !INTERFACE:
      subroutine ESMF_AlarmSetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_Time), intent(in) :: PrevRingTime
      integer, intent(out), optional :: rc
   
! !DESCRIPTION:
!     Set an {\tt Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the previous ring time
!     \item[PrevRingTime]
!          The {\tt Alarm}'s previous ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

      call c_ESMC_AlarmSetPrevRingTime(alarm, PrevRingTime, rc)

      end subroutine ESMF_AlarmSetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetStopTime - Get an alarm's stop time
!
! !INTERFACE:
      subroutine ESMF_AlarmGetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the stop time
!     \item[StopTime]
!          The {\tt Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
   
      call c_ESMC_AlarmGetStopTime(alarm, StopTime, rc)

      end subroutine ESMF_AlarmGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetStopTime - Set an alarm's stop time
!
! !INTERFACE:
      subroutine ESMF_AlarmSetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      type(ESMF_Time), intent(in) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the stop time
!     \item[StopTime]
!          The {\tt Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP

      call c_ESMC_AlarmSetStopTime(alarm, StopTime, rc)

      end subroutine ESMF_AlarmSetStopTime

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
!     Compare two alarms for equality; return true if equal, false otherwise
!     Maps to overloaded (==) operator interface function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt Alarm} to compare
!     \item[alarm2]
!          The first {\tt Alarm} to compare
!     \end{description}
!
! !REQUIREMENTS:  
!EOP

      call c_ESMC_AlarmEQ(alarm1, alarm2, ESMF_AlarmEQ)

      end function ESMF_AlarmEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Validate and Print methods inherited
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BaseValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine ESMF_BaseValidate(alarm, opt, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt Alarm}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          Alarm to validate
!     \item[{[opt]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      
      call c_ESMC_BaseValidate(alarm, opt, rc)
    
      end subroutine ESMF_BaseValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BasePrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine ESMF_BasePrint(alarm, opt, rc)

! !ARGUMENTS:
      type(ESMF_Alarm), intent(inout) :: alarm
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt Alarm}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          Alarm to print out
!     \item[{[opt]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      
      call c_ESMC_BasePrint(alarm, opt, rc)   

      end subroutine ESMF_BasePrint

!------------------------------------------------------------------------------

      end module ESMF_AlarmMod
