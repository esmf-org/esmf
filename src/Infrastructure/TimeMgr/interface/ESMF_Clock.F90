! $Id: ESMF_Clock.F90,v 1.8 2003/03/28 01:29:03 eschwab Exp $
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
!     ESMF Clock Module
      module ESMF_ClockMod
!     
!==============================================================================
!     
! This file contains the Clock class definition and all Clock class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF_TimeMgr.inc> 

!==============================================================================
!BOP
! !MODULE: ESMF_ClockMod
!     
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class ESMC\_Time implementation
!     
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! associated derived types
      use ESMF_TimeIntervalMod, only : ESMF_TimeInterval
      use ESMF_TimeMod,         only : ESMF_Time
      use ESMF_AlarmMod,        only : ESMF_Alarm

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Clock
!     
!     ! F90 class to match C++ Clock class in size and sequence

      type ESMF_Clock
      sequence
      private
        type(ESMF_TimeInterval) :: TimeStep
        type(ESMF_Time)  :: StartTime
        type(ESMF_Time)  :: StopTime
        type(ESMF_Time)  :: RefTime
        type(ESMF_Time)  :: CurrTime
        type(ESMF_Time)  :: PrevTime
        integer(ESMF_IKIND_I8) :: AdvanceCount
        type(ESMF_Alarm), dimension(MAX_ALARMS) :: AlarmList
        integer :: NumAlarms
!        integer :: ClockMutex
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Clock
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_ClockInit
      public ESMF_ClockAddAlarm
      public ESMF_ClockGetAlarmList
      public ESMF_ClockGetNumAlarms
      public ESMF_ClockSyncToWallClock
      public ESMF_ClockAdvance
      public ESMF_ClockIsStopTime
      public ESMF_ClockGetAdvanceCount
      public ESMF_ClockGetTimeInterval
      public ESMF_ClockSetTimeInterval
      public ESMF_ClockGetCurrTime
      public ESMF_ClockSetCurrTime
      public ESMF_ClockGetStartTime
      public ESMF_ClockGetStopTime
      public ESMF_ClockGetRefTime
      public ESMF_ClockGetPrevTime
      public ESMF_ClockGetCurrSimTime
      public ESMF_ClockGetPrevSimTime

! Required inherited and overridden ESMF_Base class methods

      public ESMF_BaseValidate
      public ESMF_BasePrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Clock.F90,v 1.8 2003/03/28 01:29:03 eschwab Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Init methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockInit - Initialize a clock

! !INTERFACE:
      subroutine ESMF_ClockInit(clock, TimeStep, StartTime, StopTime, &
                                RefTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_TimeInterval), intent(in), optional :: TimeStep
      type(ESMF_Time), intent(in) :: StartTime
      type(ESMF_Time), intent(in) :: StopTime
      type(ESMF_Time), intent(in), optional :: RefTime
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Initialize a {\tt Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt Clock}'s time step interval
!     \item[StartTime]
!          The {\tt Clock}'s starting time
!     \item[StopTime]
!          The {\tt Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP

      call c_ESMC_ClockInit(clock, TimeStep, StartTime, StopTime, &
                            RefTime, rc)
    
      end subroutine ESMF_ClockInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAddAlarm - Add an alarm to a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockAddAlarm(clock, Alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Alarm), intent(in) :: Alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Add an {\tt Alarm} to a {\tt Clock}'s {\tt Alarm} list
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to add an {\tt Alarm} to
!     \item[Alarm]
!          The {\tt Alarm} to add to the {\tt Clock}'s {\tt Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.1, TMG4.2
!EOP
    
!      call c_ESMC_ClockAddAlarm(clock, Alarm, rc)
    
      end subroutine ESMF_ClockAddAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAlarmList - Get a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetAlarmList(clock, AlarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Alarm), intent(out) :: AlarmList(:)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s {\tt Alarm} list     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt Alarm} list from
!     \item[AlarmList]
!          The {\tt Clock}'s {\tt Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.3
!EOP

!      call c_ESMC_ClockGetAlarmList(clock, AlarmList, rc)
    
      end subroutine ESMF_ClockGetAlarmList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetNumAlarms - Get the number of alarms in a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetNumAlarms(clock, NumAlarms, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: NumAlarms
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the number of {\tt Alarm}s in a {\tt Clock}'s {\tt Alarm} list     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the number of {\tt Alarm}s from
!     \item[NumAlarms]
!          The number of {\tt Alarm}s in the {\tt Clock}'s {\tt Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.3
!EOP

!      call c_ESMC_ClockGetNumAlarms(clock, NumAlarms, rc)
    
      end subroutine ESMF_ClockGetNumAlarms

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSyncToWallClock - Set clock's current time to wall clock time

! !INTERFACE:
      subroutine ESMF_ClockSyncToWallClock(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Set a {\tt Clock}'s current time to wall clock time     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to synchronize to wall clock time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG3.4.5
!EOP

!      call c_ESMC_ClockSyncToWallClock(clock, rc)
    
      end subroutine ESMF_ClockSyncToWallClock

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAdvance - Advance a clock's current time by one time step

! !INTERFACE:
      subroutine ESMF_ClockAdvance(clock, RingingAlarmList, &
                                   NumRingingAlarms, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(out), optional :: &
                                        RingingAlarmList
      integer, intent(out), optional :: NumRingingAlarms
      integer, intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Advance a {\tt Clock}'s current time by one time step
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance
!     \item[{[RingingAlarmList]}]
!          Return a list of any ringing alarms after the time step
!     \item[{[NumRingingAlarms]}]
!          The number of ringing alarms returned
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
! !REQUIREMENTS:
!     TMG3.4.1
!EOP

      call c_ESMC_ClockAdvance(clock, RingingAlarmList, NumRingingAlarms, rc)
    
      end subroutine ESMF_ClockAdvance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockIsStopTime - Has the clock reached its stop time ?

! !INTERFACE:
      function ESMF_ClockIsStopTime(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTime

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return true if {\tt Clock} has reached its stop time, false otherwise     
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.6
!EOP

      call c_ESMC_ClockIsStopTime(clock, ESMF_ClockIsStopTime, rc)
    
      end function ESMF_ClockIsStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAdvanceCount - Get the clock's advance count

! !INTERFACE:
      subroutine ESMF_ClockGetAdvanceCount(clock, AdvanceCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      integer(ESMF_IKIND_I8), intent(out) :: AdvanceCount
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns the number of times the {\tt Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[AdvanceCount]
!          The number of times the {\tt Clock} has been advanced
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP

      call c_ESMC_ClockGetAdvanceCount(clock, AdvanceCount, rc)
    
      end subroutine ESMF_ClockGetAdvanceCount

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetTimeInterval - Get a clock's time interval (timestep)

! !INTERFACE:
      subroutine ESMF_ClockGetTimeInterval(clock, TimeInterval, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_TimeInterval), intent(out) :: TimeInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s time interval (time step)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the time step from
!     \item[TimeInterval]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.2
!EOP

!      call c_ESMC_ClockGetTimeInterval(clock, TimeInterval, rc)
    
      end subroutine ESMF_ClockGetTimeInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetTimeInterval - Set a clock's time interval (timestep)

! !INTERFACE:
      subroutine ESMF_ClockSetTimeInterval(clock, TimeInterval, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_TimeInterval), intent(in) :: TimeInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set a {\tt Clock}'s time interval (time step)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the time step
!     \item[TimeInterval]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.2
!EOP

!      call c_ESMC_ClockSetTimeInterval(clock, TimeInterval, rc)

      end subroutine ESMF_ClockSetTimeInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrTime - Get a clock's current time

! !INTERFACE:
      subroutine ESMF_ClockGetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(out) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s current time     
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

!      call c_ESMC_ClockGetCurrTime(clock, CurrTime, rc)
    
      end subroutine ESMF_ClockGetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetCurrTime - Set a clock's current time

! !INTERFACE:
      subroutine ESMF_ClockSetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(in) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set a {\tt Clock}'s current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.3
!EOP

!      call c_ESMC_ClockSetCurrTime(clock, CurrTime, rc)
    
      end subroutine ESMF_ClockSetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStartTime - Get a clock's start time

! !INTERFACE:
      subroutine ESMF_ClockGetStartTime(clock, StartTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(out) :: StartTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s start time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the start time from
!     \item[StartTime]
!          The start time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

!      call c_ESMC_ClockGetStartTime(clock, StartTime, rc)
    
      end subroutine ESMF_ClockGetStartTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStopTime - Get a clock's stop time

! !INTERFACE:
      subroutine ESMF_ClockGetStopTime(clock, StopTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s stop time
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the stop time from
!     \item[StopTime]
!          The stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

!      call c_ESMC_ClockGetStopTime(clock, StopTime, rc)
    
      end subroutine ESMF_ClockGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetRefTime - Get a clock's reference time

! !INTERFACE:
      subroutine ESMF_ClockGetRefTime(clock, RefTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(out) :: RefTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s reference time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the reference time from
!     \item[RefTime]
!          The reference time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

!      call c_ESMC_ClockGetRefTime(clock, RefTime, rc)
    
      end subroutine ESMF_ClockGetRefTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetPrevTime - Get a clock's previous current time

! !INTERFACE:
      subroutine ESMF_ClockGetPrevTime(clock, PrevTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(out) :: PrevTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s previous current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous current time from
!     \item[PrevTime]
!          The previous current time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

!      call c_ESMC_ClockGetPrevTime(clock, PrevTime, rc)
    
      end subroutine ESMF_ClockGetPrevTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrSimTime - Get a clock's current simulation time

! !INTERFACE:
      subroutine ESMF_ClockGetCurrSimTime(clock, CurrSimTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(out) :: CurrSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s current simulation time
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current simulation time from
!     \item[CurrSimTime]
!          The current simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP

!      call c_ESMC_ClockGetCurrSimTime(clock, CurrSimTime, rc)
    
      end subroutine ESMF_ClockGetCurrSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetPrevSimTime - Get a clock's previous simulation time

! !INTERFACE:
      subroutine ESMF_ClockGetPrevSimTime(clock, PrevSimTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(out) :: PrevSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt Clock}'s previous simulation time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous simulation time from
!     \item[PrevSimTime]
!          The previous simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP

!      call c_ESMC_ClockGetPrevSimTime(clock, PrevSimTime, rc)
   
      end subroutine ESMF_ClockGetPrevSimTime

!------------------------------------------------------------------------------
!
! This section defines the overridden Validate and Print methods inherited
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BaseValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine ESMF_BaseValidate(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt Clock}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[clock]
!          Clock to validate
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      
!      call c_ESMC_BaseValidate(clock, rc)
    
      end subroutine ESMF_BaseValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BasePrint - Print out a Clock's properties

! !INTERFACE:
      subroutine ESMF_BasePrint(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt Clock}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          Clock to print out
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      
!      call c_ESMC_BasePrint(clock, rc)   

      end subroutine ESMF_BasePrint

!------------------------------------------------------------------------------

      end module ESMF_ClockMod
