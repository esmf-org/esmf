! $Id: ESMF_Clock.F90,v 1.25 2003/08/08 00:25:49 eschwab Exp $
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
!BOPI
! !MODULE: ESMF_ClockMod
!     
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation.
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation.
!     
! See {\tt ../include/ESMC\_Clock.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! associated derived types
      use ESMF_TimeIntervalMod
      use ESMF_TimeMod
      use ESMF_AlarmMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Clock
!     
!     ! F90 class type to match C++ Clock class in size only;
!     !  all dereferencing within class is performed by C++ implementation

!     ! Equivalent sequence and kind to C++:

      type ESMF_Clock
      sequence
      private
        type(ESMF_TimeInterval) :: timeStep
        type(ESMF_Time)  :: startTime
        type(ESMF_Time)  :: stopTime
        type(ESMF_Time)  :: refTime
        type(ESMF_Time)  :: currTime
        type(ESMF_Time)  :: prevTime
        integer(ESMF_IKIND_I8) :: advanceCount
        integer :: clockMutex
        integer :: numAlarms
        type(ESMF_Alarm), dimension(MAX_ALARMS) :: alarmList
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Clock
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_ClockSet
      public ESMF_ClockGetAdvanceCount
      public ESMF_ClockGetTimeStep
      public ESMF_ClockSetTimeStep
      public ESMF_ClockGetCurrTime
      public ESMF_ClockSetCurrTime
      public ESMF_ClockGetStartTime
      public ESMF_ClockGetStopTime
      public ESMF_ClockGetRefTime
      public ESMF_ClockGetPrevTime
      public ESMF_ClockGetCurrSimTime
      public ESMF_ClockGetPrevSimTime
      public ESMF_ClockAddAlarm
      public ESMF_ClockGetAlarmList
      public ESMF_ClockGetNumAlarms
      public ESMF_ClockSyncToRealTime
      public ESMF_ClockAdvance
      public ESMF_ClockIsStopTime

! Required inherited and overridden ESMF_Base class methods

      public ESMF_ClockReadRestart
      public ESMF_ClockWriteRestart
      public ESMF_ClockValidate
      public ESMF_ClockPrint
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Clock.F90,v 1.25 2003/08/08 00:25:49 eschwab Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSet - Initialize a clock

! !INTERFACE:
      subroutine ESMF_ClockSet(clock, timeStep, startTime, stopTime, &
                               refTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_TimeInterval), intent(in), optional :: timeStep
      type(ESMF_Time), intent(in) :: startTime
      type(ESMF_Time), intent(in) :: stopTime
      type(ESMF_Time), intent(in), optional :: refTime
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Initialize an {\tt ESMF\_Clock}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize.
!     \item[{[timeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[startTime]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[stopTime]
!          The {\tt ESMF\_Clock}'s stopping time.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
      call c_ESMC_ClockSet(clock, timeStep, startTime, stopTime, &
                           refTime, rc)
    
      end subroutine ESMF_ClockSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAdvanceCount - Get the clock's advance count

! !INTERFACE:
      subroutine ESMF_ClockGetAdvanceCount(clock, advanceCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      integer(ESMF_IKIND_I8), intent(out) :: advanceCount
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns the number of times the {\tt ESMF\_Clock} has been advanced
!     (time stepped).
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from.
!     \item[advanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMG3.5.1

      call c_ESMC_ClockGetAdvanceCount(clock, advanceCount, rc)
    
      end subroutine ESMF_ClockGetAdvanceCount

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetTimeStep - Get a clock's timestep interval

! !INTERFACE:
      subroutine ESMF_ClockGetTimeStep(clock, timeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: timeStep
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s timestep interval.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the time step from.
!     \item[timeStep]
!          The time step.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.5.2

      call c_ESMC_ClockGetTimeStep(clock, timeStep, rc)
    
      end subroutine ESMF_ClockGetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetTimeStep - Set a clock's timestep interval

! !INTERFACE:
      subroutine ESMF_ClockSetTimeStep(clock, timeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_TimeInterval), intent(in) :: timeStep
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s timestep interval.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the time step.
!     \item[timeStep]
!          The time step.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.4.2

      call c_ESMC_ClockSetTimeStep(clock, timeStep, rc)

      end subroutine ESMF_ClockSetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrTime - Get a clock's current time

! !INTERFACE:
      subroutine ESMF_ClockGetCurrTime(clock, currTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: currTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s current time.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current time from.
!     \item[currTime]
!          The current time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.5.4

      call c_ESMC_ClockGetCurrTime(clock, currTime, rc)
    
      end subroutine ESMF_ClockGetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetCurrTime - Set a clock's current time

! !INTERFACE:
      subroutine ESMF_ClockSetCurrTime(clock, currTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(in) :: currTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s current time.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the current time from.
!     \item[currTime]
!          The current time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.4.3

      call c_ESMC_ClockSetCurrTime(clock, currTime, rc)
    
      end subroutine ESMF_ClockSetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStartTime - Get a clock's start time

! !INTERFACE:
      subroutine ESMF_ClockGetStartTime(clock, startTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: startTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s start time.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the start time from.
!     \item[startTime]
!          The start time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.5.3

      call c_ESMC_ClockGetStartTime(clock, startTime, rc)
    
      end subroutine ESMF_ClockGetStartTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStopTime - Get a clock's stop time

! !INTERFACE:
      subroutine ESMF_ClockGetStopTime(clock, stopTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: stopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s stop time.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the stop time from.
!     \item[stopTime]
!          The stop time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.5.3

      call c_ESMC_ClockGetStopTime(clock, stopTime, rc)
    
      end subroutine ESMF_ClockGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetRefTime - Get a clock's reference time

! !INTERFACE:
      subroutine ESMF_ClockGetRefTime(clock, refTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: refTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s reference time.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the reference time from.
!     \item[refTime]
!          The reference time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.5.3

      call c_ESMC_ClockGetRefTime(clock, refTime, rc)
    
      end subroutine ESMF_ClockGetRefTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetPrevTime - Get a clock's previous current time

! !INTERFACE:
      subroutine ESMF_ClockGetPrevTime(clock, prevTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Time), intent(out) :: prevTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s previous current time.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous current time from.
!     \item[prevTime]
!          The previous current time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.5.4

      call c_ESMC_ClockGetPrevTime(clock, prevTime, rc)
    
      end subroutine ESMF_ClockGetPrevTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrSimTime - Get a clock's current simulation time

! !INTERFACE:
      subroutine ESMF_ClockGetCurrSimTime(clock, currSimTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: currSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s current simulation time.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current simulation time from.
!     \item[currSimTime]
!          The current simulation time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.5.5

      call c_ESMC_ClockGetCurrSimTime(clock, currSimTime, rc)
    
      end subroutine ESMF_ClockGetCurrSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetPrevSimTime - Get a clock's previous simulation time

! !INTERFACE:
      subroutine ESMF_ClockGetPrevSimTime(clock, prevSimTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: prevSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s previous simulation time.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous simulation time from.
!     \item[prevSimTime]
!          The previous simulation time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMG3.5.5

      call c_ESMC_ClockGetPrevSimTime(clock, prevSimTime, rc)
   
      end subroutine ESMF_ClockGetPrevSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAddAlarm - Add an alarm to a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockAddAlarm(clock, alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Alarm), intent(in) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Add an {\tt ESMF\_Alarm} to an {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to add an {\tt ESMF\_Alarm} to.
!     \item[alarm]
!          The {\tt ESMF\_Alarm} to add to the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMG4.1, TMG4.2
    
      call c_ESMC_ClockAddAlarm(clock, alarm, rc)
    
      end subroutine ESMF_ClockAddAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAlarmList - Get a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetAlarmList(clock, alarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_Alarm), intent(out) :: alarmList(:)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt ESMF\_Alarm} list from.
!     \item[alarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMG4.3

      call c_ESMC_ClockGetAlarmList(clock, alarmList, rc)
    
      end subroutine ESMF_ClockGetAlarmList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetNumAlarms - Get the number of alarms in a clock's alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetNumAlarms(clock, numAlarms, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out) :: numAlarms
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the number of {\tt ESMF\_Alarm}s in an {\tt ESMF\_Clock}'s
!     {\tt ESMF\_Alarm} list.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the number of {\tt ESMF\_Alarm}s from.
!     \item[numAlarms]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMG4.3

      call c_ESMC_ClockGetNumAlarms(clock, numAlarms, rc)
    
      end subroutine ESMF_ClockGetNumAlarms

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSyncToRealTime - Set clock's current time to wall clock time

! !INTERFACE:
      subroutine ESMF_ClockSyncToRealTime(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Set an {\tt ESMF\_Clock}'s current time to wall clock time.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to synchronize to wall clock time.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMG3.4.5

      call c_ESMC_ClockSyncToRealTime(clock, rc)
    
      end subroutine ESMF_ClockSyncToRealTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAdvance - Advance a clock's current time by one time step

! !INTERFACE:
      subroutine ESMF_ClockAdvance(clock, ringingAlarmList, &
                                   numRingingAlarms, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(out), optional :: &
                                        ringingAlarmList
      integer, intent(out), optional :: numRingingAlarms
      integer, intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Advance an {\tt ESMF\_Clock}'s current time by one time step.
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance.
!     \item[{[ringingAlarmList]}]
!          Return a list of any ringing alarms after the time step.
!     \item[{[numRingingAlarms]}]
!          The number of ringing alarms returned.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
!EOP
! !REQUIREMENTS:
!     TMG3.4.1

      call c_ESMC_ClockAdvance(clock, ringingAlarmList, numRingingAlarms, rc)
    
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
      type(ESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return true if {\tt ESMF\_Clock} has reached its stop time, false 
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
!     TMG3.5.6

      call c_ESMC_ClockIsStopTime(clock, ESMF_ClockIsStopTime, rc)
    
      end function ESMF_ClockIsStopTime

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockReadRestart - Restores a clock

! !INTERFACE:
      subroutine ESMF_ClockReadRestart(clock, timeStep, startTime, stopTime, &
                                      refTime, currTime, prevTime, &
                                      advanceCount, alarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(out) :: clock
      type(ESMF_TimeInterval), intent(in) :: timeStep
      type(ESMF_Time), intent(in) :: startTime
      type(ESMF_Time), intent(in) :: stopTime
      type(ESMF_Time), intent(in) :: refTime
      type(ESMF_Time), intent(in) :: currTime
      type(ESMF_Time), intent(in) :: prevTime
      integer(ESMF_IKIND_I8), intent(in) :: advanceCount
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(in) :: alarmList
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Restore an {\tt ESMF\_Clock}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to restore.
!     \item[timeStep]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[startTime]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[stopTime]
!          The {\tt ESMF\_Clock}'s stopping time.
!     \item[refTime]
!          The {\tt ESMF\_Clock}'s reference time.
!     \item[currTime]
!          The {\tt ESMF\_Clock}'s current time.
!     \item[PrevTime]
!          The {\tt ESMF\_Clock}'s previous time.
!     \item[advanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced.
!     \item[alarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:

      call c_ESMC_ClockReadRestart(clock, timeStep, startTime, stopTime, &
                                   refTime, currTime, prevTime, advanceCount, &
                                   alarmList, rc)
    
      end subroutine ESMF_ClockReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockWriteRestart - Saves a clock

! !INTERFACE:
      subroutine ESMF_ClockWriteRestart(clock, timeStep, startTime, stopTime, &
                                        refTime, currTime, prevTime, &
                                        advanceCount, alarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      type(ESMF_TimeInterval), intent(out) :: timeStep
      type(ESMF_Time), intent(out) :: startTime
      type(ESMF_Time), intent(out) :: stopTime
      type(ESMF_Time), intent(out) :: refTime
      type(ESMF_Time), intent(out) :: currTime
      type(ESMF_Time), intent(out) :: prevTime
      integer(ESMF_IKIND_I8), intent(out) :: advanceCount
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(out) :: alarmList
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Save an {\tt ESMF\_Clock}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to save.
!     \item[timeStep]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[startTime]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[stopTime]
!          The {\tt ESMF\_Clock}'s stopping time.
!     \item[refTime]
!          The {\tt ESMF\_Clock}'s reference time.
!     \item[currTime]
!          The {\tt ESMF\_Clock}'s current time.
!     \item[prevTime]
!          The {\tt ESMF\_Clock}'s previous time.
!     \item[advanceCount]
!          The number of times the {\tt ESMF\_Clock} has been advanced.
!     \item[alarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:

      call c_ESMC_ClockWriteRestart(clock, timeStep, startTime, stopTime, &
                                    refTime, currTime, prevTime, advanceCount, &
                                    alarmList, rc)
    
      end subroutine ESMF_ClockWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ClockValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockValidate(clock, opts, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on an {\tt ESMF\_Clock}'s properties.
!
!     The arguments are:  
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to validate.
!     \item[{[opts]}]
!          Validate options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
    
      call c_ESMC_ClockValidate(clock, opts, rc)
    
      end subroutine ESMF_ClockValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ClockPrint - Print out a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockPrint(clock, opts, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out an {\tt ESMF\_Clock}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to print out.
!     \item[{[opts]}]
!          Print options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:
!     TMGn.n.n
      
      call c_ESMC_ClockPrint(clock, opts, rc)   

      end subroutine ESMF_ClockPrint

!------------------------------------------------------------------------------

      end module ESMF_ClockMod
