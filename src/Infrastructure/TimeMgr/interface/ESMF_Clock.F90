! $Id: ESMF_Clock.F90,v 1.29 2003/09/04 18:57:56 cdeluca Exp $
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
        type(ESMF_Time)         :: startTime
        type(ESMF_Time)         :: stopTime
        type(ESMF_Time)         :: refTime
        type(ESMF_Time)         :: currTime
        type(ESMF_Time)         :: prevTime
        integer(ESMF_KIND_I8)  :: advanceCount
        integer                 :: numAlarms
        integer                 :: pad  ! to satisfy alpha compiler
        type(ESMF_Pointer), dimension(MAX_ALARMS) :: alarmList
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Clock
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_ClockSetup
      public ESMF_ClockSet
      public ESMF_ClockGet

      public ESMF_ClockAddAlarm
      public ESMF_ClockGetAlarm
      public ESMF_ClockGetRingingAlarm

      public ESMF_ClockAdvance
      public ESMF_ClockIsStopTime

      public ESMF_ClockSyncToRealTime

! Required inherited and overridden ESMF_Base class methods

      public ESMF_ClockReadRestart
      public ESMF_ClockWriteRestart
      public ESMF_ClockValidate
      public ESMF_ClockPrint
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Clock.F90,v 1.29 2003/09/04 18:57:56 cdeluca Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set/Get methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetup - Setup a Clock

! !INTERFACE:
      subroutine ESMF_ClockSetup(clock, timeStep, startTime, stopTime, &
                                 refTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(inout)         :: clock
      type(ESMF_TimeInterval), intent(in)            :: timeStep
      type(ESMF_Time),         intent(in)            :: startTime
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_Time),         intent(in),  optional :: refTime
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Sets the initial values in an {\tt ESMF\_Clock}.    
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize.
!     \item[timeStep]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[startTime]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[{[stopTime]}]
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

!     invoke C to C++ entry point
      call c_ESMC_ClockSetup(clock, timeStep, startTime, stopTime, &
                             refTime, rc)
    
      end subroutine ESMF_ClockSetup

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSet - Set one or more properties of a Clock

! !INTERFACE:
      subroutine ESMF_ClockSet(clock, timeStep, startTime, stopTime, &
                               refTime, currTime, advanceCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(inout)         :: clock
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      type(ESMF_Time),         intent(in),  optional :: startTime
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_Time),         intent(in),  optional :: refTime
      type(ESMF_Time),         intent(in),  optional :: currTime
      integer(ESMF_KIND_I8),  intent(in),  optional :: advanceCount
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Sets one or more of the properties of an {\tt ESMF\_Clock}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize.
!     \item[{[timeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[{[startTime]}]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[{[stopTime]}]
!          The {\tt ESMF\_Clock}'s stopping time.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.
!     \item[{[currTime]}]
!          The current time.
!     \item[{[advanceCount]}]
!          The number of times the clock has been timestepped.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4

!     invoke C to C++ entry point
      call c_ESMC_ClockSet(clock, timeStep, startTime, stopTime, &
                           refTime, currTime, advanceCount, rc)
    
      end subroutine ESMF_ClockSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGet - Get a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockGet(clock, timeStep, startTime, stopTime, &
                               refTime, currTime, prevTime, currSimTime, &
                               prevSimTime, advanceCount, numAlarms, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)            :: clock
      type(ESMF_TimeInterval), intent(out), optional :: timeStep
      type(ESMF_Time),         intent(out), optional :: startTime
      type(ESMF_Time),         intent(out), optional :: stopTime
      type(ESMF_Time),         intent(out), optional :: refTime
      type(ESMF_Time),         intent(out), optional :: currTime
      type(ESMF_Time),         intent(out), optional :: prevTime
      type(ESMF_TimeInterval), intent(out), optional :: currSimTime
      type(ESMF_TimeInterval), intent(out), optional :: prevSimTime
      integer(ESMF_KIND_I8),  intent(out), optional :: advanceCount
      integer,                 intent(out), optional :: numAlarms
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Gets the properties of a {\tt ESMF\_Clock}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize.
!     \item[{[timeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[{[startTime]}]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[{[stopTime]}]
!          The {\tt ESMF\_Clock}'s stopping time.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.
!     \item[{[currTime]}]
!          The current time.
!     \item[{[prevTime]}]
!          The previous time.
!     \item[currSimTime]
!          The current simulation time.
!     \item[prevSimTime]
!          The previous simulation time.
!     \item[{[advanceCount]}]
!          The number of times the {\tt ESMF\_Clock} has been advanced.
!     \item[{[numAlarms]}]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4

!     invoke C to C++ entry point
      call c_ESMC_ClockGet(clock, timeStep, startTime, stopTime, &
                           refTime, currTime, prevTime, currSimTime, &
                           prevSimTime, advanceCount, numAlarms, rc)
    
      end subroutine ESMF_ClockGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAddAlarm - Add an Alarm to a Clock's Alarm list

! !INTERFACE:
      subroutine ESMF_ClockAddAlarm(clock, alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      type(ESMF_Alarm), intent(in)            :: alarm
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Adds an {\tt alarm} to the {\tt clock}'s {\tt ESMF\_Alarm} list.
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
    
!     invoke C to C++ entry point
      call c_ESMC_ClockAddAlarm(clock, alarm, rc)
    
      end subroutine ESMF_ClockAddAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAlarm - Get an Alarm in a Clock's Alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetAlarm(clock, i, alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock),   intent(in)            :: clock
      integer,            intent(in)            :: i
      type(ESMF_Pointer), intent(out)           :: alarm
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the "i" th {\tt alarm} in the {\tt clock}'s
!     {\tt ESMF\_Alarm} list.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt ESMF\_Alarm} from.
!     \item[i]
!          The index into the {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list.
!     \item[alarm]
!          Pointer to the desired alarm.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMG4.3

!     invoke C to C++ entry point
      call c_ESMC_ClockGetAlarm(clock, i, alarm, rc)
    
      end subroutine ESMF_ClockGetAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetRingingAlarm - Get a ringing Alarm in a Clock's Alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetRingingAlarm(clock, i, alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock),   intent(in)            :: clock
      integer,            intent(in)            :: i
      type(ESMF_Pointer), intent(out)           :: alarm
      integer,            intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the "i" th ringing {\tt alarm} in the {\tt clock}'s
!     {\tt ESMF\_Alarm} list.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the ringing {\tt ESMF\_Alarm} from.
!     \item[i]
!          Get the "i"th ringing {\tt ESMF\_Alarm}.
!     \item[alarm]
!          Pointer to the desired alarm.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMG4.3

!     invoke C to C++ entry point
      call c_ESMC_ClockGetRingingAlarm(clock, i, alarm, rc)
    
      end subroutine ESMF_ClockGetRingingAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAdvance - Advance a Clock's current time by one time step

! !INTERFACE:
      subroutine ESMF_ClockAdvance(clock, timeStep, numRingingAlarms, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(inout)         :: clock
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      integer,                 intent(out), optional :: numRingingAlarms
      integer,                 intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Advances the {\tt clock}'s current time by one time step.  This
!     method optionally returns the number of ringing {\tt ESMF\_Alarm}s.
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance.
!     \item[{[timeStep]}]
!          Optional:  Saved as clock's new time step, then the advance is
!          performed.  Supports applications with variable time steps.
!     \item[{[numRingingAlarms]}]
!          The number of alarms ringing after the time step.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
!EOP
! !REQUIREMENTS:
!     TMG3.4.1

!     invoke C to C++ entry point
      call c_ESMC_ClockAdvance(clock, timeStep, numRingingAlarms, rc)
    
      end subroutine ESMF_ClockAdvance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockIsStopTime - Has the Clock reached its stop time?

! !INTERFACE:
      function ESMF_ClockIsStopTime(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTime

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if the {\tt clock} has reached its stop time, and
!     false otherwise.
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

!     invoke C to C++ entry point
      call c_ESMC_ClockIsStopTime(clock, ESMF_ClockIsStopTime, rc)
    
      end function ESMF_ClockIsStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSyncToRealTime - Set Clock's current time to wall clock time

! !INTERFACE:
      subroutine ESMF_ClockSyncToRealTime(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      integer,          intent(out), optional :: rc
    
! !DESCRIPTION:
!     Sets a {\tt clock}'s current time to the wall clock time.
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

!     invoke C to C++ entry point
      call c_ESMC_ClockSyncToRealTime(clock, rc)
    
      end subroutine ESMF_ClockSyncToRealTime

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockReadRestart - Restore the contents of a Clock

! !INTERFACE:
      subroutine ESMF_ClockReadRestart(clock, timeStep, startTime, stopTime, &
                                      refTime, currTime, prevTime, &
                                      advanceCount, numAlarms, alarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(out)                :: clock
      type(ESMF_TimeInterval), intent(in)                 :: timeStep
      type(ESMF_Time),         intent(in)                 :: startTime
      type(ESMF_Time),         intent(in)                 :: stopTime
      type(ESMF_Time),         intent(in)                 :: refTime
      type(ESMF_Time),         intent(in)                 :: currTime
      type(ESMF_Time),         intent(in)                 :: prevTime
      integer(ESMF_KIND_I8),  intent(in)                 :: advanceCount
      integer,                 intent(in)                 :: numAlarms
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(in) :: alarmList
      integer,                 intent(out), optional      :: rc
    
! !DESCRIPTION:
!     Restores the contents of an {\tt ESMF\_Clock} for restart.
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
!     \item[numAlarms]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list.
!     \item[alarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_ClockReadRestart(clock, timeStep, startTime, stopTime, &
                                   refTime, currTime, prevTime, advanceCount, &
                                   numAlarms, alarmList, rc)
    
      end subroutine ESMF_ClockReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockWriteRestart - Save the contents of a Clock 

! !INTERFACE:
      subroutine ESMF_ClockWriteRestart(clock, timeStep, startTime, stopTime, &
                                        refTime, currTime, prevTime, &
                                        advanceCount, numAlarms, alarmList, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)                  :: clock
      type(ESMF_TimeInterval), intent(out)                 :: timeStep
      type(ESMF_Time),         intent(out)                 :: startTime
      type(ESMF_Time),         intent(out)                 :: stopTime
      type(ESMF_Time),         intent(out)                 :: refTime
      type(ESMF_Time),         intent(out)                 :: currTime
      type(ESMF_Time),         intent(out)                 :: prevTime
      integer(ESMF_KIND_I8),  intent(out)                 :: advanceCount
      integer,                 intent(out)                 :: numAlarms
      type(ESMF_Alarm), dimension(MAX_ALARMS), intent(out) :: alarmList
      integer,                 intent(out), optional       :: rc

! !DESCRIPTION:
!     Saves the contents of an {\tt ESMF\_Clock} for restart.
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
!     \item[numAlarms]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list.
!     \item[alarmList]
!          The {\tt ESMF\_Clock}'s {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_ClockWriteRestart(clock, timeStep, startTime, stopTime, &
                                    refTime, currTime, prevTime, advanceCount, &
                                    numAlarms, alarmList, rc)
    
      end subroutine ESMF_ClockWriteRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ClockValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockValidate(clock, options, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Check whether a {\tt clock} is valid.
!
!     The arguments are:  
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to validate.
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
      call c_ESMC_ClockValidate(clock, options, rc)
    
      end subroutine ESMF_ClockValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ClockPrint - Print the contents of a Clock

! !INTERFACE:
      subroutine ESMF_ClockPrint(clock, options, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing and debugging, this method prints out 
!     the contents of an {\tt ESMF\_Clock}.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to print out.
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
      call c_ESMC_ClockPrint(clock, options, rc)   

      end subroutine ESMF_ClockPrint

!------------------------------------------------------------------------------

      end module ESMF_ClockMod
