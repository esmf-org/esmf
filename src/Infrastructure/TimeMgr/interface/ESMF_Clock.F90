! $Id: ESMF_Clock.F90,v 1.61 2005/02/07 23:35:58 eschwab Exp $
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
! Part of Time Manager Fortran API wrapper of C++ implementation.
!
! Defines Fortran wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation.
!     
! See {\tt ../include/ESMC\_Clock.h} for complete description.
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseTypesMod
      use ESMF_BaseMod

      ! for ReadRestart()/WriteRestart()
      use ESMF_IOSpecMod

      ! associated derived types
      use ESMF_CalendarMod
      use ESMF_TimeIntervalMod
      use ESMF_TimeMod
      use ESMF_AlarmTypeMod

      ! type definition for this module
      use ESMF_ClockTypeMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Clock definition in ESMF_ClockTypeMod to resolve mutual
!     ! method dependency with ESMF_Alarm

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!     This type is defined in ESMF_ClockTypeMod and progagated up from here.
      public ESMF_Clock
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public operator(==)
      public operator(/=)
      public ESMF_ClockAdvance
      public ESMF_ClockCreate
      public ESMF_ClockDestroy 
      public ESMF_ClockGet     
      public ESMF_ClockGetAlarm
      public ESMF_ClockGetAlarmList
      public ESMF_ClockGetNextTime
      public ESMF_ClockIsStopTime
      public ESMF_ClockIsStopTimeEnabled
      public ESMF_ClockPrint
      public ESMF_ClockReadRestart
      public ESMF_ClockSet
      public ESMF_ClockStopTimeDisable     
      public ESMF_ClockStopTimeEnable     
      public ESMF_ClockSyncToRealTime
      public ESMF_ClockValidate
      public ESMF_ClockWriteRestart
!EOPI

! !PRIVATE MEMBER FUNCTIONS:
      private ESMF_ClockEQ
      private ESMF_ClockNE
      private ESMF_ClockCreateNew
      private ESMF_ClockCreateCopy

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Clock.F90,v 1.61 2005/02/07 23:35:58 eschwab Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !IROUTINE:  ESMF_ClockOperator(==) - Test if Clock 1 is equal to Clock 2
!
! !INTERFACE:
      interface operator(==)
!     if (clock1 == clock2) then ... endif
!                  OR
!     result = (clock1 == clock2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Clock), intent(in) :: clock1
!     type(ESMF_Clock), intent(in) :: clock2
!
! !DESCRIPTION:
!     Overloads the (==) operator for the {\tt ESMF\_Clock} class.
!     Compare two clocks for equality; return true if equal,
!     false otherwise.  Comparison is based on IDs, which are distinct
!     for newly created clocks and identical for clocks created as copies.
!
!     The arguments are:
!     \begin{description}
!     \item[clock1]
!          The first {\tt ESMF\_Clock} in comparison.
!     \item[clock2]
!          The second {\tt ESMF\_Clock} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_ClockEQ
!
! !REQUIREMENTS:
!     TMGx.x.x

      end interface
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_ClockOperator(/=) - Test if Clock 1 is not equal to Clock 2
!
! !INTERFACE:
      interface operator(/=)
!     if (clock1 /= clock2) then ... endif
!                  OR
!     result = (clock1 /= clock2)
!
! !RETURN VALUE:
!     logical :: result
!
! !ARGUMENTS:
!     type(ESMF_Clock), intent(in) :: clock1
!     type(ESMF_Clock), intent(in) :: clock2
!
! !DESCRIPTION:
!     Overloads the (/=) operator for the {\tt ESMF\_Clock} class.
!     Compare two clocks for inequality; return true if not equal,
!     false otherwise.  Comparison is based on IDs, which are distinct
!     for newly created clocks and identical for clocks created as copies.
!
!     The arguments are:
!     \begin{description}
!     \item[clock1]
!          The first {\tt ESMF\_Clock} in comparison.
!     \item[clock2]
!          The second {\tt ESMF\_Clock} in comparison.
!     \end{description}
!
!EOP
! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_ClockNE
!
! !REQUIREMENTS:
!     TMGx.x.x

      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ClockCreate - Create an ESMF Clock
!
! !INTERFACE:
      interface ESMF_ClockCreate

! !PRIVATE MEMBER FUNCTIONS:
      module procedure ESMF_ClockCreateNew
      module procedure ESMF_ClockCreateCopy

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Clock} Create
!     methods. 
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
!BOP
! !IROUTINE: ESMF_ClockAdvance - Advance a Clock's current time by one time step

! !INTERFACE:
      subroutine ESMF_ClockAdvance(clock, timeStep, ringingAlarmList, &
                                   ringingAlarmCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock),               intent(inout)         :: clock
      type(ESMF_TimeInterval),        intent(in),  optional :: timeStep
      type(ESMF_Alarm), dimension(:), intent(out), optional :: ringingAlarmList
      integer,                        intent(out), optional :: ringingAlarmCount
      integer,                        intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Advances the {\tt clock}'s current time by one time step:  either the
!     {\tt clock}'s, or the passed-in {\tt timeStep} (see below).  This
!     method optionally returns a list and number of ringing {\tt ESMF\_Alarm}s.
!     See also method {\tt ESMF\_ClockGetRingingAlarms}.
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance.
!     \item[{[timeStep]}]
!          Time step is performed with given timeStep, instead of
!          the {\tt ESMF\_Clock}'s.  Does not replace the {\tt ESMF\_Clock}'s 
!          timeStep; use {\tt ESMF\_ClockSet(clock, timeStep, ...)} for
!          this purpose.  Supports applications with variable time steps.
!     \item[{[ringingAlarmList]}]
!          Returns the array of alarms that are ringing after the
!          time step.
!     \item[{[ringingAlarmCount]}]
!          The number of alarms ringing after the time step.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
!EOP
! !REQUIREMENTS:
!     TMG3.4.1

      ! initialize list size to zero for not-present list
      integer :: sizeofRingingAlarmList
      sizeofRingingAlarmList = 0

      ! get size of given ringing alarm list for C++ validation
      if (present(ringingAlarmList)) then
        sizeofRingingAlarmList = size(ringingAlarmList)
      end if

      ! invoke C to C++ entry point

      if (present(ringingAlarmList) .and. sizeofRingingAlarmList > 1) then
        ! pass address of 2nd element for C++ to calculate array step size
        call c_ESMC_ClockAdvance2(clock, timeStep, &
                                  ringingAlarmList(1), ringingAlarmList(2), &
                                  sizeofRingingAlarmList, ringingAlarmCount, rc)
      else if (sizeofRingingAlarmList == 1) then
        ! array has only one element
        call c_ESMC_ClockAdvance1(clock, timeStep, &
                                  ringingAlarmList(1), &
                                  sizeofRingingAlarmList, ringingAlarmCount, rc)
      else
        ! array is not present
        call c_ESMC_ClockAdvance0(clock, timeStep, &
                                  sizeofRingingAlarmList, ringingAlarmCount, rc)
      endif
    
      end subroutine ESMF_ClockAdvance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockCreate - Create a new ESMF Clock

! !INTERFACE:
      ! Private name; call using ESMF_ClockCreate()
      function ESMF_ClockCreateNew(name, timeStep, startTime, stopTime, &
                                   runDuration, runTimeStepCount, refTime, rc)

! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockCreateNew

! !ARGUMENTS:
      character (len=*),       intent(in),  optional :: name
      type(ESMF_TimeInterval), intent(in)            :: timeStep
      type(ESMF_Time),         intent(in)            :: startTime
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: runDuration
      integer,                 intent(in),  optional :: runTimeStepCount
      type(ESMF_Time),         intent(in),  optional :: refTime
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Creates and sets the initial values in a new {\tt ESMF\_Clock}.    
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_ClockCreate()}.
!     
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]     
!          The name for the newly created clock.  If not specified, a
!          default unique name will be generated: "ClockNNN" where NNN
!          is a unique sequence number from 001 to 999.
!     \item[timeStep]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[startTime]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[{[stopTime]}]
!          The {\tt ESMF\_Clock}'s stopping time.  If neither stopTime,
!          runDuration, nor runTimeStepCount is specified, clock runs "forever";
!          user must use other means to know when to stop (e.g. ESMF\_Alarm or
!          ESMF\_ClockGet(clock, currTime)).
!          Mutually exclusive with runDuration and runTimeStepCount.
!     \item[{[runDuration]}]
!          Alternative way to specify {\tt ESMF\_Clock}'s stopping time;
!             stopTime = startTime + runDuration.
!          Mutually exclusive with stopTime and runTimeStepCount.
!     \item[{[runTimeStepCount]}]
!          Alternative way to specify {\tt ESMF\_Clock}'s stopping time;
!             stopTime = startTime + (runTimeStepCount * timeStep).
!          Mutually exclusive with stopTime and runDuration.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.  Provides reference point
!          for simulation time (see currSimTime in ESMF\_ClockGet() below).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4

      ! initialize name length to zero for non-existent name
      integer :: nameLen
      nameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

!     invoke C to C++ entry point to allocate and initialize new clock
      call c_ESMC_ClockCreateNew(ESMF_ClockCreateNew, nameLen, name, &
                                 timeStep, startTime, stopTime, runDuration, &
                                 runTimeStepCount, refTime, rc)

      end function ESMF_ClockCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockCreate - Create a copy of an existing ESMF Clock

! !INTERFACE:
      ! Private name; call using ESMF_ClockCreate()
      function ESMF_ClockCreateCopy(clock, rc)

! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockCreateCopy

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      integer,          intent(out), optional :: rc
    
! !DESCRIPTION:
!     Creates a copy of a given {\tt ESMF\_Clock}.    
!
!     This is a private method; invoke via the public overloaded entry point
!     {\tt ESMF\_ClockCreate()}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!        The {\tt ESMF\_Clock} to copy.
!     \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point to copy clock
      call c_ESMC_ClockCreateCopy(ESMF_ClockCreateCopy, clock, rc)

      end function ESMF_ClockCreateCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockDestroy - Free all resources associated with a Clock
!
! !INTERFACE:
      subroutine ESMF_ClockDestroy(clock, rc)
!
! !ARGUMENTS:
      type(ESMF_Clock) :: clock
      integer, intent(out), optional :: rc
!     
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_Clock}.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!       Destroy contents of this {\tt ESMF\_Clock}.
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_ClockDestroy(clock, rc)

      end subroutine ESMF_ClockDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGet - Get a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockGet(clock, name, timeStep, startTime, stopTime, &
                               runDuration, runTimeStepCount, refTime, &
                               currTime, prevTime, currSimTime, prevSimTime, &
                               calendar, calendarType, timeZone, advanceCount, &
                               alarmCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)            :: clock
      character (len=*),       intent(out), optional :: name
      type(ESMF_TimeInterval), intent(out), optional :: timeStep
      type(ESMF_Time),         intent(out), optional :: startTime
      type(ESMF_Time),         intent(out), optional :: stopTime
      type(ESMF_TimeInterval), intent(out), optional :: runDuration
      real(ESMF_KIND_R8),      intent(out), optional :: runTimeStepCount
      type(ESMF_Time),         intent(out), optional :: refTime
      type(ESMF_Time),         intent(out), optional :: currTime
      type(ESMF_Time),         intent(out), optional :: prevTime
      type(ESMF_TimeInterval), intent(out), optional :: currSimTime
      type(ESMF_TimeInterval), intent(out), optional :: prevSimTime
      type(ESMF_Calendar),     intent(out), optional :: calendar
      type(ESMF_CalendarType), intent(out), optional :: calendarType
      integer,                 intent(out), optional :: timeZone
      integer(ESMF_KIND_I8),   intent(out), optional :: advanceCount
      integer,                 intent(out), optional :: alarmCount
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Gets one or more of the properties of an {\tt ESMF\_Clock}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to query.
!     \item[{[name]}]
!          The name of this clock.
!     \item[{[timeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval.
!     \item[{[startTime]}]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[{[stopTime]}]
!          The {\tt ESMF\_Clock}'s stopping time.
!     \item[{[runDuration]}]
!          Alternative way to get {\tt ESMF\_Clock}'s stopping time;
!             runDuration = stopTime - startTime.
!     \item[{[runTimeStepCount]}]
!          Alternative way to get {\tt ESMF\_Clock}'s stopping time;
!             runTimeStepCount = (stopTime - startTime) / timeStep.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.
!     \item[{[currTime]}]
!          The {\tt ESMF\_Clock}'s current time.
!     \item[{[prevTime]}]
!          The {\tt ESMF\_Clock}'s previous time.  Equals currTime at
!          the previous time step.
!     \item[{[currSimTime]}]
!          The current simulation time (currTime - refTime).
!     \item[{[prevSimTime]}]
!          The previous simulation time.  Equals currSimTime at
!          the previous time step.
!     \item[{[calendar]}]
!          The {\tt Calendar} on which all the {\tt Clock}'s times are defined.
!     \item[{[calendarType]}]
!          The {\tt CalendarType} on which all the {\tt Clock}'s times are
!          defined.
!     \item[{[timeZone]}]
!          The timezone within which all the {\tt Clock}'s times are defined.
!     \item[{[advanceCount]}]
!          The number of times the {\tt ESMF\_Clock} has been advanced.
!     \item[{[alarmCount]}]
!          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
!          {\tt ESMF\_Alarm} list.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMG3.5.1 - TMG3.5.5

      ! temp name for C++ to fill
      character (len=ESMF_MAXSTR) :: tempName

      ! initialize name lengths to zero for non-existent name
      integer :: nameLen
      integer :: tempNameLen
      nameLen = 0
      tempNameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len(name)
      end if

!     invoke C to C++ entry point
      call c_ESMC_ClockGet(clock, nameLen, tempNameLen, tempName, &
                           timeStep, startTime, stopTime, &
                           runDuration, runTimeStepCount, refTime, &
                           currTime, prevTime, currSimTime, prevSimTime, &
                           calendar, calendarType, timeZone, advanceCount, &
                           alarmCount, rc)

      ! copy temp name back to given name to restore native Fortran
      !   storage style
      if (present(name)) then
        name = tempName(1:tempNameLen)
      endif
    
      end subroutine ESMF_ClockGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAlarm - Get an Alarm in a Clock's Alarm list

! !INTERFACE:
      subroutine ESMF_ClockGetAlarm(clock, name, alarm, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      character (len=*), intent(in)            :: name
      type(ESMF_Alarm),  intent(out)           :: alarm
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Gets the {\tt alarm} whose name is the value of name in the {\tt clock}'s
!     {\tt ESMF\_Alarm} list.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt ESMF\_Alarm} from.
!     \item[name]
!          The name of the desired {\tt ESMF\_Alarm}.
!     \item[alarm]
!          The desired alarm.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
!EOP
! !REQUIREMENTS:
!     TMGx.x

      ! get length of given name for C++ validation
      integer :: nameLen
      nameLen = len_trim(name)

!     invoke C to C++ entry point
      call c_ESMC_ClockGetAlarm(clock, nameLen, name, alarm, rc)
    
      end subroutine ESMF_ClockGetAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAlarmList - Get a list of Alarms from a Clock

! !INTERFACE:
      subroutine ESMF_ClockGetAlarmList(clock, alarmListType, &
                                        alarmList, alarmCount, timeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock),               intent(in)            :: clock
      type(ESMF_AlarmListType),       intent(in)            :: alarmListType
      type(ESMF_Alarm), dimension(:), intent(out)           :: alarmList
      integer,                        intent(out)           :: alarmCount
      type(ESMF_TimeInterval),        intent(in),  optional :: timeStep
      integer,                        intent(out), optional :: rc
!   
! !DESCRIPTION:
!     Gets the {\tt clock}'s list of alarms.
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance from which to get an {\tt ESMF\_Alarm} list.
!     \item[alarmListType]
!          The type of list to get:
!            {\tt ESMF\_ALARMLIST\_ALL} :
!                Returns the {\tt ESMF\_Clock}'s entire list of alarms.
!
!            {\tt ESMF\_ALARMLIST\_NEXTRINGING} :
!                Return only those alarms that will ring upon the next
!                {\tt clock} time step.  Can optionally specify argument
!                timeStep (see below) to use instead of the {\tt clock}'s.
!                See also method {\tt ESMF\_AlarmWillRingNext()} for checking a
!                single alarm.
!
!            {\tt ESMF\_ALARMLIST\_PREVRINGING} :
!                Return only those alarms that were ringing on the previous
!                {\tt ESMF\_Clock} time step.  See also method
!                {\tt ESMF\_AlarmWasPrevRinging()} for checking a single alarm.
!
!            {\tt ESMF\_ALARMLIST\_RINGING} :
!                Returns only those {\tt clock} alarms that are currently
!                ringing.  See also method {\tt ESMF\_ClockAdvance()} for
!                getting the list of ringing alarms subsequent to a time step.
!                See also method {\tt ESMF\_AlarmIsRinging()} for checking a
!                single alarm.
!     \item[alarmList]
!          The array of returned alarms. 
!     \item[alarmCount]
!          The number of {\tt ESMF\_Alarm}s in the returned list.
!     \item[{[timeStep]}]
!          Optional time step to be used instead of the {\tt clock}'s.
!          Only used with {\tt ESMF\_ALARMLIST\_NEXTRINGING alarmListType}
!          (see above); ignored if specified with other {\tt alarmListTypes}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
!EOP
! !REQUIREMENTS:
!     TMG4.3, 4.8

      ! get size of given alarm list for C++ validation
      integer :: sizeofAlarmList
      sizeofAlarmList = size(alarmList)

      ! invoke C to C++ entry point

      if (sizeofAlarmList > 1) then
        ! pass address of 2nd element for C++ to calculate array step size
        call c_ESMC_ClockGetAlarmList2(clock, alarmListType, &
                                      alarmList(1), alarmList(2), &
                                      sizeofAlarmList, alarmCount, timeStep, rc)
      else
        ! array has only one element
        call c_ESMC_ClockGetAlarmList1(clock, alarmListType, &
                                      alarmList(1), &
                                      sizeofAlarmList, alarmCount, timeStep, rc)
      endif
    
      end subroutine ESMF_ClockGetAlarmList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetNextTime - Calculate a Clock's next time

! !INTERFACE:
      subroutine ESMF_ClockGetNextTime(clock, nextTime, timeStep, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(in)            :: clock
      type(ESMF_Time),         intent(out)           :: nextTime
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Calculates what the next time of the {\tt clock} will be, based on
!     the {\tt clock}'s current time step or an optionally passed-in
!     {\tt timeStep}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance for which to get the next time.
!     \item[nextTime]
!          The resulting {\tt ESMF\_Clock}'s next time.
!     \item[{[timeStep]}]
!          The time step interval to use instead of the clock's.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
!EOP
! !REQUIREMENTS:
!     TMGx.x, CCSM

!     invoke C to C++ entry point
      call c_ESMC_ClockGetNextTime(clock, nextTime, timeStep, rc)

      end subroutine ESMF_ClockGetNextTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockIsStopTime - Test if the Clock has reached or exceeded its stop time

! !INTERFACE:
      function ESMF_ClockIsStopTime(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTime

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if the {\tt clock} has reached or exceeded its stop time,
!     and false otherwise.
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
! !IROUTINE: ESMF_ClockIsStopTimeEnabled - Test if the Clock's stop time is enabled

! !INTERFACE:
      function ESMF_ClockIsStopTimeEnabled(clock, rc)
!
! !RETURN VALUE:
      logical :: ESMF_ClockIsStopTimeEnabled

! !ARGUMENTS:
      type(ESMF_Clock), intent(in)            :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Returns true if the {\tt clock}'s stop time is set and enabled,
!     and false otherwise.
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
!     TMGx.x, WRF

!     invoke C to C++ entry point
      call c_ESMC_ClockIsStopTimeEnabled(clock, ESMF_ClockIsStopTimeEnabled, rc)
    
      end function ESMF_ClockIsStopTimeEnabled

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
!     Prints out an {\tt ESMF\_Clock}'s properties to {\tt stdout}, in support of testing
!     and debugging.  The options control the type of information and level     
!     of detail.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to be printed out.
!     \item[{[options]}]
!          Print options. If none specified, prints all {\tt clock} property values.\\
!          "advanceCount" - print the number of times the clock has been
!                           advanced. \\
!          "alarmCount"   - print the number of alarms in the clock's list. \\
!          "alarmList"    - print the clock's alarm list. \\
!          "currTime"     - print the current clock time. \\
!          "name"         - print the clock's name. \\
!          "prevTime"     - print the previous clock time. \\
!          "refTime"      - print the clock's reference time. \\
!          "startTime"    - print the clock's start time. \\
!          "stopTime"     - print the clock's stop time. \\
!          "timeStep"     - print the clock's time step. \\
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
!BOPI
! !IROUTINE: ESMF_ClockReadRestart - Restore the contents of a Clock (not implemented)

! !INTERFACE:
      function ESMF_ClockReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Clock) :: ESMF_ClockReadRestart
!
! !ARGUMENTS:
      character (len=*), intent(in)            :: name
      type(ESMF_IOSpec), intent(in),  optional :: iospec
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Clock} object from the last call to
!     {\tt ESMF\_ClockWriteRestart()}.  (Not implemented yet).
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
      integer :: nameLen
      nameLen = len_trim(name)

!     invoke C to C++ entry point to allocate and restore clock
      call c_ESMC_ClockReadRestart(ESMF_ClockReadRestart, nameLen, name, &
                                   iospec, rc)

      end function ESMF_ClockReadRestart

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSet - Set one or more properties of a Clock

! !INTERFACE:
      subroutine ESMF_ClockSet(clock, name, timeStep, startTime, stopTime, &
                               runDuration, runTimeStepCount, refTime, &
                               currTime, advanceCount, rc)

! !ARGUMENTS:
      type(ESMF_Clock),        intent(inout)         :: clock
      character (len=*),       intent(in),  optional :: name
      type(ESMF_TimeInterval), intent(in),  optional :: timeStep
      type(ESMF_Time),         intent(in),  optional :: startTime
      type(ESMF_Time),         intent(in),  optional :: stopTime
      type(ESMF_TimeInterval), intent(in),  optional :: runDuration
      integer,                 intent(in),  optional :: runTimeStepCount
      type(ESMF_Time),         intent(in),  optional :: refTime
      type(ESMF_Time),         intent(in),  optional :: currTime
      integer(ESMF_KIND_I8),   intent(in),  optional :: advanceCount
      integer,                 intent(out), optional :: rc
    
! !DESCRIPTION:
!     Sets/resets one or more of the properties of an {\tt ESMF\_Clock} that
!     was previously initialized via {\tt ESMF\_ClockCreate()}.
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set.
!     \item[{[name]}]
!          The new name for this clock.
!     \item[{[timeStep]}]
!          The {\tt ESMF\_Clock}'s time step interval.  This is used to
!          change a clock's timestep property for those applications that need
!          variable timesteps.  Also see {\tt ESMF\_ClockAdvance()} below
!          for specifying variable timesteps that are NOT saved as the clock's
!          internal time step property.
!     \item[{[startTime]}]
!          The {\tt ESMF\_Clock}'s starting time.
!     \item[{[stopTime]}]
!          The {\tt ESMF\_Clock}'s stopping time.  If neither stopTime,
!          runDuration, nor runTimeStepCount is specified, clock runs "forever";
!          user must use other means to know when to stop (e.g. ESMF\_Alarm or
!          ESMF\_ClockGet(clock, currTime)).
!          Mutually exclusive with runDuration and runTimeStepCount.
!     \item[{[runDuration]}]
!          Alternative way to specify {\tt ESMF\_Clock}'s stopping time;
!             stopTime = startTime + runDuration.
!          Mutually exclusive with stopTime and runTimeStepCount.
!     \item[{[runTimeStepCount]}]
!          Alternative way to specify {\tt ESMF\_Clock}'s stopping time;
!             stopTime = startTime + (runTimeStepCount * timeStep).
!          Mutually exclusive with stopTime and runDuration.
!     \item[{[refTime]}]
!          The {\tt ESMF\_Clock}'s reference time.
!          See description in {\tt ESMF\_ClockCreate()} above.
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

      ! initialize name length to zero for non-existent name
      integer :: nameLen
      nameLen = 0

      ! get length of given name for C++ validation
      if (present(name)) then
        nameLen = len_trim(name)
      end if

!     invoke C to C++ entry point
      call c_ESMC_ClockSet(clock, nameLen, name, timeStep, startTime, &
                           stopTime, runDuration, runTimeStepCount, &
                           refTime, currTime, advanceCount, rc)
    
      end subroutine ESMF_ClockSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockStopTimeDisable - Disable a Clock's stop time

! !INTERFACE:
      subroutine ESMF_ClockStopTimeDisable(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Disables a {\tt ESMF\_Clock}'s stop time; {\tt ESMF\_ClockIsStopTime()}
!     will always return false, allowing a clock to run past its stopTime.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance whose stop time to disable.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGx.x, WRF
    
!     invoke C to C++ entry point
      call c_ESMC_ClockStopTimeDisable(clock, rc)

      end subroutine ESMF_ClockStopTimeDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockStopTimeEnable - Enable an Clock's stop time

! !INTERFACE:
      subroutine ESMF_ClockStopTimeEnable(clock, stopTime, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      type(ESMF_Time),  intent(in),  optional :: stopTime
      integer,          intent(out), optional :: rc

! !DESCRIPTION:
!     Enables a {\tt ESMF\_Clock}'s stop time, allowing
!     {\tt ESMF\_ClockIsStopTime()} to respect the stopTime.
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance whose stop time to enable.
!     \item[{[stopTime]}]
!          The stop time to set or reset.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!EOP
! !REQUIREMENTS:
!     TMGx.x, WRF

!     invoke C to C++ entry point
      call c_ESMC_ClockStopTimeEnable(clock, stopTime, rc)

      end subroutine ESMF_ClockStopTimeEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSyncToRealTime - Set Clock's current time to wall clock time

! !INTERFACE:
      subroutine ESMF_ClockSyncToRealTime(clock, rc)

! !ARGUMENTS:
      type(ESMF_Clock), intent(inout)         :: clock
      integer,          intent(out), optional :: rc
    
! !DESCRIPTION:
!     Sets a {\tt clock}'s current time to the wall clock time.  It is
!     accurate to the nearest second.
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to be synchronized with wall clock time.
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
!BOP
! !IROUTINE:  ESMF_ClockValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine ESMF_ClockValidate(clock, options, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      character (len=*), intent(in),  optional :: options
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Checks whether a {\tt clock} is valid.  
!
!     The arguments are:  
!     \begin{description}
!     \item[clock]
!          {\tt ESMF\_Clock} to be validated.
!     \item[{[options]}]
!          Validation options are not yet supported.
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
!BOPI
! !IROUTINE: ESMF_ClockWriteRestart - Save the contents of a Clock (not implemented)

! !INTERFACE:
      subroutine ESMF_ClockWriteRestart(clock, iospec, rc)

! !ARGUMENTS:
      type(ESMF_Clock),  intent(in)            :: clock
      type(ESMF_IOSpec), intent(in),  optional :: iospec
      integer,           intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Clock} object.  Default options are to select the
!     fastest way to save to disk.  (Not implemented yet).
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to save.
!     \item[{[iospec]}]
!          The IO specification of the restart file.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

!     invoke C to C++ entry point
      call c_ESMC_ClockWriteRestart(clock, iospec, rc)

      end subroutine ESMF_ClockWriteRestart

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_ClockEQ - Compare two Clocks for equality
!
! !INTERFACE:
      function ESMF_ClockEQ(clock1, clock2)
!
! !RETURN VALUE:
      logical :: ESMF_ClockEQ

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock1
      type(ESMF_Clock), intent(in) :: clock2

! !DESCRIPTION:
!     This method overloads the (==) operator for the {\tt ESMF\_Clock}
!     class.  See "interface operator(==)" above for complete description.
!
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_ClockEQ(clock1, clock2, ESMF_ClockEQ)

      end function ESMF_ClockEQ

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  ESMF_ClockNE - Compare two Clocks for inequality
!
! !INTERFACE:
      function ESMF_ClockNE(clock1, clock2)
!
! !RETURN VALUE:
      logical :: ESMF_ClockNE

! !ARGUMENTS:
      type(ESMF_Clock), intent(in) :: clock1
      type(ESMF_Clock), intent(in) :: clock2

! !DESCRIPTION:
!     This method overloads the (/=) operator for the {\tt ESMF\_Clock}
!     class.  See "interface operator(/=)" above for complete description.
!
!EOPI
!     invoke C to C++ entry point
      call c_ESMC_ClockNE(clock1, clock2, ESMF_ClockNE)

      end function ESMF_ClockNE

!------------------------------------------------------------------------------

      end module ESMF_ClockMod
