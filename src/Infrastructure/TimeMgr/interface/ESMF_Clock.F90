! $Id: ESMF_Clock.F90,v 1.2 2002/10/28 16:48:59 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF TimeInstant Module
!
! (all lines below between the !BOP and !EOP markers will be included in
!  the automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! put any constants or macros which apply to the whole component in this
!  include file.  anything public or esmf-wide should be up higher at
!  the top level include files.

#include <ESMF_TimeMgr.h>

!------------------------------------------------------------------------------
! module definition
!
!   module ESMF_ClockMod
!
!
! !DESCRIPTION:
!
!
!
!
!
!

!===============================================================================
!BOP
!
! !MODULE: ESMF_ClockMod
!
! !USES:
        use ESMF_TypesMod
        use ESMF_TimeIntvMod
        use ESMF_TimeInstantMod
        use ESMF_AlarmMod
!
!------------------------------------------------------------------------------

! !PUBLIC TYPES:
        implicit none

        integer, parameter :: MAX_ALARMS = 10

        type ESMF_Clock
            private
            sequence
                type(ESMF_TimeIntv) :: TimeStep
                type(ESMF_TimeInstant)  :: StartTime
                type(ESMF_TimeInstant)  :: StopTime
                type(ESMF_TimeInstant)  :: RefTime
                type(ESMF_TimeInstant)  :: CurrTime
                type(ESMF_TimeInstant)  :: PrevTime
                integer(int32) :: AdvanceCount
                type(ESMF_Alarm), dimension(MAX_ALARMS) :: AlarmList
                integer :: ClockMutex
        end type
!
! !PUBLIC MEMBER FUNCTIONS:
!        subroutine ESMF_ClockInit(this, TimeStep, StartTime, StopTime, &
!                                  RefTime, rc)
!        subroutine ESMF_ClockAddAlarm(this, Alarm, rc)
!        subroutine ESMF_ClockGetAlarmList(this, AlarmList, rc)
!        subroutine ESMF_ClockSyncToWallClock(this, rc)
!        subroutine ESMF_ClockAdvance(this, RingingAlarmList, rc)
!        subroutine ESMF_ClockGetAdvanceCount(this, AdvanceCount, rc)
!        subroutine ESMF_ClockGetTimeInterval(this, TimeInterval, rc)
!        subroutine ESMF_ClockSetTimeInterval(this, TimeInterval, rc)
!        subroutine ESMF_ClockGetCurrTime(this, CurrTime, rc)
!        subroutine ESMF_ClockSetCurrTime(this, CurrTime, rc)
!        subroutine ESMF_ClockGetStartTime(this, StartTime, rc)
!        subroutine ESMF_ClockGetStopTime(this, StopTime, rc)
!        subroutine ESMF_ClockGetRefTime(this, RefTime, rc)
!        subroutine ESMF_ClockGetPrevTime(this, PrevTime, rc)
!        subroutine ESMF_ClockGetCurrSimTime(this, CurrSimTime, rc)
!        subroutine ESMF_ClockGetPrevSimTime(this, PrevSimTime, rc)
!        function ESMF_ClockIsStopTime(this, rc)
!
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
!       Part of Time Manager F90 API wrapper of C++ implemenation
!
!       Defines F90 wrapper entry points for corresponding
!        C++ class ESMC\_Clock
!
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!
!------------------------------------------------------------------------------

    contains

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockInit

! !INTERFACE:
        subroutine ESMF_ClockInit(this, TimeStep, StartTime, StopTime, &
                                  RefTime, rc)

! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeIntv), intent(in) :: TimeStep
        type(ESMF_TimeInstant), intent(in) :: StartTime, StopTime, RefTime
        integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     
!     
!     
!EOP
! !REQUIREMENTS:  AAAn.n.n

            call c_ESMF_ClockInit(this, TimeStep, StartTime, StopTime, &
                                  RefTime, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAddAlarm

! !INTERFACE:
        subroutine ESMF_ClockAddAlarm(this, Alarm, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_Alarm), intent(in) :: Alarm
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!     
!   
!   
!EOP
! !REQUIREMENTS:  AAAn.n.n
    
            call c_ESMF_ClockAddAlarm(this, Alarm, rc)
    
        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAlarmList

! !INTERFACE:
        subroutine ESMF_ClockGetAlarmList(this, AlarmList, rc)


! !ARGUMENTS:
       type(ESMF_Clock), intent(inout) :: this
       type(ESMF_Alarm), intent(out) :: AlarmList(:)
       integer, intent(out), optional :: rc
    


! !DESCRIPTION:
!     
!   
!   
!EOP
! !REQUIREMENTS:  AAAn.n.n
   

            call c_ESMF_ClockGetAlarmList(this, AlarmList, rc)
    
        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSyncToWallClock

! !INTERFACE:
        subroutine ESMF_ClockSyncToWallClock(this, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        integer, intent(out), optional :: rc
    

! !DESCRIPTION:
!     
!   
!   
!EOP
! !REQUIREMENTS:  AAAn.n.n
   

            call c_ESMF_ClockSyncToWallClock(this, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockAdvance

! !INTERFACE:
        subroutine ESMF_ClockAdvance(this, RingingAlarmList, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_Alarm), intent(out), optional :: RingingAlarmList(:)
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!     
!   
!   
!EOP
! !REQUIREMENTS:  AAAn.n.n

			if (present(RingingAlarmList)) then
                call c_ESMF_ClockAdvance(this, RingingAlarmList, rc)
            else
                call c_ESMF_ClockAdvance(this, rc)
            end if
    
        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockIsStopTime

! !INTERFACE:
        function ESMF_ClockIsStopTime(this, rc)

!
! !RETURN VALUE:
        logical :: ESMF_ClockIsStopTime


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_ClockIsStopTime(this, ESMF_ClockIsStopTime, rc)
    
        end function


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetAdvanceCount

! !INTERFACE:
        subroutine ESMF_ClockGetAdvanceCount(this, AdvanceCount, rc)

! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        integer(int32), intent(out) :: AdvanceCount
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n



            call c_ESMF_ClockGetAdvanceCount(this, AdvanceCount, rc)
    
        end subroutine




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetTimeInterval

! !INTERFACE:
        subroutine ESMF_ClockGetTimeInterval(this, TimeInterval, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeIntv), intent(out) :: TimeInterval
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            call c_ESMF_ClockGetTimeInterval(this, TimeInterval, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetTimeInterval

! !INTERFACE:
        subroutine ESMF_ClockSetTimeInterval(this, TimeInterval, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeIntv), intent(in) :: TimeInterval
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_ClockSetTimeInterval(this, TimeInterval, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrTime

! !INTERFACE:
        subroutine ESMF_ClockGetCurrTime(this, CurrTime, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: CurrTime
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            call c_ESMF_ClockGetCurrTime(this, CurrTime, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockSetCurrTime

! !INTERFACE:
        subroutine ESMF_ClockSetCurrTime(this, CurrTime, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeInstant), intent(in) :: CurrTime
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_ClockSetCurrTime(this, CurrTime, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStartTime

! !INTERFACE:
        subroutine ESMF_ClockGetStartTime(this, StartTime, rc)

! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: StartTime
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            call c_ESMF_ClockGetStartTime(this, StartTime, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetStopTime

! !INTERFACE:
        subroutine ESMF_ClockGetStopTime(this, StopTime, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: StopTime
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n



            call c_ESMF_ClockGetStopTime(this, StopTime, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetRefTime

! !INTERFACE:
        subroutine ESMF_ClockGetRefTime(this, RefTime, rc)

! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: RefTime
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_ClockGetRefTime(this, RefTime, rc)
    
        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetRefTime

! !INTERFACE:
        subroutine ESMF_ClockGetPrevTime(this, PrevTime, rc)

! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: PrevTime
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            call c_ESMF_ClockGetPrevTime(this, PrevTime, rc)

    
        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetCurrSimTime

! !INTERFACE:
        subroutine ESMF_ClockGetCurrSimTime(this, CurrSimTime, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: CurrSimTime
        integer, intent(out), optional :: rc


! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n



            call c_ESMF_ClockGetCurrSimTime(this, CurrSimTime, rc)
    
        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ClockGetPrevSimTime

! !INTERFACE:
        subroutine ESMF_ClockGetPrevSimTime(this, PrevSimTime, rc)


! !ARGUMENTS:
        type(ESMF_Clock), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: PrevSimTime
        integer, intent(out), optional :: rc



! !DESCRIPTION:
!     
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_ClockGetPrevSimTime(this, PrevSimTime, rc)
    
        end subroutine
!EOP
!===============================================================================
    end module ESMF_ClockMod
