! $Id: ESMF_Alarm.F90,v 1.1 2002/10/07 20:23:34 eschwab Exp $
    module ESMF_AlarmMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_AlarmMod
!
! !USES:
        use ESMF_TimeIntvMod
        use ESMF_TimeInstantMod
!
! !PUBLIC TYPES:
        implicit none

        type ESMF_Alarm
            private
            sequence
                type(ESMF_TimeIntv) :: RingInterval
                type(ESMF_TimeInstant)  :: RingTime
                type(ESMF_TimeInstant)  :: NextRingTime
                type(ESMF_TimeInstant)  :: PrevRingTime
                type(ESMF_TimeInstant)  :: StopTime
                logical :: Ringing
                logical :: Enabled
                integer :: ID
                integer :: AlarmMutex
        end type

! !PUBLIC MEMBER FUNCTIONS:
!       subroutine ESMF_AlarmInit(this, RingInterval, RingTime, &
!                                 StopTime, Enabled, rc)
!       subroutine ESMF_AlarmEnable(this, rc)
!       subroutine ESMF_AlarmDisable(this, rc)
!       subroutine ESMF_AlarmTurnOn(this, rc)
!       subroutine ESMF_AlarmTurnOff(this, rc)
!       subroutine ESMF_AlarmGetRingInterval(this, RingInterval, rc)
!       subroutine ESMF_AlarmSetRingInterval(this, RingInterval, rc)
!       subroutine ESMF_AlarmGetRingTime(this, RingTime, rc)
!       subroutine ESMF_AlarmSetRingTime(this, RingTime, rc)
!       subroutine ESMF_AlarmGetNextRingTime(this, NextRingTime, rc)
!       subroutine ESMF_AlarmSetNextRingTime(this, NextRingTime, rc)
!       subroutine ESMF_AlarmGetPrevRingTime(this, PrevRingTime, rc)
!       subroutine ESMF_AlarmSetPrevRingTime(this, PrevRingTime, rc)
!       subroutine ESMF_AlarmGetStopTime(this, StopTime, rc)
!       subroutine ESMF_AlarmSetStopTime(this, StopTime, rc)
!       function ESMF_AlarmIsRinging(this, rc)
!       function ESMF_AlarmCheckRingTime(this, ClockCurrTime, rc)
!       function ESMF_AlarmEQ(alarm1, alarm2)
! 
! !PUBLIC DATA MEMBERS:
!
! !DESCRIPTION:
!       Part of Time Manager F90 API wrapper of C++ implemenation
!
!       Defines F90 wrapper entry points for corresponding
!        C++ class ESMC\_Alarm
!
! !REVISION HISTORY:
!
!  09Aug02   Earl Schwab  Initial code.
!

		interface operator(==)
			module procedure ESMF_AlarmEQ
		end interface

        private ESMF_AlarmEQ

    contains
    
        subroutine ESMF_AlarmInit(this, RingInterval, RingTime, &
                                  StopTime, Enabled, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeIntv), intent(in), optional :: RingInterval
            type(ESMF_TimeInstant), intent(in), optional :: RingTime, StopTime
            logical, intent(in) :: Enabled
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmInit(this, RingInterval, RingTime, &
                                  StopTime, Enabled, rc)

        end subroutine

        subroutine ESMF_AlarmEnable(this, rc)
            type(ESMF_Alarm), intent(inout) :: this
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmEnable(this, rc)

        end subroutine

        subroutine ESMF_AlarmDisable(this, rc)
            type(ESMF_Alarm), intent(inout) :: this
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmDisable(this, rc)

        end subroutine

        subroutine ESMF_AlarmTurnOn(this, rc)
            type(ESMF_Alarm), intent(inout) :: this
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmTurnOn(this, rc)

        end subroutine

        subroutine ESMF_AlarmTurnOff(this, rc)
            type(ESMF_Alarm), intent(inout) :: this
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmTurnOff(this, rc)

        end subroutine

        function ESMF_AlarmIsRinging(this, rc)
            logical :: ESMF_AlarmIsRinging
            type(ESMF_Alarm), intent(inout) :: this
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmIsRinging(this, ESMF_AlarmIsRinging, rc)

        end function

        ! main method used by a clock to check whether to trigger the alarm 
        function ESMF_AlarmCheckRingTime(this, ClockCurrTime, rc)
            logical :: ESMF_AlarmCheckRingTime
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(in) :: ClockCurrTime
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmCheckRingTime(this, ESMF_AlarmCheckRingTime, &
                                           ClockCurrTime, rc)

        end function

        subroutine ESMF_AlarmGetRingInterval(this, RingInterval, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeIntv), intent(out) :: RingInterval
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmGetRingInterval(this, RingInterval, rc)

        end subroutine

        subroutine ESMF_AlarmSetRingInterval(this, RingInterval, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeIntv), intent(in) :: RingInterval
            integer, intent(out), optional :: rc
    
            call c_ESMF_AlarmSetRingInterval(this, RingInterval, rc)

        end subroutine

        subroutine ESMF_AlarmGetRingTime(this, RingTime, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(out) :: RingTime
            integer, intent(out), optional :: rc
   
            call c_ESMF_AlarmGetRingTime(this, RingTime, rc)

        end subroutine

        subroutine ESMF_AlarmSetRingTime(this, RingTime, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(in) :: RingTime
            integer, intent(out), optional :: rc
   
            call c_ESMF_AlarmSetRingTime(this, RingTime, rc)

        end subroutine

        subroutine ESMF_AlarmGetNextRingTime(this, NextRingTime, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(out) :: NextRingTime
            integer, intent(out), optional :: rc
   
            call c_ESMF_AlarmGetNextRingTime(this, NextRingTime, rc)

        end subroutine

        subroutine ESMF_AlarmSetNextRingTime(this, NextRingTime, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(in) :: NextRingTime
            integer, intent(out), optional :: rc
   
            call c_ESMF_AlarmSetNextRingTime(this, NextRingTime, rc)

        end subroutine

        subroutine ESMF_AlarmGetPrevRingTime(this, PrevRingTime, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(out) :: PrevRingTime
            integer, intent(out), optional :: rc
   
            call c_ESMF_AlarmGetPrevRingTime(this, PrevRingTime, rc)

        end subroutine

        subroutine ESMF_AlarmSetPrevRingTime(this, PrevRingTime, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(in) :: PrevRingTime
            integer, intent(out), optional :: rc
   
            call c_ESMF_AlarmSetPrevRingTime(this, PrevRingTime, rc)

        end subroutine

        subroutine ESMF_AlarmGetStopTime(this, StopTime, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(out) :: StopTime
            integer, intent(out), optional :: rc
   
            call c_ESMF_AlarmGetStopTime(this, StopTime, rc)

        end subroutine

        subroutine ESMF_AlarmSetStopTime(this, StopTime, rc)
            type(ESMF_Alarm), intent(inout) :: this
            type(ESMF_TimeInstant), intent(in) :: StopTime
            integer, intent(out), optional :: rc
   
            call c_ESMF_AlarmSetStopTime(this, StopTime, rc)

        end subroutine

        ! overloaded (==) operator interface function
        function ESMF_AlarmEQ(alarm1, alarm2)
            logical :: ESMF_AlarmEQ
            type(ESMF_Alarm), intent(in) :: alarm1, alarm2

            call c_ESMF_AlarmEQ(alarm1, alarm2, ESMF_AlarmEQ)

        end function
!EOP
!===============================================================================
    end module ESMF_AlarmMod
