! $Id: ESMF_Alarm.F90,v 1.2 2002/10/22 23:34:43 svasquez Exp $
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

    module ESMF_AlarmMod
!===============================================================================
!BOP
!
! !MODULE: ESMF_AlarmMod
!
!
! !DESCRIPTION:
!       
!
!      
!     
!
!
! !USES:
        use ESMF_TimeIntvMod
        use ESMF_TimeInstantMod
        implicit none
!------------------------------------------------------------------------------

! !PRIVATE TYPES:
        private

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

! !PUBLIC TYPES:
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
!EOP

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
      character(*), parameter, private :: version = '$Id: '
!------------------------------------------------------------------------------
!BOP
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
!------------------------------------------------------------------------------

    contains

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmInit - Initializes an alarm

! !INTERFACE:
        subroutine ESMF_AlarmInit(this, RingInterval, RingTime, &
                                  StopTime, Enabled, rc)

! !ARGUMENTS:
       type(ESMF_Alarm), intent(inout) :: this
       type(ESMF_TimeIntv), intent(in), optional :: RingInterval
       type(ESMF_TimeInstant), intent(in), optional :: RingTime, StopTime
       logical, intent(in) :: Enabled
       integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an alarm
!EOP
! !REQUIREMENTS:  AAAn.n.n
            call c_ESMF_AlarmInit(this, RingInterval, RingTime, &
                                  StopTime, Enabled, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmEnable - Enables an alarm

! !INTERFACE:
        subroutine ESMF_AlarmEnable(this, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this

! !DESCRIPTION:
!     Enables an alarm
!EOP
! !REQUIREMENTS:  AAAn.n.n

            call c_ESMF_AlarmEnable(this, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmDisable

! !INTERFACE:
        subroutine ESMF_AlarmDisable(this, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Disables an alarm
!EOP
! !REQUIREMENTS:  AAAn.n.n


    
            call c_ESMF_AlarmDisable(this, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmTurnOn - Turn on an alarm


! !INTERFACE:
        subroutine ESMF_AlarmTurnOn(this, rc)

! !ARGUMENTS:
            type(ESMF_Alarm), intent(inout) :: this
            integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn on an alarm
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_AlarmTurnOn(this, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmTurnOff - Turn off an alarm

! !INTERFACE:
        subroutine ESMF_AlarmTurnOff(this, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an alarm
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_AlarmTurnOff(this, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmIsRinging - Check if alarm is ringing

! !INTERFACE:
        function ESMF_AlarmIsRinging(this, rc)

!
! !RETURN VALUE:
       logical :: ESMF_AlarmIsRinging

! !ARGUMENTS:
       type(ESMF_Alarm), intent(inout) :: this
       integer, intent(out), optional :: rc


! !DESCRIPTION:
!     Check if alarm is ringing.
!EOP
! !REQUIREMENTS:  AAAn.n.n


    
            call c_ESMF_AlarmIsRinging(this, ESMF_AlarmIsRinging, rc)

        end function  ESMF_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmCheckRingTime - main method used by a clock to check whether to trigger the alarm
!
! !INTERFACE:
        function ESMF_AlarmCheckRingTime(this, ClockCurrTime, rc)
!
! !RETURN VALUE:
        logical :: ESMF_AlarmCheckRingTime

!
! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeInstant), intent(in) :: ClockCurrTime
        integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!       
!     main method used by a clock to check whether to trigger the alarm 
!EOP
! !REQUIREMENTS:  TMG 2.4.3

            call c_ESMF_AlarmCheckRingTime(this, ESMF_AlarmCheckRingTime, &
                                           ClockCurrTime, rc)

        end function c_ESMF_AlarmCheckRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmGetRingInterval
!
! !INTERFACE:
        subroutine ESMF_AlarmGetRingInterval(this, RingInterval, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeIntv), intent(out) :: RingInterval
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!       
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_AlarmGetRingInterval(this, RingInterval, rc)

        end subroutine
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSetRingInterval
!
! !INTERFACE:
        subroutine ESMF_AlarmSetRingInterval(this, RingInterval, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeIntv), intent(in) :: RingInterval
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

    
            call c_ESMF_AlarmSetRingInterval(this, RingInterval, rc)

        end subroutine


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetRingTime
!
! !INTERFACE:
        subroutine ESMF_AlarmGetRingTime(this, RingTime, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: RingTime
        integer, intent(out), optional :: rc
   

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

            call c_ESMF_AlarmGetRingTime(this, RingTime, rc)

        end subroutine

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetRingTime
!
! !INTERFACE:
        subroutine ESMF_AlarmSetRingTime(this, RingTime, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeInstant), intent(in) :: RingTime
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

   
            call c_ESMF_AlarmSetRingTime(this, RingTime, rc)

        end subroutine

------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetNextRingTime
!
! !INTERFACE:
        subroutine ESMF_AlarmGetNextRingTime(this, NextRingTime, rc)


! !ARGUMENTS:
       type(ESMF_Alarm), intent(inout) :: this
       type(ESMF_TimeInstant), intent(out) :: NextRingTime
       integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

   
            call c_ESMF_AlarmGetNextRingTime(this, NextRingTime, rc)

        end subroutine

------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetNextRingTime
!
! !INTERFACE:
        subroutine ESMF_AlarmSetNextRingTime(this, NextRingTime, rc)


! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeInstant), intent(in) :: NextRingTime
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


   
            call c_ESMF_AlarmSetNextRingTime(this, NextRingTime, rc)

        end subroutine

------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetPrevRingTime
!
! !INTERFACE:
        subroutine ESMF_AlarmGetPrevRingTime(this, PrevRingTime, rc)


! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: PrevRingTime
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

   
            call c_ESMF_AlarmGetPrevRingTime(this, PrevRingTime, rc)

        end subroutine


------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetPrevRingTime
!
! !INTERFACE:
        subroutine ESMF_AlarmSetPrevRingTime(this, PrevRingTime, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeInstant), intent(in) :: PrevRingTime
        integer, intent(out), optional :: rc
   

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_AlarmSetPrevRingTime(this, PrevRingTime, rc)

        end subroutine


------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetStopTime
!
! !INTERFACE:
        subroutine ESMF_AlarmGetStopTime(this, StopTime, rc)


! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeInstant), intent(out) :: StopTime
        integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


   
            call c_ESMF_AlarmGetStopTime(this, StopTime, rc)

        end subroutine


------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetStopTime
!
! !INTERFACE:
        subroutine ESMF_AlarmSetStopTime(this, StopTime, rc)

! !ARGUMENTS:
        type(ESMF_Alarm), intent(inout) :: this
        type(ESMF_TimeInstant), intent(in) :: StopTime
        integer, intent(out), optional :: rc
   

! !DESCRIPTION:
!
!
!
!EOP
! !REQUIREMENTS:  AAAn.n.n


            call c_ESMF_AlarmSetStopTime(this, StopTime, rc)

        end subroutine

------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmEQ
!
! !INTERFACE:
        function ESMF_AlarmEQ(alarm1, alarm2)

!
! !RETURN VALUE:
        logical :: ESMF_AlarmEQ


! !ARGUMENTS:
        type(ESMF_Alarm), intent(in) :: alarm1, alarm2

!
! !DESCRIPTION:
!	overloaded (==) operator interface function
!
!EOP
! !REQUIREMENTS:  



            call c_ESMF_AlarmEQ(alarm1, alarm2, ESMF_AlarmEQ)

        end function  ESMF_AlarmEQ
!EOP
!===============================================================================
    end module ESMF_AlarmMod
