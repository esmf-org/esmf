! $Id: ESMF_AlarmMod.f,v 1.1 2002/11/14 23:41:25 jwolfe Exp $
      module ESMF_AlarmMod
!===============================================================================
!BOP
! !MODULE: ESMF_AlarmMod
!
! !USES:
      use ESMF_TimeMgrMod
!
! !PUBLIC TYPES:
      implicit none

      type ESMF_Alarm
        private
        sequence
        integer(8) type
        integer(8) offset
        integer(8) period
        integer(8) alarmOn 
      end type ESMF_Alarm
!
! !PUBLIC MEMBER FUNCTIONS:
!     ESMF_AlarmInitPeriodic     
!     ESMF_AlarmInitMonthly
!     ESMF_AlarmInitYearly
!     ESMF_AlarmGetType
!     ESMF_AlarmSet
!     ESMF_AlarmIsOn
!
! !PUBLIC DATA MEMBERS:
      integer, parameter :: ESMF_ALARM_PERIODIC=0,
     &                      ESMF_ALARM_MONTHLY=1,
     &                      ESMF_ALARM_YEARLY=2
!
! !DESCRIPTION:
! Alarms are part of the Modeling Framework Time Management module 
! ({\tt ESMF\_TimeMgmt}).  An alarm object can be activated at periodic, monthly, or 
! yearly intervals. 
!
!EOP
!===============================================================================


	contains
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_AlarmInitPeriodic
!
! !INTERFACE:
      function ESMF_AlarmInitPeriodic(period, offset, rc)
    
! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmInitPeriodic   ! returned alarm object
    
! !PARAMETERS:
      type(ESMF_Time), intent(in) :: period        ! time period
      type(ESMF_Time), intent(in) :: offset        ! time offset
      integer, intent(out), optional :: rc         ! return code 

! !DESCRIPTION:
!     Returns a new alarm.  
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_AlarmInitPeriodic(ESMF_AlarmInitPeriodic, period, 
     &  offset, stub)
      if (present(rc)) rc = stub

      end function ESMF_AlarmInitPeriodic

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_AlarmInitMonthly
!
! !INTERFACE:
      function ESMF_AlarmInitMonthly(rc)
    
! !RETURN VALUE:
      type(ESMF_Alarm) :: ESMF_AlarmInitMonthly     ! returned alarm object
    
! !PARAMETERS:
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Returns a new alarm object.  
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_AlarmInitMonthly(ESMF_AlarmInitMonthly, stub)
      if (present(rc)) rc = stub

      end function ESMF_AlarmInitMonthly

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_AlarmInitYearly
!
! !INTERFACE:
      function ESMF_AlarmInitYearly(rc)

! !RETURN VALUE:    
      type(ESMF_Alarm) :: ESMF_AlarmInitYearly      ! returned alarm object
    
! !PARAMETERS:
      integer, intent(out), optional :: rc          ! return code

! !DESCRIPTION:
!     Returns a new alarm object.  
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_AlarmInitYearly(ESMF_AlarmInitYearly, stub)
      if (present(rc)) rc = stub

      end function ESMF_AlarmInitYearly

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_AlarmGetType
!
! !INTERFACE:
      function ESMF_AlarmGetType(this, rc)
    
! !RETURN VALUE:
      integer :: ESMF_AlarmGetType                   ! returned type

! !PARAMETERS:
      type(ESMF_Alarm), intent(in) :: this           ! alarm object    
      integer, intent(out), optional :: rc           ! return code

! !DESCRIPTION:
!     Returns true if the alarm is on, false if not.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_AlarmGetType(this, ESMF_AlarmGetType, stub)
      if (present(rc)) rc = stub

      end function ESMF_AlarmGetType

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_AlarmIsOn
!
! !INTERFACE:
      function ESMF_AlarmIsOn(this, timeMgr, rc)
    
! !RETURN VALUE:
      logical :: ESMF_AlarmIsOn                      ! true if alarm is on

! !PARAMETERS:
      type(ESMF_Alarm), intent(in) :: this           ! alarm object    
      type(ESMF_TimeMgr), intent(in) :: timeMgr      ! time manager
      integer, intent(out), optional :: rc           ! return code

! !DESCRIPTION:
!     Returns true if the alarm is on, false if not.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_AlarmIsOn(this, timeMgr, ESMF_AlarmIsOn, stub)
      if (present(rc)) rc = stub

      end function ESMF_AlarmIsOn

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_AlarmSet
!
! !INTERFACE:
      subroutine ESMF_AlarmSet(this, alarmOn, rc)
    
! !PARAMETERS:
      type(ESMF_Alarm), intent(inout) :: this        ! alarm object    
      logical, intent(in) :: alarmOn                 ! true if alarm on
      integer, intent(out), optional :: rc           ! return code

! !DESCRIPTION:
!     Sets whether the alarm is on.
!
!EOP
!-------------------------------------------------------------------------------

      integer stub

      call ESMC_AlarmSet(this, alarmOn, stub)
      if (present(rc)) rc = stub

      end subroutine ESMF_AlarmSet

!===============================================================================
	end module









