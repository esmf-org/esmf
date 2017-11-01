#include "ESMF.h"

  module clock_utils
  
  use ESMF
  use ESMF_TestMod

  implicit none
  public get_sync_alarms

  contains

  subroutine get_sync_alarms(clocks, alarms, rc)
    type(ESMF_Clock), dimension(:), intent(in) :: clocks
    type(ESMF_Alarm), dimension(:), intent(out) :: alarms
    integer, intent(inout) :: rc

    integer :: nclocks
    integer :: i
    type(ESMF_Clock), dimension(:), allocatable :: tmp_clocks
    integer, dimension(:), allocatable :: sync_rel_tsteps
    type(ESMF_TimeInterval) :: alarm_interval, tstep
    type(ESMF_Time) :: time1, time2, stop_time

    logical :: found_syncstep = .false., done = .false.

    rc = ESMF_SUCCESS

    print *, "Obtaining the syncing alarms for components"
    nclocks = size(clocks)
    if(nclocks == 0) then
      ! FIXME: Is this a valid state?
      return
    end if
    if(size(alarms) /= nclocks) then
      print *, "The user needs to pass alarms (result) for each clock"
      rc = ESMF_FAILURE
      return
    end if

    allocate(tmp_clocks(nclocks))
    do i=1,nclocks
      tmp_clocks(i) = ESMF_ClockCreate(clocks(i), rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Failed to create tmp copy of clocks"
        return
      end if
    end do

    allocate(sync_rel_tsteps(nclocks))
    sync_rel_tsteps = 1
    done = .false.
    i = 2
    do while( (.not. ESMF_ClockIsStopTime(tmp_clocks(1), rc=rc))&
              .and. (i <= nclocks)  )
      call ESMF_ClockGet(tmp_clocks(i-1), currTime=time1, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Unable to get current time from clock :", i-1
        return
      end if
      call ESMF_ClockGet(tmp_clocks(i), currTime=time2, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Unable to get current time from clock :", i
        return
      end if
      if(time1 == time2) then
        if(i == nclocks) then
          ! Last clock matched with the (last -1) clock
          done = .true.
        end if
        ! proceed to the next clock
        i = i+1
      else
        if(.not. ESMF_ClockIsStopTime(tmp_clocks(i))) then
          ! Advance the clock
          call ESMF_ClockAdvance(tmp_clocks(i), rc=rc)
          if(rc /= ESMF_SUCCESS) then
            print *, "Advancing tmp clock failed, clock = ", i
            return
          end if
          sync_rel_tsteps(i) = sync_rel_tsteps(i) + 1
        else
          ! Start advancing clocks from the beginning
          ! - Advance the first clock manually and let the loop
          !   take over
          call ESMF_ClockAdvance(tmp_clocks(1), rc=rc) 
          if(rc /= ESMF_SUCCESS) then
            print *, "Advancing the first tmp clock failed" 
            return
          end if
          sync_rel_tsteps(1) = sync_rel_tsteps(1) + 1
          i = 2
          sync_rel_tsteps(2:nclocks) = 1
        end if
      end if
    end do

    if((nclocks > 1) .and. (.not. done)) then
      print *, "Could not find a sync time between the clocks, no alarms set"
      rc = ESMF_FAILURE
      return
    end if

    do i=1,nclocks
      call ESMF_ClockGet(clocks(i), timeStep=tstep,&
            stopTime=stop_time, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Unable to get clock properties for clock : ", i
        return
      end if

      alarm_interval = tstep * sync_rel_tsteps(i)

      alarms(i) = ESMF_AlarmCreate(clocks(i), ringInterval=alarm_interval,&
                    stopTime=stop_time, rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Unable to create alarm, ", i
        return
      end if
    end do

!    do i=1,nclocks
!      call ESMF_AlarmPrint(alarms(i), rc=rc)
!      if(rc /= ESMF_SUCCESS) then
!        print *, "Unable to print alarm : ", i
        ! Continue trying to print the next alarm
!      end if
!    end do
    deallocate(sync_rel_tsteps)

    do i=1,nclocks
      call ESMF_ClockDestroy(tmp_clocks(i), rc=rc)
      if(rc /= ESMF_SUCCESS) then
        print *, "Error destroying temp clock :", i
        ! Keep trying to destroy all temp clocks
      end if
    end do
    deallocate(tmp_clocks)
  end subroutine get_sync_alarms

  end module clock_utils
