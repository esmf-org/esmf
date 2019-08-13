#include "ESMF.h"

  module clock_utils
  
  use ESMF
  use ESMF_TestMod

  implicit none
  public get_sync_alarms

  contains

  subroutine get_sync_alarms(clocks, alarms, rc)
    type(ESMF_Clock), dimension(:), intent(in) :: clocks
    type(ESMF_Alarm), dimension(:), intent(inout) :: alarms
    integer, intent(inout) :: rc

    integer :: nclocks
    integer :: i, j
    type(ESMF_Clock), dimension(:), allocatable :: tmp_clocks
    integer, dimension(:), allocatable :: sync_rel_tsteps
    type(ESMF_TimeInterval) :: alarm_interval, tstep
    type(ESMF_Time) :: time1, time2, start_time, stop_time

    logical :: found_syncstep = .false., done = .false.

    integer(ESMF_KIND_I8) :: dbg_stime1, dbg_stime2
    character(len=ESMF_MAXSTR) :: name

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

    !call ESMF_ClockGet(tmp_clocks(1), currTime=time1, rc=rc)
    !call ESMF_TimeGet(time1, s_i8=dbg_stime1, rc=rc)
    !print *, "Start time = ", dbg_stime1

    ! Manually advance all the clocks once
    do i=1,nclocks
      call ESMF_ClockAdvance(tmp_clocks(i), rc=rc) 
      if(rc /= ESMF_SUCCESS) then
        print *, "Advancing the tmp clocks failed" 
        return
      end if
    end do
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
      !call ESMF_TimeGet(time1, s_i8=dbg_stime1, rc=rc)
      !call ESMF_TimeGet(time2, s_i8=dbg_stime2, rc=rc)
      !if(time1 >= time2) then
      !  print *, i, dbg_stime1, ">=", dbg_stime2
      !else
      !  print *, i, dbg_stime1, "<", dbg_stime2
      !end if
      if(time1 == time2) then
        if(i == nclocks) then
          ! Last clock matched with the (last -1) clock
          done = .true.
        end if
        ! proceed to the next clock
        i = i+1
      else
        if( (time1 > time2) .and. (.not. ESMF_ClockIsStopTime(tmp_clocks(i))) ) then
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
          ! Reset all clocks, except the first one
          do j=i,2,-1
            !print *, "Resetting clock : ", j
            sync_rel_tsteps(j) = 1
            ! Reset to original clock
            call ESMF_ClockDestroy(tmp_clocks(j), rc=rc)
            if(rc /= ESMF_SUCCESS) then
              print *, "Error while destroying tmp clock"
            end if
            tmp_clocks(j) = ESMF_ClockCreate(clocks(j), rc=rc)
            if(rc /= ESMF_SUCCESS) then
              print *, "Re-creating a tmp clock failed" 
              return
            end if
            ! Advance once
            call ESMF_ClockAdvance(tmp_clocks(j), rc=rc) 
            if(rc /= ESMF_SUCCESS) then
              print *, "Advancing the tmp clocks failed" 
              return
            end if
          end do 
        end if
      end if
    end do

    !print *, "The relative sync tsteps are :", sync_rel_tsteps

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
      !call ESMF_TimeIntervalPrint(alarm_interval, rc=rc)

      alarms(i) = ESMF_AlarmCreate(clocks(i), ringInterval=alarm_interval,&
                    stopTime=stop_time, ringTimeStepCount=1, rc=rc)
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
