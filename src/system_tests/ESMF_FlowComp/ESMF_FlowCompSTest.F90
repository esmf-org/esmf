
! $Id: ESMF_FlowCompSTest.F90,v 1.30 2009/10/28 02:05:36 theurich Exp $
!
! System test FlowComp
!  Description on Sourceforge under System Test #74558

!-------------------------------------------------------------------------
!ESMF_SYSTEM_removeTEST        String used by test script to count system tests.
!=========================================================================

!BOP
!
! !DESCRIPTION:
! System test FlowComp.  1 components solving fluid flow PDE's using.
! ESMF Infrastructure.
!
!\begin{verbatim}

    program FlowComp

#include "ESMF_Macros.inc"

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    use flowmod

    implicit none
    
    ! Local variables
    integer :: pet_id, npets, rc

    character(len=ESMF_MAXSTR) :: cname1
    type(ESMF_VM) :: vm
    type(ESMF_State) :: c1exp
    type(ESMF_GridComp) :: comp1

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final status message
    character(ESMF_MAXSTR) :: failMsg, finalMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! Set initial values
!
    rc = ESMF_FAILURE

    print *, "System Test FlowComp:"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!    Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize framework and get back the default global VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Find out how many PETs we were started with
    call ESMF_VMGet(vm, petCount=npets, localPET=pet_id, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    if (npets .lt. 4) then
        print *, "This system test needs to run at least 4-way, current np = ", npets
        goto 10
    endif


    ! Create the model component, giving it all PETs to run on
    cname1 = "fluid flow"
    comp1 = ESMF_GridCompCreate(name=cname1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    print *, "Created component ", trim(cname1), "rc =", rc 

    print *, "Comp Creates finished"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Register section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      call ESMF_GridCompSetServices(comp1, FlowMod_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create and initialize a clock.
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                              ESMF_CAL_GREGORIAN, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize time interval to 1 second
      call ESMF_TimeIntervalSet(time_step, s_r8=1.0D0, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize start time to 3/28/2003 10:00
      call ESMF_TimeSet(startTime, yy=2003, mm=3, dd=28, h=10, m=0, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize stop time to 3/28/2003 10:01
      call ESMF_TimeSet(stopTime, yy=2003, mm=3, dd=28, h=10, m=1, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize the clock with the above values
      clock = ESMF_ClockCreate("Clock 1", time_step, startTime, stopTime, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
 
      c1exp = ESMF_StateCreate("comp1 export", ESMF_STATE_EXPORT, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompInitialize(comp1, exportState=c1exp, clock=clock, &
                                   rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Component Initialize finished, rc =", rc
 
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        call ESMF_GridCompRun(comp1, exportState=c1exp, clock=clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        print *, "Component Run returned, rc =", rc

        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        !call ESMF_ClockPrint(clock, rc=rc)

      enddo

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(comp1, exportState=c1exp, clock=clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Component Finalize finished, rc =", rc

      print *, "---------------------------------------------------------------"
      print *, "---------------------------------------------------------------"
      print *, "Test finished, pet_id = ", pet_id
      print *, "---------------------------------------------------------------"
      print *, "---------------------------------------------------------------"

      print *, "Comp Finalize returned"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(c1exp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_ClockDestroy(clock, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_CalendarDestroy(gregorianCalendar, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_GridCompDestroy(comp1, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
10    print *, "System Test FlowComp complete."


      write(failMsg, *)  "System Test failure"
      write(testname, *) "System Test FlowComp: Fluid Solver, single component"
  
      if ((pet_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS: See output files for computed values"
        else
          write(finalMsg, *) "System Test did not succeed.  Error code ", rc
        endif
        write(0, *) ""
        write(0, *) trim(testname)
        write(0, *) trim(finalMsg)
        write(0, *) ""

      endif
    
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((rc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

      call ESMF_Finalize(rc=rc)

      end program FlowComp
    
!\end{verbatim}
    
