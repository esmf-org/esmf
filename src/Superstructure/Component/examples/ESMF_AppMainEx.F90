! $Id: ESMF_AppMainEx.F90,v 1.22 2004/05/26 11:12:01 nscollins Exp $
!
! Example code for a main Application program. 

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Example of a main Application program.
!
!
!\begin{verbatim}

!   ! Example main program showing calls to the top level
!   ! Gridded Component routines.

    module PHYS_mod
    use ESMF_Mod
    public PHYS_SetServices
    contains

    subroutine PHYS_SetServices(gcomp, rc)
      type(ESMF_GridComp) :: gcomp
      integer :: rc

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)
      
    end subroutine PHYS_SetServices
      
    subroutine my_init(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "PHYS initialize routine called"
      rc = ESMF_SUCCESS

    end subroutine my_init

    subroutine my_run(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "PHYS run routine called"
      rc = ESMF_SUCCESS

    end subroutine my_run

    subroutine my_final(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "PHYS finalize routine called"
      rc = ESMF_SUCCESS

    end subroutine my_final

    end module

    module DYNM_mod

    use ESMF_Mod
    public DYNM_SetServices
    contains

    subroutine DYNM_SetServices(gcomp, rc)
      type(ESMF_GridComp) :: gcomp
      integer :: rc

       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)
      
    end subroutine DYNM_SetServices
      
    subroutine my_init(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "DYNM initialize routine called"
      rc = ESMF_SUCCESS

    end subroutine my_init

    subroutine my_run(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "DYNM run routine called"
      rc = ESMF_SUCCESS

    end subroutine my_run

    subroutine my_final(gcomp, importState, exportState, externalclock, rc)
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "DYNM finalize routine called"
      rc = ESMF_SUCCESS

    end subroutine my_final

    end module

    module CPLR_mod

    use ESMF_Mod

    public CPLR_SetServices
    contains

    subroutine CPLR_SetServices(cpl, rc)
      type(ESMF_CplComp) :: cpl
      integer :: rc

       call ESMF_CplCompSetEntryPoint(cpl, ESMF_SETINIT, my_init, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_CplCompSetEntryPoint(cpl, ESMF_SETRUN, my_run, &
                                                     ESMF_SINGLEPHASE, rc)
       call ESMF_CplCompSetEntryPoint(cpl, ESMF_SETFINAL, my_final, &
                                                     ESMF_SINGLEPHASE, rc)
      
    end subroutine CPLR_SetServices
      
    subroutine my_init(cpl, importStatelist, exportStatelist, externalclock, rc)
      type(ESMF_CplComp) :: cpl
      type(ESMF_State) :: importStatelist
      type(ESMF_State) :: exportStatelist
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "CPLR initialize routine called"
      rc = ESMF_SUCCESS

    end subroutine my_init

    subroutine my_run(cpl, importStatelist, exportStatelist, externalclock, rc)
      type(ESMF_CplComp) :: cpl
      type(ESMF_State) :: importStatelist
      type(ESMF_State) :: exportStatelist
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "CPLR run routine called"
      rc = ESMF_SUCCESS

    end subroutine my_run

    subroutine my_final(cpl, importStatelist, exportStatelist, externalclock, rc)
      type(ESMF_CplComp) :: cpl
      type(ESMF_State) :: importStatelist
      type(ESMF_State) :: exportStatelist
      type(ESMF_Clock) :: externalclock
      integer :: rc

      print *, "CPLR finalize routine called"
      rc = ESMF_SUCCESS

    end subroutine my_final

    end module

    program ESMF_AppMainEx
    
!   ! The ESMF Framework module
    use ESMF_Mod
    
    ! User supplied modules
    use PHYS_Mod, only: PHYS_SetServices
    use DYNM_Mod, only: DYNM_SetServices
    use CPLR_Mod, only: CPLR_SetServices
    implicit none
    
!   ! Local variables
    integer :: rc
    logical :: finished
    type(ESMF_Clock) :: tclock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, stopTime
    character(ESMF_MAXSTR) :: cname, cname1, cname2
    type(ESMF_VM) :: vm
    type(ESMF_State) :: states(2)
    type(ESMF_GridComp) :: top
    type(ESMF_GridComp) :: gcomp1, gcomp2
    type(ESMF_CplComp) :: cpl
        
!-------------------------------------------------------------------------
!   ! Initialize the Framework, and get the default VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
        print *, "failed to initialize ESMF Framework"
        stop
    endif

!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
 
    print *, "Application Example 1:"

    ! Create the top level application component

    cname = "Top Level Atmosphere Model Component"
    top = ESMF_GridCompCreate(cname, configFile="setup.rc", rc=rc)  
    cname1 = "Atmosphere Physics"
    gcomp1 = ESMF_GridCompCreate(cname1, ESMF_ATM, rc=rc)  

    ! This single user-supplied subroutine must be a public entry point 
    !  and can renamed with the 'use localname => modulename' syntax if
    !  the name is not unique.
    call ESMF_GridCompSetServices(gcomp1, PHYS_SetServices, rc)

    ! (see below for what the SetServices routine will need to do.)

    print *, "Comp Create returned, name = ", trim(cname1)

    cname2 = "Atmosphere Dynamics"
    gcomp2 = ESMF_GridCompCreate(cname2, ESMF_ATM, rc=rc)

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_GridCompSetServices(gcomp2, DYNM_SetServices, rc)

    print *, "Comp Create returned, name = ", trim(cname2)

    cname = "Atmosphere Coupler"
    cpl = ESMF_CplCompCreate(cname, rc=rc)

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_CplCompSetServices(cpl, CPLR_SetServices, rc)

    print *, "Comp Create returned, name = ", trim(cname)

    ! Create the necessary import and export states used to pass data
    !  between components.

    states(1) = ESMF_StateCreate(cname1, ESMF_STATEEXPORT, rc=rc)
    states(2) = ESMF_StateCreate(cname2, ESMF_STATEIMPORT, rc=rc)
    ! See the TimeMgr document for the details on the actual code needed
    !  to set up a clock.
    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate("Gregorian", ESMF_CAL_GREGORIAN, rc)

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=rc)

    ! initialize start time to 5/1/2003
    call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize stop time to 5/2/2003
    call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=2, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize the clock with the above values
    tclock = ESMF_ClockCreate("top clock", timeStep, startTime, stopTime, rc=rc)
     
    ! Call each Init routine in turn.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_GridCompInitialize(gcomp1, exportState=states(1), clock=tclock, &
                                 rc=rc)
    call ESMF_GridCompInitialize(gcomp2, importState=states(2), clock=tclock, &
                                 rc=rc)
    call ESMF_CplCompInitialize(cpl, states(1), states(2), clock=tclock, rc=rc)
    print *, "Comp Initialize complete"

    ! Main run loop.
    finished = .false.
    do while (.not. finished)
        call ESMF_GridCompRun(gcomp1, exportState=states(1), clock=tclock, rc=rc)
        call ESMF_CplCompRun(cpl, importState=states(1), &
                                  exportState=states(2), clock=tclock, rc=rc)
        call ESMF_GridCompRun(gcomp2, importState=states(2), clock=tclock, rc=rc)
        call ESMF_ClockAdvance(tclock, timestep)
        ! query clock for current time
        if (ESMF_ClockIsStopTime(tclock)) finished = .true.
    enddo
    print *, "Comp Run complete"

    ! Give each component a chance to write out final results, clean up.
    ! Call each Finalize routine in turn.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_GridCompFinalize(gcomp1, exportState=states(1), clock=tclock, rc=rc)
    call ESMF_GridCompFinalize(gcomp2, importState=states(2), clock=tclock, rc=rc)
    call ESMF_CplCompFinalize(cpl, states(1), states(2), clock=tclock, rc=rc)
    print *, "Comp Finalize complete"

    ! Destroy components.
    call ESMF_ClockDestroy(tclock, rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc)
    call ESMF_GridCompDestroy(gcomp1, rc)
    call ESMF_GridCompDestroy(gcomp2, rc)
    call ESMF_CplCompDestroy(cpl, rc)
    print *, "Comp Destroy returned"

    print *, "Application Example 1 finished"

    call ESMF_Finalize(rc=rc)

    end program ESMF_AppMainEx

    ! Each Component must supply a SetServices routine which makes the
    !  following types of calls:
    !
    !! call ESMF_GridCompSetEntryPoint(gcomp1, ESMF_SETINIT, PHYS_Init, 1, rc)
    !! call ESMF_GridCompSetEntryPoint(gcomp1, ESMF_SETINIT, PHYS_InitPhase2, 2, rc)
    !! call ESMF_GridCompSetEntryPoint(gcomp1, ESMF_SETRUN, PHYS_Run, 0, rc)
    !! call ESMF_GridCompSetEntryPoint(gcomp1, ESMF_SETFINAL, PHYS_Final, 0, rc)
    !
    ! The arguments are: the component, the type of routine, 
    !  the name of the internal subroutine which contains the user code, 
    !  and a "phase" or index number to support multiple entry points 
    !  of the same type for codes which need to compute part of the process
    !  and then allow another component to run before completing the function.

!\end{verbatim}
    
