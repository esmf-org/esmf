! $Id: ESMF_AppMainEx.F90,v 1.11 2004/01/06 17:50:11 svasquez Exp $
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

    public PHYS_SetServices

    contains

    subroutine PHYS_SetServices()
    end subroutine

    end module


    module DYNM_mod

    public DYNM_SetServices

    contains

    subroutine DYNM_SetServices()
    end subroutine

    end module


    module CPLR_mod

    public CPLR_SetServices

    contains

    subroutine CPLR_SetServices()
    end subroutine

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
    integer :: x, y, i, rc
    logical :: finished
    type(ESMF_Clock) :: tclock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, stopTime
    integer :: delistall(18), delist1(8), delist2(8), delist3(16)
    character(ESMF_MAXSTR) :: cname, cname1, cname2
    type(ESMF_DELayout) :: layoutall, layout1, layout2, layout3
    type(ESMF_State) :: states(2), bothstates
    type(ESMF_GridComp) :: top
    type(ESMF_GridComp) :: gcomp1, gcomp2
    type(ESMF_CplComp) :: cpl
        
!-------------------------------------------------------------------------
!   ! Initialize the Framework

    call ESMF_Initialize(rc=rc)

!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
 
    print *, "Application Example 1:"

    ! Create the top level application component

    cname = "Top Level Application"
    top = ESMF_GridCompCreate(cname, configfile="/home/myname/model1/setup", rc=rc)  

    delist1 = (/ (i, i=0,7) /)
    layout1 = ESMF_DELayoutCreate(delist1, 2, (/ 2, 4 /), (/ 0, 0 /), rc)

    cname1 = "Atmosphere Physics"
    gcomp1 = ESMF_GridCompCreate(cname1, layout1, ESMF_ATM, rc=rc)  

    ! This single user-supplied subroutine must be a public entry point 
    !  and can renamed with the 'use localname => modulename' syntax if
    !  the name is not unique.
    call ESMF_GridCompSetServices(gcomp1, PHYS_SetServices, rc)

    ! (see below for what the SetServices routine will need to do.)

    print *, "Comp Create returned, name = ", trim(cname1)

    delist2 = (/ (i, i=8,15) /)
    layout2 = ESMF_DELayoutCreate(delist1, 2, (/ 2, 4 /), (/ 2, 0 /), rc)

    cname2 = "Atmosphere Dynamics"
    gcomp2 = ESMF_GridCompCreate(cname2, layout2, ESMF_ATM, rc=rc)

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_GridCompSetServices(gcomp1, DYNM_SetServices, rc)

    print *, "Comp Create returned, name = ", trim(cname2)

    delist3 = (/ (i, i=0,15) /)
    layout3 = ESMF_DELayoutCreate(delist1, 2, (/ 4, 4 /), (/ 0, 0 /), rc)

    cname = "Atmosphere Coupler"
    cpl = ESMF_CplCompCreate(cname, layout3, rc=rc)

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_CplCompSetServices(gcomp1, CPLR_SetServices, rc)

    print *, "Comp Create returned, name = ", trim(cname)

    ! Create the necessary import and export states used to pass data
    !  between components.

    states(1) = ESMF_StateCreate(cname1, ESMF_STATEEXPORT, rc=rc)
    states(2) = ESMF_StateCreate(cname2, ESMF_STATEIMPORT, rc=rc)
    bothstates = ESMF_StateCreate(cname, rc=rc)
    call ESMF_StateAddData(bothstates, 2, states, rc)

    ! See the TimeMgr document for the details on the actual code needed
    !  to set up a clock.
    ! initialize calendar to be Gregorian type
    call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=rc)

    ! initialize start time to 5/1/2003
    call ESMF_TimeSet(startTime, yr=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize stop time to 5/2/2003
    call ESMF_TimeSet(stopTime, yr=2003, mm=5, dd=2, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize the clock with the above values
    tclock = ESMF_ClockCreate("top clock", timeStep, startTime, stopTime, rc=rc)
     
    ! Call each Init routine in turn.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_GridCompInitialize(gcomp1, exportstate=states(1), clock=tclock, &
                                 rc=rc)
    call ESMF_GridCompInitialize(gcomp2, importstate=states(2), clock=tclock, &
                                 rc=rc)
    call ESMF_CplCompInitialize(cpl, statelist=bothstates, clock=tclock, rc=rc)
    print *, "Comp Initialize complete"


    ! Main run loop.
    finished = .false.
    do while (.not. finished)

        call ESMF_GridCompRun(gcomp1, exportstate=states(1), clock=tclock, rc=rc)
        call ESMF_CplCompRun(cpl, statelist=bothstates, clock=tclock, rc=rc)
        call ESMF_GridCompRun(gcomp2, importstate=states(2), clock=tclock, rc=rc)
   
        call ESMF_ClockAdvance(tclock, timestep)

        ! query clock for current time
        if (ESMF_ClockIsStopTime(tclock)) finished = .true.

    enddo
    print *, "Comp Run complete"


    ! Give each component a chance to write out final results, clean up.
    ! Call each Init routine in turn.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_GridCompFinalize(gcomp1, exportstate=states(1), clock=tclock, rc=rc)
    call ESMF_GridCompFinalize(gcomp2, importstate=states(2), clock=tclock, rc=rc)
    call ESMF_CplCompFinalize(cpl, statelist=bothstates, clock=tclock, rc=rc)
    print *, "Comp Finalize complete"


    ! Destroy components.
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
    
