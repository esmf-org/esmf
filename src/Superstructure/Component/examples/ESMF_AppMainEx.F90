! $Id: ESMF_AppMainEx.F90,v 1.8 2003/04/04 15:13:16 nscollins Exp $
!
! Example code for a main program Application. 

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Example of a main program Application.
!
!
!\begin{verbatim}

!   ! Example main program showing calls to the Application Component routines.
!   ! An Application Component is not nestable inside a larger application.

    program ESMF_AppMainEx
    
!   ! The ESMF Framework module
    use ESMF_Mod
    
    ! User supplied modules
    use PHYS_Mod, only: PHYS_SetServices
    use DYNM_Mod, only: DYNM_SetServices
    use CPLR_Mod, only: CPLR_SetServices
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    integer :: timestep, timeend
    logical :: finished
    type(ESMF_Clock) :: clock
    integer :: delistall(18), delist1(8), delist2(8), delist3(2)
    character(ESMF_MAXSTR) :: cname
    type(ESMF_DELayout) :: layoutall, layout1, layout2, layout3
    type(ESMF_State) :: states(2)
    type(ESMF_AppComp) :: app
    type(ESMF_GridComp) :: gcomp1, gcomp2, cpl
    type(ESMF_CplComp) :: cpl
        
!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
 
    print *, "Application Example 1:"

    ! Create the top level application component

    cname = "Top Level Application"
    app = ESMF_AppCompCreate(cname, configfile="/home/myname/model1/setup", rc=rc)  

    delist1 = (/ (i, i=0,7) /)
    layout1 = ESMF_DELayoutCreate(2, 4, delist1, ESMF_XFAST, rc)

    cname = "Atmosphere Physics"
    comp1 = ESMF_GridCompCreate(cname, layout1, ESMF_ATM, rc=rc)  

    ! This single user-supplied subroutine must be a public entry point 
    !  and can renamed with the 'use localname => modulename' syntax if
    !  the name is not unique.
    call ESMF_GridCompSetServices(comp1, PHYS_SetServices, rc=rc)

    ! (see below for what the SetServices routine will need to do.)

    print *, "Comp Create returned, name = ", trim(cname)

    delist2 = (/ (i, i=8,15) /)
    layout2 = ESMF_DELayoutCreate(2, 4, delist2, ESMF_XFAST, rc)

    cname = "Atmosphere Dynamics"
    comp2 = ESMF_GridCompCreate(cname, layout2, ESMF_ATM, rc=rc)

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_GridCompSetServices(comp1, DYNM_SetServices, rc=rc)

    print *, "Comp Create returned, name = ", trim(cname)

    delist3 = (/ (i, i=0,16) /)
    layout3 = ESMF_DELayoutCreate(1, 16, delist3, ESMF_XFAST, rc)

    cname = "Atmosphere Coupler"
    cpl = ESMF_CplCompCreate(cname, layout3, rc=rc)

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_CplCompSetServices(comp1, CPLR_SetServices, rc=rc)

    print *, "Comp Create returned, name = ", trim(cname)

    ! Create the necessary import and export states used to pass data
    !  between components.

    states(1) = ESMF_StateCreate(cname1, ESMF_EXPORTSTATE, rc=rc)
    states(2) = ESMF_StateCreate(cname2, ESMF_IMPORTSTATE, rc=rc)

    ! See the TimeMgr document for the details on the actual code needed
    !  to set up a clock.
    clock = ESMF_ClockInit()
     
    ! Call each Init routine in turn.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_GridCompInitialize(comp1, exportstate=states(1), clock=clock, rc=rc)
    call ESMF_GridCompInitialize(comp2, importstate=states(2), clock=clock, rc=rc)
    call ESMF_CplCompInitialize(cpl, statelist=states, clock=clock, rc=rc)
    print *, "Comp Initialize complete"


    ! Main run loop.
    finished = .false.
    do while (.not. finished)

        call ESMF_GridCompRun(comp1, exportstate=states(1), clock=clock, rc=rc)
        call ESMF_CplCompRun(cpl, statelist=states, clock=clock, rc=rc)
        call ESMF_GridCompRun(comp2, importstate=states(2), clock=clock, rc=rc)
   
        call ESMF_ClockAdvance(clock, timesteps)

        ! query clock for current time
        if (ESMF_ClockIsStopTime(clock) finished = .true.

    enddo
    print *, "Comp Run complete"


    ! Give each component a chance to write out final results, clean up.
    ! Call each Init routine in turn.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_GridCompFinalize(comp1, exportstate=states(1), clock=clock, rc=rc)
    call ESMF_GridCompFinalize(comp2, importstate=states(2), clock=clock, rc=rc)
    call ESMF_CplCompFinalize(cpl, statelist=states, clock=clock, rc=rc)
    print *, "Comp Finalize complete"


    ! Destroy components.
    call ESMF_GridCompDestroy(comp1, rc)
    call ESMF_GridCompDestroy(comp2, rc)
    call ESMF_CplCompDestroy(cpl, rc)
    print *, "Comp Destroy returned"


    print *, "Application Example 1 finished"


    end program ESMF_AppMainEx
    

    ! Each Component must supply a SetServices routine which makes the
    !  following types of calls:
    !
    !! call ESMF_GridCompSetEntryPoint(comp1, ESMF_SETINIT, PHYS_Init, 1, rc)
    !! call ESMF_GridCompSetEntryPoint(comp1, ESMF_SETINIT, PHYS_InitPhase2, 2, rc)
    !! call ESMF_GridCompSetEntryPoint(comp1, ESMF_SETRUN, PHYS_Run, 0, rc)
    !! call ESMF_GridCompSetEntryPoint(comp1, ESMF_SETFINAL, PHYS_Final, 0, rc)
    !
    ! The arguments are: the component, the type of routine, 
    !  the name of the internal subroutine which contains the user code, 
    !  and a "phase" or index number to support multiple entry points 
    !  of the same type for codes which need to compute part of the process
    !  and then allow another component to run before completing the function.

!\end{verbatim}
    
