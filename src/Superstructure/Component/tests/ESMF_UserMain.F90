! $Id: ESMF_UserMain.F90,v 1.3 2003/04/04 15:38:36 nscollins Exp $
!
! Test code which creates a new Application Component. 
!   Expects to be compiled with ESMF_UserCComp.F90 and ESMF_UserGComp.F90

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Tests of Application Create code.
!
!
!\begin{verbatim}

    program User_Application
    
!   ! ESMF Framework module
    use ESMF_Mod

    use UserGridCompMod, only: User_SetServices => grid_services
    use UserCplCompMod, only: User_SetServices => coupler_services
    
    implicit none
    
!   ! Local variables
    integer :: rc

    type(ESMF_AppComp) :: appcomp
    type(ESMF_GridComp) :: oceangridcomp, atmgridcomp
    type(ESMF_CplComp) :: cplcomp

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: aclock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, stopTime
    integer(ESMF_IKIND_I8) :: advanceCount

    type(ESMF_Config) :: aconfig
    type(ESMF_DELayout) :: alayout

    type(ESMF_Grid) :: grid1, grid2
    type(ESMF_State) :: atmimport, ocnexport, cplstates(2)

    character(ESMF_MAXSTR) :: aname, gname1, gname2, cname
    character(1024) :: configfile
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Create an Application which in turn creates 2 Gridded components and
!   !    a Coupler component.
 
    print *, "Application Test 1:"

    !-------------------------------------------------------------------------
    !  Create the Application

    aname = "Test Application"
    configfile="/home/nancy/models/startup.conf"
    appcomp = ESMF_AppCompCreate(name=aname, configfile=configfile, rc=rc)

    ! Query for the layout and config objects
    call ESMF_AppGet(appcomp, layout=alayout, config=aconfig)

    ! Create the application clock

    ! initialize calendar to be Gregorian type
    call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalInit(timeStep, H=6, rc=rc)

    ! initialize start time to 3/28/2003
    call ESMF_TimeInit(startTime, YR=2003, MM=5, DD=1, &
                       cal=gregorianCalendar, rc=rc)

    ! initialize stop time to 3/29/2003
    call ESMF_TimeInit(stopTime, YR=2003, MM=5, DD=2, &
                       cal=gregorianCalendar, rc=rc)

    ! initialize the clock with the above values
    call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)


    print *, "App Comp Create completed, name = ", trim(aname)

    !-------------------------------------------------------------------------
    !  Create the 2 gridded components

    grid1 = ESMF_GridCreate("ocean grid", rc=rc)
    gname1 = "Ocean"
    oceangridcomp = ESMF_GridCompCreate(name=gname1, layout=alayout, &
                                    mtype=ESMF_OCEAN, grid=grid1, &
                                    config=aconfig, rc=rc)  

    print *, "Grid Comp Create completed, name = ", trim(gname1)

    grid2 = ESMF_GridCreate("atm grid", rc=rc)
    gname2 = "Atmosphere"
    atmgridcomp = ESMF_GridCompCreate(name=gname2, layout=alayout, &
                                    mtype=ESMF_ATM, grid=grid2, &
                                    config=aconfig, rc=rc)  

    print *, "Grid Comp Create completed, name = ", trim(gname2)

    !-------------------------------------------------------------------------
    !  Create the coupler

    cname = "Ocean2Atmosphere"
    cplcomp = ESMF_CplCompCreate(name=cname, layout=alayout, config=aconfig, &
                                 rc=rc)  

    print *, "Cpl Comp Create completed, name = ", trim(cname)

    !-------------------------------------------------------------------------
    !  Register the entry points for each component

    call ESMF_GridCompSetServices(oceangridcomp, grid_services, rc)
    call ESMF_GridCompSetServices(atmgridcomp, grid_services, rc)
    call ESMF_CplCompSetServices(cplcomp, cpl_services, rc)

    !-------------------------------------------------------------------------
    !  Create the States

    atmimport = ESMF_StateCreate(ESMF_IMPORTSTATE, "atmosphere import")
    ocnexport = ESMF_StateCreate(ESMF_EXPORTSTATE, "ocean export")
    cplstates(1) = ocnexport
    cplstates(2) = atmimport
 
    !-------------------------------------------------------------------------
    !  Initialize each component
    !
    !   Full Init argument list is:
    !   call ESMF_GridCompInitialize(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompInitialize(comp, statelist, clock, phase, rc)


    call ESMF_GridCompInitialize(oceangridcomp, exportstate=ocnexport, &
                                                    clock=aclock, rc=rc)
    call ESMF_GridCompInitialize(atmgridcomp, atmimport, clock=aclock, rc=rc)
    call ESMF_CplCompInitialize(cplcomp, cplstates, aclock, rc=rc)
    print *, "Initialization complete"


    !-------------------------------------------------------------------------
    !  Run the main loop
    !
    !   Full Run argument list is:
    !   call ESMF_GridCompRun(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompRun(comp, statelist, clock, phase, rc)

    do
   
        call ESMF_GridCompRun(oceangridcomp, exportstate=ocnexport, 
                                                         clock=aclock, rc=rc)

        call ESMF_CplCompRun(cplcomp, cplstates, aclock, rc=rc)

        call ESMF_GridCompRun(atmgridcomp, atmimport, clock=aclock, rc=rc)


        call ESMF_ClockAdvance(aclock)

        if (ESMF_ClockIsStopTime(aclock) exit

    enddo

    print *, "Main Run loop complete"

    !-------------------------------------------------------------------------
    !  Finalize each component
    !
    !   Full Final argument list is:
    !   call ESMF_GridCompFinalize(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompFinalize(comp, statelist, clock, phase, rc)


    call ESMF_GridCompFinalize(component=oceangridcomp, exportstate=ocnexport, &
                                                    clock=aclock, rc=rc)
    call ESMF_CplCompFinalize(cplcomp, statelist=cplstates, clock=aclock, &
                                                                   rc=rc)
    call ESMF_GridCompFinalize(component=atmgridcomp, importstate=atmimport, &
                                                    clock=aclock, rc=rc)
    print *, "Finalization complete"


    !-------------------------------------------------------------------------
    !  Destroy each component
    !

    call ESMF_GridCompDestroy(oceangridcomp, rc)
    call ESMF_GridCompDestroy(atmgridcomp, rc)
    call ESMF_CplCompDestroy(cplcomp, rc)

    call ESMF_AppCompDestroy(appcomp, rc)

    print *, "Cleanup complete"

    !-------------------------------------------------------------------------

    print *, "Application Test 1 finished."


    end program User_Application
    
!\end{verbatim}
    
