! $Id: ESMF_UserMain.F90,v 1.15 2007/06/23 04:01:03 cdeluca Exp $
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

    use UserIGridCompMod, only: User_SetServices => igrid_services
    use UserCplCompMod, only: User_SetServices => coupler_services
    
    implicit none
    
!   ! Local variables
    integer :: rc

    type(ESMF_IGridComp) :: topcomp
    type(ESMF_IGridComp) :: oceanigridcomp, atmigridcomp
    type(ESMF_CplComp) :: cplcomp

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: tclock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, stopTime
    integer(ESMF_KIND_I8) :: advanceCount

    type(ESMF_Config) :: tconfig
    type(ESMF_DELayout) :: tlayout

    type(ESMF_IGrid) :: igrid1, igrid2
    type(ESMF_State) :: atmimport, ocnexport

    character(ESMF_MAXSTR) :: tname, gname1, gname2, cname
    character(1024) :: configFile
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Create an top component which in turn creates 2 IGridded components and
!   !    a Coupler component.
 
    print *, "Application Test 1:"

    !-------------------------------------------------------------------------
    !  Create the top level IGridded Component

    tname = "Test Application"
    configFile="/home/nancy/models/startup.conf"
    topcomp = ESMF_IGridCompCreate(name=tname, configFile=configFile, rc=rc)

    ! Query for the layout and config objects
    call ESMF_IGridCompGet(topcomp, layout=tlayout, config=tconfig)

    ! Create the main application clock

    ! initialize calendar to be Gregorian type
    gregorianCalendar = ESMF_CalendarCreate("Gregorian", &
                                            ESMF_CAL_GREGORIAN, rc)

    ! initialize time interval to 6 hours
    call ESMF_TimeIntervalSet(timeStep, h=6, rc=rc)

    ! initialize start time to 5/1/2003
    call ESMF_TimeSet(startTime, yy=2003, mm=5, dd=1, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize stop time to 5/2/2003
    call ESMF_TimeSet(stopTime, yy=2003, mm=5, dd=2, &
                      calendar=gregorianCalendar, rc=rc)

    ! initialize the clock with the above values
    tclock = ESMF_ClockCreate(timeStep, startTime, stopTime, rc=rc)


    print *, "Top Component Create completed, name = ", trim(tname)


    !-------------------------------------------------------------------------
    !  Create the 2 igridded components

    igrid1 = ESMF_IGridCreate("ocean igrid", rc=rc)
    gname1 = "Ocean"
    oceanigridcomp = ESMF_IGridCompCreate(name=gname1, layout=tlayout, &
                                    mtype=ESMF_OCEAN, igrid=igrid1, &
                                    config=tconfig, rc=rc)  

    print *, "IGrid Comp Create completed, name = ", trim(gname1)

    igrid2 = ESMF_IGridCreate("atm igrid", rc=rc)
    gname2 = "Atmosphere"
    atmigridcomp = ESMF_IGridCompCreate(name=gname2, layout=tlayout, &
                                    mtype=ESMF_ATM, igrid=igrid2, &
                                    config=tconfig, rc=rc)  

    print *, "IGrid Comp Create completed, name = ", trim(gname2)

    !-------------------------------------------------------------------------
    !  Create the coupler

    cname = "Ocean2Atmosphere"
    cplcomp = ESMF_CplCompCreate(name=cname, layout=tlayout, config=tconfig, &
                                 rc=rc)  

    print *, "Cpl Comp Create completed, name = ", trim(cname)

    !-------------------------------------------------------------------------
    !  Register the entry points for each component

    call ESMF_IGridCompSetServices(oceanigridcomp, igrid_services, rc)
    call ESMF_IGridCompSetServices(atmigridcomp, igrid_services, rc)
    call ESMF_CplCompSetServices(cplcomp, cpl_services, rc)

    !-------------------------------------------------------------------------
    !  Create the States

    atmimport = ESMF_StateCreate("atmosphere import", ESMF_IMPORTSTATE, rc=rc)
    ocnexport = ESMF_StateCreate("ocean export", ESMF_EXPORTSTATE, rc=rc)
 
    !-------------------------------------------------------------------------
    !  Initialize each component
    !
    !   Full Init argument list is:
    !   call ESMF_IGridCompInitialize(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompInitialize(comp, import, export, clock, phase, rc)


    call ESMF_IGridCompInitialize(oceanigridcomp, exportState=ocnexport, &
                                                    clock=tclock, rc=rc)
    call ESMF_IGridCompInitialize(atmigridcomp, atmimport, clock=tclock, rc=rc)
    call ESMF_CplCompInitialize(cplcomp, ocnexport, atmimport, tclock, rc=rc)
    print *, "Initialization complete"


    !-------------------------------------------------------------------------
    !  Run the main loop
    !
    !   Full Run argument list is:
    !   call ESMF_IGridCompRun(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompRun(comp, import, export, clock, phase, rc)

    do
   
        call ESMF_IGridCompRun(oceanigridcomp, exportState=ocnexport, 
                                                         clock=tclock, rc=rc)

        call ESMF_CplCompRun(cplcomp, ocnexport, atmimport, tclock, rc=rc)

        call ESMF_IGridCompRun(atmigridcomp, atmimport, clock=tclock, rc=rc)


        call ESMF_ClockAdvance(aclock)

        if (ESMF_ClockIsStopTime(aclock) exit

    enddo

    print *, "Main Run loop complete"

    !-------------------------------------------------------------------------
    !  Finalize each component
    !
    !   Full Final argument list is:
    !   call ESMF_IGridCompFinalize(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompFinalize(comp, import, export, clock, phase, rc)


    call ESMF_IGridCompFinalize(oceanigridcomp, exportState=ocnexport, &
                                                    clock=tclock, rc=rc)
    call ESMF_CplCompFinalize(cplcomp, ocnexport, atmimport, clock=tclock, &
                                                                   rc=rc)
    call ESMF_IGridCompFinalize(component=atmigridcomp, importState=atmimport, &
                                                    clock=tclock, rc=rc)
    print *, "Finalization complete"


    !-------------------------------------------------------------------------
    !  Destroy each component
    !

    call ESMF_ClockDestroy(tclock, rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc)

    call ESMF_IGridCompDestroy(oceanigridcomp, rc)
    call ESMF_IGridCompDestroy(atmigridcomp, rc)
    call ESMF_CplCompDestroy(cplcomp, rc)

    call ESMF_IGridCompDestroy(topcomp, rc)

    print *, "Cleanup complete"

    !-------------------------------------------------------------------------

    print *, "Application Test 1 finished."


    end program User_Application
    
!\end{verbatim}
    
