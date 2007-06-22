! $Id: ESMF_UserMain.F90,v 1.14 2007/06/22 23:21:49 cdeluca Exp $
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

    use UserInternGridCompMod, only: User_SetServices => interngrid_services
    use UserCplCompMod, only: User_SetServices => coupler_services
    
    implicit none
    
!   ! Local variables
    integer :: rc

    type(ESMF_InternGridComp) :: topcomp
    type(ESMF_InternGridComp) :: oceaninterngridcomp, atminterngridcomp
    type(ESMF_CplComp) :: cplcomp

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: tclock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime, stopTime
    integer(ESMF_KIND_I8) :: advanceCount

    type(ESMF_Config) :: tconfig
    type(ESMF_DELayout) :: tlayout

    type(ESMF_InternGrid) :: interngrid1, interngrid2
    type(ESMF_State) :: atmimport, ocnexport

    character(ESMF_MAXSTR) :: tname, gname1, gname2, cname
    character(1024) :: configFile
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Create an top component which in turn creates 2 InternGridded components and
!   !    a Coupler component.
 
    print *, "Application Test 1:"

    !-------------------------------------------------------------------------
    !  Create the top level InternGridded Component

    tname = "Test Application"
    configFile="/home/nancy/models/startup.conf"
    topcomp = ESMF_InternGridCompCreate(name=tname, configFile=configFile, rc=rc)

    ! Query for the layout and config objects
    call ESMF_InternGridCompGet(topcomp, layout=tlayout, config=tconfig)

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
    !  Create the 2 interngridded components

    interngrid1 = ESMF_InternGridCreate("ocean interngrid", rc=rc)
    gname1 = "Ocean"
    oceaninterngridcomp = ESMF_InternGridCompCreate(name=gname1, layout=tlayout, &
                                    mtype=ESMF_OCEAN, interngrid=interngrid1, &
                                    config=tconfig, rc=rc)  

    print *, "InternGrid Comp Create completed, name = ", trim(gname1)

    interngrid2 = ESMF_InternGridCreate("atm interngrid", rc=rc)
    gname2 = "Atmosphere"
    atminterngridcomp = ESMF_InternGridCompCreate(name=gname2, layout=tlayout, &
                                    mtype=ESMF_ATM, interngrid=interngrid2, &
                                    config=tconfig, rc=rc)  

    print *, "InternGrid Comp Create completed, name = ", trim(gname2)

    !-------------------------------------------------------------------------
    !  Create the coupler

    cname = "Ocean2Atmosphere"
    cplcomp = ESMF_CplCompCreate(name=cname, layout=tlayout, config=tconfig, &
                                 rc=rc)  

    print *, "Cpl Comp Create completed, name = ", trim(cname)

    !-------------------------------------------------------------------------
    !  Register the entry points for each component

    call ESMF_InternGridCompSetServices(oceaninterngridcomp, interngrid_services, rc)
    call ESMF_InternGridCompSetServices(atminterngridcomp, interngrid_services, rc)
    call ESMF_CplCompSetServices(cplcomp, cpl_services, rc)

    !-------------------------------------------------------------------------
    !  Create the States

    atmimport = ESMF_StateCreate("atmosphere import", ESMF_IMPORTSTATE, rc=rc)
    ocnexport = ESMF_StateCreate("ocean export", ESMF_EXPORTSTATE, rc=rc)
 
    !-------------------------------------------------------------------------
    !  Initialize each component
    !
    !   Full Init argument list is:
    !   call ESMF_InternGridCompInitialize(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompInitialize(comp, import, export, clock, phase, rc)


    call ESMF_InternGridCompInitialize(oceaninterngridcomp, exportState=ocnexport, &
                                                    clock=tclock, rc=rc)
    call ESMF_InternGridCompInitialize(atminterngridcomp, atmimport, clock=tclock, rc=rc)
    call ESMF_CplCompInitialize(cplcomp, ocnexport, atmimport, tclock, rc=rc)
    print *, "Initialization complete"


    !-------------------------------------------------------------------------
    !  Run the main loop
    !
    !   Full Run argument list is:
    !   call ESMF_InternGridCompRun(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompRun(comp, import, export, clock, phase, rc)

    do
   
        call ESMF_InternGridCompRun(oceaninterngridcomp, exportState=ocnexport, 
                                                         clock=tclock, rc=rc)

        call ESMF_CplCompRun(cplcomp, ocnexport, atmimport, tclock, rc=rc)

        call ESMF_InternGridCompRun(atminterngridcomp, atmimport, clock=tclock, rc=rc)


        call ESMF_ClockAdvance(aclock)

        if (ESMF_ClockIsStopTime(aclock) exit

    enddo

    print *, "Main Run loop complete"

    !-------------------------------------------------------------------------
    !  Finalize each component
    !
    !   Full Final argument list is:
    !   call ESMF_InternGridCompFinalize(comp, import, export, clock, phase, rc)
    !   call ESMF_CplCompFinalize(comp, import, export, clock, phase, rc)


    call ESMF_InternGridCompFinalize(oceaninterngridcomp, exportState=ocnexport, &
                                                    clock=tclock, rc=rc)
    call ESMF_CplCompFinalize(cplcomp, ocnexport, atmimport, clock=tclock, &
                                                                   rc=rc)
    call ESMF_InternGridCompFinalize(component=atminterngridcomp, importState=atmimport, &
                                                    clock=tclock, rc=rc)
    print *, "Finalization complete"


    !-------------------------------------------------------------------------
    !  Destroy each component
    !

    call ESMF_ClockDestroy(tclock, rc)
    call ESMF_CalendarDestroy(gregorianCalendar, rc)

    call ESMF_InternGridCompDestroy(oceaninterngridcomp, rc)
    call ESMF_InternGridCompDestroy(atminterngridcomp, rc)
    call ESMF_CplCompDestroy(cplcomp, rc)

    call ESMF_InternGridCompDestroy(topcomp, rc)

    print *, "Cleanup complete"

    !-------------------------------------------------------------------------

    print *, "Application Test 1 finished."


    end program User_Application
    
!\end{verbatim}
    
