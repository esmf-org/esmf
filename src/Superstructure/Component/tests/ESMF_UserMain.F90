! $Id: ESMF_UserMain.F90,v 1.2 2003/04/03 22:43:58 nscollins Exp $
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
    
!   ! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_Mod

    use UserGridCompMod, only: User_SetServices => grid_services
    use UserCplCompMod, only: User_SetServices => coupler_services
    
    implicit none
    
!   ! Local variables
    integer :: rc

    type(ESMF_AppComp) :: appcomp
    type(ESMF_GridComp) :: oceangridcomp, atmgridcomp
    type(ESMF_CplComp) :: cplcomp

    type(ESMF_Clock) :: aclock
    type(ESMF_Config) :: aconfig
    type(ESMF_DELayout) :: alayout

    type(ESMF_Grid) :: grid1, grid2
    type(ESMF_State) :: atmimport, ocnexport, cplstates(2)

    character(ESMF_MAXSTR) :: aname, gname1, gname2, cname
    character(1024) :: afilepath
        
!-------------------------------------------------------------------------
!   ! Test 1:
!   !
!   !  Create an Application which in turn creates 2 Gridded components and
!   !    a Coupler component.
 
    print *, "Application Test 1:"

    !-------------------------------------------------------------------------
    !  Create the Application

    aname = "Test Application"
    afilepath="/home/nancy/models/startup.conf"
    appcomp = ESMF_AppCompCreate(name=aname, filepath=afilepath, rc=rc)

    ! Query for the layout and config objects
    call ESMF_AppGet(appcomp, layout=alayout, config=aconfig)

    ! Create the application clock
    aclock = ESMF_ClockCreate()

    print *, "App Comp Create returned, name = ", trim(aname)

    !-------------------------------------------------------------------------
    !  Create the 2 gridded components

    grid1 = ESMF_GridCreate()
    gname1 = "Ocean"
    oceangridcomp = ESMF_GridCompCreate(name=gname1, layout=alayout, &
                                    mtype=ESMF_OCEAN, grid=grid1, &
                                    config=aconfig, rc=rc)  

    print *, "Grid Comp Create returned, name = ", trim(gname1)

    grid2 = ESMF_GridCreate()
    gname2 = "Atmosphere"
    atmgridcomp = ESMF_GridCompCreate(name=gname2, layout=alayout, &
                                    mtype=ESMF_ATM, grid=grid2, &
                                    config=aconfig, rc=rc)  

    print *, "Grid Comp Create returned, name = ", trim(gname2)

    !-------------------------------------------------------------------------
    !  Create the coupler

    cname = "Ocean2Atmosphere"
    cplcomp = ESMF_CplCompCreate(name=cname, layout=alayout, config=aconfig, &
                                 rc=rc)  

    print *, "Cpl Comp Create returned, name = ", trim(cname)

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
    
