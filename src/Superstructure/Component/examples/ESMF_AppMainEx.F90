! $Id: ESMF_AppMainEx.F90,v 1.6 2003/02/20 21:52:00 nscollins Exp $
!
! Example code for a main program Application.  See ESMF_AppCompEx.F90
!   for an example of an embeddable Application.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! Example of a main program Application.  See also the following example
!  for a Component which can either be an Application or can be nested
!  in a higher level Component.
!
!
!\begin{verbatim}

!   ! Example main program showing calls to the Component routines.
!   ! This version is not nestable inside a larger application.
    program ESMF_AppMainEx
    
!   ! Some common definitions.  This requires the C preprocessor.
    #include "ESMF.h"

!   ! Other ESMF modules which are needed by Comps
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_ClockMod
    use ESMF_StateMod
    use ESMF_CompMod
    
    ! User supplied modules
    use PHYS_Mod, only: PHYS_Register
    use DYNM_Mod, only: DYNM_Register
    use CPLR_Mod, only: CPLR_Register
    implicit none
    
!   ! Local variables
    integer :: x, y, rc
    integer :: timestep, timeend
    logical :: finished
    type(ESMF_Clock) :: clock
    integer :: delistall(18), delist1(8), delist2(8), delist3(2)
    character(ESMF_MAXSTR) :: cname
    type(ESMF_Layout) :: layoutall, layout1, layout2, layout3
    type(ESMF_State) :: statelist(2)
    type(ESMF_Comp) :: app, comp1, comp2, cpl
        
!-------------------------------------------------------------------------
!   !
!   !  Create, Init, Run, Finalize, Destroy Components.
 
    print *, "Application Example 1:"

    ! Create the top level application component.  Create a Layout and
    !  a Clock to pass in.  

    delistall = (/ (i, i=0,17) /)
    layoutall = ESMF_LayoutCreate(1, 18, delistall, rc)

    ! See the TimeMgr document for the details on this.
    clock = ESMF_ClockInit()

    cname = "Top Level Application"
    app = ESMF_CompCreate(cname, layoutall, ESMF_APPCOMP, ESMF_GENERAL, &
                            clock, "/home/myname/model1/setup", rc=rc)  

    delist1 = (/ (i, i=0,7) /)
    layout1 = ESMF_LayoutCreate(2, 4, delist1, ESMF_XFAST, rc)

    cname = "Atmosphere Physics"
    comp1 = ESMF_CompCreate(cname, layout1, ESMF_GRIDCOMP, ESMF_ATM, &
                             clock, "/home/myname/model1/setup", rc=rc)  

    ! This single user-supplied subroutine must be a public entry point,
    !  and must be either a unique name among all components, or be 
    !  renamed with the 'use localname => modulename' syntax.
    call ESMF_CompRegister(comp1, PHYS_Register, rc=rc)

    ! (see below for what the Register routine will need to do.)

    print *, "Comp Create returned, name = ", trim(cname)

    delist2 = (/ (i, i=8,15) /)
    layout2 = ESMF_LayoutCreate(2, 4, delist2, ESMF_XFAST, rc)

    cname = "Atmosphere Dynamics"
    comp2 = ESMF_CompCreate(cname, layout2, ESMF_GRIDCOMP, ESMF_ATM, &
                             clock, "/home/myname/model1/setup", rc=rc)  

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_CompRegister(comp1, DYNM_Register, rc=rc)

    print *, "Comp Create returned, name = ", trim(cname)

    delist3 = (/ (i, i=16,17) /)
    layout3 = ESMF_LayoutCreate(2, 1, delist3, ESMF_XFAST, rc)

    cname = "Atmosphere Coupler"
    comps(1) = comp1
    comps(2) = comp2
    cpl = ESMF_CompCreate(cname, layout3, ESMF_CPLCOMP, comps, &
                           filepath="/home/myname/model1/setup", rc=rc)  

    ! This single user-supplied subroutine must be a public entry point.
    call ESMF_CompRegister(comp1, CPLR_Register, rc=rc)

    print *, "Comp Create returned, name = ", trim(cname)

    ! Query the components for their import and export states, and
    ! set them in the coupler state list.
    call ESMF_CompGetState(comp1, ESMF_IMPORTSTATE, localstates(1), rc=rc)
    call ESMF_CompGetState(comp2, ESMF_EXPORTSTATE, localstates(2), rc=rc)
    call ESMF_CompSetState(cpl, statelist=localstates, rc=rc)
     
    ! Call each Init routine in turn.  There is an optional index number
    !  for those components which have multiple entry points.
    call ESMF_CompInit(comp1, rc=rc)
    call ESMF_CompInit(comp2, rc=rc)
    call ESMF_CompInit(cpl, rc=rc)
    print *, "Comp Init returned"


    ! Main run loop.
    finished = .false.
    timesteps = 5
    do while (.not. finished)

        call ESMF_CompRun(comp1, clock, timesteps, rc)
        call ESMF_CompRun(comp2, clock, timesteps, rc)
        call ESMF_CompRun(cpl, clock, rc)
   
        call ESMF_ClockAdvance(clock, timesteps)

        ! query clock for current time
        if (currenttime .gt. endtime) finished = .true.
    enddo


    !
    call ESMF_CompFinalize(comp1, rc)
    call ESMF_CompFinalize(comp2, rc)
    call ESMF_CompFinalize(cpl, rc)
    print *, "Comp Finalize returned"


    call ESMF_CompDestroy(comp1, rc)
    call ESMF_CompDestroy(comp2, rc)
    call ESMF_CompDestroy(cpl, rc)
    print *, "Comp Destroy returned"


    print *, "Application Example 1 finished"


    end program ESMF_AppMainEx
    

    ! Each Component must supply a Register routine which makes the
    !  following calls:
    !! call ESMF_CompSetRoutine(comp1, "init", 1, PHYS_Init)
    !! call ESMF_CompSetRoutine(comp1, "init", 2, PHYS_InitPhase2)
    !! call ESMF_CompSetRoutine(comp1, "run", 1, PHYS_Run)
    !! call ESMF_CompSetRoutine(comp1, "final", 1, PHYS_Final)
    ! (so PHYS_Init(), etc can be private methods inside the module).
    ! The arguments are: the component, the type of routine, the index
    !  number (to support multiple entry points of the same type), and
    !  the name of the internal subroutine which contains the user code.

!\end{verbatim}
    
