! $Id: ESMF_SysTest74559.F90,v 1.10 2003/05/02 21:54:27 nscollins Exp $
!
! ESMF Coupled Flow Demo
!

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! ESMF Coupled Flow Demo
!   2 components and 1 coupler, two-way coupling.
!   Flow application.
!
!
!\begin{verbatim}

    program ESMF_CoupledFlowDemo

    ! ESMF Framework module, defines all ESMF data types and procedures
    use ESMF_Mod
    
    ! User Component registration routines
    use   InjectorMod, only : Injector_register
    use FlowSolverMod, only : FlowSolver_register
    use    CouplerMod, only : Coupler_register

    implicit none
    
    ! Local variables

    ! Components
    type(ESMF_AppComp) :: app
    type(ESMF_GridComp) :: INcomp, FScomp
    type(ESMF_CplComp) :: cpl

    ! States and Layouts
    character(len=ESMF_MAXSTR) :: cnameIN, cnameFS, cplname
    type(ESMF_DELayout) :: layoutApp, layoutIN, layoutFS
    type(ESMF_State) :: INimp, INexp, FSimp, FSexp
    type(ESMF_State) :: cplstateF2I, cplstateI2F, cplbothlists

    integer :: de_id, ndes, rc, delist(16)
    integer :: i, mid, quart

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

    print *, "Coupled Flow Demo"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    Create section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

    ! Create the top level application component.  This initializes the
    ! ESMF Framework as well.
    app = ESMF_AppCompCreate("Coupled Flow Demo", rc=rc)

    ! Query application for layout.
    call ESMF_AppCompGet(app, layout=layoutApp, rc=rc)

    ! Sanity check the number of DEs we were started on.
    call ESMF_DELayoutGetNumDEs(layoutApp, ndes, rc)
    if ((ndes .lt. 4) .or. (ndes .gt. 16)) then
        print *, "This test needs to run at least 4-way and no more than 16-way."
        print *, "The requested number of processors was ", ndes
        goto 10
    endif
    if (mod(ndes, 4) .ne. 0) then
        print *, "This test needs to run on some multiple of 4 processors,"
        print *, " at least 4-way and no more than 16-way."
        print *, "The requested number of processors was ", ndes
        goto 10
    endif

    ! Set up the component layouts so they are different, so we can show
    !  we really are routing data between processors.
    delist = (/ (i, i=0, ndes-1) /)
    mid = ndes/2
    quart = ndes/4


    ! Create the 2 model components and coupler.  The first component will
    !  run on a 2 x N/2 layout, the second will be on a 4 x N/4 layout.
    !  The coupler will run on the original default 1 x N layout.
    cnameIN = "Injector model"
    layoutIN = ESMF_DELayoutCreate(delist, 2, (/ mid, 2 /), (/ 0, 0 /), rc)
    INcomp = ESMF_GridCompCreate(cnameIN, layout=layoutIN, rc=rc)

    cnameFS = "Flow Solver model"
    layoutFS = ESMF_DELayoutCreate(delist, 2, (/ quart, 4 /), (/ 0, 0 /), rc)
    FScomp = ESMF_GridCompCreate(cnameFS, layout=layoutFS, rc=rc)

    cplname = "Two-way coupler"
    cpl = ESMF_CplCompCreate(cplname, layout=layoutApp, rc=rc)


    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(INcomp, Injector_register, rc)
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_GridCompSetServices(FScomp, FlowSolver_register, rc)
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_CplCompSetServices(cpl, Coupler_register, rc)
      print *, "Comp SetServices finished, rc= ", rc

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarInit(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)

      ! initialize time interval to 2 seconds
      call ESMF_TimeIntervalInit(timeStep, S=int(2,kind=ESMF_IKIND_I8), rc=rc)

      ! initialize start time to 12May2003, 9:00 am
      call ESMF_TimeInit(startTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=5, DD=12, H=9, M=0, S=int(0,kind=ESMF_IKIND_I8), &
                         cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 15May2003, 9:00 am
      call ESMF_TimeInit(stopTime, YR=int(2003,kind=ESMF_IKIND_I8), &
                         MM=5, DD=15, H=9, M=0, S=int(0,kind=ESMF_IKIND_I8), &
                         cal=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)


 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
      ! Create import/export states for Injection Component
      INimp = ESMF_StateCreate("Injection Input", ESMF_STATEIMPORT,  cnameIN)
      INexp = ESMF_StateCreate("Injection Feedback", ESMF_STATEEXPORT, cnameIN)

      call ESMF_GridCompInitialize(INcomp, INimp, INexp, clock, rc=rc)
      print *, "Injection Model Initialize finished, rc =", rc
 
      ! Create import/export states for FlowSolver Component
      FSimp = ESMF_StateCreate("FlowSolver Input", ESMF_STATEIMPORT, cnameFS)
      FSexp = ESMF_StateCreate("FlowSolver Feedback ", ESMF_STATEEXPORT, cnameFS)

      call ESMF_GridCompInitialize(FScomp, FSimp, FSexp, clock, rc=rc)
      print *, "Flow Model Initialize finished, rc =", rc

      ! Create a list of 2 states for Coupler, direction 1
      cplstateI2F = ESMF_StateCreate("Coupler States Injector to FlowSolver", &
                                                      ESMF_STATELIST, cplname)
      call ESMF_StateAddData(cplstateI2F, INexp, rc=rc)
      call ESMF_StateAddData(cplstateI2F, FSimp, rc=rc)
 
      ! Create a list of 2 states for Coupler, direction 2
      cplstateF2I = ESMF_StateCreate("Coupler States FlowSolver to Injector", &
                                                      ESMF_STATELIST, cplname)
      call ESMF_StateAddData(cplstateF2I, FSexp, rc=rc)
      call ESMF_StateAddData(cplstateF2I, INimp, rc=rc)
 
      ! Create a list of the previous 2 statelists for initialization
      cplbothlists = ESMF_StateCreate("All Coupler states", ESMF_STATELIST, cplname)

      call ESMF_StateAddData(cplbothlists, cplstateI2F, rc=rc)
      call ESMF_StateAddData(cplbothlists, cplstateF2I, rc=rc)

      call ESMF_CplCompInitialize(cpl, cplbothlists, clock, rc=rc)
      print *, "Coupler Initialize finished, rc =", rc
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Run section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      print *, "Run Loop Start time"
      call ESMF_ClockPrint(clock, "currtime string", rc)

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        ! Run FlowSolver Component
        call ESMF_GridCompRun(FScomp, FSimp, FSexp, clock, rc=rc)

        ! Couple export state of FlowSolver to import of Injector
        call ESMF_CplCompRun(cpl, cplstateF2I, clock, rc=rc)
  
        ! Run Injector Component
        call ESMF_GridCompRun(INcomp, INimp, INexp, clock, rc=rc)
  
        ! Couple export state of Injector to import of FlowSolver
        call ESMF_CplCompRun(cpl, cplstateI2F, clock, rc=rc)
  
        ! Advance the time
        call ESMF_ClockAdvance(clock, rc=rc)
        !call ESMF_ClockPrint(clock, "currtime string", rc)


      enddo
      print *, "Run Loop End time"
      call ESMF_ClockPrint(clock, "currtime string", rc)
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Finalize section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      ! Finalize Injector Component    
      call ESMF_GridCompFinalize(INcomp, INimp, INexp, clock, rc=rc)

      ! Finalize FlowSolver Component
      call ESMF_GridCompFinalize(FScomp, FSimp, FSimp, clock, rc=rc)

      ! Finalize Coupler
      call ESMF_CplCompFinalize(cpl, cplstateI2F, clock, rc=rc)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Destroy section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(INimp, rc)
      call ESMF_StateDestroy(INexp, rc)
      call ESMF_StateDestroy(FSimp, rc)
      call ESMF_StateDestroy(FSexp, rc)
      call ESMF_StateDestroy(cplstateI2F, rc)
      call ESMF_StateDestroy(cplstateF2I, rc)

      call ESMF_GridCompDestroy(INcomp, rc)
      call ESMF_GridCompDestroy(FScomp, rc)
      call ESMF_CplCompDestroy(cpl, rc)

      call ESMF_DELayoutDestroy(layoutIN, rc)
      call ESMF_DELayoutDestroy(layoutFS, rc)

      call ESMF_AppCompDestroy(app, rc)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
10    print *, "Coupled Flow Demo complete!"

      end program ESMF_CoupledFlowDemo
    
!\end{verbatim}
    
