! $Id: ESMF_SysTest74559.F90,v 1.16 2003/07/31 23:03:57 jwolfe Exp $
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

#include <ESMF_Macros.inc>

    ! ESMF Framework module, defines all ESMF data types and procedures
    use ESMF_Mod
    use ESMF_TestMod
    
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

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final status message
    character(ESMF_MAXSTR) :: failMsg, finalMsg

        
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
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Query application for layout.
    call ESMF_AppCompGet(app, layout=layoutApp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Get our DE number for later
    call ESMF_DELayoutGetDEId(layoutApp, de_id, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    ! Sanity check the number of DEs we were started on.
    call ESMF_DELayoutGetNumDEs(layoutApp, ndes, rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
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
    if (rc .ne. ESMF_SUCCESS) goto 10
    INcomp = ESMF_GridCompCreate(cnameIN, layout=layoutIN, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cnameFS = "Flow Solver model"
    layoutFS = ESMF_DELayoutCreate(delist, 2, (/ quart, 4 /), (/ 0, 0 /), rc)
    if (rc .ne. ESMF_SUCCESS) goto 10
    FScomp = ESMF_GridCompCreate(cnameFS, layout=layoutFS, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10

    cplname = "Two-way coupler"
    cpl = ESMF_CplCompCreate(cplname, layout=layoutApp, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 10


    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(INcomp, Injector_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_GridCompSetServices(FScomp, FlowSolver_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_CplCompSetServices(cpl, Coupler_register, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Comp SetServices finished, rc= ", rc

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Create and initialize a clock.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      ! initialize calendar to be Gregorian type
      call ESMF_CalendarSet(gregorianCalendar, ESMF_CAL_GREGORIAN, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize time interval to 2 seconds
      call ESMF_TimeIntervalSet(timeStep, s_i4=2, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize start time to 12May2003, 9:00 am
      call ESMF_TimeSet(startTime, yr_i4=2003, mm_i4=5, dd_i4=12, h_i4=9, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize stop time to 12May2003, 3:00 pm
      ! to keep runtime down
      call ESMF_TimeSet(stopTime, yr_i4=2003, mm_i4=5, dd_i4=12, h_i4=15, &
                        calendar=gregorianCalendar, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! initialize the clock with the above values
      call ESMF_ClockSet(clock, timeStep, startTime, stopTime, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10


 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
      ! Create import/export states for Injection Component
      INimp = ESMF_StateCreate("Injection Input", ESMF_STATEIMPORT, &
                                                              cnameIN, rc=rc)
      INexp = ESMF_StateCreate("Injection Feedback", ESMF_STATEEXPORT, &
                                                              cnameIN, rc=rc)

      call ESMF_GridCompInitialize(INcomp, INimp, INexp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Injection Model Initialize finished, rc =", rc
 
      ! Create import/export states for FlowSolver Component
      FSimp = ESMF_StateCreate("FlowSolver Input", ESMF_STATEIMPORT, &
                                                               cnameFS, rc=rc)
      FSexp = ESMF_StateCreate("FlowSolver Feedback ", ESMF_STATEEXPORT, &
                                                               cnameFS, rc=rc)

      call ESMF_GridCompInitialize(FScomp, FSimp, FSexp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Flow Model Initialize finished, rc =", rc

      ! Create a list of 2 states for Coupler, direction 1
      cplstateI2F = ESMF_StateCreate("Coupler States Injector to FlowSolver", &
                                               ESMF_STATELIST, cplname, rc=rc)
      call ESMF_StateAddData(cplstateI2F, INexp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateAddData(cplstateI2F, FSimp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
 
      ! Create a list of 2 states for Coupler, direction 2
      cplstateF2I = ESMF_StateCreate("Coupler States FlowSolver to Injector", &
                                               ESMF_STATELIST, cplname, rc=rc)
      call ESMF_StateAddData(cplstateF2I, FSexp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateAddData(cplstateF2I, INimp, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
 
      ! Create a list of the previous 2 statelists for initialization
      cplbothlists = ESMF_StateCreate("All Coupler states", ESMF_STATELIST, &
                                                               cplname, rc=rc)

      call ESMF_StateAddData(cplbothlists, cplstateI2F, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateAddData(cplbothlists, cplstateF2I, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_CplCompInitialize(cpl, cplbothlists, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      print *, "Coupler Initialize finished, rc =", rc
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Run section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      print *, "Run Loop Start time"
      call ESMF_ClockPrint(clock, "currtime string", rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        ! Run FlowSolver Component
        call ESMF_GridCompRun(FScomp, FSimp, FSexp, clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10

        ! Couple export state of FlowSolver to import of Injector
        call ESMF_CplCompRun(cpl, cplstateF2I, clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
  
        ! Run Injector Component
        call ESMF_GridCompRun(INcomp, INimp, INexp, clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
  
        ! Couple export state of Injector to import of FlowSolver
        call ESMF_CplCompRun(cpl, cplstateI2F, clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
  
        ! Advance the time
        call ESMF_ClockAdvance(clock, rc=rc)
        if (rc .ne. ESMF_SUCCESS) goto 10
        !call ESMF_ClockPrint(clock, "currtime string", rc)


      enddo
      print *, "Run Loop End time"
      call ESMF_ClockPrint(clock, "currtime string", rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Finalize section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      ! Finalize Injector Component    
      call ESMF_GridCompFinalize(INcomp, INimp, INexp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! Finalize FlowSolver Component
      call ESMF_GridCompFinalize(FScomp, FSimp, FSimp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      ! Finalize Coupler
      call ESMF_CplCompFinalize(cpl, cplstateI2F, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Destroy section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(INimp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(INexp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(FSimp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(FSexp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(cplstateI2F, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_StateDestroy(cplstateF2I, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_GridCompDestroy(INcomp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_GridCompDestroy(FScomp, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_CplCompDestroy(cpl, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_DELayoutDestroy(layoutIN, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10
      call ESMF_DELayoutDestroy(layoutFS, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

      call ESMF_AppCompDestroy(app, rc)
      if (rc .ne. ESMF_SUCCESS) goto 10

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
10    print *, "Coupled Flow Demo complete!"

      if ((de_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
        write(failMsg, *)  "System Test failure"
        write(testname, *) "System Test 74559: Coupled Fluid Flow"
  
        call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                          testname, failMsg, testresult, ESMF_SRCLINE)

        ! Separate message to console, for quick confirmation of success/failure
        if (rc .eq. ESMF_SUCCESS) then
          write(finalMsg, *) "SUCCESS!! See output files for computed values"
        else
          write(finalMsg, *) "System Test did not succeed.  Error code ", rc
        endif
        write(0, *) ""
        write(0, *) trim(testname)
        write(0, *) trim(finalMsg)
        write(0, *) ""

      endif
    
      end program ESMF_CoupledFlowDemo
    
!\end{verbatim}
    
