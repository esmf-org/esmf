! $Id: ESMF_SysTest74559.F90,v 1.3 2003/04/28 17:48:06 nscollins Exp $
!
! ESMF Coupled Flow Demo
!
! System test code #74559

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 74559.  
!   2 components and 1 coupler, two-way coupling.
!   Flow application.
!
!
!\begin{verbatim}

    program ESMF_CoupledFlowDemo

    ! ESMF Framework module
    use ESMF_Mod
    
    use HeatFlowMod, only : HeatMod_register
    use     FlowMod, only : FlowMod_register
    use      CplMod, only : Coupler_register

    implicit none
    
    ! Local variables
    integer :: de_id, ndes, rc, delist(8)
    integer :: mid, quart
    character(len=ESMF_MAXSTR) :: aname, cnameHI, cnameFS, cplname
    type(ESMF_DELayout) :: layoutApp, layoutHI, layoutFS
    type(ESMF_State) :: HIimp, HIexp, FSimp, FSexp, cplstateF2H, cplstateH2F
    type(ESMF_AppComp) :: app
    type(ESMF_GridComp) :: compHI, compFS
    type(ESMF_CplComp) :: cpl

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime
    integer(ESMF_IKIND_I8) :: advanceCount

        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

    print *, "System Test #74559:"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    Create section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!

    ! Create the top level application component.
    aname = "System Test #74559"
    app = ESMF_AppCompCreate(aname, rc=rc)
    print *, "Created component ", trim(aname), ",  rc =", rc

    ! Query application for layout.
    call ESMF_AppCompGet(app, layout=layoutApp, rc=rc)
    call ESMF_DELayoutPrint(layoutApp, rc=rc)

    call ESMF_DELayoutGetNumDEs(layoutApp, ndes, rc)
    if (ndes .lt. 4) then
        print *, "This system test needs to run at least 4-way, current np = ", ndes
        goto 10
    endif

    ! Set up the component layouts so they are different, so we can show
    !  we really are routing data between processors.
    mid = ndes/2
    quart = ndes/4


    ! Create the 2 model components and coupler.  The first component will
    !  run on a 2 x N/2 layout, the second will be on a 4 x N/4 layout.
    !  The coupler will run on the original default 1 x N layout.
    cnameHI = "Heat injector model"
    delist = (/ (i=0, ndes-1) /)
    layoutHI = ESMF_DELayoutCreate(delist, 2, (/ mid, 2 /), (/ 0, 0 /), rc)
    HIcomp = ESMF_GridCompCreate(cnameHS, layout=layoutHI, rc=rc)
    print *, "Created component ", trim(cnameHS), "rc =", rc
    call ESMF_DELayoutPrint(layoutHI, rc=rc)

    cnameFS = "Flow solver model"
    delist = (/ (i=0, ndes-1) /)
    layoutFS = ESMF_DELayoutCreate(delist, 2, (/ quart, 4 /), (/ 0, 0 /), rc)
    FScomp = ESMF_GridCompCreate(cnameFS, layout=layoutFS, rc=rc)
    print *, "Created component ", trim(cnameFS), "rc =", rc
    call ESMF_DELayoutPrint(layoutFS, rc=rc)

    cplname = "Two-way coupler"
    cpl = ESMF_CplCompCreate(cplname, layout=layoutApp, rc=rc)
    print *, "Created component ", trim(cplname), ", rc =", rc


    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      call ESMF_GridCompSetServices(HIcomp, HeatMod_register, rc)
      print *, "Comp SetServices finished, rc= ", rc

      call ESMF_GridCompSetServices(FScomp, FlowMod_register, rc)
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
      call ESMF_TimeIntervalInit(timeStep, S=2, rc=rc)

      ! initialize start time to 14May2003, 9:00 am
      call ESMF_TimeInit(startTime, YR=2003, MM=5, DD=14, H=9, M=0, S=0, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize stop time to 15May2003, 9:00 am
      call ESMF_TimeInit(stopTime, YR=2003, MM=5, DD=15, H=9, M=0, S=0, &
                         cal=gregorianCalendar, rc=rc)

      ! initialize the clock with the above values
      call ESMF_ClockInit(clock, timeStep, startTime, stopTime, rc=rc)


 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
      HIimp = ESMF_StateCreate("Heat Injection Feedback", ESMF_STATEIMPORT, &
                               cnameHI)
      HIexp = ESMF_StateCreate("Heat Injection Source", ESMF_STATEEXPORT, &
                               cnameHI)

      call ESMF_GridCompInitialize(HIcomp, HIimp, HIexp, clock, rc=rc)
      print *, "Heat Model Initialize finished, rc =", rc
 
      FSimp = ESMF_StateCreate("Flow Solver Input", ESMF_STATEIMPORT, &
                               cnameFS)
      FSexp = ESMF_StateCreate("Flow Solver Feedback ", ESMF_STATEEXPORT, &
                                cnameFS)

      call ESMF_GridCompInitialize(FScomp, FSimp, FSexp, clock, rc=rc)
      print *, "Flow Model Initialize finished, rc =", rc

      cplstateH2F = ESMF_StateCreate("Coupler States Heat to Flow", &
                                                      ESMF_STATELIST, cplname)
      call ESMF_StateAddData(cplstate, HIexp, rc=rc)
      call ESMF_StateAddData(cplstate, FSimp, rc=rc)
 
      cplstateF2H = ESMF_StateCreate("Coupler States Flow to Heat", &
                                                      ESMF_STATELIST, cplname)
      call ESMF_StateAddData(cplstate, FSimp, rc=rc)
      call ESMF_StateAddData(cplstate, HIexp, rc=rc)
 
      call ESMF_CplCompInitialize(cpl, cplstateH2F, clock, rc=rc)
      print *, "Coupler Initialize finished, rc =", rc
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Run section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      do while (.not. ESMF_ClockIsStopTime(clock, rc))

        call ESMF_GridCompRun(HIcomp, HIimp, HIexp, clock, rc=rc)
        print *, "Heat Model Run returned, rc =", rc
  
        call ESMF_CplCompRun(cpl, cplstateH2F, clock, rc=rc)
        print *, "Coupler Run returned, rc =", rc
  
        call ESMF_GridCompRun(FScomp, FSimp, FSexp, clock, rc=rc)
        print *, "Flow Model Run returned, rc =", rc

        call ESMF_CplCompRun(cpl, cplstateF2H, clock, rc=rc)
        print *, "Coupler Run returned, rc =", rc
  
        call ESMF_ClockAdvance(clock, rc=rc)

      enddo
 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Finalize section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Print result

      call ESMF_GridCompFinalize(HIcomp, HIimp, HIexp, clock, rc=rc)
      print *, "Heat Model Finalize finished, rc =", rc

      call ESMF_GridCompFinalize(FScomp, FSimp, FSimp, clock, rc=rc)
      print *, "Flow Model Finalize finished, rc =", rc

      call ESMF_CplCompFinalize(cpl, cplstate, clock, rc=rc)
      print *, "Coupler Finalize finished, rc =", rc


      ! Figure out our local processor id for message below.
      call ESMF_DELayoutGetDEID(layoutApp, de_id, rc)

      print *, "------------------------------------------------------------"
      print *, "------------------------------------------------------------"
      print *, "Test finished, DE id = ", de_id
      print *, "------------------------------------------------------------"
      print *, "------------------------------------------------------------"

      print *, "Comp Finalize returned"

!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Destroy section
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Clean up

      call ESMF_StateDestroy(HIimp, rc)
      call ESMF_StateDestroy(HIexp, rc)
      call ESMF_StateDestroy(FSimp, rc)
      call ESMF_StateDestroy(FSexp, rc)
      call ESMF_StateDestroy(cplstateH2F, rc)
      call ESMF_StateDestroy(cplstateF2H, rc)

      call ESMF_GridCompDestroy(HIcomp, rc)
      call ESMF_GridCompDestroy(FScomp, rc)
      call ESMF_CplCompDestroy(cpl, rc)

      call ESMF_DELayoutDestroy(layoutHI, rc)
      call ESMF_DELayoutDestroy(layoutFS, rc)

      call ESMF_AppCompDestroy(app, rc)
      print *, "All Destroy routines done"

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
10    print *, "Coupled Flow Demo complete!"

      end program ESMF_CoupledFlowDemo
    
!\end{verbatim}
    
