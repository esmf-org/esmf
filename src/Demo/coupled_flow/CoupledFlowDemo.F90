! $Id: CoupledFlowDemo.F90,v 1.2 2003/05/02 20:53:17 nscollins Exp $
!
! ESMF Coupled Flow Demo - A Gridded Component which can be called either 
!   directly from an Application Component or nested in a larger application.
!

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! ESMF Coupled Flow Demo Component
!   contains 2 nested components and 1 coupler, two-way coupling.
!   Flow application.  
!
!
!\begin{verbatim}

    module CoupledFlowMod

    ! ESMF Framework module, defines all ESMF data types and procedures
    use ESMF_Mod
    
    ! User Component registration routines
    use   InjectorMod, only : Injector_register
    use FlowSolverMod, only : FlowSolver_register
    use    CouplerMod, only : Coupler_register

    implicit none
    private
    
    ! Local (private) global variables

    ! Components
    type(ESMF_GridComp) :: INcomp, FScomp
    type(ESMF_CplComp) :: cpl

    ! States and Layouts
    character(len=ESMF_MAXSTR) :: cnameIN, cnameFS, cplname
    type(ESMF_DELayout) :: layoutTop, layoutIN, layoutFS
    type(ESMF_State) :: INimp, INexp, FSimp, FSexp
    type(ESMF_State) :: cplstateF2I, cplstateI2F, cplbothlists

    ! instantiate a clock, a calendar, and timesteps
    type(ESMF_Clock) :: clock
    type(ESMF_Calendar) :: gregorianCalendar
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Time) :: startTime
    type(ESMF_Time) :: stopTime

    ! Public entry point
    public CoupledFlow_register

!------------------------------------------------------------------------------

    contains
        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!   Registration routine

!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

    subroutine CoupledFlow_register(comp, rc)
        type(ESMF_GridComp), intent(inout) :: comp
        integer, intent(out) :: rc

        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                        coupledflow_init, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                        coupledflow_run, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                        coupledflow_final, ESMF_SINGLEPHASE, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

    end subroutine

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!    Init section

!-------------------------------------------------------------------------
!   !  User Comp Component created by higher level calls, here is the
!   !   Initialization routine.


subroutine coupledflow_init(gcomp, importstate, exportstate, clock, rc)
    type(ESMF_GridComp), intent(inout) :: gcomp
    type(ESMF_State), intent(inout) :: importstate, exportstate
    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: rc

    integer :: ndes, delist(16)
    integer :: i, mid, quart


    ! Get our layout from the component
    call ESMF_GridCompGet(gcomp, layout=layoutTop, rc=rc)

    ! Sanity check the number of DEs we were started on.
    call ESMF_DELayoutGetNumDEs(layoutTop, ndes, rc)
    !if ((ndes .lt. 4) .or. (ndes .gt. 16)) then
    !    print *, "This demo needs to run at least 4-way and no more than 16-way."
    !    print *, "The requested number of processors was ", ndes
    !    goto 10
    !endif
    !if (mod(ndes, 4) .ne. 0) then
    !    print *, "This demo needs to run on some multiple of 4 processors,"
    !    print *, " at least 4-way and no more than 16-way."
    !    print *, "The requested number of processors was ", ndes
    !    goto 10
    !endif

    ! Set up the component layouts so they are different, so we can show
    !  we really are routing data between processors.
    delist = (/ (i, i=0, ndes-1) /)
    mid = ndes/2
    quart = ndes/4


    ! Create the 2 model components and coupler.  The first component will
    !  run on a 2 x N/2 layout, the second will be on a 4 x N/4 layout.
    !  The coupler will run on the original default 1 x N layout.
    cnameIN = "Injector model"
    !layoutIN = ESMF_DELayoutCreate(delist, 2, (/ 1, 1 /), (/ 0, 0 /), rc)
    layoutIN = ESMF_DELayoutCreate(delist, 2, (/ mid, 2 /), (/ 0, 0 /), rc)
    !layoutIN = ESMF_DELayoutCreate(delist, 2, (/ quart, 4 /), (/ 0, 0 /), rc)
    INcomp = ESMF_GridCompCreate(cnameIN, layout=layoutIN, rc=rc)

    cnameFS = "Flow Solver model"
    !layoutFS = ESMF_DELayoutCreate(delist, 2, (/ 1, 1 /), (/ 0, 0 /), rc)
    layoutFS = ESMF_DELayoutCreate(delist, 2, (/ quart, 4 /), (/ 0, 0 /), rc)
    FScomp = ESMF_GridCompCreate(cnameFS, layout=layoutFS, rc=rc)

    cplname = "Two-way coupler"
    cpl = ESMF_CplCompCreate(cplname, layout=layoutTop, rc=rc)


    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section for subcomponents
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
!  Init section for subcomponents
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
 
end subroutine coupledflow_init

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Run section

!   !  The Run routine where data is computed.
!   !

subroutine coupledflow_run(comp, importstate, exportstate, clock, rc)
     type(ESMF_GridComp), intent(inout) :: comp
     type(ESMF_State), intent(inout) :: importstate, exportstate
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out) :: rc

     ! Local variables
     type(ESMF_Clock) :: localclock

     ! make our own local copy of the clock
     localclock = clock
  

     print *, "Run Loop Start time"
     call ESMF_ClockPrint(localclock, "currtime string", rc)

     do while (.not. ESMF_ClockIsStopTime(localclock, rc))

        ! Run FlowSolver Component
        call ESMF_GridCompRun(FScomp, FSimp, FSexp, localclock, rc=rc)

        ! Couple export state of FlowSolver to import of Injector
        call ESMF_CplCompRun(cpl, cplstateF2I, localclock, rc=rc)
  
        ! Run Injector Component
        call ESMF_GridCompRun(INcomp, INimp, INexp, localclock, rc=rc)
  
        ! Couple export state of Injector to import of FlowSolver
        call ESMF_CplCompRun(cpl, cplstateI2F, localclock, rc=rc)
  
        ! Advance the time
        call ESMF_ClockAdvance(localclock, rc=rc)
        !call ESMF_ClockPrint(localclock, "currtime string", rc)


     enddo
     print *, "Run Loop End time"
     call ESMF_ClockPrint(localclock, "currtime string", rc)
 
end subroutine coupledflow_run

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!     Finalize section

!   !  The Finalization routine where things are deleted and cleaned up.
!   !

subroutine coupledflow_final(comp, importstate, exportstate, clock, rc)
     type(ESMF_GridComp), intent(inout) :: comp
     type(ESMF_State), intent(inout) :: importstate, exportstate
     type(ESMF_Clock), intent(in) :: clock
     integer, intent(out) :: rc


      ! First let subcomponents finalize

      ! Finalize Injector Component    
      call ESMF_GridCompFinalize(INcomp, INimp, INexp, clock, rc=rc)

      ! Finalize FlowSolver Component
      call ESMF_GridCompFinalize(FScomp, FSimp, FSimp, clock, rc=rc)

      ! Finalize Coupler
      call ESMF_CplCompFinalize(cpl, cplstateI2F, clock, rc=rc)


      ! Then clean them up

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

      print *, "Coupled Flow Demo complete!"

end subroutine coupledflow_final


      end module CoupledFlowMod
    
!\end{verbatim}
    
