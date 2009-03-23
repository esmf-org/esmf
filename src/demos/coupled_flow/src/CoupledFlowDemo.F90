! $Id: CoupledFlowDemo.F90,v 1.6 2009/03/23 20:40:48 theurich Exp $
!
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: CoupledFlowDemo.F90 - Top level Gridded Component source
!
! !DESCRIPTION:
! ESMF Coupled Flow Demo - A Gridded Component which can be called either 
!   directly from an Application Driver or nested in a larger application.
!   It contains 2 nested subcomponents and 1 Coupler Component which does 
!   two-way coupling between the subcomponents.
!
!EOP
!------------------------------------------------------------------------------
!
!

    module CoupledFlowMod

    ! ESMF module, defines all ESMF data types and procedures
    use ESMF_Mod
    
    ! User Component registration routines
    use   InjectorMod, only : Injector_register
    use FlowSolverMod, only : FlowSolver_register
    use    CouplerMod, only : Coupler_register

    implicit none
    private
    
    ! Subcomponents
    type(ESMF_GridComp), save :: INcomp, FScomp
    type(ESMF_CplComp), save :: cpl

    ! States and DELayouts for the Subcomponents
    character(len=ESMF_MAXSTR) :: cnameIN, cnameFS, cplname
    type(ESMF_DELayout), save :: layoutTop, layoutIN, layoutFS
    type(ESMF_State), save :: INimp, INexp, FSimp, FSexp

    ! Public entry point
    public CoupledFlow_register

!------------------------------------------------------------------------------

    contains
        
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: CoupledFlow_register - Externally visible registration routine

! !INTERFACE:
      subroutine CoupledFlow_register(comp, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp), intent(inout) :: comp
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied setservices routine.
!     The register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that these are
!     private to the module.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

!BOP
!  !DESCRIPTION:
! \subsubsection{Example of Set Services Usage:}
!
!   The following code registers with ESMF 
!   the subroutines to be called to Init, Run, and Finalize this component.
!\begin{verbatim}
        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, coupledflow_init, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, coupledflow_run, rc=rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, coupledflow_final, rc=rc)

!\end{verbatim}
!EOP
        print *, "CoupledFlowDemo: Registered Initialize, Run, and Finalize routines"

    end subroutine

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: coupledflow_init - initialization routine

! !INTERFACE:
      subroutine coupledflow_init(gcomp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp), intent(inout) :: gcomp
     type(ESMF_State), intent(inout) :: importState, exportState
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied init routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Importstate.
!     \item[exportState]
!          Exportstate.
!     \item[clock] 
!          External clock.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI
    integer :: npets
    integer :: mid, by2, quart, by4

    type(ESMF_IGrid) :: igridTop, igridIN, igridFS
    real(ESMF_KIND_R8) :: mincoords(ESMF_MAXIGRIDDIM), maxcoords(ESMF_MAXIGRIDDIM)
    integer :: counts(ESMF_MAXIGRIDDIM)
    type(ESMF_IGridHorzStagger) :: horz_stagger
    type(ESMF_VM) :: vm
    integer :: halo_width = 1


    ! Get our vm and igrid from the component
    call ESMF_GridCompGet(gcomp, vm=vm, igrid=igridTop, rc=rc)
    call ESMF_IGridGet(igridTop, delayout=layoutTop, rc=rc)

    ! Sanity check the number of PETs we were started on.
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if (npets .eq. 1) then
      mid = 1
      by2 = 1
      quart = 1
      by4 = 1
    else
       if ((npets .lt. 4) .or. (npets .gt. 16)) then
           print *, "This demo needs to run at least 4-way and no more "
           print *, "  than 16-way, on a multiple of 4 processors."
           print *, "The requested number of processors was ", npets
           rc = ESMF_FAILURE
           return
       endif
       if (mod(npets, 4) .ne. 0) then
           print *, "This demo needs to run on some multiple of 4 processors,"
           print *, " at least 4-way and no more than 16-way."
           print *, "The requested number of processors was ", npets
           rc = ESMF_FAILURE
           return
       endif
       mid = npets/2
       by2 = 2
       quart = npets/4
       by4 = 4
     endif

    ! Create the 2 model components and coupler. 

!BOP
! !DESCRIPTION:
! \subsubsection{Example of Component Creation:}
!
!   The following code creates 2 Gridded Components on the same set of PETs 
!   (persistent execution threads) as the top level Component, but each 
!   of the IGrids useds by these Components will have a different connectivity.
!   It also creates a Coupler Component on the same PET set.
!
!\begin{verbatim}
    cnameIN = "Injector model"
    INcomp = ESMF_GridCompCreate(name=cnameIN, rc=rc)

    cnameFS = "Flow Solver model"
    FScomp = ESMF_GridCompCreate(name=cnameFS, rc=rc)

    cplname = "Two-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, rc=rc)
!\end{verbatim}
!EOP

    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section for subcomponents
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
    call ESMF_GridCompSetServices(INcomp, Injector_register, rc)
    print *, "Injector Comp SetServices finished, rc= ", rc

    call ESMF_GridCompSetServices(FScomp, FlowSolver_register, rc)
    print *, "FlowSolver Comp SetServices finished, rc= ", rc

    call ESMF_CplCompSetServices(cpl, Coupler_register, rc)
    print *, "Coupler Comp SetServices finished, rc= ", rc

 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init section for subcomponents.  Create subigrids on separate DELayouts,
!    and create import/export states for subcomponents.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
    ! 
    ! Create and attach subigrids to the subcomponents.
    !
    call ESMF_IGridGet(igridTop, horzRelLoc=ESMF_CELL_CENTER, &
                               globalCellCountPerDim=counts, &
                               minGlobalCoordPerDim=mincoords, &
                               maxGlobalCoordPerDim=maxcoords, &
                               horzstagger=horz_stagger, &      
                               rc=rc)


    igridIN = ESMF_IGridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=mincoords, &
                             maxGlobalCoordPerDim=maxcoords, &
                             horzstagger=horz_stagger, &      
                             name="Injector igrid", rc=rc)

    layoutIN = ESMF_DELayoutCreate(vm, (/ mid, by2 /), rc=rc)
    call ESMF_IGridDistribute(igridIN, delayout=layoutIN, rc=rc)


    call ESMF_GridCompSet(INcomp, igrid=igridIN, rc=rc)

    igridFS = ESMF_IGridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=mincoords, &
                             maxGlobalCoordPerDim=maxcoords, &
                             horzstagger=horz_stagger, &      
                             name="Flow Solver igrid", rc=rc)

    layoutFS = ESMF_DELayoutCreate(vm, (/ quart, by4 /), rc=rc)
    call ESMF_IGridDistribute(igridFS, delayout=layoutFS, rc=rc)

    call ESMF_GridCompSet(FScomp, igrid=igridFS, rc=rc)


    !
    ! Create import/export states for Injection Component
    !
!BOP
! !DESCRIPTION:
! \subsubsection{Example of State Creation:}
!
!   The following code creates Import and Export States for the
!   Injection subcomponent.  All information being passed between
!   subcomponents will be described by these States.
!
!\begin{verbatim}
    INimp = ESMF_StateCreate("Injection Input", ESMF_STATE_IMPORT,  rc=rc)
    INexp = ESMF_StateCreate("Injection Feedback", ESMF_STATE_EXPORT, rc=rc)
!\end{verbatim}
!EOP
    ! 
    ! Initialize the injector component, first phase 
    !
    call ESMF_GridCompInitialize(INcomp, INimp, INexp, clock, 1, rc=rc)
    print *, "Injection Model Initialize finished, rc =", rc
 
    !
    ! Create import/export states for FlowSolver Component
    !
    FSimp = ESMF_StateCreate("FlowSolver Input", ESMF_STATE_IMPORT, rc=rc)
    FSexp = ESMF_StateCreate("FlowSolver Feedback ", ESMF_STATE_EXPORT, rc=rc)

    !
    ! Initialize the flow solver component, first phase
    !
    call ESMF_GridCompInitialize(FScomp, FSimp, FSexp, clock, 1, rc=rc)
    print *, "Flow Model Initialize finished, rc =", rc

    !
    ! Initialize the coupler, once for each export state
    ! (note this is not 2 phases - it is calling the same code each time.)
    !
    call ESMF_CplCompInitialize(cpl, FSexp, INimp, clock, rc=rc)
    call ESMF_CplCompInitialize(cpl, INexp, FSimp, clock, rc=rc)
    print *, "Coupler Initialize finished, rc =", rc
 
    !
    ! Second phase of init
    !
    call ESMF_GridCompInitialize(INcomp, INimp, INexp, clock, 2, rc=rc)
    print *, "Injection Model Initialize finished, rc =", rc
 
    call ESMF_GridCompInitialize(FScomp, FSimp, FSexp, clock, 2, rc=rc)
    print *, "Flow Model Initialize finished, rc =", rc

    end subroutine coupledflow_init

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: coupledflow_run - run routine

! !INTERFACE:
      subroutine coupledflow_run(comp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp), intent(inout) :: comp
     type(ESMF_State), intent(inout) :: importState, exportState
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied run routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Importstate.
!     \item[exportState]
!          Exportstate.
!     \item[clock] 
!          External clock.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

     ! Local variables
     type(ESMF_Clock) :: localclock


     ! make our own local copy of the clock
     localclock = ESMF_ClockCreate(clock, rc)

     print *, "Run Loop Start time"
     call ESMF_ClockPrint(localclock, "currtime string", rc)

     do while (.not. ESMF_ClockIsStopTime(localclock, rc))

        ! Run FlowSolver Component
        call ESMF_GridCompRun(FScomp, FSimp, FSexp, localclock, rc=rc)

        ! Couple export state of FlowSolver to import of Injector
        call ESMF_CplCompRun(cpl, FSexp, INimp, localclock, rc=rc)
  
        ! Run Injector Component
        call ESMF_GridCompRun(INcomp, INimp, INexp, localclock, rc=rc)
  
        ! Couple export state of Injector to import of FlowSolver
        call ESMF_CplCompRun(cpl, INexp, FSimp, localclock, rc=rc)
  
        ! Advance the time
        call ESMF_ClockAdvance(localclock, rc=rc)
      
        ! This demo runs a lot of time steps and only outputs files
        ! every N iterations.  This print statement, if commented in,
        ! generates a lot of output.
        !call ESMF_ClockPrint(localclock, "currtime string", rc)

     enddo

     print *, "Run Loop End time"
     call ESMF_ClockPrint(localclock, "currtime string", rc)
 
     call ESMF_ClockDestroy(localclock, rc)

end subroutine coupledflow_run


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE:  coupledflow_final - user supplied finalize routine

! !INTERFACE:
      subroutine coupledflow_final(comp, importState, exportState, clock, rc)
!
! !ARGUMENTS:
     type(ESMF_GridComp), intent(inout) :: comp
     type(ESMF_State), intent(inout) :: importState, exportState
     type(ESMF_Clock), intent(inout) :: clock
     integer, intent(out) :: rc
!
! !DESCRIPTION:
!     User-supplied finalize routine.
!
!     The arguments are:
!     \begin{description}
!     \item[comp] 
!          Component.
!     \item[importState]
!          Importstate.
!     \item[exportState]
!          Exportstate.
!     \item[clock] 
!          External clock.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors,
!          otherwise {\tt ESMF\_FAILURE}.
!     \end{description}
!
!EOPI

      ! First finalize all subcomponents

      ! Finalize Injector Component    
      call ESMF_GridCompFinalize(INcomp, INimp, INexp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) then
          print *, "Injector Component Finalize routine returned error"
          return
      endif

      ! Finalize FlowSolver Component
      call ESMF_GridCompFinalize(FScomp, FSimp, FSimp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) then
          print *, "FlowSolver Component Finalize routine returned error"
          return
      endif

      ! Finalize Coupler
      call ESMF_CplCompFinalize(cpl, INexp, FSimp, clock, rc=rc)
      if (rc .ne. ESMF_SUCCESS) then
          print *, "Coupler Component Finalize routine returned error"
          return
      endif

      print *, "CoupledFlowMod finished calling all subcomponent Finalize routines"

      ! Then clean them up

      print *, "ready to destroy all states"
      call ESMF_StateDestroy(INimp, rc)
      call ESMF_StateDestroy(INexp, rc)
      call ESMF_StateDestroy(FSimp, rc)
      call ESMF_StateDestroy(FSexp, rc)

      print *, "ready to destroy all components"
      call ESMF_GridCompDestroy(INcomp, rc)
      call ESMF_GridCompDestroy(FScomp, rc)
      call ESMF_CplCompDestroy(cpl, rc)

      print *, "ready to destroy all delayouts"
      !call ESMF_DELayoutDestroy(layoutIN, rc)
      !call ESMF_DELayoutDestroy(layoutFS, rc)

      print *, "end of CoupledFlowMod Finalization routine"
      rc = ESMF_SUCCESS

    end subroutine coupledflow_final


    end module CoupledFlowMod
    
    
