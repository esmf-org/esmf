! $Id: CoupledFlowDemo.F90,v 1.10 2004/03/20 03:55:03 cdeluca Exp $
!
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: CoupledFlowDemo.F90 - Top level Gridded Component source
!
! !DESCRIPTION:
! ESMF Coupled Flow Demo - A Gridded Component which can be called either 
!   directly from an Application Component or nested in a larger application.
!   It contains 2 nested subcomponents and 1 coupler component which does 
!   two-way coupling between the subcomponents.
!
!EOP
!------------------------------------------------------------------------------
!
!

    module CoupledFlowMod

    ! ESMF Framework module, defines all ESMF data types and procedures
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

    ! States and Layouts for the Subcomponents
    character(len=ESMF_MAXSTR) :: cnameIN, cnameFS, cplname
    type(ESMF_DELayout) :: layoutTop, layoutIN, layoutFS
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
!     User-supplied SetServices routine.
!     The Register routine sets the subroutines to be called
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
!   The following code registers with the ESMF Framework
!   the subroutines to be called to Init, Run, and Finalize this component.
!\begin{verbatim}
        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, &
                                        coupledflow_init, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, &
                                        coupledflow_run, ESMF_SINGLEPHASE, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, &
                                        coupledflow_final, ESMF_SINGLEPHASE, rc)

!\end{verbatim}
!EOP
        print *, "Registered Initialize, Run, and Finalize routines"

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
    integer :: i
    integer :: ndes, delist(16)   ! 16 is max DEs we will handle right now.
    integer :: mid, by2, quart, by4

    type(ESMF_Grid) :: gridTop, gridIN, gridFS
    real(ESMF_KIND_R8) :: mincoords(ESMF_MAXGRIDDIM), maxcoords(ESMF_MAXGRIDDIM)
    integer :: counts(ESMF_MAXGRIDDIM)
    type(ESMF_GridType) :: horz_gridtype, vert_gridtype
    type(ESMF_GridStagger) :: horz_stagger, vert_stagger
    type(ESMF_CoordSystem) :: horz_coord_system, vert_coord_system
    integer :: halo_width = 1



    ! Get our layout and grid from the component
    call ESMF_GridCompGet(gcomp, delayout=layoutTop, grid=gridTop, rc=rc)

    ! Sanity check the number of DEs we were started on.
    call ESMF_DELayoutGetNumDEs(layoutTop, ndes, rc)
    if (ndes .eq. 1) then
      mid = 1
      by2 = 1
      quart = 1
      by4 = 1
    else
       if ((ndes .lt. 4) .or. (ndes .gt. 16)) then
           print *, "This demo needs to run at least 4-way and no more than 16-way."
           print *, "The requested number of processors was ", ndes
           rc = ESMF_FAILURE
           return
       endif
       if (mod(ndes, 4) .ne. 0) then
           print *, "This demo needs to run on some multiple of 4 processors,"
           print *, " at least 4-way and no more than 16-way."
           print *, "The requested number of processors was ", ndes
           rc = ESMF_FAILURE
           return
       endif
       mid = ndes/2
       by2 = 2
       quart = ndes/4
       by4 = 4
     endif

    ! Set up the component layouts so they are different, so we can show
    !  we really are routing data between processors.
    delist = (/ (i, i=0, ndes-1) /)

    ! Create the 2 model components and coupler.  The first component will
    !  run on a 2 x N/2 layout, the second will be on a 4 x N/4 layout.
    !  The coupler will run on the original default 1 x N layout.

!BOP
! !DESCRIPTION:
! \subsubsection{Example of Layout Creation:}
!
!   The following code creates 2 sublayouts on the same set of PEs (processing
!   elements) as the top level Component, but each of the sublayouts has a
!   different connectivity.
!\begin{verbatim}
    cnameIN = "Injector model"
    layoutIN = ESMF_DELayoutCreate(delist, 2, (/ mid, by2 /), (/ 0, 0 /), rc)
    INcomp = ESMF_GridCompCreate(cnameIN, delayout=layoutIN, rc=rc)
!\end{verbatim}
!EOP

    cnameFS = "Flow Solver model"
    layoutFS = ESMF_DELayoutCreate(delist, 2, (/ quart, by4 /), (/ 0, 0 /), rc)
    FScomp = ESMF_GridCompCreate(cnameFS, delayout=layoutFS, rc=rc)

    cplname = "Two-way coupler"
    cpl = ESMF_CplCompCreate(cplname, delayout=layoutTop, rc=rc)


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
!  Init section for subcomponents.  Create subgrids on separate layouts,
!    and create import/export states for subcomponents.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
    ! 
    ! Create and attach subgrids to the subcomponents.
    !
    call ESMF_GridGet(gridTop, globalCellCountPerDim=counts, &
                               minGlobalCoordPerDim=mincoords, &
                               maxGlobalCoordPerDim=maxcoords, &
                               horzGridType=horz_gridtype, &
                               vertGridType=vert_gridtype, &       
                               horzStagger=horz_stagger, &      
                               vertStagger=vert_stagger, &
                               horzCoordSystem=horz_coord_system, &
                               vertCoordSystem=vert_coord_system, &
                               rc=rc)

    gridIN = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                             minGlobalCoordPerDim=mincoords, &
                             maxGlobalCoordPerDim=maxcoords, &
                             layout=layoutIN, &
                             horzGridType=horz_gridtype, &
                             vertGridType=vert_gridtype, &       
                             horzStagger=horz_stagger, &      
                             vertStagger=vert_stagger, &
                             horzCoordSystem=horz_coord_system, &
                             vertCoordSystem=vert_coord_system, &
                             name="Injector grid", rc=rc)

    call ESMF_GridCompSet(INcomp, grid=gridIN, rc=rc)

    gridFS = ESMF_GridCreateLogRectUniform(2, counts=counts, &
                             minGlobalCoordPerDim=mincoords, &
                             maxGlobalCoordPerDim=maxcoords, &
                             layout=layoutFS, &
                             horzGridType=horz_gridtype, &
                             vertGridType=vert_gridtype, &       
                             horzStagger=horz_stagger, &      
                             vertStagger=vert_stagger, &
                             horzCoordSystem=horz_coord_system, &
                             vertCoordSystem=vert_coord_system, &
                             name="Flow Solver grid", rc=rc)

    call ESMF_GridCompSet(FScomp, grid=gridFS, rc=rc)


    !
    ! Create import/export states for Injection Component
    !
!BOP
! !DESCRIPTION:
! \subsubsection{Example of State Creation:}
!
!   The following code creates Import and Export States for the
!   Injection subcomponent.  All information being passed to other
!   subcomponents will be described by these States.
!
!\begin{verbatim}
    INimp = ESMF_StateCreate("Injection Input", ESMF_STATEIMPORT,  cnameIN)
    INexp = ESMF_StateCreate("Injection Feedback", ESMF_STATEEXPORT, cnameIN)
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
    FSimp = ESMF_StateCreate("FlowSolver Input", ESMF_STATEIMPORT, cnameFS)
    FSexp = ESMF_StateCreate("FlowSolver Feedback ", ESMF_STATEEXPORT, cnameFS)

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
     localclock = clock

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
        !call ESMF_ClockPrint(localclock, "currtime string", rc)


     enddo
     print *, "Run Loop End time"
     call ESMF_ClockPrint(localclock, "currtime string", rc)
 
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

      ! Finalize FlowSolver Component
      call ESMF_GridCompFinalize(FScomp, FSimp, FSimp, clock, rc=rc)

      ! Finalize Coupler
      call ESMF_CplCompFinalize(cpl, INexp, FSimp, clock, rc=rc)


      ! Then clean them up

      call ESMF_StateDestroy(INimp, rc)
      call ESMF_StateDestroy(INexp, rc)
      call ESMF_StateDestroy(FSimp, rc)
      call ESMF_StateDestroy(FSexp, rc)

      call ESMF_GridCompDestroy(INcomp, rc)
      call ESMF_GridCompDestroy(FScomp, rc)
      call ESMF_CplCompDestroy(cpl, rc)

      call ESMF_DELayoutDestroy(layoutIN, rc)
      call ESMF_DELayoutDestroy(layoutFS, rc)

    end subroutine coupledflow_final


    end module CoupledFlowMod
    
    
