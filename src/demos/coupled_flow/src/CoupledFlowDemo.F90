! $Id: CoupledFlowDemo.F90,v 1.23 2010/11/03 04:58:54 theurich Exp $
!
!------------------------------------------------------------------------------
!BOE
!
! !MODULE: CoupledFlowDemo.F90 - Top level Gridded Component source
!
! !DESCRIPTION:
! ESMF Coupled Flow Demo - A Gridded Component which can be called either 
!   directly from an Application Driver or nested in a larger application.
!   It contains 2 nested subcomponents and 1 Coupler Component which does 
!   two-way coupling between the subcomponents.
!
!EOE
!------------------------------------------------------------------------------
!
!

    module CoupledFlowMod

    ! ESMF module, defines all ESMF data types and procedures
    use ESMF
    
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

!BOE
!  !DESCRIPTION:
! \subsubsection{Example of Set Services Usage:}
!
!   The following code registers with ESMF 
!   the subroutines to be called to Init, Run, and Finalize this component.
!BOC
    ! Register the callback routines.

    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=coupledflow_init, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=coupledflow_run, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=coupledflow_final, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

!EOC
!EOE
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
    integer :: npets, urc
    integer :: mid, by2, quart, by4

    type(ESMF_Grid) :: gridTop, gridIN, gridFS
    type(ESMF_VM) :: vm
    integer :: halo_width(2) = 1, minIndex(2), maxIndex(2)
    type(ESMF_Array) :: coordX, coordY, newCoordX, newCoordY


    ! Get our vm and grid from the component
    call ESMF_GridCompGet(gcomp, vm=vm, grid=gridTop, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    ! Sanity check the number of PETs we were started on.
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
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

!BOE
!
! \subsubsection{Example of Component Creation:}
!
!   The following code creates 2 Gridded Components on the same set of PETs 
!   (persistent execution threads) as the top level Component, but each 
!   of the Grids useds by these Components will have a different connectivity.
!   It also creates a Coupler Component on the same PET set. Each gridded
!   component has a Grid attached internally.
!
!BOC
    cnameIN = "Injector model"
    INcomp = ESMF_GridCompCreate(name=cnameIN, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    cnameFS = "Flow Solver model"
    FScomp = ESMF_GridCompCreate(name=cnameFS, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    cplname = "Two-way coupler"
    cpl = ESMF_CplCompCreate(name=cplname, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC
!EOE

    print *, "Comp Creates finished"


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Register section for subcomponents
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
    call ESMF_GridCompSetServices(INcomp, Injector_register, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    print *, "Injector Comp SetServices finished, rc= ", rc

    call ESMF_GridCompSetServices(FScomp, FlowSolver_register, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    print *, "FlowSolver Comp SetServices finished, rc= ", rc

    call ESMF_CplCompSetServices(cpl, Coupler_register, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    print *, "Coupler Comp SetServices finished, rc= ", rc

 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!  Init section for subcomponents.  Create subgrids on separate DELayouts,
!    and create import/export states for subcomponents.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 
    ! 
    ! Create and attach subgrids to the subcomponents.
    !
    call ESMF_GridGet(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          minIndex=minIndex, maxIndex=maxIndex, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    ! print *, minIndex, maxIndex
    ! Injector Flow Grid
!BOE
! Create the Injector Grid:
!BOC
    gridIN = ESMF_GridCreateShapeTile(minIndex=minIndex, maxIndex=maxIndex, &
                             regDecomp=(/ mid, by2 /), &
                             coordDep1=(/1/), &
                             coordDep2=(/2/), &
                             gridEdgeLWidth=(/0,0/), &
                             name="Injector grid", rc=rc)
!EOC
!EOE
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    ! Copy coordinates for the other staggers
    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_EDGE2, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_EDGE2, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_EDGE2, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_EDGE2, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

!BOE
! Set the Injector Grid in the Injector Component:
!BOC
    call ESMF_GridCompSet(INcomp, grid=gridIN, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC
!EOE

    ! FlowSolver Grid
!BOE
! Create the FlowSolver Grid:
!BOC
    gridFS = ESMF_GridCreateShapeTile(minIndex=minIndex, maxIndex=maxIndex, &
                             regDecomp=(/ quart, by4 /), &
                             coordDep1=(/1/), &
                             coordDep2=(/2/), &
                             gridEdgeLWidth=(/0,0/), &
                             name="Flow Solver grid", rc=rc)
!EOC
!EOE
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridFS, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridFS, &
          staggerLoc=ESMF_STAGGERLOC_CENTER, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_EDGE1, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_EDGE2, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridGetCoord(gridTop, &
          staggerLoc=ESMF_STAGGERLOC_EDGE2, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_EDGE2, &
          coordDim=1, array=CoordX, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    call ESMF_GridSetCoord(gridIN, &
          staggerLoc=ESMF_STAGGERLOC_EDGE2, &
          coordDim=2, array=CoordY, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

!BOE
! Set the FlowSolver Grid in the FlowSolver Component:
!BOC
    call ESMF_GridCompSet(FScomp, grid=gridFS, rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC
!EOE

    !
    ! Create import/export states for Injection Component
    !
!BOE
!
! \subsubsection{Example of State Creation:}
!
!   The following code creates Import and Export States for the
!   Injection subcomponent.  All information being passed between
!   subcomponents will be described by these States.
!
!BOC
    INimp = ESMF_StateCreate(statename="Injection Input", statetype=ESMF_STATE_IMPORT, &
        rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    INexp = ESMF_StateCreate(statename="Injection Feedback", statetype=ESMF_STATE_EXPORT, &
        rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC
!EOE
    ! 
    ! Initialize the injector component, first phase 
    !
    call ESMF_GridCompInitialize(INcomp, INimp, INexp, clock, phase=1, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
    print *, "Injection Model Initialize finished, rc =", rc
 
    !
    ! Create import/export states for FlowSolver Component
    !
    FSimp = ESMF_StateCreate(statename="FlowSolver Input", statetype=ESMF_STATE_IMPORT, &
        rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    FSexp = ESMF_StateCreate(statename="FlowSolver Feedback ", statetype=ESMF_STATE_EXPORT, &
        rc=rc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

    !
    ! Initialize the flow solver component, first phase
    !
    call ESMF_GridCompInitialize(FScomp, FSimp, FSexp, clock, phase=1, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
    print *, "Flow Model Initialize finished, rc =", rc

    !
    ! Initialize the coupler, once for each export state
    ! (note this is not 2 phases - it is calling the same code each time.)
    !
    call ESMF_CplCompInitialize(cpl, FSexp, INimp, clock, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
    call ESMF_CplCompInitialize(cpl, INexp, FSimp, clock, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
    print *, "Coupler Initialize finished, rc =", rc
 
    !
    ! Second phase of init
    !
    call ESMF_GridCompInitialize(INcomp, INimp, INexp, clock, phase=2, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
    print *, "Injection Model Initialize finished, rc =", rc
 
    call ESMF_GridCompInitialize(FScomp, FSimp, FSexp, clock, phase=2, rc=rc, userRc=urc)
    if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
    if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
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
     integer          :: urc


!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Time Stepping Loop:}
!
! Advancing in time with ESMF clock, the coupled flow component calls
! the run methods of the gridded components and coupler component sequentially:
!BOC
     ! Make our own local copy of the clock
     localclock = ESMF_ClockCreate(clock, rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

     print *, "Run Loop Start time"
     call ESMF_ClockPrint(localclock, "currtime string", rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

     do while (.not. ESMF_ClockIsStopTime(localclock, rc))

        ! Run FlowSolver Component
        call ESMF_GridCompRun(FScomp, FSimp, FSexp, localclock, rc=rc, userRc=urc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
        if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)

        ! Couple export state of FlowSolver to import of Injector
        call ESMF_CplCompRun(cpl, FSexp, INimp, localclock, rc=rc, userRc=urc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
        if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
  
        ! Run Injector Component
        call ESMF_GridCompRun(INcomp, INimp, INexp, localclock, rc=rc, userRc=urc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
        if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
  
        ! Couple export state of Injector to import of FlowSolver
        call ESMF_CplCompRun(cpl, INexp, FSimp, localclock, rc=rc, userRc=urc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
        if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)
  
        ! Advance the time
        call ESMF_ClockAdvance(localclock, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      
        ! This demo runs a lot of time steps and only outputs files
        ! every N iterations.  This print statement, if commented in,
        ! generates a lot of output.
        !call ESMF_ClockPrint(localclock, "currtime string", rc)

     enddo

     print *, "Run Loop End time"
     call ESMF_ClockPrint(localclock, "currtime string", rc=rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC
!EOE
 
!BOE
!
! !DESCRIPTION:
! \subsubsection{Example of Clock Destruction:}
!
! At the end of run method, destroy the clock used to iterate through time:
!BOC
     call ESMF_ClockDestroy(localclock, rc)
     if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!EOC
!EOE

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

      integer :: urc

      ! First finalize all subcomponents

      ! Finalize Injector Component    
      call ESMF_GridCompFinalize(INcomp, INimp, INexp, clock, rc=rc, userRc=urc)
      if (rc .ne. ESMF_SUCCESS .or. urc .ne. ESMF_SUCCESS) then
          print *, "Injector Component Finalize routine returned error"
          return
      endif

      ! Finalize FlowSolver Component
      call ESMF_GridCompFinalize(FScomp, FSimp, FSimp, clock, rc=rc, userRc=urc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)

      ! Finalize Coupler
      call ESMF_CplCompFinalize(cpl, INexp, FSimp, clock, rc=rc, userRc=urc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      if(urc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=urc)

      print *, "CoupledFlowMod finished calling all subcomponent Finalize routines"

      ! Then clean them up

      print *, "ready to destroy all states"
      call ESMF_StateDestroy(INimp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateDestroy(INexp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateDestroy(FSimp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateDestroy(FSexp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      print *, "ready to destroy all components"
      call ESMF_GridCompDestroy(INcomp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridCompDestroy(FScomp, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_CplCompDestroy(cpl, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      print *, "end of CoupledFlowMod Finalization routine"
      rc = ESMF_SUCCESS

    end subroutine coupledflow_final


    end module CoupledFlowMod
    
    
