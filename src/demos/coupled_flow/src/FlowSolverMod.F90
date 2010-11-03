! $Id: FlowSolverMod.F90,v 1.23 2010/11/03 22:48:47 theurich Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !MODULE: FlowSolverMod.F90 - Source file for Flow Solver Component
!
! !DESCRIPTION:
!  This component does a finite difference solution of the PDE's for
!  semi-compressible fluid flow.  It uses an explicit solution
!  method on a staggered mesh with velocities and momentum located at cell
!  faces and other physical quantities at cell centers.  The component
!  assumes a logically rectangular two-dimensional Cartesian mesh with
!  constant cell spacing.  It also employs a donor-cell advection scheme.
!  Although the algorithm is general, the boundary conditions are coded to
!  assume constant inflow on the left, outflow on the right, and free-slip
!  insulated boundaries on the top and bottom.  This component will allow
!  the user to construct flow obstacles with different energies, and it
!  accepts a second inflow from the bottom boundary that can be controlled
!  by a second component.  For material properties, this component uses an
!  ideal gas equation of state, and assumes constant ratio of specific
!  heats, thermal conductivity, and specific heat capacity.  There is no
!  system of units assumed by the component -- it is up to the user to
!  ensure dimensional consistency.
!
!  The following are the semi-compressible flow equations used in this 
!  component.
!
!$\frac{\partial \rho}{\partial t} + \frac{\partial \rho u}{\partial x}
!+ \frac{\partial \rho v}{\partial y} = 0 $
!
!$\frac{\partial \rho u}{\partial t} + \frac{\partial \rho u^{2}}{\partial x} 
!+ \frac{\partial \rho u v}{\partial y} = - \frac{\partial(p + q)}{\partial x}$
!
!$\frac{\partial \rho v}{\partial t} + \frac{\partial \rho u v}{\partial x} 
!+ \frac{\partial \rho v^{2}}{\partial y} = - \frac{\partial(p + q)}{\partial y}$
!
!$\frac{\partial p I}{\partial t} + \frac{\partial \rho u I}{\partial x} +
!\frac{\partial \rho v I}{\partial y} = -(p + q)\left(\frac{\partial u}
!{\partial x} + \frac{\partial v}{\partial y}\right) + \frac{k}{b}\left(
!\frac{\partial^{2}I}{\partial x^{2}} + \frac{\partial^{2}I}{\partial y^{2}}\right)$
!
!$p = (\gamma - 1)\rho I$
!
!$q = -q_{o}\rho u_{in}(dx^{2} + dy^{2})^{1/2} \left(\frac{\partial u}
!{\partial x} + \frac{\partial v}{\partial y}\right)$
!
!$if q < 0 set q = 0$
!
!Where
!\begin{tabular}{ll}
!$\rho$ & density \\
!$t$ & time \\
!$u$ & x-component of velocity \\
!$v$ & y-component of velocity\\
!$p$ & pressure\\
!$q$ & artificial velocity\\
!$I$ & standard internal energy\\
!$\gamma$ & ratio of specific heats\\
!$k$ & thermal conductivity\\
!$b$ & specific heat capacity\\
!$q_{o}$ & artificial viscosity coefficient, dimensionless\\
!$u_{in}$ & inflow velocity (representative velocity)\\
!\end{tabular}
!
!
!EOP

      module FlowSolverMod
!
! ESMF modules
!
      use ESMF_Mod
      use FlowArraysMod
    
      implicit none

      private
    
      public FlowSolver_register

      contains

!-------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowSolver_register - Registers the init, run, and finalize routines.

! !INTERFACE:
      subroutine FlowSolver_register(comp, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: comp
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     The register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that these are
!     private to the module.
!     \begin{description}
!     \item [comp]
!           A Gridded Component.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
! Initialize return code
!
      rc = ESMF_FAILURE
!
! Register the callback routines.
!
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=Flow_Init1, phase=1, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, userRoutine=Flow_Init2, phase=2, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, userRoutine=FlowSolve, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, userRoutine=Flow_Final, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      print *, "FlowSolverMod: Registered Initialize, Run, and Finalize routines"

      rc = ESMF_SUCCESS

      end subroutine FlowSolver_register

!-------------------------------------------------------------------------
!BOPI
! !IROUTINE: Flow_Init1 - Initialization routine

! !INTERFACE:
      subroutine Flow_Init1(gcomp, import_state, export_state, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     This subroutine is the registered init routine.  It reads input,
!     creates the Grid, attaches it to the Gridded Component,
!     initializes data, and sets the import and export States.
!     \begin{description}
!     \item [gcomp]
!           A Gridded Component.
!     \item [import\_state]
!           State containing the import list.
!     \item [export\_state]
!           State containing the export list.
!     \item [clock]
!           Clock describing the external time.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), dimension(ESMF_MAXDIM) :: global_min_coord
      real(ESMF_KIND_R8), dimension(ESMF_MAXDIM) :: global_max_coord
      integer :: counts(2), elb(2), eub(2)
      real(ESMF_KIND_R8), pointer :: coordX(:), coordY(:)
      namelist /input/ uin, rhoin, siein, &
                       gamma, akb, q0, u0, v0, sie0, rho0, &
                       printout, sieobs, nobsdesc, iobs_min, iobs_max, &
                       jobs_min, jobs_max, iflo_min, iflo_max
!BOP
!
! !DESCRIPTION:
! \subsubsection{Namelist Input Parameters for Flowsolver:}
!     The following variables must be input to the FlowSolver Component to run.
!     They are located in a file called "coupled\_flow\_input."
!
!     The variables are:
!     \begin{description}
!     \item [uin]
!           Inflow velocity at left boundary.
!     \item [rhoin]
!           Inflow density at left boundary.
!     \item [siein]
!           Inflow specific internal energy at left boundary.
!     \item [gamma]
!           Ratio of specific heats for the fluid (assumed constant).
!     \item [akb]
!           Thermal conductivity over specific heat capacity (assumed
!           constant).
!     \item [q0]
!           Dimensionless linear artificial viscosity coefficient
!           (should be between 0.1 and 0.2).
!     \item [u0]
!           Initial velocity in the first grid direction.
!     \item [v0]
!           Initial velocity in the second grid direction.
!     \item [sie0]
!           Initial specific internal energy.
!     \item [rho0]
!           Initial density.
!     \item [printout]
!           Number of cycles between graphical output files.
!     \item [sieobs]
!           Specific internal energy of the obstacles.
!     \item [nobsdesc]
!           Number of obstacle descriptors.  Each descriptor defines a
!           block of cells that will serve as an obstacle and not allow
!           fluid flow.
!     \item [iobs\_min]
!           Minimum global cell number in the first grid direction defining
!           a block of cells to be an obstacle.  Must be [nobsdesc] number
!           of these.
!     \item [iobs\_max]
!           Maximum global cell number in the first grid direction defining
!           a block of cells to be an obstacle.  Must be [nobsdesc] number
!           of these.
!     \item [jobs\_min]
!           Minimum global cell number in the second grid direction defining
!           a block of cells to be an obstacle.  Must be [nobsdesc] number
!           of these.
!     \item [jobs\_max]
!           Maximum global cell number in the second grid direction defining
!           a block of cells to be an obstacle.  Must be [nobsdesc] number
!           of these.
!     \item [iflo\_min]
!           Minimum global grid cell number for the second inflow along the
!           bottom boundary.
!     \item [iflo\_max]
!           Maximum global grid cell number for the second inflow along the
!           bottom boundary.
!     \end{description}
!
!EOP
!
! Initialize return code
!
      rc = ESMF_FAILURE
!
! Read in input file
!
      open(10, status="old", file="coupled_flow_input",action="read",iostat=rc)
      if (rc .ne. 0) then
        print *, "Error!  Failed to open namelist file 'coupled_flow_input' "
        stop
      endif
      read(10, input, end=20)
   20 continue
!
! Query component for information.
!
      call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

!
! Extract and calculate some other quantities
!
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridGet(grid, localDE=0, staggerloc=ESMF_STAGGERLOC_CENTER, &
        exclusiveLBound=elb, exclusiveUBound=eub, &
        rc = rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, fptr=CoordX, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_GridGetCoord(grid, localDE=0, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, fptr=CoordY, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      dx = (CoordX(eub(1))-CoordX(elb(1)))/(eub(1)-elb(1))
      dy = (CoordY(eub(2))-CoordY(elb(2)))/(eub(2)-elb(2))

!
! Initialize the data
!
      call FlowInit(gcomp, clock, rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flow_init:  flowinit"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!
! Precompute the Halo communication pattern; since all data variables are
! the same data type and size, the same handle can be reused for all of them.
!
      call ESMF_FieldHaloStore(field_u, routehandle=halohandle, rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!
! For initialization, add all fields to the import state.  Only the ones
! needed will be copied over to the export state for coupling.
!
      call ESMF_StateAdd(import_state, field_sie, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(import_state, field_u, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(import_state, field_v, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(import_state, field_rho, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(import_state, field_p, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(import_state, field_q, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(import_state, field_flag, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      !
      ! This is adding names only to the export list, marked
      ! as "not needed".  The coupler will mark the ones needed based
      ! on the requirements of the component(s) this is coupled to.
      !
      call ESMF_StateAdd(export_state, "SIE", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(export_state, "U", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(export_state, "V", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(export_state, "RHO", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(export_state, "P", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(export_state, "Q", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_StateAdd(export_state, "FLAG", rc=rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

! Give the export state an initial set of values for the SIE Field.
      call ESMF_StateAdd(export_state, field_sie, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      rc = ESMF_SUCCESS

      end subroutine Flow_Init1

!-------------------------------------------------------------------------
!BOPI
! !IROUTINE: Flow_Init2 - Initialization routine, second phase

! !INTERFACE:
      subroutine Flow_Init2(gcomp, import_state, export_state, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     This subroutine is the second phase of the init routine.
!     It sets the export fields in the export state for what's required.
!     \begin{description}
!     \item [gcomp]
!           A Gridded Component.
!     \item [import\_state]
!           State containing the import list.
!     \item [export\_state]
!           State containing the export list.
!     \item [clock]
!           Clock describing the external time.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: i, datacount, count
      character(len=ESMF_MAXSTR), dimension(7) :: datanames
      type(ESMF_Field) :: thisfield
      type(ESMF_StateItemType) :: itemtype
!
! Initialize return code
!
      rc = ESMF_FAILURE
      datacount = 7
      datanames(1) = "SIE"
      datanames(2) = "U"
      datanames(3) = "V"
      datanames(4) = "RHO"
      datanames(5) = "P"
      datanames(6) = "Q"
      datanames(7) = "FLAG"

      !
      ! Update export state with needed fields
      !
      do i=1, datacount

        ! check isneeded flag here
        if (.not. ESMF_StateIsNeeded(export_state, itemName=datanames(i), rc=rc)) then 
           cycle
        endif

        ! add Field from importState to exportState if it doesn't exist in exportState
        call ESMF_StateGet(export_state, itemSearch=datanames(i), itemCount=count, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
        call ESMF_StateGet(export_state, name=datanames(i), stateitemtype=itemtype, rc=rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
        if((count .lt. 1) .or. (itemtype .eq. ESMF_STATEITEM_NAME)) then 
          ! Set export data in export state
          call ESMF_StateGet(import_state, itemName=datanames(i), field=thisfield, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
          call ESMF_StateAdd(export_state, field=thisfield, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
        endif

      enddo

      rc = ESMF_SUCCESS

      end subroutine Flow_Init2

!-------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowInit - Initializes data for the FlowSolver.

! !INTERFACE:
      subroutine FlowInit(gcomp, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: gcomp
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc
!
! !DESCRIPTION:
!     The FlowInit routine initializes data necessary to run the
!     FlowSolver.
!     \begin{description}
!     \item [gcomp]
!           A Gridded Component.
!     \item [clock]
!           Clock describing the external time.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: status
      integer :: i, j, n, x, y, nx, ny, ncounts(2), pos(2), de_id
      integer, dimension(1,2) :: local, global
      real(ESMF_KIND_R8) :: s_
      type(ESMF_Grid) :: grid
      type(ESMF_DistGrid) :: distgrid
      type(ESMF_DELayout) :: layout
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!
! get Grid from Component
!
      call ESMF_GridCompGet(gcomp, grid=grid, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  grid comp get"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
!
! create space for global arrays
!
      call FlowArraysAlloc(grid, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  arraysglobalalloc"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
!
! flags for boundary conditions (ordered left, right, bottom, top):
!     nbc(i) = 1       inflow
!     nbc(i) = 2       outflow
!     nbc(i) = 3       insulated, free-slip
!     nbc(i) = 4       insulated, no-slip
!     nbc(i) = 5       specified temperature, free-slip
!     nbc(i) = 6       specified temperature, no-slip
! TODO:  could be read in, but for now set here
!
      nbc(1) = 1
      nbc(2) = 2
      nbc(3) = 3
      nbc(4) = 3
!
! set flags, based on NBC's and position in delayout
! note: since we're operating on a five-point stencil, we don't
!       really care out how the corners get set
!
! First, get size of delayout and position of my DE to determine if
! this DE is on the domain boundary
!
      ncounts = 1
      de_id = 0
      pos = 1
      nx = ncounts(1)
      ny = ncounts(2)
      x = pos(1)
      y = pos(2)
!
! Set all cells to 0 by default
!
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          flag(i,j) = 0.0
        enddo
      enddo
!
! Left boundary
!
      if (x.eq.1) then
        do j = jmin_t, jmax_t
          do i = imin_t, imin-1
            flag(i,j) = 1.0
          enddo
        enddo
      endif
!
! Right boundary
!
      if (x.eq.nx) then
        do j = jmin_t, jmax_t
          do i = imax+1, imax_t
            flag(i,j) = 2.0
          enddo
        enddo
      endif
!
! Bottom boundary
!
      if (y.eq.1) then
        do j = jmin_t, jmin-1
          do i = imin_t, imax_t
            flag(i,j) = 3.0
          enddo
        enddo
      endif
!
! Top boundary
!
      if (y.eq.ny) then
        do j = jmax+1, jmax_t
          do i = imin_t, imax_t
            flag(i,j) = 4.0
          enddo
        enddo
        do i = imin_t, imax_t
          if (flag(i,jmax).eq.0.0) flag(i,jmax) = 5.0
        enddo
      endif
!
! set up initial velocities 
!
! u has uneven halo in i direction
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          u(i,j) = u0
          rhou(i,j) = rho0*u0
        enddo
      enddo
! v has uneven halo in j direction
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          v(i,j) = v0
          rhov(i,j) = rho0*v0
        enddo
      enddo
!
! set up initial state properties
!
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          p(i,j) = (gamma-1.0) * rho0 * sie0
          q(i,j) = q0
          sie(i,j) = sie0
          rho(i,j) = rho0
          rhoi(i,j) = rho0*sie0
        enddo
      enddo
!
! initialize inlet parameters TODO:  assume for now only left boundary
!                                    can be inflow
!
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          if (flag(i,j).eq.1.0) then
            rho(i,j) = rhoin
            sie(i,j) = siein
            rhoi(i,j) = rhoin*siein
          endif
        enddo
      enddo
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          if (flag(i,j).eq.1.0) then
            u(i,j) = uin
            rhou(i,j) = rhoin*uin
          endif
        enddo
      enddo
!
! add any obstacles
!
      do n = 1, nobsdesc
        do j = jobs_min(n),jobs_max(n)
          global(1,2) = j
          do i = iobs_min(n),iobs_max(n)
            global(1,1) = i
            local = global
            if (local(1,1).gt.-1) local(1,1) = local(1,1) + 1
            if (local(1,2).gt.-1) local(1,2) = local(1,2) + 1
            if(local(1,1).ne.-1 .and. local(1,2).ne.-1) then
              flag(local(1,1)-1,local(1,2)-1) = -1
            endif
          enddo
        enddo
      enddo
!
! add injector section
!
      do i = iflo_min, iflo_max
        do j = 1, 2 
          global(1,1) = i
          global(1,2) = j
          local = global
          if (local(1,1).gt.-1) local(1,1) = local(1,1) + 1
          if (local(1,2).gt.-1) local(1,2) = local(1,2) + 1
          if(local(1,1).ne.-1 .and. local(1,2).ne.-1) then
            flag(local(1,1)-1,local(1,2)-1) = 10
          endif
        enddo
      enddo
!
! obstacle normal boundary conditions
!
      do j = jmin, jmax
        do i = imin, imax
          if (flag(i,j) .eq. -1.0) then
            u(i,j) = 0.0
            rhou(i,j) = 0.0
          endif
          if (flag(i,j) .eq. -1.0) then
            v(i,j) = 0.0
            rhov(i,j) = 0.0
          endif
          if (flag(i+1,j) .eq. -1.0) then
            u(i,j) = 0.0
            rhou(i,j) = 0.0
          endif
          if (flag(i,j+1) .eq. -1.0) then
            v(i,j) = 0.0
            rhov(i,j) = 0.0
          endif
        enddo
      enddo
!
! obstacle tangential boundary conditions for free-slip
!
      do j = jmin, jmax
        do i = imin, imax
          if (flag(i,j).eq.-1.0 .and. flag(i,j+1).ne.-1.0 .and. flag(i+1,j).eq.-1.0) then
            u(i,j) = u(i,j+1)
            rhou(i,j) = rhou(i,j+1)
          endif
          if (flag(i,j).eq.-1.0 .and. flag(i,j-1).ne.-1.0 .and. flag(i+1,j).eq.-1.0) then
            u(i,j) = u(i,j-1)
            rhou(i,j) = rhou(i,j-1)
          endif
          if (flag(i,j).eq.-1.0 .and. flag(i+1,j).ne.-1.0 .and. flag(i,j+1).eq.-1.0) then
            v(i,j) = v(i+1,j)
            rhov(i,j) = rhov(i+1,j)
          endif
          if (flag(i,j).eq.-1.0 .and. flag(i-1,j).ne.-1.0 .and. flag(i,j+1).eq.-1.0) then
            v(i,j) = v(i-1,j)
            rhov(i,j) = rhov(i-1,j)
          endif
        enddo
      enddo
!
! initialize timestep from ESMF Clock
!
      call ESMF_ClockGet(clock, timeStep=time_step, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: clock get timestep"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_TimeIntervalGet(time_step, s_r8=s_, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: time interval get"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      dt = s_
      write(*,*) 'dt = ', dt
! 
! Check initial data for stability
!
      call FlowStability(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flow stability"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowInit

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowSolve - Run routine for this component, solves PDE's

! !INTERFACE:
      subroutine FlowSolve(gcomp, import_state, export_state, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     The FlowSolve subroutine is the registered "run" routine for the
!     FlowSolver.  It calls all other necessary routines for the FlowSolver
!     algorithm and checks the output interval.
!     \begin{description}
!     \item [gcomp]
!           A Gridded Component.
!     \item [import\_state]
!           State containing data obtained from other components.
!     \item [export\_state]
!           State containing data needed by other components.
!     \item [clock]
!           An {\tt ESMF\_Clock} object containing the current time,
!           time step, and stop time.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: i, j
      integer :: counter = 0
      integer :: print_count = 0
      real(ESMF_KIND_R8) :: s_
      integer :: datacount, count
      character(len=ESMF_MAXSTR), dimension(7) :: datanames
      type(ESMF_Field) :: thisfield
      type(ESMF_StateItemType) :: itemtype

      datacount = 7
      datanames(1) = "SIE"
      datanames(2) = "U"
      datanames(3) = "V"
      datanames(4) = "RHO"
      datanames(5) = "P"
      datanames(6) = "Q"
      datanames(7) = "FLAG"
!
! Set initial values
!
      rc = ESMF_FAILURE
!
! Increment counter
!
      counter = counter + 1
! 
! Get timestep from clock
!
      call ESMF_ClockGet(clock, timeStep=time_step, rc=rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: clock get timestep"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      call ESMF_TimeIntervalGet(time_step, s_r8=s_, rc=rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: time interval get"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      dt = s_
!
! Copy injection values from exclusive domain into ghost cells
! and set other physical parameters to be consistent
!
      do j = jmin, jmax
        do i = imin, imax
          if (flag(i,j).eq.10) then
            if (flag(i,j-1).eq.10) then
              sie(i,j-1) = sie(i,j)
              v(i,j-1) = v(i,j)
              rho(i,j-1) = rho(i,j)
            endif
            rhoi(i,j) = rho(i,j)*sie(i,j)
            rhov(i,j) = rho(i,j)*v(i,j)
            rhou(i,j) = 0.0
            u(i,j) = 0.0
          endif
        enddo
      enddo
!
! calculate RHOU's and RHOV's (momentum)
!
      call FlowRhoVel(rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowrhovel"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!    
! calculate RHOI's (energy)
!
      call FlowRhoI(rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowrhoi"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!    
! determine new densities and internal energies
!
      call FlowRho(rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowrho"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!
!  update velocities
!
      call FlowVel(rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowvel"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!
!  new pressures and viscosities
!
      call FlowState(rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
!
!  put needed data in export state
!
      do i=1, datacount
          if (.not. ESMF_StateIsNeeded(export_state, itemName=datanames(i), rc=rc)) then 
              cycle
          endif
          ! add Field from importState to exportState if it doesn't exist in exportState
          call ESMF_StateGet(export_state, itemSearch=datanames(i), itemCount=count, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
          call ESMF_StateGet(export_state, name=datanames(i), stateitemtype=itemtype, rc=rc)
          if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
          if((count .lt. 1) .or. (itemtype .eq. ESMF_STATEITEM_NAME)) then 
            ! Set export data in export state
            call ESMF_StateGet(import_state, itemName=datanames(i), field=thisfield, rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
            call ESMF_StateAdd(export_state, field=thisfield, rc=rc)
            if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
          endif
      enddo
!
! Print graphics every printout steps
!
      if(mod(counter, printout) .eq. 0) then
        print_count = print_count + 1
        call FlowPrint(gcomp, clock, print_count, rc)
        if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
      endif

      rc = ESMF_SUCCESS

      end subroutine FlowSolve

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowRhoVel - Calculate momentum arrays RHOU and RHOV.

! !INTERFACE:
      subroutine FlowRhoVel(rc)
!
! !ARGUMENTS:
      integer, optional, intent(out) :: rc
!
! !DESCRIPTION:
!     The FloRhoVel routine calculates the momentum arrays RHOU and RHOV
!     with donor-cell advection.
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: status
      integer :: i, j
      real(kind=ESMF_KIND_R4) :: u_ij, u_ipj, rhouu_m, rhouu_p
      real(kind=ESMF_KIND_R4) :: v_ipjm, v_ipjp, rhouv_p, rhouv_m
      real(kind=ESMF_KIND_R4) :: v_ij, v_ijp, rhovv_m, rhovv_p
      real(kind=ESMF_KIND_R4) :: u_imjp, u_ipjp, rhovu_p, rhovu_m
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!
! calculate RHOU's and RHOV's
!
      do j = jmin, jmax
        do i = imin, imax
          u_ij  = 0.5 * (u(i-1,j) + u(i,j))
          u_ipj = 0.5 * (u(i+1,j) + u(i,j))
          if (u_ij .ge. 0.0) then
            rhouu_m = u_ij * rhou(i-1,j)
          else
            rhouu_m = u_ij * rhou(i,j)
          endif
          if (u_ipj .ge. 0.0) then
            rhouu_p = u_ipj * rhou(i,j)
          else
            rhouu_p = u_ipj * rhou(i+1,j)
          endif
          v_ipjm = 0.5 * (v(i,j-1) + v(i+1,j-1))
          v_ipjp = 0.5 * (v(i,j)   + v(i+1,j))
          if (v_ipjm .ge. 0.0) then
            rhouv_m = v_ipjm * rhou(i,j-1)
          else
            rhouv_m = v_ipjm * rhou(i,j)
          endif
          if (v_ipjp .ge. 0.0) then
            rhouv_p = v_ipjp * rhou(i,j)
          else
            rhouv_p = v_ipjp * rhou(i,j+1)
          endif
          rhou(i,j) = rhou(i,j) + (dt/dx)*(rhouu_m-rhouu_p) &
                    + (dt/dy)*(rhouv_m-rhouv_p) &
                    + (dt/dx)*(p(i,j)+q(i,j)-p(i+1,j)-q(i+1,j))
        enddo
      enddo
!
! Update RHOU with Halo
!
!BOE
!
!
! \subsubsection{Example of FieldHalo Usage:}
!
!     The following piece of code provides an example of haloing the data in a
!     Field.  Currently the Field halo routine assumes the entire halo is 
!     updated completely; i.e. the user cannot specify halo width or side 
!     separately.
!     Field halo uses a Route object to transfer data from the exclusive
!     domain of one DE to the halo region of another.
!BOC
      call ESMF_FieldHalo(field_rhou, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoVel:  rhou halo"
        if(present(rc)) rc = status
        return
      endif
!EOC
!EOE

      do j = jmin, jmax
        do i = imin, imax
          v_ij  = 0.5 * (v(i,j-1) + v(i,j))
          v_ijp = 0.5 * (v(i,j+1) + v(i,j))
          if (v_ij .ge. 0.0) then
            rhovv_m = v_ij * rhov(i,j-1)
          else
            rhovv_m = v_ij * rhov(i,j)
          endif
          if (v_ijp .ge. 0.0) then
            rhovv_p = v_ijp * rhov(i,j)
          else
            rhovv_p = v_ijp * rhov(i,j+1)
          endif
          u_imjp = 0.5 * (u(i-1,j) + u(i-1,j+1))
          u_ipjp = 0.5 * (u(i,j)   + u(i,j+1))
          if (u_imjp .ge. 0.0) then
            rhovu_m = u_imjp * rhov(i-1,j)
          else
            rhovu_m = u_imjp * rhov(i,j)
          endif
          if (u_ipjp .ge. 0.0) then
            rhovu_p = u_ipjp * rhov(i,j)
          else
            rhovu_p = u_ipjp * rhov(i+1,j)
          endif
          rhov(i,j) = rhov(i,j) + (dt/dy)*(rhovv_m-rhovv_p) &
                    + (dt/dx)*(rhovu_m-rhovu_p) &
                    + (dt/dy)*(p(i,j)+q(i,j)-p(i,j+1)-q(i,j+1))
        enddo
      enddo
!
! Update RHOV with halo
!
      call ESMF_FieldHalo(field_rhov, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoVel:  rhov halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
  
      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowRhoVel

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowRhoI - Calculate heat energy array RHOI.

! !INTERFACE:
      subroutine FlowRhoI(rc)
!
! !ARGUMENTS:
      integer, optional, intent(out) :: rc
!
! !DESCRIPTION:
!     The FlowRhoI routine calculates the heat energy array RHOI using a
!     donor-cell scheme.
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: status
      integer :: i, j
      real(kind=ESMF_KIND_R4) :: rhoiu_m, rhoiu_p, rhoiv_m, rhoiv_p
      real(kind=ESMF_KIND_R4) :: dsiedx2, dsiedy2
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!    
! calculate RHOI's
!
      do j = jmin, jmax
        do i = imin, imax
          if (flag(i,j).ge.0.0) then
          if (u(i-1,j) .ge. 0.0) then
            rhoiu_m = u(i-1,j) * rhoi(i-1,j)
          else
            rhoiu_m = u(i-1,j) * rhoi(i,j)
          endif
          if (u(i,j) .ge. 0.0) then
            rhoiu_p = u(i,j) * rhoi(i,j)
          else
            rhoiu_p = u(i,j) * rhoi(i+1,j)
          endif
          if (v(i,j-1) .ge. 0.0) then
            rhoiv_m = v(i,j-1) * rhoi(i,j-1)
          else
            rhoiv_m = v(i,j-1) * rhoi(i,j)
          endif
          if (v(i,j) .ge. 0.0) then
            rhoiv_p = v(i,j) * rhoi(i,j)
          else
            rhoiv_p = v(i,j) * rhoi(i,j+1)
          endif
          dsiedx2 = (sie(i+1,j)+sie(i-1,j)-2.*sie(i,j))/dx**2
          dsiedy2 = (sie(i,j+1)+sie(i,j-1)-2.*sie(i,j))/dy**2
!
! Add boundary conditions to second derivatives
!
          if (flag(i+1,j).eq.-1.0) dsiedx2 = (2.*sieobs+sie(i-1,j)-3.*sie(i,j))/dx**2
          if (flag(i-1,j).eq.-1.0) dsiedx2 = (sie(i+1,j)+2.*sieobs-3.*sie(i,j))/dx**2
          if (flag(i,j+1).eq.-1.0) dsiedy2 = (2.*sieobs+sie(i,j-1)-3.*sie(i,j))/dy**2
          if (flag(i,j-1).eq.-1.0) dsiedy2 = (sie(i,j+1)+2.*sieobs-3.*sie(i,j))/dy**2
          if (flag(i-1,j).eq.1.0) dsiedx2 = (sie(i+1,j)+2.*siein-3.*sie(i,j))/dx**2
          if (flag(i+1,j).eq.2.0) dsiedx2 = (sie(i-1,j)-sie(i,j))/dx**2
          if (flag(i,j-1).eq.3.0) dsiedy2 = (sie(i,j+1)-sie(i,j))/dy**2
          if (flag(i,j+1).eq.4.0) dsiedy2 = (sie(i,j-1)-sie(i,j))/dy**2

          rhoi(i,j) = rhoi(i,j) + (dt/dx)*(rhoiu_m-rhoiu_p) &
                    + (dt/dy)*(rhoiv_m-rhoiv_p) &
                    - dt*(p(i,j)+q(i,j))*((u(i,j)-u(i-1,j))/dx &
                                        + (v(i,j)-v(i,j-1))/dy) &
                    + dt*akb*(dsiedx2+dsiedy2)
          endif
        enddo
      enddo
!
!  add boundary conditions to RHOI's
!
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          if (flag(i,j).eq.1.0) then
            rhoi(i,j) = rhoin*siein 
          endif
          if (flag(i,j).eq.2.0) then
            rhoi(i,j) = rhoi(imax,j)
          endif
          if (flag(i,j).eq.3.0) then
            rhoi(i,j) = rhoi(i,jmin)
          endif
          if (flag(i,j).eq.4.0) then
            rhoi(i,j) = rhoi(i,jmax)
          endif
        enddo
      enddo
!
! Update RHOI with halo
!
      call ESMF_FieldHalo(field_rhoi, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoI:  rhoi halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowRhoI

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowRho - Calculate density and standard energy arrays RHO and SIE.

! !INTERFACE:
      subroutine FlowRho(rc)
!
! !ARGUMENTS:
      integer, optional, intent(out) :: rc
!
! !DESCRIPTION:
!     The FlowRho routine calculates the density and standard internal energy
!     arrays RHO and SIE.
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: status
      integer :: i, j
      real(kind=ESMF_KIND_R4), dimension(imax,jmax) :: rho_new
      real(kind=ESMF_KIND_R4) :: rhou_m, rhou_p, rhov_m, rhov_p
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!    
! determine new densities
!
      do j = jmin, jmax
        do i = imin, imax
          if (u(i-1,j).ge.0.0) then
            rhou_m = u(i-1,j)*rho(i-1,j)
          else
            rhou_m = u(i-1,j)*rho(i,j)
          endif
          if (u(i,j).ge.0.0) then
            rhou_p = u(i,j)*rho(i,j)
          else
            rhou_p = u(i,j)*rho(i+1,j)
          endif
          if (v(i,j-1).ge.0.0) then
            rhov_m = v(i,j-1)*rho(i,j-1)
          else
            rhov_m = v(i,j-1)*rho(i,j)
          endif
          if (v(i,j).ge.0.0) then
            rhov_p = v(i,j)*rho(i,j)
          else
            rhov_p = v(i,j)*rho(i,j+1)
          endif
          rho_new(i,j) = rho(i,j) + (dt/dx)*(rhou_m-rhou_p) &
                       + (dt/dy)*(rhov_m-rhov_p)
        enddo
      enddo
!
!  update densities and internal energies
!
      do j = jmin, jmax
        do i = imin, imax
          rho(i,j) = rho_new(i,j)
          if (rho_new(i,j).gt.0.0) sie(i,j) = rhoi(i,j)/rho_new(i,j)
        enddo
      enddo
!
!  add boundary conditions  WARNING: these are not general
!
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          if (flag(i,j).eq.1.0) then
            sie(i,j) = 2.*siein - sie(imin,j)
            rho(i,j) = 2.*rhoin - rho(imin,j)
          endif
          if (flag(i,j).eq.2.0) then
            sie(i,j) = sie(imax,j)
            rho(i,j) = rho(imax,j)
          endif
          if (flag(i,j).eq.3.0) then
            sie(i,j) = sie(i,jmin)
          endif
          if (flag(i,j).eq.4.0) then
            sie(i,j) = sie(i,jmax)
          endif
          if (flag(i,j).eq.-1.0) then
            sie(i,j) = sieobs
            rho(i,j) = rho0
          endif
        enddo
      enddo
!
! Update the RHO and SIE arrays with halo.
!
      call ESMF_FieldHalo(field_rho, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRho:  rho halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldHalo(field_sie, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRho:  sie halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowRho

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowVel - Calculates the velocity arrays U and V.

! !INTERFACE:
      subroutine FlowVel(rc)
!
! !ARGUMENTS:
      integer, optional, intent(out) :: rc
!
! !DESCRIPTION:
!     The FlowVel routine calculates the velocity arrays U and V, and
!     modifies the momentum arrays RHOU and RHOV at boundaries.
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: status
      integer :: i, j
      real(kind=ESMF_KIND_R4) :: rhoav
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!
!  update velocities
!
      do j = jmin, jmax
        do i = imin, imax
          rhoav = 0.5*(rho(i,j) + rho(i+1,j))
          if (rhoav.gt.0.0) u(i,j) = rhou(i,j)/rhoav
        enddo
      enddo
      do j = jmin, jmax
        do i = imin, imax
          rhoav = 0.5*(rho(i,j) + rho(i,j+1))
          if (rhoav.gt.0.0) v(i,j) = rhov(i,j)/rhoav
        enddo
      enddo
      do j = jmin, jmax
        do i = imin, imax
          omega(i,j) = (v(i,j+1)-v(i,j))/dx - (u(i+1,j)-u(i,j))/dy
        enddo
      enddo
!
!  add boundary conditions  WARNING: these are not general
!
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          if (flag(i,j).eq.1.0) then
            u(i,j) = uin
            rhou(i,j) = uin*rho(i,j)
            v(i,j) = 0.0
            rhov(i,j) = 0.0
          endif
          if (flag(i,j).eq.2.0) then
            u(i,j) = u(imax,j)
            v(i,j) = v(imax,j)
          endif
          if (flag(i,j).eq.3.0) then
            u(i,j) = u(i,jmin)
            rhou(i,j) = u(i,j)*rho(i,j)
            v(i,j) = 0.0
            rhov(i,j) = 0.0
          endif
          if (flag(i,j).eq.4.0) then
            u(i,j) = u(i,jmax)
            rhou(i,j) = u(i,j)*rho(i,j)
            v(i,j) = 0.0
            rhov(i,j) = 0.0
          endif
          if (flag(i,j).eq.5.0) then
            v(i,j) = 0.0
            rhov(i,j) = 0.0
          endif
          if (flag(i,j).eq.10.0) then
            u(i,j) = 0.0
            rhou(i,j) = 0.0
          endif
        enddo
      enddo
!
! obstacle normal boundary conditions
!
      do j = jmin_t, jmax
        do i = imin_t, imax
          if (flag(i,j) .eq. -1.0) then
            u(i,j) = 0.0
            rhou(i,j) = 0.0
            v(i,j) = 0.0
            rhov(i,j) = 0.0
          endif
          if (flag(i+1,j) .eq. -1.0) then
            u(i,j) = 0.0
            rhou(i,j) = 0.0
          endif
          if (flag(i,j+1) .eq. -1.0) then
            v(i,j) = 0.0
            rhov(i,j) = 0.0
          endif
        enddo
      enddo
!
! obstacle tangential boundary conditions for free-slip
!
      do j = jmin, jmax
        do i = imin, imax
          if (flag(i,j).eq.-1.0 .and. flag(i,j+1).ne.-1.0 .and. flag(i+1,j).eq.-1.0) then
            u(i,j) = u(i,j+1)
            rhou(i,j) = rhou(i,j+1)
          endif
          if (flag(i,j).eq.-1.0 .and. flag(i,j-1).ne.-1.0 .and. flag(i+1,j).eq.-1.0) then
            u(i,j) = u(i,j-1)
            rhou(i,j) = rhou(i,j-1)
          endif
          if (flag(i,j).eq.-1.0 .and. flag(i+1,j).ne.-1.0 .and. flag(i,j+1).eq.-1.0) then
            v(i,j) = v(i+1,j)
            rhov(i,j) = rhov(i+1,j)
          endif
          if (flag(i,j).eq.-1.0 .and. flag(i-1,j).ne.-1.0 .and. flag(i,j+1).eq.-1.0) then
            v(i,j) = v(i-1,j)
            rhov(i,j) = rhov(i-1,j)
          endif
        enddo
      enddo
!
! Halo all the velocity and momentum arrays
!
      call ESMF_FieldHalo(field_u, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  u halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldHalo(field_v, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  v halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldHalo(field_rhou, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  rhou halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldHalo(field_rhov, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  rhov halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowVel

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowState - Calculates the state arrays for pressure and viscosity.

! !INTERFACE:
      subroutine FlowState(rc)
!
! !ARGUMENTS:
      integer, optional, intent(out) :: rc
!
! !DESCRIPTION:
!     The FlowState routine calculates the state arrays P and Q for pressure
!     and viscosity.  Pressure assumes an ideal gas formulation and the 
!     viscosity is a linear artifical form.
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: status
      integer :: i, j
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!
!  new pressures and viscosities
!
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          p(i,j) = (gamma-1.0)*rho(i,j)*sie(i,j)
        enddo
      enddo
      do j = jmin, jmax_t
        do i = imin, imax_t
          q(i,j) = q0*rho(i,j)*uin*sqrt(dx**2+dy**2)*((u(i-1,j)-u(i,j))/dx &
                                                     +(v(i,j-1)-v(i,j))/dy)
          q(i,j) = max(q(i,j), 0.0)
        enddo
      enddo
!
!  add boundary conditions
!
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          if (flag(i,j).eq.2.0) then
            p(i,j) = p(imax,j)
            q(i,j) = q(imax,j)
          endif
          if (flag(i,j).eq.3.0) then
            p(i,j) = p(i,jmin)
            q(i,j) = q(i,jmin)
          endif
          if (flag(i,j).eq.4.0) then
            p(i,j) = p(i,jmax)
            q(i,j) = q(i,jmax)
          endif
        enddo
      enddo
!
! Halo calculated fields to update
!
      call ESMF_FieldHalo(field_p, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowState:  p halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldHalo(field_q, halohandle, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowState:  q halo"
      endif
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowState

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowStability - Checks current data for instabilities

! !INTERFACE:
      subroutine FlowStability(rc)
!
! !ARGUMENTS:
      integer, optional, intent(out) :: rc
!
! !DESCRIPTION:
!     The FlowStability routine compares the current data against stability
!     criteria for the courant number, viscosity, and mach number.
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: status
      real :: scale, c
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!
! Check courant limit with inflow
!
      scale = sqrt(dx**2 + dy**2)
      if(uin*dt/scale.ge.0.20) then
        print *, "Courant limit exceeded by inflow conditions"
        print *, "Please decrease uin, decrease dt, or increase dx and dy ", &
                 "and try again."
        return
      endif
!
! Check viscosity term
!
      if(q0*rhoin*uin/scale.ge.0.20) then
        print *, "Viscosity limit exceeded by inflow conditions"
        print *, "Please decrease q0, decrease rhoin, decrease uin, or ", &
                 "increase dx and dy and try again."
        return
      endif
!
! Check mach number
!
      if(gamma.le.1.0) then
        print *, "Gamma must be greater than 1.0"
        print *, "Please change gamma and try again."
        return
      endif
      c = sqrt(gamma*(gamma-1.0)*siein)
      if(uin/c.ge.1.50) then
        print *, "Mach number limit exceeded by inflow conditions"
        print *, "Please decrease uin, decrease gamma, or increase siein ", &
                 "and try again."
        return
      endif

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowStability

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: FlowPrint - Print out the SIE, U, and V arrays.

! !INTERFACE:
      subroutine FlowPrint(gcomp, clock, file_no, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gcomp
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(in) :: file_no
      integer, optional, intent(out) :: rc
!
! !DESCRIPTION:
!     The FlowPrint routine outputs the SIE, U, and V arrays with the specified
!     file number.  It also prints out the FLAG array at the beginning of the
!     run. \\
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. \\
!
!     \begin{description}
!     \item [gcomp]
!           A Gridded Component.
!     \item [clock]
!           Clock describing the external time.
!     \item [file\_no]
!           File number for output files, 999 max.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI
!
! Local variables
!
      integer :: status
      integer :: i, j, pet_id
      integer(kind=ESMF_KIND_I8) :: frame
      type(ESMF_Array) :: outarray
      type(ESMF_VM) :: vm
      character(len=ESMF_MAXSTR) :: filename
!
! Set initial values
!
      status = ESMF_FAILURE
!
! Initialize return code
!
      if(present(rc)) then
        rc = ESMF_FAILURE
      endif
!
! Collect results on DE 0 and output to a file
!
      call ESMF_GridCompGet(gcomp, vm=vm, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_VMGet(vm, localPet=pet_id, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
!
! Frame number from computation
!
      call ESMF_ClockGet(clock, advanceCount=frame, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
!
! And now test output to a file
!
      call ESMF_FieldWrite(field_u, file="U_velocity.nc", timeslice=file_no, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      call ESMF_FieldWrite(field_v, file="V_velocity.nc", timeslice=file_no, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      call ESMF_FieldWrite(field_omega, file="OMEGA.nc", timeslice=file_no, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      call ESMF_FieldWrite(field_sie, file="SIE.nc", timeslice=file_no, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
!
! First time through output two more files
!
      if(file_no .eq. 1) then
        call ESMF_FieldWrite(field_flag, file="FLAG.nc", rc=status)
        if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

        do j = jmin, jmax
          do i = imin, imax
            de(i,j) = pet_id
          enddo
        enddo
        call ESMF_FieldWrite(field_de, file="DE.nc", rc=status)
        if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      endif

      if(present(rc)) rc = ESMF_SUCCESS

 20   format(a,".",I3.3,".",a)

      end subroutine FlowPrint

!----------------------------------------------------------------------------------
!BOPI
! !IROUTINE: Flow_Final - Deallocates all arrays.

! !INTERFACE:
      subroutine Flow_Final(gcomp, import_state, export_state, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     The Flow\_Final routine is the registered finalize routine for the
!     FlowSolver.  It deallocates the memory allocated during the init
!     process.
!     \begin{description}
!     \item [gcomp]
!           A Gridded Component.
!     \item [import\_state]
!           State containing the import list.
!     \item [export\_state]
!           State containing the export list.
!     \item [clock]
!           Clock describing the external time.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOPI

!
! Set initial values
!
      rc = ESMF_FAILURE
!
! Deallocate arrays
!
      call FlowArraysDealloc(rc)
      if(rc .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flow_Final"
      endif
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)
   
      call ESMF_FieldHaloRelease(halohandle, rc)
      if(rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=rc)

      rc = ESMF_SUCCESS

      end subroutine Flow_Final

!------------------------------------------------------------------------------
    end module FlowSolverMod
    
