! $Id: FlowSolverMod.F90,v 1.34 2009/01/16 05:28:25 theurich Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  Solves semi-compressible flow with energy PDE's.  Uses an explicit
!  solution method on a staggered mesh with velocities and momentum 
!  located at cell faces and other physical quantities at cell centers.
!  Employs a donor-cell advection scheme.
!
!\begin{verbatim}

      module FlowSolverMod
!
! ESMF modules
!
      use ESMF_Mod
      use FlowArraysMod
    
      implicit none

      private

      ! Compute the halo pattern once and reuse it for each execution.
      type(ESMF_RouteHandle), save :: precomputed_halo
    
      public FlowSolver_register

      contains

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: FlowSolver_register - Registers the init, run, and finalize routines.

! !INTERFACE:
      subroutine FlowSolver_register(comp, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: comp
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     The Register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that these are
!     private to the module.
!     \begin{description}
!     \item [comp]
!           Pointer to a {\tt Gridded Component} object.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
!
! Local variables
!
! Initialize return code
!
      rc = ESMF_FAILURE
!
! Register the callback routines.
!
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, Flow_Init, 0, rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, FlowSolve, 0, rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, Flow_Final, 0, rc)

      print *, "Registered Initialize, Run, and Finalize routines"

      rc = ESMF_SUCCESS

      end subroutine FlowSolver_register

!-------------------------------------------------------------------------
!BOP
! !IROUTINE: Flow_Init - Initialization routine

! !INTERFACE:
      subroutine Flow_Init(gcomp, import_state, export_state, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     This subroutine is the registered init routine.  It reads input,
!     creates the {\tt IGrid}, attaches it to the {\tt Gridded Component},
!     initializes data, and sets the import and export {\tt States}.
!     \begin{description}
!     \item [gcomp]
!           Pointer to a {\tt Gridded Component} object.
!     \item [import_state]
!           Pointer to a {\tt State} object containing the import list.
!     \item [export_state]
!           Pointer to a {\tt State} object containing the export list.
!     \item [clock]
!           Pointer to a {\tt Clock} object.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
!
! Local variables
!
      integer :: status
      type(ESMF_DELayout) :: layout
      type(ESMF_VM) :: vm
      type(ESMF_IGrid) :: igrid
      real(kind=ESMF_KIND_R8) :: g_min(2), g_max(2)
      real(kind=ESMF_KIND_R8) :: x_min, x_max, y_min, y_max
      integer :: counts(2)
      type(ESMF_IGridHorzStagger) :: horz_stagger
      integer :: npets
      namelist /input/ counts, x_min, x_max, y_min, y_max, &
                       uin, rhoin, siein, &
                       gamma, akb, q0, u0, v0, sie0, rho0, &
                       printout, sieobs, nobsdesc, iobs_min, iobs_max, &
                       jobs_min, jobs_max, iflo_min, iflo_max
!
! Set initial values
!
      status = ESMF_FAILURE
      rc = ESMF_FAILURE
!
! Read in input file
!
      open(10, status="old", file="coupled_flow_input", action="read", &
           iostat=status)
      if (status .ne. 0) then
         print *, "ERROR: unable to open file coupled_flow_input for reading" 
      endif
      read(10, input, end=20)
   20 continue
!
! Calculate some other quantities
!
      ! TODO: reorder the namelist input so we can read the values directly
      ! into g_min and g_max instead of having to go through intermediates.
      g_min(1) = x_min
      g_max(1) = x_max
      g_min(2) = y_min
      g_max(2) = y_max
      ! Should be calls to PhysGrid eventually
      dx = (g_max(1) - g_min(1))/counts(1)   
      dy = (g_max(1) - g_min(1))/counts(2)
!
! Query component for information.
!

      ! Find out how many PETs we were given to run on
      call ESMF_GridCompGet(gcomp, vm=vm, rc=status)
      call ESMF_VMGet(vm, petCount=npets, rc=status)

      ! Set up the component layouts so they are different, so we can show
      !  we really are routing data between processors.
      layout = ESMF_DELayoutCreate(vm, (/ npets/2, 2 /), rc=rc)

!
! Create the IGrid
!
      horz_stagger = ESMF_IGRID_HORZ_STAGGER_C_NE

      igrid = ESMF_IGridCreateHorzXYUni(counts=counts, &
                             minGlobalCoordPerDim=g_min, &
                             maxGlobalCoordPerDim=g_max, &
                             horzStagger=horz_stagger, &
                             name="source igrid", rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flow_init:  igrid create"
        return
      endif
      call ESMF_IGridDistribute(igrid, delayout=layout, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flow_init:  igrid distribute"
        return
      endif
!
! Set the IGrid in the igridded Component
!
      call  ESMF_GridCompSet(gcomp, igrid=igrid, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flow_init:  igrid comp set"
        return
      endif
!
! Initialize the data
!
      call FlowInit(gcomp, clock, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flow_init:  flowinit"
        return
      endif
!
! For initialization, add all fields to the import state.  Only the ones
! needed will be copied over to the export state for coupling.
!
      call ESMF_StateAddField(import_state, field_sie, rc)
      call ESMF_StateAddField(import_state, field_u, rc)
      call ESMF_StateAddField(import_state, field_v, rc)
      call ESMF_StateAddField(import_state, field_rho, rc)
      call ESMF_StateAddField(import_state, field_p, rc)
      call ESMF_StateAddField(import_state, field_q, rc)
      call ESMF_StateAddField(import_state, field_flag, rc)
!
! This is adding names only to the export list, marked by default
! as "not needed".  The coupler will mark the ones needed based
! on the requirements of the component(s) this is coupled to.
!
      call ESMF_StateAddNameOnly(export_state, "SIE", rc)
      call ESMF_StateAddNameOnly(export_state, "U", rc)
      call ESMF_StateAddNameOnly(export_state, "V", rc)
      call ESMF_StateAddNameOnly(export_state, "RHO", rc)
      call ESMF_StateAddNameOnly(export_state, "P", rc)
      call ESMF_StateAddNameOnly(export_state, "Q", rc)
      call ESMF_StateAddNameOnly(export_state, "FLAG", rc)

! temporary fix
      call ESMF_StateAddField(export_state, field_sie, rc)
      rc = ESMF_SUCCESS

! and precompute the halo for one variable - the others can reuse the same
! route handle because they have identical distributions.
      call ESMF_FieldHaloStore(field_sie, precomputed_halo, rc=rc)

      end subroutine Flow_Init

!-------------------------------------------------------------------------
!BOP
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
!           Pointer to a {\tt Gridded Component} object.
!     \item [clock]
!           Pointer to a {\tt Clock} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j, n, x, y, nx, ny, de_id, ncounts(2), pos(2)
      integer, dimension(1,2) :: local, global
      real(ESMF_KIND_R8) :: s_
      type(ESMF_IGrid) :: igrid
      type(ESMF_DELayout) :: layout
!
! Set initial values
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
!
! Initialize return code
!
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
!
! get IGrid from Component
!
      call ESMF_GridCompGet(gcomp, igrid=igrid, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  igrid comp get"
        return
      endif
!
! create space for global arrays
!
      call FlowArraysAlloc(igrid, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  arraysglobalalloc"
        return
      endif
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
! set flags, based on NBC's and position in layout
! note: since we're operating on a five-point stencil, we don't
!       really care out how the corners get set
!
! First, get size of layout and position of my DE to determine if
! this DE is on the domain boundary
!
      call ESMF_IGridGet(igrid, delayout=layout, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  igrid comp get"
        return
      endif
      call ESMF_DELayoutGetDeprecated(layout, localDE=de_id, deCountPerDim=ncounts, &
                               rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  layout get size"
        return
      endif
      nx = ncounts(1)
      ny = ncounts(2)
      call ESMF_DELayoutGetDELocalInfo(layout, de_id, coord=pos, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  layout get position"
        return
      endif
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
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          u(i,j) = u0
          v(i,j) = v0
          rhou(i,j) = rho0*u0
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
            u(i,j) = uin
            rhoi(i,j) = rhoin*siein
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
            call ESMF_IGridGlobalToDELocalIndex(igrid, global2d=global, &
                                               local2d=local, &
                                               horzRelloc=ESMF_CELL_CENTER, &
                                               rc=status)
            if (local(1,1).gt.-1) local(1,1) = local(1,1) + 1
            if (local(1,2).gt.-1) local(1,2) = local(1,2) + 1
  ! TODO:  The above two lines are junk, making up for the halo width which is
  !        no longer in IGrid. GlobalToLocal should be an Array method
            local(1,2) = local(1,2) + 1
            if(local(1,1).ne.-1 .and. local(1,2).ne.-1) then
              flag(local(1,1),local(1,2)) = -1
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
          call ESMF_IGridGlobalToDELocalIndex(igrid, global2d=global, &
                                             local2d=local, &
                                             horzRelloc=ESMF_CELL_CENTER, &
                                             rc=status)
            if (local(1,1).gt.-1) local(1,1) = local(1,1) + 1
            if (local(1,2).gt.-1) local(1,2) = local(1,2) + 1
  ! TODO:  The above two lines are junk, making up for the halo width which is
  !        no longer in IGrid. GlobalToLocal should be an Array method
          if(local(1,1).ne.-1 .and. local(1,2).ne.-1) then
            flag(local(1,1),local(1,2)) = 10
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
        return
      endif
      call ESMF_TimeIntervalGet(time_step, s_r8=s_, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: time interval get"
        return
      endif
      dt = s_
      write(*,*) 'dt = ', dt

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowInit

!------------------------------------------------------------------------------
!BOP
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
!     \item [ccomp]
!           Pointer to a {\tt Gridded Component} object.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
!
! Local variables
!
      integer :: status
      integer :: i, j
      integer :: counter = 0
      integer :: print_count = 0
      real(ESMF_KIND_R8) :: s_
      integer :: datacount
      character(len=ESMF_MAXSTR), dimension(7) :: datanames
      type(ESMF_Field) :: thisfield

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
      status = ESMF_FAILURE
      rc = ESMF_FAILURE
!
! Increment counter
!
      counter = counter + 1
! 
! Get timestep from clock
!
      call ESMF_ClockGet(clock, timeStep=time_step, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: clock get timestep"
        return
      endif
      call ESMF_TimeIntervalGet(time_step, s_r8=s_, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: time interval get"
        return
      endif
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
      call FlowRhoVel(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowrhovel"
        return
      endif
!    
! calculate RHOI's (energy)
!
      call FlowRhoI(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowrhoi"
        return
      endif
!    
! determine new densities and internal energies
!
      call FlowRho(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowrho"
        return
      endif
!
!  update velocities
!
      call FlowVel(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowvel"
        return
      endif
!
!  new pressures and viscosities
!
      call FlowState(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve"
        return
      endif
      ! Debug
      call ESMF_StateAddField(export_state, field_sie, rc=status)
      call ESMF_StateAddField(export_state, field_u, rc=status)
      call ESMF_StateAddField(export_state, field_v, rc=status)
      call ESMF_StateAddField(export_state, field_rho, rc=status)
      call ESMF_StateAddField(export_state, field_flag, rc=status)
      !
      ! Update export state with needed fields
      !
      do i=1, datacount

          ! check isneeded flag here
          if (.not. ESMF_StateIsNeeded(export_state, datanames(i), rc)) then 
              cycle
          endif

          ! Set export data in export state
          call ESMF_StateGetField(import_state, datanames(i), thisfield, rc=status)
          call ESMF_StateAddField(export_state, thisfield, rc=status)

        enddo

!
! Print graphics every printout steps
!
      if(mod(counter, printout) .eq. 0) then
        print_count = print_count + 1
        call FlowPrint(gcomp, clock, print_count, status)
      endif

      rc = ESMF_SUCCESS

      end subroutine FlowSolve

!------------------------------------------------------------------------------
!BOP
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
!EOP
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      real(kind=ESMF_KIND_R4) :: u_ij, u_ipj, rhouu_m, rhouu_p
      real(kind=ESMF_KIND_R4) :: v_ipjm, v_ipjp, rhouv_p, rhouv_m
      real(kind=ESMF_KIND_R4) :: v_ij, v_ijp, rhovv_m, rhovv_p
      real(kind=ESMF_KIND_R4) :: u_imjp, u_ipjp, rhovu_p, rhovu_m
!
! Set initial values
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
!
! Initialize return code
!
      if(present(rc)) then
        rcpresent=.TRUE.
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
      call ESMF_FieldHalo(field_rhou, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoVel:  rhou halo"
        return
      endif

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
! Update RHOV with Halo
!
      call ESMF_FieldHalo(field_rhov, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoVel:  rhov halo"
        return
      endif
  
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowRhoVel

!------------------------------------------------------------------------------
!BOP
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
!EOP
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      real(kind=ESMF_KIND_R4) :: rhoiu_m, rhoiu_p, rhoiv_m, rhoiv_p
      real(kind=ESMF_KIND_R4) :: dsiedx2, dsiedy2
!
! Set initial values
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
!
! Initialize return code
!
      if(present(rc)) then
        rcpresent=.TRUE.
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
! Update RHOI with Halo
!
      call ESMF_FieldHalo(field_rhoi, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoI:  rhoi halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowRhoI

!------------------------------------------------------------------------------
!BOP
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
!EOP
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      real(kind=ESMF_KIND_R4), dimension(imax,jmax) :: rho_new
      real(kind=ESMF_KIND_R4) :: rhou_m, rhou_p, rhov_m, rhov_p
!
! Set initial values
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
!
! Initialize return code
!
      if(present(rc)) then
        rcpresent=.TRUE.
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
! Update the RHO and SIE arrays with Halo.
!
      call ESMF_FieldHalo(field_rho, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRho:  rho halo"
        return
      endif
      call ESMF_FieldHalo(field_sie, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRho:  sie halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowRho

!------------------------------------------------------------------------------
!BOP
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
!EOP
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      real(kind=ESMF_KIND_R4) :: rhoav
!
! Set initial values
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
!
! Initialize return code
!
      if(present(rc)) then
        rcpresent=.TRUE.
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
      call ESMF_FieldHalo(field_u, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  u halo"
        return
      endif
      call ESMF_FieldHalo(field_v, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  v halo"
        return
      endif
      call ESMF_FieldHalo(field_rhou, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  rhou halo"
        return
      endif
      call ESMF_FieldHalo(field_rhov, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  rhov halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowVel

!------------------------------------------------------------------------------
!BOP
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
!EOP
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
!
! Set initial values
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
!
! Initialize return code
!
      if(present(rc)) then
        rcpresent=.TRUE.
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
      call ESMF_FieldHalo(field_p, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowState:  p halo"
        return
      endif
      call ESMF_FieldHalo(field_q, precomputed_halo, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowState:  q halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowState

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: FlowPrint - Print out the SIE, U, and V arrays.

! !INTERFACE:
      subroutine FlowPrint(gcomp, clock, file_no, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gcomp
      type(ESMF_Clock) :: clock
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
!           Pointer to a {\tt Gridded Component} object.
!     \item [clock]
!           Pointer to a {\tt Clock} object.
!     \item [file_no]
!           File number for output files, 999 max.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: de_id
      integer(kind=ESMF_KIND_I8) :: frame
      type(ESMF_InternArray) :: outarray
      type(ESMF_IGrid) :: igrid
      type(ESMF_DELayout) :: layout
      character(len=ESMF_MAXSTR) :: filename
!
! Set initial values
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
!
! Initialize return code
!
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
!
! Collect results on DE 0 and output to a file
!
      call ESMF_GridCompGet(gcomp, igrid=igrid, rc=status)
      call ESMF_IGridGet(igrid, delayout=layout, rc=status)
      call ESMF_DELayoutGetDeprecated(layout, localDe=de_id, rc=status)
!
! Frame number from computation
!
      call ESMF_ClockGet(clock, advanceCount=frame, rc=status)
!
! And now test output to a file
!
      call ESMF_FieldGather(field_u, 0, outarray, rc=status)
      if (de_id .eq. 0) then
        write(filename, 20)  "U_velocity", file_no
        call ESMF_InternArrayWrite(outarray, filename=filename, rc=status)
        call ESMF_InternArrayDestroy(outarray, rc=status)
      endif

      call ESMF_FieldGather(field_v, 0, outarray, rc=status)
      if (de_id .eq. 0) then
        write(filename, 20)  "V_velocity", file_no
        call ESMF_InternArrayWrite(outarray, filename=filename, rc=status)
        call ESMF_InternArrayDestroy(outarray, rc=status)
      endif

      call ESMF_FieldGather(field_sie, 0, outarray, rc=status)
      if (de_id .eq. 0) then
        write(filename, 20)  "SIE", file_no
        call ESMF_InternArrayWrite(outarray, filename=filename, rc=status)
        call ESMF_InternArrayDestroy(outarray, rc=status)
      endif
!
! First time through output two more files
!
      if(file_no .eq. 1) then
        call ESMF_FieldGather(field_flag, 0, outarray, rc=status)
        if (de_id .eq. 0) then
          write(filename, 20)  "FLAG", file_no
          call ESMF_InternArrayWrite(outarray, filename=filename, rc=status)
          call ESMF_InternArrayDestroy(outarray, rc=status)
        endif
      endif

      if(rcpresent) rc = ESMF_SUCCESS

 20   format(a,".",I3.3)

      end subroutine FlowPrint

!----------------------------------------------------------------------------------
!BOP
! !IROUTINE: Flow_Final - Deallocates all arrays.

! !INTERFACE:
      subroutine Flow_Final(gcomp, import_state, export_state, clock, rc)
!
! !ARGUMENTS:
      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     The Flow_Final routine is the registered finalize routine for the
!     FlowSolver.  It deallocates the memory allocated during the init
!     process.
!     \begin{description}
!     \item [gcomp]
!           Pointer to a {\tt Gridded Component} object.
!     \item [import_state]
!           Pointer to a {\tt State} object containing the import list.
!     \item [export_state]
!           Pointer to a {\tt State} object containing the export list.
!     \item [clock]
!           Pointer to a {\tt Clock} object.
!     \item [rc]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
!
! Local variables
!
      integer :: status
!
! Set initial values
!
      status = ESMF_FAILURE
      rc = ESMF_FAILURE
!
! Deallocate arrays
!
      call FlowArraysDealloc(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flow_Final"
        return
      endif

!
! Release the route information stored for halos
!
      call ESMF_FieldHaloRelease(precomputed_halo, rc)
      rc = ESMF_SUCCESS

      end subroutine Flow_Final

!------------------------------------------------------------------------------
    end module FlowSolverMod
!\end{verbatim}
    
