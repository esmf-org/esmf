! $Id: FlowMod.F90,v 1.8 2003/04/28 22:50:41 jwolfe Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  Solves semi-compressible flow with energy PDE's.
!
!\begin{verbatim}

      module FlowMod
!
! ESMF modules
!
      use ESMF_Mod
      use ArraysGlobalMod
    
      implicit none
    
      public FlowMod_register

      contains

!-------------------------------------------------------------------------
!   !  The Register routine sets the subroutines to be called
!   !   as the init, run, and finalize routines.  Note that these are
!   !   private to the module.

      subroutine FlowMod_register(comp, rc)

      type(ESMF_GridComp) :: comp
      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
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
! Register the callback routines.
!
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, User1_Init, 0, status)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, FlowSolve, 0, status)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, User1_Final, 0, status)

      print *, "Registered Initialize, Run, and Finalize routines"

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowMod_register

!-------------------------------------------------------------------------
 
      subroutine User1_Init(gcomp, import_state, export_state, clock, rc)

      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
      real :: x_min, x_max, y_min, y_max
      integer :: i_max, j_max
      integer :: horz_gridtype, vert_gridtype
      integer :: horz_stagger, vert_stagger
      integer :: horz_coord_system, vert_coord_system
      integer :: myde, halo_width
      namelist /input/ i_max, j_max, x_min, x_max, y_min, y_max, &
                       uin, rhoin, siein, vin2, rhoin2, siein2, &
                       gamma, akb, q0, u0, v0, sie0, rho0, &
                       printout, sieobs, nobsdesc, iobs_min, iobs_max, &
                       jobs_min, jobs_max, iflo_min, iflo_max
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
! Read in input file
!
      open(10, status="old", file="coupled_wave_input")
      read(10, input, end=20)
   20 continue
!
! Calculate some other quantities
!
      dx = (x_max - x_min)/i_max      ! Should be calls to PhysGrid
      dy = (y_max - y_min)/j_max
!
! Query component for information.
!
      call ESMF_GridCompGet(gcomp, layout=layout, rc=status)
!
! Create the Grid
!
      horz_gridtype = ESMF_GridType_XY
      vert_gridtype = ESMF_GridType_Unknown
      horz_stagger = ESMF_GridStagger_A
      vert_stagger = ESMF_GridStagger_Unknown
      horz_coord_system = ESMF_CoordSystem_Cartesian
      vert_coord_system = ESMF_CoordSystem_Unknown
      halo_width = 1

      grid = ESMF_GridCreate(i_max=i_max, j_max=j_max, &
                             x_min=x_min, x_max=x_max, &
                             y_min=y_min, y_max=y_max, &
                             layout=layout, &
                             horz_gridtype=horz_gridtype, &
                             vert_gridtype=vert_gridtype, &
                             horz_stagger=horz_stagger, &
                             vert_stagger=vert_stagger, &
                             horz_coord_system=horz_coord_system, &
                             vert_coord_system=vert_coord_system, &
                             halo_width=halo_width, &
                             name="source grid", rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in User1_init:  grid create"
        return
      endif

      call  ESMF_GridCompSet(gcomp, grid=grid, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in User1_init:  grid comp set"
        return
      endif

      call FlowInit(gcomp, clock, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in User1_init:  grid create"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine User1_Init

!-------------------------------------------------------------------------
 
      subroutine FlowInit(gcomp, clock, rc)

! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: gcomp
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j, n, x, y, nx, ny
      integer, dimension(1,2) :: local, global
      double precision :: s_
      type(ESMF_Grid) :: grid
      type(ESMF_DElayout) :: layout
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
! get Grid from Component
!
      call ESMF_GridCompGet(gcomp, grid=grid, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  grid comp get"
        return
      endif
!
! create space for global arrays
!
      call ArraysGlobalAlloc(grid, status)
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
      call ESMF_GridCompGet(gcomp, layout=layout, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  grid comp get"
        return
      endif
      call ESMF_DELayoutGetSize(layout, nx, ny, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  layout get size"
        return
      endif
      call ESMF_DELayoutGetDEPosition(layout, x, y, status)
      x = x + 1
      y = y + 1
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in Flowinit:  layout get position"
        return
      endif
      do j = jmin_t, jmax_t
        do i = imin_t, imax_t
          flag(i,j) = 0.0
        enddo
      enddo
      if (x.eq.1) then  ! left boundary
        do j = jmin_t, jmax_t
          do i = imin_t, imin-1
            flag(i,j) = 1.0
          enddo
        enddo
      endif
      if (x.eq.nx) then  ! right boundary
        do j = jmin_t, jmax_t
          do i = imax+1, imax_t
            flag(i,j) = 2.0
          enddo
        enddo
      endif
      if (y.eq.1) then  ! bottom boundary
        do j = jmin_t, jmin-1
          do i = imin_t, imax_t
            flag(i,j) = 3.0
          enddo
        enddo
      endif
      if (y.eq.ny) then  ! top boundary
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
! add an obstacle
!
      do n = 1, nobsdesc
        do j = jobs_min(n),jobs_max(n)
          global(1,2) = j
          do i = iobs_min(n),iobs_max(n)
            global(1,1) = i
            call ESMF_GridGlobalToLocalIndex(grid, global2d=global, &
                                             local2d=local, rc=status)
            if(local(1,1).ne.-1 .and. local(1,2).ne.-1) then
              flag(local(1,1),local(1,2)) = -1
            endif
          enddo
        enddo
      enddo
!
! add inflow section
!
      do i = iflo_min, iflo_max
        global(1,1) = i
        global(1,2) = 1
        call ESMF_GridGlobalToLocalIndex(grid, global2d=global, &
                                         local2d=local, rc=status)
        if(local(1,1).ne.-1 .and. local(1,2).ne.-1) then
          flag(local(1,1),local(1,2)) = 10
        endif
      enddo
!
! obstacle normal boundary conditions here
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
! obstacle tangential boundary conditions here
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
! initialize timestep
!
      call ESMF_ClockGetTimeStep(clock, time_step, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: clock get timestep"
        return
      endif
      call ESMF_TimeIntervalGet(time_step, s_=s_, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: time interval get"
        return
      endif
      dt = s_
      write(*,*) 'dt = ', dt

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowInit

!------------------------------------------------------------------------------

      subroutine FlowSolve(gcomp, import_state, export_state, clock, rc)

      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: counter = 0
      integer :: print_count = 0
      double precision :: s_
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
! Increment counter
!
      counter = counter + 1
! 
! Get timestep from clock
!
      call ESMF_ClockGetTimeStep(clock, time_step, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: clock get timestep"
        return
      endif
      call ESMF_TimeIntervalGet(time_step, s_=s_, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: time interval get"
        return
      endif
      dt = s_
!
! calculate RHOU's and RHOV's
!
      call FlowRhoVel(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve: flowrhovel"
        return
      endif
!    
! calculate RHOI's
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
!  new p's and q's
!
      call FlowState(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowSolve"
        return
      endif
!
! Print graphics every printout steps
!
      if(mod(counter, printout) .eq. 0) then
        print_count = print_count + 1
        call FlowPrint(gcomp, clock, print_count, status)
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowSolve

!------------------------------------------------------------------------------

      subroutine FlowRhoVel(rc)

      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      real :: u_ij, u_ipj, rhouu_m, rhouu_p, v_ipjm, v_ipjp, rhouv_p, rhouv_m
      real :: v_ij, v_ijp, rhovv_m, rhovv_p, u_imjp, u_ipjp, rhovu_p, rhovu_m
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
      call ESMF_FieldHalo(field_rhou, status)
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
      call ESMF_FieldHalo(field_rhov, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoVel:  rhov halo"
        return
      endif
  
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowRhoVel

!------------------------------------------------------------------------------
 
      subroutine FlowRhoI(rc)

      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      real :: rhoiu_m, rhoiu_p, rhoiv_m, rhoiv_p, dsiedx2, dsiedy2
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
          if (flag(i+1,j).eq.-1.0) dsiedx2 = (2.*sieobs+sie(i-1,j)-3.*sie(i,j))/dx**2
          if (flag(i-1,j).eq.-1.0) dsiedx2 = (sie(i+1,j)+2.*sieobs-3.*sie(i,j))/dx**2
          if (flag(i,j+1).eq.-1.0) dsiedy2 = (2.*sieobs+sie(i,j-1)-3.*sie(i,j))/dy**2
          if (flag(i,j-1).eq.-1.0) dsiedy2 = (sie(i,j+1)+2.*sieobs-3.*sie(i,j))/dy**2
!         if (flag(i+1,j).eq.10.0) dsiedx2 = (2.*siein2+sie(i-1,j)-3.*sie(i,j))/dx**2
!         if (flag(i-1,j).eq.10.0) dsiedx2 = (sie(i+1,j)+2.*siein2-3.*sie(i,j))/dx**2
!         if (flag(i,j-1).eq.10.0) dsiedy2 = (sie(i,j+1)+2.*siein2-3.*sie(i,j))/dy**2
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
!  add boundary conditions  WARNING: these are not general
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
      call ESMF_FieldHalo(field_rhoi, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoI:  rhoi halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowRhoI

!------------------------------------------------------------------------------

      subroutine FlowRho(rc)

      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      real, dimension(imax,jmax) :: rho_new  ! sloppy, but OK for now
      real :: rhou_m, rhou_p, rhov_m, rhov_p, dsiedx2, dsiedy2
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
!         if (flag(i,j).eq.1.0) then
!           sie(i,j) = 2.*siein - sie(imin,j)
!           rho(i,j) = 2.*rhoin - rho(imin,j)
!         endif
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
!         if (flag(i,j).eq.10.0) then
!           sie(i,j) = siein2
!           rho(i,j) = rhoin2
!         endif
        enddo
      enddo
      call ESMF_FieldHalo(field_rho, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRho:  rho halo"
        return
      endif
      call ESMF_FieldHalo(field_sie, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRho:  sie halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowRho

!------------------------------------------------------------------------------

      subroutine FlowVel(rc)

      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: i, j
      real :: rhoav
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
! obstacle normal boundary conditions here
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
! obstacle tangential boundary conditions here
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
      call ESMF_FieldHalo(field_u, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  u halo"
        return
      endif
      call ESMF_FieldHalo(field_v, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  v halo"
        return
      endif
      call ESMF_FieldHalo(field_rhou, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  rhou halo"
        return
      endif
      call ESMF_FieldHalo(field_rhov, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowVel:  rhov halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowVel

!------------------------------------------------------------------------------

      subroutine FlowState(rc)

      integer, optional, intent(out) :: rc
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
!  new p's and q's
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
!  add boundary conditions  WARNING: these are not general
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

      call ESMF_FieldHalo(field_p, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowState:  p halo"
        return
      endif
      call ESMF_FieldHalo(field_q, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowState:  q halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowState

!------------------------------------------------------------------------------

      subroutine FlowPrint(gcomp, clock, file_no, rc)

      type(ESMF_GridComp) :: gcomp
      type(ESMF_Clock) :: clock
      integer, intent(in) :: file_no
      integer, optional, intent(out) :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: ni, nj, i, j, de_id
      integer(kind=ESMF_IKIND_I8) :: frame
      type(ESMF_Array) :: array2
      type(ESMF_Grid) :: grid
      type(ESMF_DELayout) :: layout
      type(ESMF_AxisIndex), dimension(2) :: indext, indexe
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
      call ESMF_GridCompGet(gcomp, layout=layout, rc=status)
      call ESMF_DELayoutGetDEID(layout, de_id, status)
!
! Frame number from computation
!
      call ESMF_ClockGetAdvanceCount(clock, frame, status)
!
! And now test output to a file
!
!     call ESMF_StateGetData(import_state, "U", field_u, status)
      call ESMF_FieldAllGather(field_u, array2, status)
      if (de_id .eq. 0) then
        write(filename, 20)  "U_velocity", file_no
        call ESMF_ArrayWrite(array2, filename=filename, rc=status)
      endif
!     call ESMF_ArrayDestroy(array2, status)

!     call ESMF_StateGetData(import_state, "V", field_v, status)
      call ESMF_FieldAllGather(field_v, array2, status)
      if (de_id .eq. 0) then
        write(filename, 20)  "V_velocity", file_no
        call ESMF_ArrayWrite(array2, filename=filename, rc=status)
      endif
!     call ESMF_ArrayDestroy(array2, status)

!     call ESMF_StateGetData(import_state, "SIE", field_sie, status)
      call ESMF_FieldAllGather(field_sie, array2, status)
      if (de_id .eq. 0) then
        write(filename, 20)  "SIE", file_no
        call ESMF_ArrayWrite(array2, filename=filename, rc=status)
      endif
!     call ESMF_ArrayDestroy(array2, status)

      if(file_no .eq. 1) then
!       call ESMF_StateGetData(import_state, "FLAG", field_flag, status)
        call ESMF_FieldAllGather(field_flag, array2, status)
        if (de_id .eq. 0) then
          write(filename, 20)  "FLAG", file_no
          call ESMF_ArrayWrite(array2, filename=filename, rc=status)
        endif
!       call ESMF_ArrayDestroy(array2, status)
      endif

      if(rcpresent) rc = ESMF_SUCCESS

 20   format(a,".",I3.3)

      end subroutine FlowPrint

!----------------------------------------------------------------------------------

      subroutine User1_Final(gcomp, import_state, export_state, clock, rc)

      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock) :: clock
      integer, intent(out), optional :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
!
! Set initial values
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
!
! Initialize return code
!
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      call ArraysGlobalDealloc(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in User1_Final"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine User1_Final

!------------------------------------------------------------------------------
    end module FlowMod
!\end{verbatim}
    
