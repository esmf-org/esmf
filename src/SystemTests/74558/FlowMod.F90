! $Id: FlowMod.F90,v 1.4 2003/04/05 00:05:21 jwolfe Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  Solves semi-compressible flow with energy PDE's.
!
!
!\begin{verbatim}

    module FlowMod
    
!   Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

!   ESMF modules
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_DELayoutMod
    use ESMF_ClockMod
    use ESMF_ArrayMod
    use ESMF_GridMod
    use ESMF_DataMapMod
    use ESMF_FieldMod
    use ESMF_StateMod
    use ESMF_CompMod

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
        integer :: rc

        print *, "in user register routine"

        ! Register the callback routines.

        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETINIT, User1_Init, 0, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETRUN, FlowSolve, 0, rc)
        call ESMF_GridCompSetEntryPoint(comp, ESMF_SETFINAL, User1_Final, 0, rc)

        print *, "Registered Initialize, Run, and Finalize routines"

        ! If desired, this routine can register a private data block
        ! to be passed in to the routines above:
        ! call ESMF_CompSetData(comp, mydatablock, rc)

    end subroutine FlowMod_register

!-------------------------------------------------------------------------
 
    subroutine User1_Init(gcomp, import_state, export_state, clock, rc)

      type(ESMF_GridComp) :: gcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_Clock) :: clock
      integer, intent(out), optional :: rc
!
! Local variables
!
      integer :: status
      integer :: i, j
      logical :: rcpresent
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: index
      real :: x_min, x_max, y_min, y_max
      integer :: i_max, j_max
      integer :: horz_gridtype, vert_gridtype
      integer :: horz_stagger, vert_stagger
      integer :: horz_coord_system, vert_coord_system
      integer :: myde, halo_width
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
!
! Query component for information.
!
        call ESMF_GridCompGet(gcomp, layout=layout, rc=rc)
!
! Create the Grid
!
        i_max = 40
        j_max = 20
        x_min = 0.0
        x_max = 20.0
        y_min = 0.0
        y_max = 5.0
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

      call FlowInit(gcomp, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in User1_init:  grid create"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine User1_Init

!-------------------------------------------------------------------------
 
    subroutine FlowInit(gcomp, rc)

! !ARGUMENTS:
      type(ESMF_GridComp), intent(inout) :: gcomp
      integer, intent(out), optional :: rc
!
! Local variables
!
      integer :: status
      integer :: i, j
      logical :: rcpresent
      type(ESMF_Grid) :: grid
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
! read in parameters   ! TODO: just set here for now
!
      uin = 1.4
      rhoin = 6.0
      siein= 0.50
      gamma = 1.40
      akb = 1.0
      q0 = 0.05
      u0 = 1.0
      v0 = 0.0
      sie0 = 0.50
      rho0 = 6.0
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
! initialize inlet parameters  TODO: identify de's with inflow regions
!
      do j = jmin_t, jmax_t
        rho(1,j) = rhoin
        sie(1,j) = siein
        u(1,j) = uin
        rhoi(1,j) = rhoin*siein
        rhou(1,j) = rhoin*uin
      enddo
      call ESMF_FieldHalo(field_rho, status)   ! TODO:  shouldn't have to halo
                                               ! here, but every left column on
                                               ! each DE is getting inlet values
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowInit:  rho halo"
        return
      endif
      call ESMF_FieldHalo(field_sie, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowInit:  sie halo"
        return
      endif
      call ESMF_FieldHalo(field_u, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowInit:  u halo"
        return
      endif
      call ESMF_FieldHalo(field_rhoi, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowInit:  rhoi halo"
        return
      endif
      call ESMF_FieldHalo(field_rhou, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowInit:  rhou halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowInit

!----------------------------------------------------------------------------------

    subroutine FlowSolve(gcomp, import_state, export_state, clock, rc)

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
! 
! Get timestep from clock
!
!     call ESMF_ClockGetTimeStep(clock, dt, status)
!     if(status .NE. ESMF_SUCCESS) then
!       print *, "ERROR in FlowSolve: clock get timestep"
!       return
!     endif
      dt = 0.02
!
! calculate RHOU's and RHOV's
!
      call FlowRhoVel
!    
! calculate RHOI's
!
      call FlowRhoI
!    
! determine new densities and internal energies
!
      call FlowRho
!
!  update velocities
!
      call FlowVel
!
!  new p's and q's
!
      call FlowState

!     if(status .NE. ESMF_SUCCESS) then
!       print *, "ERROR in FlowSolve"
!       return
!     endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowSolve

!----------------------------------------------------------------------------------

      subroutine FlowRhoVel(rc)

      integer, intent(out), optional :: rc
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
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
! calculate RHOU's and RHOV's
!
      do j = jmin, jmax
        do i = imin, imax-1
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

      do j = jmin, jmax-1
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

!----------------------------------------------------------------------------------
 
      subroutine FlowRhoI(rc)

      integer, intent(out), optional :: rc
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
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!    
! calculate RHOI's
!
      do j = jmin, jmax
        do i = imin, imax
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
!  TODO  add BC's to modify dsiedx2 and dsiedy2
          rhoi(i,j) = rhoi(i,j) + (dt/dx)*(rhoiu_m-rhoiu_p) &
                    + (dt/dy)*(rhoiv_m-rhoiv_p) &
                    - dt*(p(i,j)+q(i,j))*((u(i,j)-u(i-1,j))/dx + (v(i,j)-v(i,j-1))/dy) &
                    + dt*akb*(dsiedx2+dsiedy2)
        enddo
      enddo
      call ESMF_FieldHalo(field_rhoi, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowRhoI:  rhoi halo"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowRhoI

!----------------------------------------------------------------------------------

      subroutine FlowRho(rc)

      integer, intent(out), optional :: rc
!
! Local variables
!
      integer :: status
      logical :: rcpresent
      real, dimension(imax,jmax) :: rho_new  ! sloppy, but OK for now
      integer :: i, j
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
        rcpresent = .TRUE.
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
!  TODO:  add BC's to sie array
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

!----------------------------------------------------------------------------------

      subroutine FlowVel(rc)

      integer, intent(out), optional :: rc
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
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!  update velocities
!
      do j = jmin, jmax
        do i = imin, imax-1
          rhoav = 0.5*(rho(i,j) + rho(i+1,j))
          if (rhoav.gt.0.0) u(i,j) = rhou(i,j)/rhoav
        enddo
      enddo
      do j = jmin, jmax
        u(imax,j) = u(imax-1,j)
      enddo
      do j = jmin, jmax-1
        do i = imin, imax
          rhoav = 0.5*(rho(i,j) + rho(i,j+1))
          if (rhoav.gt.0.0) v(i,j) = rhov(i,j)/rhoav
        enddo
      enddo
!  TODO:  if there are obstacles, set their normal velocities to zero
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

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowVel

!----------------------------------------------------------------------------------

      subroutine FlowState(rc)

      integer, intent(out), optional :: rc
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
        rcpresent = .TRUE.
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
      do j = jmin, jmax
        do i = imin, imax
          q(i,j) = q0*rho(i,j)*uin*sqrt(dx**2+dy**2)*((u(i-1,j)-u(i,j))/dx &
                                                     +(v(i,j-1)-v(i,j))/dy)
          q(i,j) = max(q(i,j), 0.0)
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
      integer :: ni, nj, i, j, de_id
      type(ESMF_Array) :: array1
      type(ESMF_Grid) :: grid
      type(ESMF_DELayout) :: layout
      real, dimension(:,:), pointer :: ldata
      type(ESMF_AxisIndex), dimension(2) :: indext, indexe
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
!
! Print out some results before finalizing
!
      ! Get a pointer to the data Array in the Field
      call ESMF_FieldGetData(field_u, array1, rc=rc)
      print *, "data back from field"

      ! Get size of local array
      call ESMF_FieldGetGrid(field_u, grid, status)
      call ESMF_GridGetDELayout(grid, layout, status)
      call ESMF_GridGetDE(grid, lcelltot_index=indext, &
                          lcellexc_index=indexe, rc=rc)
      call ESMF_DELayoutGetDEID(layout, de_id, rc)
      ni = indext(1)%r - indext(1)%l + 1
      nj = indext(2)%r - indext(2)%l + 1
      print *, "allocating", ni, " by ",nj," cells on DE", de_id
      allocate(ldata(ni,nj))

      ! Get a pointer to the start of the data
      call ESMF_ArrayGetData(array1, ldata, ESMF_NO_COPY, rc)

      ! output only the exclusive cells
      ! Print results
      print *, "------------------------------------------------------"
      write(*,*) 'de_id = ',de_id
      do j = indexe(2)%r,indexe(2)%l,-1
        write(*,10) (ldata(i,j), i=indexe(1)%l,indexe(1)%r)
 10     format(20(1x,1pe8.2))
      enddo
      print *, "------------------------------------------------------"


      call ArraysGlobalDealloc(status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in User1_Final"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine User1_Final

!----------------------------------------------------------------------------------
    end module FlowMod
!\end{verbatim}
    
