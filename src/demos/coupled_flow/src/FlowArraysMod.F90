! $Id: FlowArraysMod.F90,v 1.8 2010/10/04 14:05:05 feiliu Exp $
!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: FlowArraysMod.F90 - Source file for Data for Flow Solver
!
! !DESCRIPTION:
!  Allocate and deallocate ESMF objects which handle data arrays
!  including ESMF\_Fields, ESMF\_Grids,  and ESMF\_Arrays.
!
!EOP
!
      module FlowArraysMod
!
! ESMF modules
!
      use ESMF_Mod
    
      implicit none
!
! arrays
!
      public :: sie, u, v, rho, rhoi, rhou, rhov, p, q, flag, de
      public :: nbc
      public :: iobs_min, iobs_max, jobs_min, jobs_max

      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: sie, u, v, omega
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: rho, rhoi, rhou, rhov
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: p, q, flag, de
      integer, dimension(4), save :: nbc
      integer, dimension(50), save :: iobs_min, iobs_max, jobs_min, jobs_max
!
! Fields
!
      public :: field_sie, field_u, field_v, field_rho, field_rhoi, field_rhou, &
                field_rhov, field_p, field_q, field_flag, field_de, halohandle, &
                field_omega

      type(ESMF_Field), save :: field_sie, field_u, field_v, field_rho, field_rhoi, &
                                field_rhou, field_rhov, field_p, field_q, field_flag, &
                                field_de, field_omega
      type(ESMF_RouteHandle), save :: halohandle
!
! scalars here
!
      public :: imin_ec, imax_ec, jmin_ec, jmax_ec
      public :: imin_tc, imax_tc, jmin_tc, jmax_tc
      public :: imin_ee1, imax_ee1, jmin_ee1, jmax_ee1
      public :: imin_te1, imax_te1, jmin_te1, jmax_te1
      public :: imin_ee2, imax_ee2, jmin_ee2, jmax_ee2
      public :: imin_te2, imax_te2, jmin_te2, jmax_te2

      public :: printout
      public :: nobsdesc
      public :: iflo_min, iflo_max
      public :: dt, dx, dy
      public :: uin, rhoin, siein
      public :: vin2, rhoin2, siein2
      public :: gamma, akb
      public :: q0, u0, v0, sie0, rho0
      public :: sieobs

      integer :: imin_ec, imax_ec, jmin_ec, jmax_ec
      integer :: imin_tc, imax_tc, jmin_tc, jmax_tc
      integer :: imin_ee1, imax_ee1, jmin_ee1, jmax_ee1
      integer :: imin_te1, imax_te1, jmin_te1, jmax_te1
      integer :: imin_ee2, imax_ee2, jmin_ee2, jmax_ee2
      integer :: imin_te2, imax_te2, jmin_te2, jmax_te2

      integer :: printout
      integer :: nobsdesc
      integer :: iflo_min, iflo_max
      type(ESMF_TimeInterval), save :: time_step
      real(kind=ESMF_KIND_R8) :: dt
      real(kind=ESMF_KIND_R4) :: dx, dy
      real(kind=ESMF_KIND_R4) :: uin, rhoin, siein
      real(kind=ESMF_KIND_R4) :: vin2, rhoin2, siein2
      real(kind=ESMF_KIND_R4) :: gamma, akb
      real(kind=ESMF_KIND_R4) :: q0, u0, v0, sie0, rho0
      real(kind=ESMF_KIND_R4) :: sieobs

      contains

!-------------------------------------------------------------------------
 
      subroutine FlowArraysAlloc(grid, rc)

      type(ESMF_Grid) :: grid
      integer, intent(out), optional :: rc

! Local variables
!
      integer :: status
      integer :: haloLWidth(2), haloUWidth(2)
      type(ESMF_ArraySpec) :: arrayspec
      integer, dimension(2) :: lb, ub, tlb, tub
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
! create fields and get pointers to data
!
      haloLWidth = 1
      haloUWidth = 1
!BOP
!
! !DESCRIPTION:
! \subsubsection{Example of Field Creation and Array Usage:}
!
!     The following piece of code provides an example of Field creation used in
!     the demo.  In this example we create a Field from an ArraySpec, which
!     designates the rank, type, and kind of the data.  First initialize the
!     ArraySpec with rank 2 for a two-dimensional array
!     and kind ESMF\_KIND\_R4:
!\begin{verbatim}
      call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R4, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
!\end{verbatim}
!     Next, create a Field named "SIE" using the ArraySpec with a relative
!     location (relloc) at the cell centers:
!\begin{verbatim}
      field_sie  = ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="SIE", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
!\end{verbatim}
!     Once the Field has been created, we get a pointer to the Array
!     data (the Fortran 90 array), and call it "sie".
!     Inside the Component "sie" can be used like an array made by an
!     F90 allocation but will reference the data inside "field\_sie."
!\begin{verbatim}
      call ESMF_FieldGet(field_sie, farrayPtr=sie, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
!\end{verbatim}
!EOP

      field_u    = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE1, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="U", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_u, farrayPtr=u, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_v    = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE2, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="V", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_v, farrayPtr=v, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_rho  = ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="RHO", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_rho, farrayPtr=rho, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_rhoi = ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="RHOI", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_rhoi, farrayPtr=rhoi, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_rhou = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE1, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="RHOU", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_rhou, farrayPtr=rhou, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_rhov = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE2, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="RHOV", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_rhov, farrayPtr=rhov, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_p    = ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="P", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_p, farrayPtr=p, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_omega= ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="OMEGA", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_omega, farrayPtr=omega, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_q    = ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="Q", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_q, farrayPtr=q, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_flag = ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="FLAG", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_flag, farrayPtr=flag, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_de   = ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="DE", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_de, farrayPtr=de, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowArraysAlloc"
        return
      endif
!
! get bounds information from Field DE for center stagger
!
      call ESMF_FieldGetBounds(field_de, &
                        totalLBound=tlb, totalUBound=tub, &
                        exclusiveLBound=lb, exclusiveUBound=ub, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      ! Computational region: data unique to this DE
      imin_ec = lb(1)
      imax_ec = ub(1)
      jmin_ec = lb(2)
      jmax_ec = ub(2)
      ! Total region: data plus the halo widths
      imin_tc = tlb(1) 
      imax_tc = tub(1)
      jmin_tc = tlb(2)
      jmax_tc = tub(2)
!
! get bounds information from Field U for edge1 stagger
!
      call ESMF_FieldGetBounds(field_u, &
                        totalLBound=tlb, totalUBound=tub, &
                        exclusiveLBound=lb, exclusiveUBound=ub, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      ! Computational region: data unique to this DE
      imin_ee1 = lb(1)
      imax_ee1 = ub(1)
      jmin_ee1 = lb(2)
      jmax_ee1 = ub(2)
      ! Total region: data plus the halo widths
      imin_te1 = tlb(1) 
      imax_te1 = tub(1)
      jmin_te1 = tlb(2)
      jmax_te1 = tub(2)
!
! get bounds information from Field V for edge2 stagger
!
      call ESMF_FieldGetBounds(field_v, &
                        totalLBound=tlb, totalUBound=tub, &
                        exclusiveLBound=lb, exclusiveUBound=ub, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      ! Computational region: data unique to this DE
      imin_ee2 = lb(1)
      imax_ee2 = ub(1)
      jmin_ee2 = lb(2)
      jmax_ee2 = ub(2)
      ! Total region: data plus the halo widths
      imin_te2 = tlb(1) 
      imax_te2 = tub(1)
      jmin_te2 = tlb(2)
      jmax_te2 = tub(2)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowArraysAlloc

!-------------------------------------------------------------------------
 
      subroutine FlowArraysDealloc(rc)

      integer, intent(out), optional :: rc

! Local variables
!
      integer :: status
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
! deallocate global arrays - destroy the Fields
!
      call ESMF_FieldDestroy(field_sie , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_u   , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_v   , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_rho , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_rhoi, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_rhou, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_rhov, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_p   , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_q   , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_flag, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldDestroy(field_de  , rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowArraysDealloc"
        return
      endif

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine FlowArraysDealloc

!------------------------------------------------------------------------------
    end module FlowArraysMod
    
