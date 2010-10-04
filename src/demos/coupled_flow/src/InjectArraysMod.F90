! $Id: InjectArraysMod.F90,v 1.8 2010/10/04 14:05:05 feiliu Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOPI
!
! !DESCRIPTION:
!  Global storage of arrays and scalars, using the following 
!    ESMF objects: ESMF\_Field, ESMF\_Grid, ESMF\_Array.
!
!EOPI

      module InjectArraysMod
!
! ESMF modules
!
      use ESMF_Mod
    
      implicit none
!
! arrays
!
      public :: sie, u, v, rho, rhoi, rhou, rhov, p, q, flag
      public :: nbc
      public :: iobs_min, iobs_max, jobs_min, jobs_max

      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: sie, u, v
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: rho, rhoi, rhou, rhov
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: p, q, flag
      integer, dimension(4), save :: nbc
      integer, dimension(50), save :: iobs_min, iobs_max, jobs_min, jobs_max
!
! Fields
!
      public :: field_sie, field_u, field_v, field_rho, field_rhoi, &
                field_rhou, field_rhov, field_p, field_q, field_flag

      type(ESMF_Field), save :: field_sie, field_u, field_v, field_rho, &
                                field_rhoi, field_rhou, field_rhov, &
                                field_p, field_q, field_flag
!
! scalars here
!
      public :: imin, imax, jmin, jmax
      public :: imin_t, imax_t, jmin_t, jmax_t
      public :: printout
      public :: nobsdesc
      public :: iflo_min, iflo_max
      public :: dt, dx, dy
      public :: uin, rhoin, siein
      public :: vin2, rhoin2, siein2
      public :: gamma, akb
      public :: q0, u0, v0, sie0, rho0
      public :: sieobs
      integer :: imin, imax, jmin, jmax
      integer :: imin_t, imax_t, jmin_t, jmax_t
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
 
      subroutine InjectArraysAlloc(grid, rc)

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
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_R4, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_sie  = ESMF_FieldCreate(grid, arrayspec, &
                   maxHaloLWidth=haloLWidth, maxHaloUWidth=haloUWidth, name="SIE", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_sie, farrayPtr=sie, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

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

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in InjectArraysAlloc"
        return
      endif
!
! get bounds information from Field flag
!      
      call ESMF_FieldGetBounds(field_flag, &
                        totalLBound=tlb, totalUBound=tub, &
                        exclusiveLBound=lb, exclusiveUBound=ub, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      ! Computational region: data unique to this DE
      imin = lb(1)
      imax = ub(1)
      jmin = lb(2)
      jmax = ub(2)
      ! Total region: data plus the halo widths
      imin_t = tlb(1) 
      imax_t = tub(1)
      jmin_t = tlb(2)
      jmax_t = tub(2)

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine InjectArraysAlloc

!-------------------------------------------------------------------------
 
      subroutine InjectArraysDealloc(rc)

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

      if(present(rc)) rc = ESMF_SUCCESS

      end subroutine InjectArraysDealloc

!------------------------------------------------------------------------------
    end module InjectArraysMod
    
