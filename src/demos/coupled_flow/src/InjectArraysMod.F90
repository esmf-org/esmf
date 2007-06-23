! $Id: InjectArraysMod.F90,v 1.3 2007/06/23 04:01:15 cdeluca Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOPI
!
! !DESCRIPTION:
!  Global storage of arrays and scalars, using the following 
!    ESMF objects: ESMF\_Field, ESMF\_IGrid, ESMF\_Array.
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
 
      subroutine InjectArraysAlloc(igrid, rc)

      type(ESMF_IGrid) :: igrid
      integer, intent(out), optional :: rc

! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: haloWidth
      type(ESMF_ArraySpec) :: arrayspec
      integer, dimension(2) :: lb, ub
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
! create fields and get pointers to data
!
      haloWidth = 1
      call ESMF_ArraySpecSet(arrayspec, rank=2, &
                             typekind=ESMF_TYPEKIND_R4)

      field_sie  = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="SIE", rc=status)
      call ESMF_FieldGetDataPointer(field_sie, sie, ESMF_DATA_REF, rc=status)

      field_u    = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_EFACE, &
                   haloWidth=haloWidth, name="U", rc=status)
      call ESMF_FieldGetDataPointer(field_u, u, ESMF_DATA_REF, rc=status)

      field_v    = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_NFACE, &
                   haloWidth=haloWidth, name="V", rc=status)
      call ESMF_FieldGetDataPointer(field_v, v, ESMF_DATA_REF, rc=status)

      field_rho  = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="RHO", rc=status)
      call ESMF_FieldGetDataPointer(field_rho, rho, ESMF_DATA_REF, rc=status)

      field_rhoi = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="RHOI", rc=status)
      call ESMF_FieldGetDataPointer(field_rhoi, rhoi, ESMF_DATA_REF, rc=status)

      field_rhou = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_EFACE, &
                   haloWidth=haloWidth, name="RHOU", rc=status)
      call ESMF_FieldGetDataPointer(field_rhou, rhou, ESMF_DATA_REF, rc=status)

      field_rhov = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_NFACE, &
                   haloWidth=haloWidth, name="RHOV", rc=status)
      call ESMF_FieldGetDataPointer(field_rhov, rhov, ESMF_DATA_REF, rc=status)

      field_p    = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="P", rc=status)
      call ESMF_FieldGetDataPointer(field_p, p, ESMF_DATA_REF, rc=status)

      field_q    = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="Q", rc=status)
      call ESMF_FieldGetDataPointer(field_q, q, ESMF_DATA_REF, rc=status)

      field_flag = ESMF_FieldCreate(igrid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="FLAG", rc=status)
      call ESMF_FieldGetDataPointer(field_flag, flag, ESMF_DATA_REF, rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in InjectArraysAlloc"
        return
      endif
!
! set some of the scalars from Field information
!      
      call ESMF_FieldGet(field_flag, haloWidth=haloWidth, lbounds=lb, &
                         ubounds=ub, rc=rc)

      ! Computational region: data unique to this DE
      imin = lb(1) + haloWidth
      imax = ub(1) - haloWidth
      jmin = lb(2) + haloWidth
      jmax = ub(2) - haloWidth
      ! Total region: data plus the halo widths
      imin_t = lb(1)
      imax_t = ub(1)
      jmin_t = lb(2)
      jmax_t = ub(2)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine InjectArraysAlloc

!-------------------------------------------------------------------------
 
      subroutine InjectArraysDealloc(rc)

      integer, intent(out), optional :: rc

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
! deallocate global arrays - destroy the Fields
!
      call ESMF_FieldDestroy(field_sie , rc=status)
      call ESMF_FieldDestroy(field_u   , rc=status)
      call ESMF_FieldDestroy(field_v   , rc=status)
      call ESMF_FieldDestroy(field_rho , rc=status)
      call ESMF_FieldDestroy(field_rhoi, rc=status)
      call ESMF_FieldDestroy(field_rhou, rc=status)
      call ESMF_FieldDestroy(field_rhov, rc=status)
      call ESMF_FieldDestroy(field_p   , rc=status)
      call ESMF_FieldDestroy(field_q   , rc=status)
      call ESMF_FieldDestroy(field_flag, rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in InjectArraysDealloc"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine InjectArraysDealloc

!------------------------------------------------------------------------------
    end module InjectArraysMod
    
