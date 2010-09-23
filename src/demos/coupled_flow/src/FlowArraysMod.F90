! $Id: FlowArraysMod.F90,v 1.6 2010/09/23 21:12:50 feiliu Exp $
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

      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: sie, u, v
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: rho, rhoi, rhou, rhov
      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: p, q, flag, de
      integer, dimension(4), save :: nbc
      integer, dimension(50), save :: iobs_min, iobs_max, jobs_min, jobs_max
!
! Fields
!
      public :: field_sie, field_u, field_v, field_rho, field_rhoi, field_rhou, &
                field_rhov, field_p, field_q, field_flag, field_de, halohandle

      type(ESMF_Field), save :: field_sie, field_u, field_v, field_rho, field_rhoi, &
                                field_rhou, field_rhov, field_p, field_q, field_flag, &
                                field_de
      type(ESMF_RouteHandle), save :: halohandle
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
 
      subroutine FlowArraysAlloc(grid, rc)

      type(ESMF_Grid) :: grid
      integer, intent(out), optional :: rc

! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: haloLWidth(2), haloUWidth(2)
      type(ESMF_ArraySpec) :: arrayspec
      integer, dimension(2) :: tlb, tub, clb, cub
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
      haloLWidth = 0
      haloUWidth = 2
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
                   maxHaloLWidth=(/0,0/), maxHaloUWidth=(/1,2/), name="U", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_u, farrayPtr=u, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_v    = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE2, &
                   maxHaloLWidth=(/0,0/), maxHaloUWidth=(/2,1/), name="V", rc=status)
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
                   maxHaloLWidth=(/0,0/), maxHaloUWidth=(/1,2/), name="RHOU", rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)
      call ESMF_FieldGet(field_rhou, farrayPtr=rhou, rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      field_rhov = ESMF_FieldCreate(grid, arrayspec, staggerloc=ESMF_STAGGERLOC_EDGE2, &
                   maxHaloLWidth=(/0,0/), maxHaloUWidth=(/2,1/), name="RHOV", rc=status)
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
! set some of the scalars from Field information
!
      call ESMF_FieldGetBounds(field_de, &
           totalLBound=tlb, totalUBound=tub, &
           rc=status)
      if(status /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT, rc=status)

      ! Computational region: data unique to this DE
      imin = tlb(1)+1
      imax = tub(1)-1
      jmin = tlb(2)+1
      jmax = tub(2)-1
      ! Total region: data plus the halo widths
      imin_t = tlb(1) 
      imax_t = tub(1)
      jmin_t = tlb(2)
      jmax_t = tub(2)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowArraysAlloc

!-------------------------------------------------------------------------
 
      subroutine FlowArraysDealloc(rc)

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

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowArraysDealloc

!------------------------------------------------------------------------------
    end module FlowArraysMod
    
