! $Id: FlowArraysMod.F90,v 1.1 2004/10/14 17:00:55 nscollins Exp $
!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: FlowArraysMod.F90 - Source file for Data for Flow Solver
!
! !DESCRIPTION:
!  Allocate and Deallocate ESMF Framework objects which handle data arrays
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
                field_rhov, field_p, field_q, field_flag, field_de

      type(ESMF_Field), save :: field_sie, field_u, field_v, field_rho, field_rhoi, &
                                field_rhou, field_rhov, field_p, field_q, field_flag, &
                                field_de
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
      integer :: haloWidth
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Array) :: array_temp
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: indexe
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: indext
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
!BOP
!
! !DESCRIPTION:
! \subsubsection{Example of Field Creation and Array Usage:}
!
!     The following piece of code provides an example of Field creation used in
!     the Demo.  In this example, we create a Field from an ArraySpec, which
!     designates the rank, type, and kind of the data.  First initialize the
!     ArraySpec with rank 2 for a two-dimensional array, type ESMF\_DATA\_REAL,
!     and kind ESMF\_KIND\_R4:
!\begin{verbatim}
      call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                              kind=ESMF_R4)
!\end{verbatim}
!     Next, create a Field named "SIE" using the ArraySpec with a relative
!     location (relloc) at the cell centers:
!\begin{verbatim}
      field_sie  = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="SIE", rc=status)
!\end{verbatim}
!     Once the Field has been created, we have to get a pointer to the Array
!     inside it.  In this example, we are not interested in the Array itself
!     so we use a temporary array:
!\begin{verbatim}
      call ESMF_FieldGetArray(field_sie, array_temp, rc=status)
!\end{verbatim}
!     Here we are getting a pointer to the data inside the Array and calling it
!     "sie."  Inside the Component "sie" can be used like an array made by an
!     F90 allocation but will reference the data inside "field\_sie."
!\begin{verbatim}
      call ESMF_ArrayGetData(array_temp, sie, ESMF_DATA_REF, status)
!\end{verbatim}
!EOP

      field_u    = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_EFACE, &
                   haloWidth=haloWidth, name="U", rc=status)
      call ESMF_FieldGetArray(field_u, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, u, ESMF_DATA_REF, status)

      field_v    = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_NFACE, &
                   haloWidth=haloWidth, name="V", rc=status)
      call ESMF_FieldGetArray(field_v, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, v, ESMF_DATA_REF, status)

      field_rho  = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="RHO", rc=status)
      call ESMF_FieldGetArray(field_rho, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rho, ESMF_DATA_REF, status)

      field_rhoi = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="RHOI", rc=status)
      call ESMF_FieldGetArray(field_rhoi, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhoi, ESMF_DATA_REF, status)

      field_rhou = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_EFACE, &
                   haloWidth=haloWidth, name="RHOU", rc=status)
      call ESMF_FieldGetArray(field_rhou, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhou, ESMF_DATA_REF, status)

      field_rhov = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_NFACE, &
                   haloWidth=haloWidth, name="RHOV", rc=status)
      call ESMF_FieldGetArray(field_rhov, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhov, ESMF_DATA_REF, status)

      field_p    = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="P", rc=status)
      call ESMF_FieldGetArray(field_p, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, p, ESMF_DATA_REF, status)

      field_q    = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="Q", rc=status)
      call ESMF_FieldGetArray(field_q, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, q, ESMF_DATA_REF, status)

      field_flag = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="FLAG", rc=status)
      call ESMF_FieldGetArray(field_flag, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, flag, ESMF_DATA_REF, status)

      field_de   = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="DE", rc=status)
      call ESMF_FieldGetArray(field_de, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, de, ESMF_DATA_REF, status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowArraysAlloc"
        return
      endif
!
! set some of the scalars from array information
!
      call ESMF_ArrayGetAxisIndex(array_temp, totalindex=indext, &
                                  compindex=indexe, rc=status)
      imin = indexe(1)%min
      imax = indexe(1)%max
      jmin = indexe(2)%min
      jmax = indexe(2)%max
      imin_t = indext(1)%min
      imax_t = indext(1)%max
      jmin_t = indext(2)%min
      jmax_t = indext(2)%max

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
      call ESMF_FieldDestroy(field_u   , rc=status)
      call ESMF_FieldDestroy(field_v   , rc=status)
      call ESMF_FieldDestroy(field_rho , rc=status)
      call ESMF_FieldDestroy(field_rhoi, rc=status)
      call ESMF_FieldDestroy(field_rhou, rc=status)
      call ESMF_FieldDestroy(field_rhov, rc=status)
      call ESMF_FieldDestroy(field_p   , rc=status)
      call ESMF_FieldDestroy(field_q   , rc=status)
      call ESMF_FieldDestroy(field_flag, rc=status)
      call ESMF_FieldDestroy(field_de  , rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in FlowArraysDealloc"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine FlowArraysDealloc

!----------------------------------------------------------------------------------
    end module FlowArraysMod
    
