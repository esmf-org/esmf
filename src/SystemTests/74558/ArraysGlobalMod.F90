! $Id: ArraysGlobalMod.F90,v 1.6 2003/07/28 15:19:36 jwolfe Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  Global storage of arrays and scalars
!
!
!\begin{verbatim}

    module ArraysGlobalMod
    
! Some common definitions.  This requires the C preprocessor.
#include "ESMF.h"

! ESMF modules
    use ESMF_Mod
    
    implicit none
    save
!
! arrays
!
    public :: sie, u, v, rho, rhoi, rhou, rhov, p, q, flag
    public :: nbc

    real, dimension(:,:), pointer :: sie, u, v, rho, rhoi, rhou, rhov, p, q, flag
    integer, dimension(4) :: nbc
!
! Fields
!
    public :: field_sie, field_u, field_v, field_rho, field_rhoi, field_rhou, &
              field_rhov, field_p, field_q, field_flag

    type(ESMF_Field), save :: field_sie, field_u, field_v, field_rho, field_rhoi, &
                              field_rhou, field_rhov, field_p, field_q, field_flag
!
! scalars here
!
    public :: imin, imax, jmin, jmax
    public :: imin_t, imax_t, jmin_t, jmax_t
    public :: dt, dx, dy
    public :: uin, rhoin, siein
    public :: gamma, akb
    public :: q0, u0, v0, sie0, rho0
    integer :: imin, imax, jmin, jmax
    integer :: imin_t, imax_t, jmin_t, jmax_t
    type(ESMF_TimeInterval) :: time_step
    real :: dt, dx, dy
    real :: uin, rhoin, siein
    real :: gamma, akb
    real :: q0, u0, v0, sie0, rho0

    contains

!-------------------------------------------------------------------------
 
      subroutine ArraysGlobalAlloc(grid, rc)

      type(ESMF_Grid) :: grid
      integer, intent(out), optional :: rc

! Local variables
!
      integer :: status
      logical :: rcpresent
      integer :: halo_width
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
      halo_width = 1
      call ESMF_ArraySpecInit(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                              kind=ESMF_KIND_R4)

      field_sie  = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   haloWidth=halo_width, name="SIE", rc=status)
      call ESMF_FieldGetData(field_sie, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, sie, ESMF_DATA_REF, status)

      field_u    = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_EAST, &
                   haloWidth=halo_width, name="U", rc=status)
      call ESMF_FieldGetData(field_u, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, u, ESMF_DATA_REF, status)

      field_v    = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_NORTH, &
                   haloWidth=halo_width, name="V", rc=status)
      call ESMF_FieldGetData(field_v, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, v, ESMF_DATA_REF, status)

      field_rho  = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   haloWidth=halo_width, name="RHO", rc=status)
      call ESMF_FieldGetData(field_rho, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rho, ESMF_DATA_REF, status)

      field_rhoi = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   haloWidth=halo_width, name="RHOI", rc=status)
      call ESMF_FieldGetData(field_rhoi, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhoi, ESMF_DATA_REF, status)

      field_rhou = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_EAST, &
                   haloWidth=halo_width, name="RHOU", rc=status)
      call ESMF_FieldGetData(field_rhou, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhou, ESMF_DATA_REF, status)

      field_rhov = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_NORTH, &
                   haloWidth=halo_width, name="RHOV", rc=status)
      call ESMF_FieldGetData(field_rhov, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhov, ESMF_DATA_REF, status)

      field_p    = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   haloWidth=halo_width, name="P", rc=status)
      call ESMF_FieldGetData(field_p, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, p, ESMF_DATA_REF, status)

      field_q    = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   haloWidth=halo_width, name="Q", rc=status)
      call ESMF_FieldGetData(field_q, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, q, ESMF_DATA_REF, status)

      field_flag = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   haloWidth=halo_width, name="FLAG", rc=status)
      call ESMF_FieldGetData(field_flag, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, flag, ESMF_DATA_REF, status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArraysGlobalAlloc"
        return
      endif
!
! set some of the scalars from grid information
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
! TODO  need to add calls to get dx and dy from physgrid -- just set for today
      dx = 5.0
      dy = 2.50

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ArraysGlobalAlloc

!-------------------------------------------------------------------------
 
      subroutine ArraysGlobalDealloc(rc)

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
        print *, "ERROR in ArraysGlobalDealloc"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ArraysGlobalDealloc

!----------------------------------------------------------------------------------
    end module ArraysGlobalMod
!\end{verbatim}
    
