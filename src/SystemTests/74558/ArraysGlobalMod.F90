! $Id: ArraysGlobalMod.F90,v 1.4 2003/04/24 16:43:17 nscollins Exp $
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

    type(ESMF_Field) :: field_sie, field_u, field_v, field_rho, field_rhoi, &
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
      call ESMF_ArraySpecInit(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                              kind=ESMF_KIND_R4)

      field_sie  = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   name="SIE", rc=status)
      call ESMF_FieldGetData(field_sie, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, sie, ESMF_DATA_REF, status)

      field_u    = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_EAST, &
                   name="U", rc=status)
      call ESMF_FieldGetData(field_u, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, u, ESMF_DATA_REF, status)

      field_v    = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_NORTH, &
                   name="V", rc=status)
      call ESMF_FieldGetData(field_v, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, v, ESMF_DATA_REF, status)

      field_rho  = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   name="RHO", rc=status)
      call ESMF_FieldGetData(field_rho, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rho, ESMF_DATA_REF, status)

      field_rhoi = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   name="RHOI", rc=status)
      call ESMF_FieldGetData(field_rhoi, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhoi, ESMF_DATA_REF, status)

      field_rhou = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_EAST, &
                   name="RHOU", rc=status)
      call ESMF_FieldGetData(field_rhou, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhou, ESMF_DATA_REF, status)

      field_rhov = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_NORTH, &
                   name="RHOV", rc=status)
      call ESMF_FieldGetData(field_rhov, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, rhov, ESMF_DATA_REF, status)

      field_p    = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   name="P", rc=status)
      call ESMF_FieldGetData(field_p, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, p, ESMF_DATA_REF, status)

      field_q    = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   name="Q", rc=status)
      call ESMF_FieldGetData(field_q, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, q, ESMF_DATA_REF, status)

      field_flag = ESMF_FieldCreate(grid, arrayspec, relloc=ESMF_CELL_CENTER, &
                   name="FLAG", rc=status)
      call ESMF_FieldGetData(field_flag, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, flag, ESMF_DATA_REF, status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArraysGlobalAlloc"
        return
      endif
!
! set some of the scalars from grid information
!
      call ESMF_GridGetDE(grid, lcelltot_index=indext, lcellexc_index=indexe, &
                          rc=status)
      imin = indexe(1)%l
      imax = indexe(1)%r
      jmin = indexe(2)%l
      jmax = indexe(2)%r
      imin_t = indext(1)%l
      imax_t = indext(1)%r
      jmin_t = indext(2)%l
      jmax_t = indext(2)%r
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
    
