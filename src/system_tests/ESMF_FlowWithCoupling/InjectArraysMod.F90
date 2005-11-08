! $Id: InjectArraysMod.F90,v 1.7 2005/11/08 21:11:45 nscollins Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
!  Global storage of arrays and scalars
!
!
!\begin{verbatim}

      module InjectArraysMod
!
! Some common definitions.  This requires the C preprocessor.
!
#include "ESMF.h"
!
! ESMF modules
!
      use ESMF_Mod
    
      implicit none
      !save
!
! arrays
!
      public :: sie, u, v, rho, rhoi, rhou, rhov, p, q, flag
      public :: nbc
      public :: iobs_min, iobs_max, jobs_min, jobs_max

      real(kind=ESMF_KIND_R4), dimension(:,:), pointer, save :: sie, u, v, rho, rhoi, &
                                                           rhou, rhov, p, q, flag
      integer, dimension(4), save :: nbc
      integer, dimension(50), save :: iobs_min, iobs_max, jobs_min, jobs_max
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
      real(kind=ESMF_KIND_R4) :: dt, dx, dy
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
      logical :: rcpresent
      integer :: haloWidth
      type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Array) :: array_temp
      integer :: lowerindex(2), upperindex(2)

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
      call ESMF_ArraySpecSet(arrayspec, rank=2, type=ESMF_DATA_REAL, &
                              kind=ESMF_R4)

      field_sie  = ESMF_FieldCreate(grid, arrayspec, horzRelloc=ESMF_CELL_CENTER, &
                   haloWidth=haloWidth, name="SIE", rc=status)
      call ESMF_FieldGetArray(field_sie, array_temp, rc=status)
      call ESMF_ArrayGetData(array_temp, sie, ESMF_DATA_REF, status)

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

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in InjectArraysAlloc"
        return
      endif
!
! set some of the scalars from array information
!

      lowerindex = lbound(flag)
      upperindex = ubound(flag)

      imin_t = lowerindex(1)
      imax_t = upperindex(1)
      jmin_t = lowerindex(2)
      jmax_t = upperindex(2)
      imin = imin_t + haloWidth
      imax = imax_t - haloWidth
      jmin = jmin_t + haloWidth
      jmax = jmax_t - haloWidth

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

!----------------------------------------------------------------------------------
    end module InjectArraysMod
!\end{verbatim}
    
