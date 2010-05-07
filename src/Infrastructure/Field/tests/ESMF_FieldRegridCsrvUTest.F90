! $Id: ESMF_FieldRegridCsrvUTest.F90,v 1.4 2010/05/07 17:21:54 rokuingh Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridCsrvUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridCsrvUTest - Unit tests for conservative Field Regrid methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 conservative Field Regrid unit tests.
!
!EOPI
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF_Mod
    use ESMF_GridUtilMod

    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id'

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = 1
    logical :: itrp = .false.
    logical :: csrv = .false.

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
#ifdef ESMF_TESTEXHAUSTIVE
      ! do test
      call test_csrvregrid(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-1"
      write(name, *) "Conservative regridding interpolation error"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-5"
      write(name, *) "Conservative regridding conservation error"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 
#define ESMF_METHOD "test_csrvregrid"

      subroutine test_csrvregrid(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: grid360
  type(ESMF_Grid) :: grid180
  type(ESMF_Field) :: srcField360
  type(ESMF_Field) :: field180
  type(ESMF_Field) :: xfield180
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srciwts, dstiwts
  type(ESMF_Array) :: array180
  type(ESMF_Array) :: xarray180
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: lonArray360
  type(ESMF_Array) :: srcArray360
  type(ESMF_Array) :: srciwtsArray, dstiwtsArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),xfptr(:,:),errorfptr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srciwtsptr(:,:), dstiwtsptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
  real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: srcmass(1), dstmass(1), srcmassg(1), dstmassg(1)
  real(ESMF_KIND_R8) :: maxerror(1), minerror(1), error
  real(ESMF_KIND_R8) :: maxerrorg(1), minerrorg(1), errorg

  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  rc=ESMF_SUCCESS

  ! get pet info
  call ESMF_VMGetGlobal(vm, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return

  ! Establish the resolution of the grids
  src_nx = 180
  src_ny = 100

  dst_nx = 90
  dst_ny = 50

  ! setup source grid
  grid360=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  grid180=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcField360 = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srciwts = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(grid180, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   field180 = ESMF_FieldCreate(grid180, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xfield180 = ESMF_FieldCreate(grid180, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstiwts = ESMF_FieldCreate(grid180, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(grid360, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(grid180, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(grid360, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! array180
  call ESMF_FieldGet(field180, array=array180, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray360
  call ESMF_FieldGet(srcField360, array=srcArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xarray180
  call ESMF_FieldGet(xfield180, array=xarray180, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(errorField, array=errorArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(srciwts, array=srciwtsArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! errorArray
  call ESMF_FieldGet(dstiwts, array=dstiwtsArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  !! get longitude array
  call ESMF_GridGetCoord(grid360, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=lonArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! setup for coordinates
  src_dx = 360./src_nx
  src_dy = 180./src_ny

  DEG2RAD = 3.14159265/180.0

  ! Get memory and set coords for src
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcField360, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates as 0 to 360
        fptrXC(i1,i2) = REAL(i1-1)*src_dx
        fptrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        lon = fptrXC(i1,i2)
        lat = fptrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        fptr(i1,i2) = 1.
        !fptr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        !fptr(i1,i2) = 2. + sin(2.*theta)**16.*cos(16.*phi)

     enddo
     enddo

  enddo    ! lDE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  dst_dx = 360./dst_nx
  dst_dy = 180./dst_ny

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(field180, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xfield180, lDE, xfptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error dst clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error dst clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error dst cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error dst cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set destination coordinates
        fptrXC(i1,i2) = (REAL(i1-1)*dst_dx)
        fptrYC(i1,i2) = -90.  + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        lon = fptrXC(i1,i2)
        lat = fptrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set dst exact data
        xfptr(i1,i2) = 1.
        !xfptr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        !xfptr(i1,i2) = 2. + sin(2.*theta)**16.*cos(16.*phi)

        ! init destination mesh to 0
        fptr(i1,i2) = 0.
     
     enddo
     enddo

  enddo    ! lDE

  ! Regrid store
  call ESMF_FieldRegridStore(srcField360, dstField=field180, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          regridConserve=ESMF_REGRID_CONSERVE_ON, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField360, field180, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetIwts(srcField360, srciwts, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetIwts(field180, dstiwts, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get src Field
     call ESMF_FieldGet(field180, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field
     call ESMF_FieldGet(xfield180, lDE, xfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(errorField, lDE, errorfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstiwts, lDE, dstiwtsptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     minerror(1) = 100000.
     maxerror(1) = 0.
     error = 0.
     dstmass = 0.
 
     ! destination grid
     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (xfptr(i1,i2) .ne. 0.0) then
           errorfptr(i1,i2)=ABS(fptr(i1,i2) - xfptr(i1,i2))/ABS(fptr(i1,i2))
           error = error + errorfptr(i1,i2)
           if (errorfptr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfptr(i1,i2)
           endif
           if (errorfptr(i1,i2) < minerror(1)) then
             minerror(1) = errorfptr(i1,i2)
           endif
        else
           errorfptr(i1,i2)=ABS(fptr(i1,i2) - xfptr(i1,i2))
           error = error + errorfptr(i1,i2)
           if (errorfptr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfptr(i1,i2)
           endif
           if (errorfptr(i1,i2) < minerror(1)) then
             minerror(1) = errorfptr(i1,i2)
           endif
        endif
        dstmass = dstmass + dstiwtsptr(i1,i2)
     enddo
     enddo

  enddo    ! lDE

  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField360, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srciwts, lDE, srciwtsptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srciwtsptr(i1,i2)
     enddo
     enddo

  enddo    ! lDE

  srcmassg(1) = 0.
  dstmassg(1) = 0.
  
  call ESMF_VMAllReduce(vm, srcmass, srcmassg, 1, ESMF_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, dstmass, dstmassg, 1, ESMF_SUM, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, maxerror, maxerrorg, 1, ESMF_MAX, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_VMAllReduce(vm, minerror, minerrorg, 1, ESMF_MIN, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! return answer based on correct flags
  itrp = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-5) itrp = .true.

  csrv = .false.
  if (maxerrorg(1) < 10E-1) csrv = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) " "
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(dstmassg(1)-srcmassg(1))/srcmassg(1)
    write(*,*) "SRC mass = ", srcmassg(1)
    write(*,*) "DST mass = ", dstmassg(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", maxerrorg(1)
    write(*,*) "Min Error = ", minerrorg(1)
    write(*,*) "Avg Error = ", (maxerrorg(1) + minerrorg(1))/2
    write(*,*) " "

  endif

  spherical_grid = 1
  call ESMF_MeshIO(vm, grid360, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray360, srciwtsarray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, grid180, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", array180, xarray180, errorArray, dstiwtsarray, rc=localrc, &
               spherical=spherical_grid)

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField360, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(field180, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xfield180, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(grid360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(grid180, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_csrvregrid

end program ESMF_FieldRegridCsrvUTest
