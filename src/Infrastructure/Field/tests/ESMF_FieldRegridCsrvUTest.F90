! $Id: ESMF_FieldRegridCsrvUTest.F90,v 1.17 2011/02/23 17:08:09 w6ws Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research,
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


      ! do test
      call test_csrvregridWMasks(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-1"
      write(name, *) "Conservative regridding with Masks interpolation error"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-5"
      write(name, *) "Conservative regridding with Masks conservation error"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! return result
      call ESMF_Test((csrv.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      ! do test
      call test_cartcsrvregridWMasks(itrp, csrv, rc)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding interpolation
      write(failMsg, *) "Interpolation maximum error is greater than 10^-1"
      write(name, *) "Conservative regridding on cartesian grid with Masks interpolation error"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! return result
      call ESMF_Test((itrp.eqv..true. .and. rc.eq.ESMF_SUCCESS), name, &
                      failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test conservative regridding conservation
      write(failMsg, *) "Conservation relative error is greater than 10^-5"
      write(name, *) "Conservative regridding on cartesian grid with Masks conservation error"

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
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 180
  Src_ny = 100

!  Src_nx = 144
!  Src_ny = 72

  Src_dx = 360.0/Src_nx
  Src_dy = 180.0/Src_ny

  Dst_nx = 100
  Dst_ny = 80

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup source grid
  srcGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
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

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

!    write(*,*) "A Corner: x=[",clbnd(1),cubnd(1),"] y=[",clbnd(2),cubnd(2),"]"

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Src_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Src_dy - 90.0
        yp1= REAL(i2-1+1)*Src_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx + 0.5*Src_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! set src data 
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
        !farrayPtr(i1,i2) = 1.
        farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

     enddo
     enddo

  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Dst_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Dst_dy - 90.0
        yp1= REAL(i2-1+1)*Dst_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

     enddo
     enddo


  enddo    ! lDE


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get src Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
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
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif
        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo

  enddo    ! lDE
  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcAreaptr(i1,i2)*farrayPtr(i1,i2)
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
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-7) csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-1) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then
    write(*,*) "=== Spherical grids ==="
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

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_csrvregrid


      subroutine test_csrvregridwmasks(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),xfarrayPtr(:,:),errorfarrayPtr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: Src_dx, Src_dy, yp1
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 180
  Src_ny = 100

  Src_dx = 360.0/Src_nx
  Src_dy = 180.0/Src_ny

  Dst_nx = 100
  Dst_ny = 80

  Dst_dx = 360.0/Dst_nx
  Dst_dy = 180.0/Dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8


  ! setup source grid
  srcGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
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

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Src_dy - 90.0
     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Src_dy - 90.0
        yp1= REAL(i2-1+1)*Src_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Src_dx + 0.5*Src_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! set src data 
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
       ! farrayPtr(i1,i2) = 1.
       farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        if ((lat>-45) .and. (lat<45)) then
           srcMask(i1,i2)=1
        else
           srcMask(i1,i2)=0
        endif
        
     enddo
     enddo
  enddo    ! lDE



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set dest coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx
        farrayPtrYC(i1,i2) = REAL(i2-1)*Dst_dy - 90.0

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        y= REAL(i2-1)*Dst_dy - 90.0
        yp1= REAL(i2-1+1)*Dst_dy - 90.0

        ! Set source coordinates
        farrayPtrXC(i1,i2) = REAL(i1-1)*Dst_dx + 0.5*Dst_dx
        farrayPtrYC(i1,i2) = (y+yp1)/2.0

        ! init dst data
        farrayPtr(i1,i2) = 0.0

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
       ! xfarrayPtr(i1,i2) = 1.0

        if ((lon>-45) .and. (lon<45)) then
           dstMask(i1,i2)=1
        else 
           dstMask(i1,i2)=0
        endif

     enddo
     enddo


  enddo    ! lDE


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField, dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
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
        ! skip if masked
        if (dstMask(i1,i2) .eq. 1) cycle

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
        ! (Note that this is what SCRIP does)
        if (dstFracptr(i1,i2) .lt. 0.999) cycle

        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))/ABS(xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        else
           errorfarrayPtr(i1,i2)=ABS(farrayPtr(i1,i2) - xfarrayPtr(i1,i2))
           error = error + errorfarrayPtr(i1,i2)
           if (errorfarrayPtr(i1,i2) > maxerror(1)) then
             maxerror(1) = errorfarrayPtr(i1,i2)
           endif
           if (errorfarrayPtr(i1,i2) < minerror(1)) then
             minerror(1) = errorfarrayPtr(i1,i2)
           endif
        endif

     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*farrayPtr(i1,i2)
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
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-7)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-1) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Spherical grids with masks ==="
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

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_csrvregridwmasks


      subroutine test_cartcsrvregridwmasks(itrp, csrv, rc)
        logical, intent(out)  :: itrp
        logical, intent(out)  :: csrv
        integer, intent(out)  :: rc
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errorField
  type(ESMF_Field) :: srcArea, dstArea
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: xdstArray
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: srcAreaArray, dstAreaArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: srcMask(:,:), dstMask(:,:)
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),xfptr(:,:),errorfptr(:,:),iwtsptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount, i
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string

  integer :: Src_nx, Src_ny
  integer :: Dst_nx, Dst_ny
  real(ESMF_KIND_R8) :: x,y
  real(ESMF_KIND_R8) :: cnr_x,cnr_xp1,cnr_y,cnr_yp1
  real(ESMF_KIND_R8) :: Src_dx, Src_dy
  real(ESMF_KIND_R8) :: Dst_dx, Dst_dy
  real(ESMF_KIND_R8) :: Src_minx, Src_miny
  real(ESMF_KIND_R8) :: Src_maxx, Src_maxy
  real(ESMF_KIND_R8) :: Dst_minx, Dst_miny
  real(ESMF_KIND_R8) :: Dst_maxx, Dst_maxy
   real(ESMF_KIND_R8) :: ctheta, stheta
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  Src_nx = 30
  Src_ny = 30

  Src_minx=0.0
  Src_miny=0.0

  Src_maxx=10.0
  Src_maxy=10.0

  Dst_nx = 20
  Dst_ny = 20

  Dst_minx=0.0
  Dst_miny=0.0

  Dst_maxx=10.0
  Dst_maxy=10.0


  ! setup source grid
  srcGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! setup dest. grid
  dstGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcFracField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   srcArea = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFracField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   xdstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstArea = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Add Masks
  call ESMF_GridAddItem(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! xdstArray
  call ESMF_FieldGet(xdstField, array=xdstArray, rc=localrc)
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

  ! area Array
  call ESMF_FieldGet(srcArea, array=srcAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! area Array
  call ESMF_FieldGet(dstArea, array=dstAreaArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source Grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! SET CORNER STAGGER COORDS
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set source coordinates
        fptrXC(i1,i2) = ((Src_maxx-Src_minx)*REAL(i1-1)/REAL(Src_nx-1))+Src_minx
        fptrYC(i1,i2) = ((Src_maxy-Src_miny)*REAL(i2-1)/REAL(Src_ny-1))+Src_miny
     enddo
     enddo


     !! SET CENTER STAGGER COORDS, FUNC, ETC. 
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, farrayPtr=srcMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        !!! compute corner coordinates surrounding center
        cnr_x = ((Src_maxx-Src_minx)*REAL(i1-1)/REAL(Src_nx-1))+Src_minx
        cnr_xp1 = ((Src_maxx-Src_minx)*REAL(i1-1+1)/REAL(Src_nx-1))+Src_minx

        cnr_y = ((Src_maxy-Src_miny)*REAL(i2-1)/REAL(Src_ny-1))+Src_miny
        cnr_yp1 = ((Src_maxy-Src_miny)*REAL(i2-1+1)/REAL(Src_ny-1))+Src_miny

        ! Calc Center coordinates as average of corner coords
        x = (cnr_x+cnr_xp1)/2.0
        y = (cnr_y+cnr_yp1)/2.0

        ! Set source value
        fptr(i1,i2)=x+y+20.0

        ! Set Center coordinates
        fptrXC(i1,i2) = x
        fptrYC(i1,i2) = y

        ! Set Mask
        if ((x > 4.0) .and. (x < 6.0)) then
           srcMask(i1,i2)=1
        else
           srcMask(i1,i2)=0
        endif
        
     enddo
     enddo
  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif   

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        fptrXC(i1,i2) = ((Dst_maxx-Dst_minx)*REAL(i1-1)/REAL(Dst_nx-1))+Dst_minx
        fptrYC(i1,i2) = ((Dst_maxy-Dst_miny)*REAL(i2-1)/REAL(Dst_ny-1))+Dst_miny

     enddo
     enddo


    !! DO CENTER STAGGER STUFF

     !! get coord 1
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact pointer
     call ESMF_FieldGet(xdstField, lDE, xfptr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        !!! compute corner coordinates surrounding center
        cnr_x = ((Dst_maxx-Dst_minx)*REAL(i1-1)/REAL(Dst_nx-1))+Dst_minx
        cnr_xp1 = ((Dst_maxx-Dst_minx)*REAL(i1-1+1)/REAL(Dst_nx-1))+Dst_minx

        cnr_y = ((Dst_maxy-Dst_miny)*REAL(i2-1)/REAL(Dst_ny-1))+Dst_miny
        cnr_yp1 = ((Dst_maxy-Dst_miny)*REAL(i2-1+1)/REAL(Dst_ny-1))+Dst_miny

        ! Calc Center coordinates as average of corner coords
        x = (cnr_x+cnr_xp1)/2.0
        y = (cnr_y+cnr_yp1)/2.0

        ! Set Center coordinates
        fptrXC(i1,i2) = x
        fptrYC(i1,i2) = y
     
        ! Init dest
        fptr(i1,i2)=0.0

        ! Init exact destination value
        xfptr(i1,i2)=x+y+20.0

        ! Set mask
        if ((y > 4.0) .and. (y < 6.0)) then
           dstMask(i1,i2)=1
        else
           dstMask(i1,i2)=0
        endif
        
     enddo
     enddo
  enddo    ! lDE


  ! Regrid store
  call ESMF_FieldRegridStore(srcField, srcMaskValues=(/1/), &
          dstField=dstField, dstMaskValues=(/1/), &
          routeHandle=routeHandle, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          regridMethod=ESMF_REGRID_METHOD_CONSERVE, &
          regridScheme=ESMF_REGRID_SCHEME_NATIVE, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(srcArea, &
          regridScheme=ESMF_REGRID_SCHEME_NATIVE, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  ! Get the integration weights
  call ESMF_FieldRegridGetArea(dstArea, &
          regridScheme=ESMF_REGRID_SCHEME_NATIVE, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Check if the values are close
  do lDE=0,localDECount-1

     ! get dst Field
     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get exact destination Field
     call ESMF_FieldGet(xdstField, lDE, xfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Get destination mask field
     call ESMF_GridGetItem(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, farrayPtr=dstMask, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get error Field
     call ESMF_FieldGet(errorField, lDE, errorfptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
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
        ! skip if masked
        if (dstMask(i1,i2) .eq. 1) cycle

        ! This is WRONG, shouldn't include Frac
        ! dstmass = dstmass + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*fptr(i1,i2)

        ! Instead do this
        dstmass = dstmass + dstAreaptr(i1,i2)*fptr(i1,i2)

        ! If this destination cell isn't covered by a sig. amount of source, then compute error on it.
        ! (Note that this is what SCRIP does)
        if (dstFracptr(i1,i2) .lt. 0.999) cycle

        if (xfptr(i1,i2) .ne. 0.0) then
           errorfptr(i1,i2)=ABS(fptr(i1,i2) - xfptr(i1,i2))/ABS(xfptr(i1,i2))
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

     enddo
     enddo

  enddo    ! lDE


  srcmass(1) = 0.
  do lDE=0,localDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcmass(1) = srcmass(1) + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*fptr(i1,i2)
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
  csrv = .false.
  if (ABS(dstmassg(1)-srcmassg(1))/srcmassg(1) < 10E-7)  csrv = .true.

  itrp = .false.
  if (maxerrorg(1) < 10E-1) itrp = .true.

  ! Uncomment these calls to see some actual regrid results
  if (localPet == 0) then

    write(*,*) "=== Cartesian grids with masks ==="
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

#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, srcAreaArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, xdstArray, errorArray, dstAreaarray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(srcFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstArea, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFracField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

      end subroutine test_cartcsrvregridwmasks




end program ESMF_FieldRegridCsrvUTest
