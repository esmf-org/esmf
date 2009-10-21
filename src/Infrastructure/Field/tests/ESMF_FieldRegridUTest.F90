! $Id: ESMF_FieldRegridUTest.F90,v 1.6 2009/10/21 21:09:22 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_FieldRegridUTest

!------------------------------------------------------------------------------

#include "ESMF.h"

!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldRegridUTest - Unit tests for Field Regrid methods
!
! !DESCRIPTION:
!
! The code in this file drives F90 Field Regrid unit tests.
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

    ! individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(512) :: name

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
 
#ifdef ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid between -180-180 sphere and a 360 sphere
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid between a 0 to 360 sphere and a -180 to 180 sphere"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regrid180vs360(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test 3D regrid
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "3D Regrid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regrid3D(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with destination masks"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridDstMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with source masks"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSrcMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid sphere with source masks"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphSrcMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with corner stagger"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridCnr(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid with edge stagger"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridEdge(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

contains 
#define ESMF_METHOD "ESMF_TESTS"

      subroutine test_regrid180vs360(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: grid360
  type(ESMF_Grid) :: grid180
  type(ESMF_Field) :: srcField360
  type(ESMF_Field) :: dstField360
  type(ESMF_Field) :: field180
  type(ESMF_Field) :: errorField
  type(ESMF_Array) :: array180
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: lonArray360
  type(ESMF_Array) :: srcArray360, dstArray360
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandle1
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:),errorfptr(:,:)
  integer :: petMap2D(2,2,1)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
  real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: rangle, xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: RAD2DEG

  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.
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
  !dst_nx = 284
!  dst_nx = 154
!  dst_ny = 100
  dst_nx = 90
  dst_ny = 50

  src_nx = 90
  src_ny = 50

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

   dstField360 = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(grid360, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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

  ! dstArray360
  call ESMF_FieldGet(dstField360, array=dstArray360, rc=localrc)
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



  !! get longitude array
  call ESMF_GridGetCoord(grid360, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=lonArray360, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif


  ! Write results to a mesh
  num_arrays = 1

! Test interpolation on the sphere
! Set the source grid coordinates to be a 0 to 360 grid

  src_dx = 360./src_nx
  src_dy = 180./src_ny

  DEG2RAD = 3.14159265/180.0
  RAD2DEG = 1./DEG2RAD

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


     ! get src destination Field pointer
     call ESMF_FieldGet(dstField360, lDE, fptr2,   rc=localrc)
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
        ! (something relatively smooth, that varies everywhere)
        fptr(i1,i2) = x+y+z+15.0

         ! initialize src destination field
         fptr2(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  dst_dx = 360./dst_nx
  dst_dy = 180./dst_ny

  rangle = DEG2RAD*20.

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


    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error dst clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error dst clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error dst cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error dst cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! Set destination coordinates as -180 to 180
        fptrXC(i1,i2) = -180. + (REAL(i1-1)*dst_dx)
        fptrYC(i1,i2) = -90.  + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! init destination mesh to 0
        fptr(i1,i2) = 0.
     
     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the 0 to 360 grid to the -180 to 180 grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcField360, dstField=field180, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
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

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  !!!!!!!! Regrid back from the -180 to 180 grid to the 0 to 360 grid
  ! Regrid store
  call ESMF_FieldRegridStore(field180, dstField=dstField360, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(field180, dstField360, routeHandle, rc=localrc)
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
     call ESMF_FieldGet(srcField360, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field
     call ESMF_FieldGet(dstField360, lDE, fptr2,  rc=localrc)
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


     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (fptr(i1,i2) .ne. 0.0) then
           errorfptr(i1,i2)=ABS((fptr(i1,i2) - fptr2(i1,i2))/fptr(i1,i2))
        else
           errorfptr(i1,i2)=(fptr(i1,i2) - fptr2(i1,i2))
        endif
        if (ABS(errorfptr(i1,i2)) .gt. 0.01) then
            correct=.false. 
        endif

     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, grid360, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray360, dstArray360, errorArray, lonArray360, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, grid180, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", array180, rc=localrc, &
               spherical=spherical_grid)
#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField360, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField360, rc=localrc)
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

  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

      end subroutine test_regrid180vs360

      subroutine test_regrid3D(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: dstFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: errorField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: errorArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA, dstArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandle1
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:,:),fptr2(:,:,:),errorfptr(:,:,:)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(3)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(3)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, A_nz, B_nx, B_ny, B_nz
  integer num_arrays

  real(ESMF_KIND_R8) :: A_minx,A_miny,A_minz
  real(ESMF_KIND_R8) :: A_maxx,A_maxy,A_maxz
  real(ESMF_KIND_R8) :: B_minx,B_miny,B_minz
  real(ESMF_KIND_R8) :: B_maxx,B_maxy,B_maxz
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  B_nx = 20
  B_ny = 20
  B_nz = 20

  A_nx = 10
  A_ny = 10
  A_nz = 10

  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  B_minz = 0.0

  B_maxx = 10.0
  B_maxy = 10.0
  B_maxz = 10.0

  A_minx = 0.0
  A_miny = 0.0
  A_minz = 0.0

  A_maxx = 10.0
  A_maxy = 10.0
  A_maxz = 10.0

  ! setup source grid
  gridA=ESMF_GridCreateShapeTile(minIndex=(/1,1,1/),maxIndex=(/A_nx,A_ny,A_nz/),regDecomp=(/petCount,1,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateShapeTile(minIndex=(/1,1,1/),maxIndex=(/B_nx,B_ny,B_nz/),regDecomp=(/1,1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   dstFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   errorField = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! dstArrayA
  call ESMF_FieldGet(dstFieldA, array=dstArrayA, rc=localrc)
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


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field pointer
     call ESMF_FieldGet(dstFieldA, lDE, fptr2,   rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        ! Set source coordinates
        fptrXC(i1,i2,i3) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        fptrYC(i1,i2,i3) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny
        fptrZC(i1,i2,i3) = ((A_maxz-A_minz)*REAL(i3-1)/REAL(A_nz-1))+A_minz

        ! set src data
        ! (something  smooth, that varies everywhere)
        fptr(i1,i2,i3) = fptrXC(i1,i2,i3)+fptrYC(i1,i2,i3)+fptrZC(i1,i2,i3)+15.0

        ! initialize src destination field
        fptr2(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        ! Set source coordinates
        fptrXC(i1,i2,i3) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        fptrYC(i1,i2,i3) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny
        fptrZC(i1,i2,i3) = ((B_maxz-B_minz)*REAL(i3-1)/REAL(B_nz-1))+B_minz

        ! initialize destination field
        fptr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcFieldA, dstField=fieldB, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  !!!!!!!! Regrid back from the B grid to the A grid
  ! Regrid store
  call ESMF_FieldRegridStore(fieldB, dstField=dstFieldA, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(fieldB, dstFieldA, routeHandle, rc=localrc)
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
     call ESMF_FieldGet(srcFieldA, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field
     call ESMF_FieldGet(dstFieldA, lDE, fptr2,  rc=localrc)
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


     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        if (fptr(i1,i2,i3) .ne. 0.0) then
           errorfptr(i1,i2,i3)=ABS((fptr(i1,i2,i3) - fptr2(i1,i2,i3))/fptr(i1,i2,i3))
        else
           errorfptr(i1,i2,i3)=(fptr(i1,i2,i3) - fptr2(i1,i2,i3))
        endif
        if (ABS(errorfptr(i1,i2,i3)) .gt. 0.001) then
            correct=.false. 
        endif

     enddo
     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
  spherical_grid = 0
!  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
!               "srcmesh", srcArrayA, dstArrayA, errorArray, rc=localrc, &
!               spherical=spherical_grid)
!  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
!               "dstmesh", arrayB, rc=localrc, &
!               spherical=spherical_grid)


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(errorField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

      end subroutine test_regrid3D


      subroutine test_regridDstMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  real(ESMF_KIND_R8) :: B_minx,B_miny
  real(ESMF_KIND_R8) :: B_maxx,B_maxy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  B_nx = 20
  B_ny = 20

  A_nx = 10
  A_ny = 10


  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  
  B_maxx = 10.0
  B_maxy = 10.0
  
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 10.0
  A_maxy = 10.0
  
  ! setup source grid
  gridA=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate Masks
  call ESMF_GridAddItem(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, fptr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        fptrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

        ! set src data
        fptr(i1,i2) = 20.0

	! set mask
	dx=fptrXC(i1,i2)-((A_maxx+A_minx)/2.0)
	dy=fptrYC(i1,i2)-((A_maxy+A_miny)/2.0)
	if (sqrt(dx*dx+dy*dy) < 1.0) then
           maskA(i1,i2) = 2
	else
           maskA(i1,i2) = 0
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
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, fptr=maskB, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        fptrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

	! set mask
	dx=fptrXC(i1,i2)-((B_maxx+B_minx)/2.0)
	dy=fptrYC(i1,i2)-((B_maxy+B_miny)/2.0)
	if (sqrt(dx*dx+dy*dy) < 2.0) then
           maskB(i1,i2) = 3
	else
           maskB(i1,i2) = 0
	endif

        ! initialize destination field
        fptr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, dstMaskValues=(/1,2,3,4/), &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if we missed the correct spots
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, fptr=maskB, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! make sure we used the mask
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	if (maskB(i1,i2) .ne. 0) then	
	   ! if masked out should be 0.0 (but give ourselves room for imprecision)
	   if (fptr(i1,i2) > 2.0) then
             correct=.false.
           endif
	else
	   ! If not masked out should be 20 (but give ourselves room for imprecision)
	   if (fptr(i1,i2) < 18.0) then
             correct=.false.
	    endif
        endif

     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridDstMask



      subroutine test_regridSrcMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  real(ESMF_KIND_R8), pointer :: fptrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  real(ESMF_KIND_R8) :: B_minx,B_miny
  real(ESMF_KIND_R8) :: B_maxx,B_maxy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  B_nx = 20
  B_ny = 20

  A_nx = 11
  A_ny = 11


  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  
  B_maxx = 10.0
  B_maxy = 10.0
  
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 10.0
  A_maxy = 10.0
  
  ! setup source grid
  gridA=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate Masks
  call ESMF_GridAddItem(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! arrayBPatch
  call ESMF_FieldGet(fieldBPatch, array=arrayBPatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, fptr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        fptrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! set mask (circle of radius 2 around center)
        ! and source data based on mask
	dx=fptrXC(i1,i2)-((A_maxx+A_minx)/2.0)
	dy=fptrYC(i1,i2)-((A_maxy+A_miny)/2.0)
	if (sqrt(dx*dx+dy*dy) < 2.0) then
           maskA(i1,i2) = 2
           fptr(i1,i2) = -1000.0 
	else
           maskA(i1,i2) = 0
           fptr(i1,i2) = 20.0 
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
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, fptrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        fptrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination fields
        fptr(i1,i2)=0.0
        fptrPatch(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, &
	  unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

#ifdef ESMF_LAPACK
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldBPatch, &
	  unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandlePatch, &
          regridMethod=ESMF_REGRID_METHOD_PATCH, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldBPatch, routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif


  ! Check if we're using any of the bad source points
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, fptrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working should always be >= 0.0 
        if (fptr(i1,i2) < 0.0) then
           correct=.false.
        endif

#ifdef ESMF_LAPACK
        ! if working should always be >= 0.0 
        if (fptrPatch(i1,i2) < 0.0) then
           correct=.false.
        endif
#endif

     enddo
     enddo

  enddo    ! lDE

  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)

#ifdef ESMF_LAPACK
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, arrayBPatch, rc=localrc, &
               spherical=spherical_grid)
#else
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif



#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldBPatch, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridSrcMask


      subroutine test_regridSphSrcMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Field) :: fieldBPatch
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandlePatch
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  real(ESMF_KIND_R8), pointer :: fptrPatch(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: A_dx, A_dy
  real(ESMF_KIND_R8) :: B_dx, B_dy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

   fieldBPatch = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate Masks
  call ESMF_GridAddItem(gridA, staggerloc=ESMF_STAGGERLOC_CENTER, &
         item=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! arrayBPatch
  call ESMF_FieldGet(fieldBPatch, array=arrayBPatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           item=ESMF_GRIDITEM_MASK, fptr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        fptrXC(i1,i2) = REAL(i1-1)*A_dx
        fptrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

	! set mask region around 180
        ! and source data based on mask
	dx=fptrXC(i1,i2)-180.0
	if (abs(dx) < 45.0) then
           maskA(i1,i2) = 2
           fptr(i1,i2) = -1000.0 
	else
           maskA(i1,i2) = 0
           fptr(i1,i2) = 20.0 
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
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, fptrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        fptrXC(i1,i2) = REAL(i1-1)*B_dx
        fptrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        fptr(i1,i2)=0.0
        fptrPatch(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, &
	  unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

#ifdef ESMF_LAPACK
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldBPatch, &
	  unmappedDstAction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandlePatch, &
          regridMethod=ESMF_REGRID_METHOD_PATCH, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldBPatch, routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandlePatch, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif


  ! Check if we're using any of the bad source points
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, fptrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working should always be >= 0.0 
        if (fptr(i1,i2) < 0.0) then
           correct=.false.
        endif

#ifdef ESMF_LAPACK
        ! if working should always be >= 0.0 
        if (fptrPatch(i1,i2) < 0.0) then
           correct=.false.
        endif
#endif


     enddo
     enddo

  enddo    ! lDE

  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)

#ifdef ESMF_LAPACK
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, arrayBPatch, rc=localrc, &
               spherical=spherical_grid)
#else
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif



#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


   call ESMF_FieldDestroy(fieldBPatch, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridSphSrcMask


      subroutine test_regridCnr(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  real(ESMF_KIND_R8) :: B_minx,B_miny
  real(ESMF_KIND_R8) :: B_maxx,B_maxy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  B_nx = 20
  B_ny = 20

  A_nx = 10
  A_ny = 10


  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  
  B_maxx = 10.0
  B_maxy = 10.0
  
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 10.0
  A_maxy = 10.0
  
  ! setup source grid
  gridA=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CORNER, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        fptrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! func to interpolate
        fptr(i1,i2) = 20.0+fptrXC(i1,i2)+fptrYC(i1,i2)

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        fptrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination field
        fptr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	!! if error is too big report an error
	if (abs(fptr(i1,i2)-(20.0+fptrXC(i1,i2)+fptrYC(i1,i2))) > 0.0001) then
           correct=.false.	
	endif	
     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CORNER, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridCnr



      subroutine test_regridEdge(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB
  type(ESMF_Field) :: srcFieldA
  type(ESMF_Field) :: fieldB
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer A_nx, A_ny, B_nx, B_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: A_minx,A_miny
  real(ESMF_KIND_R8) :: A_maxx,A_maxy
  real(ESMF_KIND_R8) :: B_minx,B_miny
  real(ESMF_KIND_R8) :: B_maxx,B_maxy
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  B_nx = 20
  B_ny = 20

  A_nx = 10
  A_ny = 10


  ! Establish the coordinates of the grids
  B_minx = 0.0
  B_miny = 0.0
  
  B_maxx = 10.0
  B_maxy = 10.0
  
  A_minx = 0.0
  A_miny = 0.0
  
  A_maxx = 10.0
  A_maxy = 10.0
  
  ! setup source grid
  gridA=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcFieldA = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_EDGE1, name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   fieldB = ESMF_FieldCreate(gridB, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Allocate coordinates
  call ESMF_GridAddCoord(gridA, staggerloc=ESMF_STAGGERLOC_EDGE1, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(gridA, localDECount=localDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get arrays
  ! arrayB
  call ESMF_FieldGet(fieldB, array=arrayB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! srcArrayA
  call ESMF_FieldGet(srcFieldA, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Write results to a mesh
  num_arrays = 1

  ! Construct 3D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_EDGE1, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_EDGE1, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, fptr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        fptrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! func to interpolate
        fptr(i1,i2) = 20.0+fptrXC(i1,i2)+fptrYC(i1,i2)

     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        fptrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination field
        fptr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldRegrid(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	!! if error is too big report an error
	if (abs(fptr(i1,i2)-(20.0+fptrXC(i1,i2)+fptrYC(i1,i2))) > 0.0001) then
           correct=.false.	
	endif	
     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", arrayB, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcFieldA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(fieldB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_GridDestroy(gridA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(gridB, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridEdge


end program ESMF_FieldRegridUTest
