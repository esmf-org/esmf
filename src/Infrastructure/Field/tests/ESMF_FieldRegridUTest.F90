! $Id: ESMF_FieldRegridUTest.F90,v 1.8.2.1 2010/02/05 19:56:15 svasquez Exp $
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

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Grid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshToGrid(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Grid to Mesh"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridGridToMesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Mesh"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshToMesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Mesh to Grid 3D"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMeshToGrid3D(rc)

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


 subroutine test_regridMeshToGrid(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:), fptr1D(:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: dst_minx,dst_miny
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy

  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

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

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif

  ! Establish the resolution of the grids
  dst_nx = 10
  dst_ny = 10

  ! Establish the coordinates of the grids
  dst_minx = 0.1
  dst_miny = 0.1
  
  dst_maxx = 1.9
  dst_maxy = 1.9

  ! setup source Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
                  2.0,2.0 /) ! node id 9

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numQuadElems=3
     numTriElems=2
     numTotElems=numQuadElems+numTriElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numQuadElems+3*numTriElems))
     elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,9,8/)   ! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
       numTriElems=0
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, fptr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        fptr1D(i2) = 20.0+x+y

        ! Advance to next owner
        i2=i2+1
     endif
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


  ! setup dest. grid
  dstGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/2,2/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        fptrYC(i1,i2) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny

        ! initialize destination field
        fptr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
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


  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=clbnd, &
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
  call ESMF_MeshIO(vm, srcMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridMeshToGrid


 subroutine test_regridGridToMesh(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:), fptr1D(:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: src_minx,src_miny
  real(ESMF_KIND_R8) :: src_maxx,src_maxy

  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

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

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif

  ! Establish the resolution of the grids
  src_nx = 10
  src_ny = 10

  ! Establish the coordinates of the grids
  src_minx = -0.1
  src_miny = -0.1
  
  src_maxx = 2.1
  src_maxy = 2.1  


  ! setup src grid
  srcGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/2,2/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Source grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(srcGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(srcField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        fptrXC(i1,i2) = ((src_maxx-src_minx)*REAL(i1-1)/REAL(src_nx-1))+src_minx
        fptrYC(i1,i2) = ((src_maxy-src_miny)*REAL(i2-1)/REAL(src_ny-1))+src_miny

        ! initialize destination field
        fptr(i1,i2)=fptrXC(i1,i2)+fptrYC(i1,i2)+20.0

     enddo
     enddo

  enddo    ! lDE



  ! Create Dest Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
                  2.0,2.0 /) ! node id 9

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numQuadElems=3
     numTriElems=2
     numTotElems=numQuadElems+numTriElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numQuadElems+3*numTriElems))
     elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,9,8/)   ! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
       numTriElems=0
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
   endif


  ! Create Mesh structure in 1 step
  dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
     endif

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
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

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, fptr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  fptr1D=0.0


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, fptr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

	!! if error is too big report an error
	if ( abs( fptr1D(i2)-(x+y+20.0) ) > 0.0001) then
           correct=.false.	
	endif	

        ! Advance to next owner
        i2=i2+1
     endif
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)



  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, dstMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(srcGrid, rc=localrc)
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

 end subroutine test_regridGridToMesh



 subroutine test_regridMeshToMesh(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:), fptr1D(:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  real(ESMF_KIND_R8) :: dx,dy
  
  real(ESMF_KIND_R8) :: x,y
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

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

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif


  ! Setup Src Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/-0.1,-0.1, & ! node id 1
                   1.0,-0.1, & ! node id 2
                   2.1,-0.1, & ! node id 3
                  -0.1, 1.0, & ! node id 4
                   1.0, 1.0, & ! node id 5
                   2.1, 1.0, & ! node id 6
                  -0.1, 2.1, & ! node id 7
                   1.0, 2.1, & ! node id 8
                   2.1, 2.1 /) ! node id 9

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numQuadElems=3
     numTriElems=2
     numTotElems=numQuadElems+numTriElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numQuadElems+3*numTriElems))
     elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,9,8/)   ! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/-0.1, -0.1, & ! node id 1
                     1.0, -0.1, & ! node id 2
                    -0.1,  1.0, & ! node id 4
                     1.0,  1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
       numTriElems=0
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,-0.1, & ! node id 2
                    2.1,-0.1, & ! node id 3
                    1.0, 1.0, & ! node id 5
                    2.1, 1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/-0.1,1.0, & ! node id 4
                      1.0,1.0, & ! node id 5
                     -0.1,2.1, & ! node id 7
                      1.0,2.1 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.1,1.0, &  ! node id 6
                     1.0,2.1, &  ! node id 8
                     2.1,2.1 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, fptr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

        ! Set source function
        fptr1D(i2) = 20.0+x+y

        ! Advance to next owner
        i2=i2+1
     endif
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


  ! Create Dest Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 2D Mesh the size is 2x the
     ! number of nodes.
     allocate(nodeCoords(2*numNodes))
     nodeCoords=(/0.0,0.0, & ! node id 1
                  1.0,0.0, & ! node id 2
                  2.0,0.0, & ! node id 3
                  0.0,1.0, & ! node id 4
                  1.0,1.0, & ! node id 5
                  2.0,1.0, & ! node id 6
                  0.0,2.0, & ! node id 7
                  1.0,2.0, & ! node id 8
                  2.0,2.0 /) ! node id 9

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numQuadElems=3
     numTriElems=2
     numTotElems=numQuadElems+numTriElems

     ! Allocate and fill the element id array.
     allocate(elemIds(numTotElems))
     elemIds=(/1,2,3,4,5/) 


     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numTotElems))
     elemTypes=(/ESMF_MESHELEMTYPE_QUAD, & ! elem id 1
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 2
                 ESMF_MESHELEMTYPE_TRI,  & ! elem id 3
                 ESMF_MESHELEMTYPE_QUAD, & ! elem id 4
                 ESMF_MESHELEMTYPE_QUAD/)  ! elem id 5


     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numQuadElems+3*numTriElems))
     elemConn=(/1,2,5,4, &  ! elem id 1
                2,3,5,   &  ! elem id 2
                3,6,5,   &  ! elem id 3
                4,5,8,7, &  ! elem id 4
                5,6,9,8/)   ! elem id 5


 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/0.0,0.0, & ! node id 1
                    1.0,0.0, & ! node id 2
                    0.0,1.0, & ! node id 4
                    1.0,1.0 /) ! node id 5

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 5

       ! Set the number of each type of element, plus the total number.
       numQuadElems=1
       numTriElems=0
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(2*numNodes))
       nodeCoords=(/1.0,0.0, & ! node id 2
                    2.0,0.0, & ! node id 3
                    1.0,1.0, & ! node id 5
                    2.0,1.0 /) ! node id 6

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1/)  ! node id 6

       ! Set the number of each type of element, plus the total number.
       numQuadElems=0
       numTriElems=2
       numTotElems=numQuadElems+numTriElems

       ! Allocate and fill the element id array.
       allocate(elemIds(numTotElems))
       elemIds=(/2,3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numTotElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TRI, & ! elem id 2
                   ESMF_MESHELEMTYPE_TRI/)  ! elem id 3

       ! Allocate and fill the element connection type array.
       allocate(elemConn(4*numQuadElems+3*numTriElems))
       elemConn=(/1,2,3, & ! elem id 2
                  2,4,3/)  ! elem id 3

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/0.0,1.0, & ! node id 4
                     1.0,1.0, & ! node id 5
                     0.0,2.0, & ! node id 7
                     1.0,2.0 /) ! node id 8

        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2/)  ! node id 8

        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/4/) 

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 4

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 4

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 2D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(2*numNodes))
        nodeCoords=(/1.0,1.0, &  ! node id 5
                     2.0,1.0, &  ! node id 6
                     1.0,2.0, &  ! node id 8
                     2.0,2.0 /)  ! node id 9

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3/)  ! node id 9
 
        ! Set the number of each type of element, plus the total number.
        numQuadElems=1
        numTriElems=0
        numTotElems=numQuadElems+numTriElems

        ! Allocate and fill the element id array.
        allocate(elemIds(numTotElems))
        elemIds=(/5/)  

        ! Allocate and fill the element topology type array.
        allocate(elemTypes(numTotElems))
        elemTypes=(/ESMF_MESHELEMTYPE_QUAD/) ! elem id 5

        ! Allocate and fill the element connection type array.
        allocate(elemConn(4*numQuadElems+3*numTriElems))
        elemConn=(/1,2,4,3/) ! elem id 5
       endif
   endif


  ! Create Mesh structure in 1 step
  dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
     endif

  
  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, fptr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  fptr1D=0.0



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
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

  ! Check destination field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, fptr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! loop through nodes and make sure interpolated values are reasonable
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(2*i1-1)
        y=nodeCoords(2*i1)

	!! if error is too big report an error
	if ( abs( fptr1D(i2)-(x+y+20.0) ) > 0.0001) then
           correct=.false.	
	endif	

        ! Advance to next owner
        i2=i2+1
     endif
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)



  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, dstMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_MeshDestroy(dstMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_MeshDestroy(srcMesh, rc=localrc)
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

 end subroutine test_regridMeshToMesh


 subroutine test_regridMeshToGrid3D(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:,:), fptr1D(:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:,:),fptrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:,:),fptr2(:,:,:)
  integer :: clbnd(3),cubnd(3)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(3)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(3)
  character(len=ESMF_MAXSTR) :: string
  integer dst_nx,dst_ny,dst_nz
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy,dz
  
  real(ESMF_KIND_R8) :: dst_minx,dst_miny,dst_minz
  real(ESMF_KIND_R8) :: dst_maxx,dst_maxy,dst_maxz

  real(ESMF_KIND_R8) :: x,y,z
  
  integer :: spherical_grid

  integer, pointer :: larrayList(:)
  integer :: localPet, petCount

  integer, pointer :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), pointer :: nodeCoords(:)
  integer, pointer :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems

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

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif


  ! Establish the resolution of the grids
  dst_nx = 10
  dst_ny = 10
  dst_nz = 5

  ! Establish the coordinates of the grids
  dst_minx = 0.1
  dst_miny = 0.1
  dst_minz = 0.1
  
  dst_maxx = 1.9
  dst_maxy = 1.9
  dst_maxz = 0.9
  
  ! setup source Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=18

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9, &
               10,11,12,13,14,15,16,17,18/) 

     ! Allocate and fill node coordinate array.
     ! Since this is a 3D Mesh the size is 3x the
     ! number of nodes.
     allocate(nodeCoords(3*numNodes))
     nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                  1.0,0.0,0.0, & ! node id 2
                  2.0,0.0,0.0, & ! node id 3
                  0.0,1.0,0.0, & ! node id 4
                  1.0,1.0,0.0, & ! node id 5
                  2.0,1.0,0.0, & ! node id 6
                  0.0,2.0,0.0, & ! node id 7
                  1.0,2.0,0.0, & ! node id 8
                  2.0,2.0,0.0, & ! node id 9
                  0.0,0.0,1.0, & ! node id 10
                  1.0,0.0,1.0, & ! node id 11
                  2.0,0.0,1.0, & ! node id 12
                  0.0,1.0,1.0, & ! node id 13
                  1.0,1.0,1.0, & ! node id 14
                  2.0,1.0,1.0, & ! node id 15
                  0.0,2.0,1.0, & ! node id 16
                  1.0,2.0,1.0, & ! node id 17
                  2.0,2.0,1.0 /) ! node id 18

     ! Allocate and fill the node owner array.
     ! Since this Mesh is all on PET 0, it's just set to all 0.
     allocate(nodeOwners(numNodes))
     nodeOwners=0 ! everything on PET 0

     ! Set the number of each type of element, plus the total number.
     numElems=4

     ! Allocate and fill the element id array.
     allocate(elemIds(numElems))
     elemIds=(/1,2,3,4/) 

     ! Allocate and fill the element topology type array.
     allocate(elemTypes(numElems))
     elemTypes=ESMF_MESHELEMTYPE_HEX
                 

     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(8*numElems))
     elemConn=(/1,2,5,4,10,11,14,13, &  ! elem id 1
                2,3,6,5,11,12,15,14, &  ! elem id 2
                4,5,8,7,13,14,17,16,   &  ! elem id 3
                5,6,9,8,14,15,18,17/)  ! elem id 4

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=8

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,5,10,11,13,14/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                    1.0,0.0,0.0, & ! node id 2
                    0.0,1.0,0.0, & ! node id 4
                    1.0,1.0,0.0, & ! node id 5
                    0.0,0.0,1.0, & ! node id 10
                    1.0,0.0,1.0, & ! node id 11
                    0.0,1.0,1.0, & ! node id 13
                    1.0,1.0,1.0 /) ! node id 14


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0, & ! node id 5
                    0, & ! node id 10
                    0, & ! node id 11
                    0, & ! node id 13
                    0/)  ! node id 14

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=8

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,6,11,12,14,15/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 3D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                    2.0,0.0,0.0, & ! node id 3
                    1.0,1.0,0.0, & ! node id 5
                    2.0,1.0,0.0, & ! node id 6
                    1.0,0.0,1.0, & ! node id 11
                    2.0,0.0,1.0, & ! node id 12
                    1.0,1.0,1.0, & ! node id 14
                    2.0,1.0,1.0/) ! node id 15

       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    0, & ! node id 5
                    1, & ! node id 6
                    0, & ! node id 11
                    1, & ! node id 12
                    0, & ! node id 14
                    1/)  ! node id 15

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=8

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,7,8,13,14,16,17/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/0.0,1.0,0.0, & ! node id 4
                     1.0,1.0,0.0, & ! node id 5
                     0.0,2.0,0.0, & ! node id 7
                     1.0,2.0,0.0, & ! node id 8
                     0.0,1.0,1.0, & ! node id 13
                     1.0,1.0,1.0, & ! node id 14
                     0.0,2.0,1.0, & ! node id 16
                     1.0,2.0,1.0/) ! node id 17


        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     0, & ! node id 5
                     2, & ! node id 7
                     2, & ! node id 8
                     0, & ! node id 13
                     0, & ! node id 14
                     2, & ! node id 16
                     2/)  ! node id 17
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1


     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=8

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/5,6,8,9,14,15,17,18/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/1.0,1.0,0.0, &  ! node id 5
                     2.0,1.0,0.0, &  ! node id 6
                     1.0,2.0,0.0, &  ! node id 8
                     2.0,2.0,0.0, &  ! node id 9
                     1.0,1.0,1.0, &  ! node id 14
                     2.0,1.0,1.0, &  ! node id 15
                     1.0,2.0,1.0, &  ! node id 17
                     2.0,2.0,1.0/)  ! node id 18

        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 5
                     1, & ! node id 6
                     2, & ! node id 8
                     3, & ! node id 9
                     0, & ! node id 14
                     1, & ! node id 15
                     2, & ! node id 17
                     3/)  ! node id 18
 
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_HEX/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(8*numElems))
       elemConn=(/1,2,4,3,5,6,8,7/) ! elem id 1

       endif
    endif

   ! Create Mesh structure in 1 step
   srcMesh=ESMF_MeshCreate(parametricDim=3,spatialDim=3, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create source field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, fptr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(3*i1-2)
        y=nodeCoords(3*i1-1)
        z=nodeCoords(3*i1)

        ! Set source function
        fptr1D(i2) = 20.0+x+y+z

        ! Advance to next owner
        i2=i2+1
     endif
  enddo


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)



  ! setup dest. grid
  dstGrid=ESMF_GridCreateShapeTile(minIndex=(/1,1,1/),maxIndex=(/dst_nx,dst_ny,dst_nz/), &
            regDecomp=(/2,2,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc)

   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER_VCENTER, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=localDECount, rc=localrc)
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


  ! srcArrayA
  call ESMF_FieldGet(srcField, array=srcArrayA, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=fclbnd, &
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
        fptrXC(i1,i2,i3) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        fptrYC(i1,i2,i3) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny
        fptrZC(i1,i2,i3) = ((dst_maxz-dst_minz)*REAL(i3-1)/REAL(dst_nz-1))+dst_minz

        ! initialize destination field
        fptr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          rc=localrc)
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

  ! Check error
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            fptr=fptrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            fptr=fptrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            fptr=fptrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif
   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

	!! if error is too big report an error
	if (abs(fptr(i1,i2,i3)-(20.0+fptrXC(i1,i2,i3)+fptrYC(i1,i2,i3)+ &
                                     fptrZC(i1,i2,i3))) > 0.0001) then
           correct=.false.	
	endif	
     enddo
     enddo
     enddo

  enddo    ! lDE


  ! Uncomment these calls to see some actual regrid results
#if 0
  spherical_grid = 0
  call ESMF_MeshIO(vm, srcMesh, ESMF_STAGGERLOC_EDGE1, &
               "srcmesh", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif

  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_GridDestroy(dstGrid, rc=localrc)
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

 end subroutine test_regridMeshToGrid3D



end program ESMF_FieldRegridUTest
