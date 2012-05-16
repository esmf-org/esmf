! $Id: ESMF_FieldRegridUTest.F90,v 1.48 2012/05/16 22:33:56 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2012, University Corporation for Atmospheric Research,
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
    use ESMF
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
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
 
#ifdef ESMF_TESTEXHAUSTIVE
#if 1
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
      ! Test regridding with a field with extra dimensions
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid between two Fields with ungridded dimensions"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridExtraFieldDim(rc)

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
#if 0
      !------------------------------------------------------------------------

      !EX_OFF_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Regrid from Tetrahedral Mesh to 3D Grid"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridTetMeshToGrid3D(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
#endif

      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding Sphere with ESMF_POLEMETHOD_NONE"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphPoleNone(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding Sphere with ESMF_POLEMETHOD_ALLAVG"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphPoleAllAvg(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding Sphere with ESMF_POLEMETHOD_NPNTAVG"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphPoleNpntAvg(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------

      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding Sphere with ESMF_POLEMETHOD_TEETH"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphPoleTeeth(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with distgrid specified sphere
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding from distgrid connections"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridDGSph(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------


      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test using matrix (factorList, factorIndexList) " // &
                     "from ESMF_FieldRegridStore() in ESMF_FieldSMMStore()"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMatrixFactor(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------



      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test using DEPRECATED matrix arguments (weights, indices) " // &
                     "from ESMF_FieldRegridStore() in ESMF_FieldSMMStore()"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridMatrix(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding on a grid with indexflag=ESMF_INDEX_DELOCAL"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridDELOCAL(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------



      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid matrix
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Test regridding on a grid with an irregular distribution"

      ! initialize 
      rc=ESMF_SUCCESS
      
      ! do test
      call test_regridIrreg(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:),errorfarrayPtr(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids

  dst_nx = 90
  dst_ny = 50

  src_nx = 90
  src_ny = 50

  ! setup source grid
  grid360=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                  coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
!                                  poleType=(/ESMF_POLETYPE_MONOPOLE,ESMF_POLETYPE_BIPOLE/), & 
                                  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  grid180=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(grid360, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcField360, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field pointer
     call ESMF_FieldGet(dstField360, lDE, farrayPtr2,   rc=localrc)
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
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        ! (something relatively smooth, that varies everywhere)
        farrayPtr(i1,i2) = x+y+z+15.0

         ! initialize src destination field
         farrayPtr2(i1,i2)=0.0

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetCoord(grid180, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(field180, lDE, farrayPtr, computationalLBound=fclbnd, &
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
        farrayPtrXC(i1,i2) = -180. + (REAL(i1-1)*dst_dx)
        farrayPtrYC(i1,i2) = -90.  + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

        ! init destination mesh to 0
        farrayPtr(i1,i2) = 0.
     
     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the 0 to 360 grid to the -180 to 180 grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcField360, dstField=field180, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
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
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
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
     call ESMF_FieldGet(srcField360, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field
     call ESMF_FieldGet(dstField360, lDE, farrayPtr2,  rc=localrc)
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


     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (farrayPtr(i1,i2) .ne. 0.0) then
           errorfarrayPtr(i1,i2)=ABS((farrayPtr(i1,i2) - farrayPtr2(i1,i2))/farrayPtr(i1,i2))
        else
           errorfarrayPtr(i1,i2)=(farrayPtr(i1,i2) - farrayPtr2(i1,i2))
        endif
        if (ABS(errorfarrayPtr(i1,i2)) .gt. 0.01) then
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
write(*,*) "LOCALRC=",localrc
  call ESMF_MeshIO(vm, grid180, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", array180, rc=localrc, &
               spherical=spherical_grid)
write(*,*) "LOCALRC=",localrc
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:),farrayPtr2(:,:,:),errorfarrayPtr(:,:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/A_nx,A_ny,A_nz/),regDecomp=(/petCount,1,1/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/B_nx,B_ny,B_nz/),regDecomp=(/1,1,petCount/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src destination Field pointer
     call ESMF_FieldGet(dstFieldA, lDE, farrayPtr2,   rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        ! Set source coordinates
        farrayPtrXC(i1,i2,i3) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2,i3) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny
        farrayPtrZC(i1,i2,i3) = ((A_maxz-A_minz)*REAL(i3-1)/REAL(A_nz-1))+A_minz

        ! set src data
        ! (something  smooth, that varies everywhere)
        farrayPtr(i1,i2,i3) = farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+farrayPtrZC(i1,i2,i3)+15.0

        ! initialize src destination field
        farrayPtr2(i1,i2,i3)=0.0

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
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
        farrayPtrXC(i1,i2,i3) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2,i3) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny
        farrayPtrZC(i1,i2,i3) = ((B_maxz-B_minz)*REAL(i3-1)/REAL(B_nz-1))+B_minz

        ! initialize destination field
        farrayPtr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcFieldA, dstField=fieldB, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src destination Field
     call ESMF_FieldGet(dstFieldA, lDE, farrayPtr2,  rc=localrc)
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


     !! check relative error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)
        if (farrayPtr(i1,i2,i3) .ne. 0.0) then
           errorfarrayPtr(i1,i2,i3)=ABS((farrayPtr(i1,i2,i3) - farrayPtr2(i1,i2,i3))/farrayPtr(i1,i2,i3))
        else
           errorfarrayPtr(i1,i2,i3)=(farrayPtr(i1,i2,i3) - farrayPtr2(i1,i2,i3))
        endif
        if (ABS(errorfarrayPtr(i1,i2,i3)) .gt. 0.001) then
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_CART,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

        ! set src data
        farrayPtr(i1,i2) = 20.0

	! set mask
	dx=farrayPtrXC(i1,i2)-((A_maxx+A_minx)/2.0)
	dy=farrayPtrYC(i1,i2)-((A_maxy+A_miny)/2.0)
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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskB, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

	! set mask
	dx=farrayPtrXC(i1,i2)-((B_maxx+B_minx)/2.0)
	dy=farrayPtrYC(i1,i2)-((B_maxy+B_miny)/2.0)
	if (sqrt(dx*dx+dy*dy) < 2.0) then
           maskB(i1,i2) = 3
	else
           maskB(i1,i2) = 0
	endif

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, dstMaskValues=(/1,2,3,4/), &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskB, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! make sure we used the mask
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	if (maskB(i1,i2) .ne. 0) then	
	   ! if masked out should be 0.0 (but give ourselves room for imprecision)
	   if (farrayPtr(i1,i2) > 2.0) then
             correct=.false.
           endif
	else
	   ! If not masked out should be 20 (but give ourselves room for imprecision)
	   if (farrayPtr(i1,i2) < 18.0) then
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_GridAddItem(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! set mask (circle of radius 2 around center)
        ! and source data based on mask
	dx=farrayPtrXC(i1,i2)-((A_maxx+A_minx)/2.0)
	dy=farrayPtrYC(i1,i2)-((A_maxy+A_miny)/2.0)
	if (sqrt(dx*dx+dy*dy) < 2.0) then
           maskA(i1,i2) = 2
           farrayPtr(i1,i2) = -1000.0 
	else
           maskA(i1,i2) = 0
           farrayPtr(i1,i2) = 20.0 
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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination fields
        farrayPtr(i1,i2)=0.0
        farrayPtrPatch(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandlePatch, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
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

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working should always be >= 0.0 
        if (farrayPtr(i1,i2) < 0.0) then
           correct=.false.
        endif

#ifdef ESMF_LAPACK
        ! if working should always be >= 0.0 
        if (farrayPtrPatch(i1,i2) < 0.0) then
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_GridGetItem(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskA, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

	! set mask region around 180
        ! and source data based on mask
	dx=farrayPtrXC(i1,i2)-180.0
	if (abs(dx) < 45.0) then
           maskA(i1,i2) = 2
           farrayPtr(i1,i2) = -1000.0 
	else
           maskA(i1,i2) = 0
           farrayPtr(i1,i2) = 20.0 
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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0
        farrayPtrPatch(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, srcMaskValues=(/1,2/), &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandlePatch, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
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

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working should always be >= 0.0 
        if (farrayPtr(i1,i2) < 0.0) then
           correct=.false.
        endif

#ifdef ESMF_LAPACK
        ! if working should always be >= 0.0 
        if (farrayPtrPatch(i1,i2) < 0.0) then
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


 subroutine test_regridSphDstMask(rc)
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
  integer(ESMF_KIND_I4), pointer :: maskB(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                              coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
  call ESMF_GridAddItem(gridB, staggerloc=ESMF_STAGGERLOC_CENTER, &
         itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Set source data
        farrayPtr(i1,i2) = 20.0 

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetItem(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                           itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskB, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	! set mask region around 180
        ! and source data based on mask
	dx=farrayPtrXC(i1,i2)-180.0
	if (abs(dx) < 45.0) then
           maskB(i1,i2) = 2
           farrayPtr(i1,i2) = -1000.0 
	else
           maskB(i1,i2) = 0
           farrayPtr(i1,i2) = 0.0 
	endif

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0
        farrayPtrPatch(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore(srcFieldA, &
          dstField=fieldB, dstMaskValues=(/2/), &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
  call ESMF_FieldRegridStore(srcFieldA, &
          dstField=fieldBPatch, dstMaskValues=(/2/), &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandlePatch, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
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

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(fieldBPatch, lDE, farrayPtrPatch,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working mask values should be unchanged
        if (maskB(i1,i2) == 2) then
          if (farrayPtr(i1,i2) /= -1000) then
            correct=.false.
          endif
        endif

#ifdef ESMF_LAPACK
        ! if working should be unchanged
        if (maskB(i1,i2) == 2) then
          if (farrayPtrPatch(i1,i2) /= -1000) then
             correct=.false.
          endif
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

 end subroutine test_regridSphDstMask


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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  B_nx = 30
  B_ny = 30

  A_nx = 20
  A_ny = 20


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
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_CART,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CORNER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! func to interpolate
        farrayPtr(i1,i2) = 20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2)-(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))) > 0.0001) then
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  gridA=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_EDGE1, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((A_maxx-A_minx)*REAL(i1-1)/REAL(A_nx-1))+A_minx
        farrayPtrYC(i1,i2) = ((A_maxy-A_miny)*REAL(i2-1)/REAL(A_ny-1))+A_miny

	! func to interpolate
        farrayPtr(i1,i2) = 20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((B_maxx-B_minx)*REAL(i1-1)/REAL(B_nx-1))+B_minx
        farrayPtrYC(i1,i2) = ((B_maxy-B_miny)*REAL(i2-1)/REAL(B_ny-1))+B_miny

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2)-(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))) > 0.0001) then
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
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
        farrayPtr1D(i2) = 20.0+x+y

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
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/2,2/), &
                                  coordSys=ESMF_COORDSYS_CART,indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2)-(20.0+farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2))) > 0.0001) then
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  srcGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/2,2/), &
                                coordSys=ESMF_COORDSYS_CART, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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


     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates
        farrayPtrXC(i1,i2) = ((src_maxx-src_minx)*REAL(i1-1)/REAL(src_nx-1))+src_minx
        farrayPtrYC(i1,i2) = ((src_maxy-src_miny)*REAL(i2-1)/REAL(src_ny-1))+src_miny

        ! initialize destination field
        farrayPtr(i1,i2)=farrayPtrXC(i1,i2)+farrayPtrYC(i1,i2)+20.0

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
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

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
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
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
	if ( abs( farrayPtr1D(i2)-(x+y+20.0) ) > 0.0001) then
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
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
        farrayPtr1D(i2) = 20.0+x+y

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
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! clear destination Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  farrayPtr1D=0.0



  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
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
	if ( abs( farrayPtr1D(i2)-(x+y+20.0) ) > 0.0001) then
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:),farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:),farrayPtr2(:,:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

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
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
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
        farrayPtr1D(i2) = 20.0+x+y+z

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
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/dst_nx,dst_ny,dst_nz/), &
              coordSys=ESMF_COORDSYS_CART, regDecomp=(/2,2,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
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
        farrayPtrXC(i1,i2,i3) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2,i3) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny
        farrayPtrZC(i1,i2,i3) = ((dst_maxz-dst_minz)*REAL(i3-1)/REAL(dst_nz-1))+dst_minz

        ! initialize destination field
        farrayPtr(i1,i2,i3)=0.0

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
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
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
	if (abs(farrayPtr(i1,i2,i3)-(20.0+farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+ &
                                     farrayPtrZC(i1,i2,i3))) > 0.0001) then
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


      subroutine test_regridSphPoleNone(rc)
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  !! THESE RESOLUTIONS ARE SETUP SO THE DEST GRID (GRID B) HAS
  !! ONE ROW ABOVE THE TOP ROW OF THE SOURCE GRID (GRID A) AND
  !! ONE ROW BELOW THE BOTTOM ROW OF THE SOURCE GRID.
  !! THIS ALLOWS THE POLE INFO TO BE MORE EASILY CHECKED.

  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=ESMF_POLEMETHOD_NONE, &
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


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working the top and bottom dest rows should be close to 0.0
        if ((i2 .eq. 1) .or. (i2 .eq. B_ny)) then
           if (farrayPtr(i1,i2) .gt. 0.000001) then
              correct=.false.
           endif
        else ! otherwise they should be close to 20
           if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
              correct=.false.
           endif
	endif
     enddo
     enddo

  enddo    ! lDE


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

 end subroutine test_regridSphPoleNone


      subroutine test_regridSphPoleAllAvg(rc)
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  !! THESE RESOLUTIONS ARE SETUP SO THE DEST GRID (GRID B) HAS
  !! ONE ROW ABOVE THE TOP ROW OF THE SOURCE GRID (GRID A) AND
  !! ONE ROW BELOW THE BOTTOM ROW OF THE SOURCE GRID.
  !! THIS ALLOWS THE POLE INFO TO BE MORE EASILY CHECKED.

  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=ESMF_POLEMETHOD_ALLAVG, &
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


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
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

 end subroutine test_regridSphPoleAllAvg



      subroutine test_regridSphPoleNpntAvg(rc)
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  !! THESE RESOLUTIONS ARE SETUP SO THE DEST GRID (GRID B) HAS
  !! ONE ROW ABOVE THE TOP ROW OF THE SOURCE GRID (GRID A) AND
  !! ONE ROW BELOW THE BOTTOM ROW OF THE SOURCE GRID.
  !! THIS ALLOWS THE POLE INFO TO BE MORE EASILY CHECKED.

  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=ESMF_POLEMETHOD_NPNTAVG, &
          regridPoleNPnts=4, &
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


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
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

 end subroutine test_regridSphPoleNpntAvg



      subroutine test_regridSphPoleTeeth(rc)
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  !! THESE RESOLUTIONS ARE SETUP SO THE DEST GRID (GRID B) HAS
  !! ONE ROW ABOVE THE TOP ROW OF THE SOURCE GRID (GRID A) AND
  !! ONE ROW BELOW THE BOTTOM ROW OF THE SOURCE GRID.
  !! THIS ALLOWS THE POLE INFO TO BE MORE EASILY CHECKED.

  A_nx = 10
  A_ny = 10

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 21
  B_ny = 21

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          polemethod=ESMF_POLEMETHOD_TEETH, &
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


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
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

 end subroutine test_regridSphPoleTeeth



 subroutine test_regridDGSph(rc)
   integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  type(ESMF_DistGrid) :: srcDistgrid
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD
  
  integer :: spherical_grid

  integer :: localPet, petCount

  type(ESMF_DistGridConnection) :: connectionList(3) ! 3 connections


  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  src_nx = 20
  src_ny = 20

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 80
  dst_ny = 80

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! Create connectionList
  ! periodicity
  call ESMF_DistgridConnectionSet(connection=connectionList(1), &
        tileIndexA=1,tileIndexB=1, &
        positionVector=(/src_nx,0/), &
        orientationVector=(/1,2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  call ESMF_DistgridConnectionSet(connection=connectionList(2), &
        tileIndexA=1,tileIndexB=1, &
        positionVector=(/src_nx+1,2*src_ny+1/), &
        orientationVector=(/-1,-2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  call ESMF_DistgridConnectionSet(connection=connectionList(3), &
        tileIndexA=1,tileIndexB=1, &
        positionVector=(/src_nx/2,1/), &
        orientationVector=(/1,-2/), rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source distgrid
  srcDistgrid=ESMF_DistgridCreate(minIndex=(/1,1/), maxIndex=(/src_nx,src_ny/), regDecomp=(/petCount,1/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              connectionList=connectionList,                 &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup source grid
  srcGrid=ESMF_GridCreate(distgrid=srcDistgrid, indexflag=ESMF_INDEX_GLOBAL, &
                          coordSys=ESMF_COORDSYS_SPH_DEG, &
                          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
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

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact src data
        !farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        farrayPtr(i1,i2) = 1.0
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)
  
        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        xfarrayPtr(i1,i2) = 1.0

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.false., isLatLonDeg=.true., filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check results
  do lDE=0,localDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2)) .gt. 0.001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, srcGrid, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArray, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, dstGrid, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", dstArray, rc=localrc, &
               spherical=spherical_grid)
#endif


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.false., isLatLonDeg=.true., filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.true., isLatLonDeg=.true., filename="dstGrid", array1=dstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
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
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#if 0
  ! Free the srcDistgrid
  call ESMF_DistgridDestroy(srcDistgrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif

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

 end subroutine test_regridDGSph



      subroutine test_regridMatrixFactor(rc)
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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

  integer(ESMF_KIND_I4), pointer:: indices(:,:)
  real(ESMF_KIND_R8), pointer :: weights(:)
  
  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  A_nx = 21
  A_ny = 21

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 10
  B_ny = 10

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          factorIndexList=indices, factorList=weights, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do SMM
  call ESMF_FieldSMMStore(srcFieldA, fieldB, routeHandle=routeHandle, &
         factorList=weights, factorIndexList=indices, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldSMM(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldSMMRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
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

  ! get rid of matrix
  deallocate(weights)
  deallocate(indices)


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridMatrixFactor


      subroutine test_regridMatrix(rc)
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrPatch(:,:)
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

  integer(ESMF_KIND_I4), pointer:: indices(:,:)
  real(ESMF_KIND_R8), pointer :: weights(:)
  
  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  A_nx = 21
  A_ny = 21

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 10
  B_ny = 10

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny

  
  ! setup source grid
  gridA=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridA, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     ! get src pointer
     call ESMF_FieldGet(srcFieldA, lDE, farrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*A_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! Init source value
        farrayPtr(i1,i2) = 20.0  

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
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(gridB, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
  
        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  ! NOTE THAT THE FOLLOWING METHOD USES ARGUMENTS THAT ARE DEPRECATED. 
  ! Please use (factorList, factorIndexList) instead of (weights, indices)  
  ! See test_RegridMatrixFactor() for an example of their use. 
  call ESMF_FieldRegridStore( &
	  srcFieldA, &
          dstField=fieldB, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          weights=weights, indices=indices,          & ! DEPRECATED
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR,   &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do SMM
  call ESMF_FieldSMMStore(srcFieldA, fieldB, routeHandle=routeHandle, &
         factorList=weights, factorIndexList=indices, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldSMM(srcFieldA, fieldB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldSMMRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check if Pole is actually none
  do lDE=0,localDECount-1

     call ESMF_FieldGet(fieldB, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-20.0) .gt. 0.000001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  spherical_grid = 1
  call ESMF_MeshIO(vm, gridA, ESMF_STAGGERLOC_CENTER, &
               "srcgrid", srcArrayA, rc=localrc, &
               spherical=spherical_grid)
  call ESMF_MeshIO(vm, gridB, ESMF_STAGGERLOC_CENTER, &
               "dstgrid", arrayB, rc=localrc, &
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

  ! get rid of matrix
  deallocate(weights)
  deallocate(indices)


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridMatrix



      subroutine test_regridDELOCAL(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_Array) :: tmpArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)

  real(ESMF_KIND_R8), pointer :: x_coord(:,:)
  real(ESMF_KIND_R8), pointer :: y_coord(:,:)
  real(ESMF_KIND_R8), pointer :: data(:,:)
  real(ESMF_KIND_R8), pointer :: xdata(:,:)

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
  real(ESMF_KIND_R8) :: DEG2RAD, lat, lon, theta, phi
  real(ESMF_KIND_R8) :: rel_error  
  real(ESMF_KIND_R8) :: max_rel_error  

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
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

  ! Establish the resolution of the grids
  A_nx = 183
  A_ny = 95

  A_dx=360.0/A_nx
  A_dy=180.0/A_ny

  B_nx = 10
  B_ny = 10

  B_dx=360.0/B_nx
  B_dy=180.0/B_ny


  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  
  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/A_nx,A_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_DELOCAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_DELOCAL, &
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


  ! Allocate coordinates
  call ESMF_GridAddCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! Fill src Coords
  allocate(x_coord(A_nx,A_ny))
  allocate(y_coord(A_nx,A_ny))
  allocate(data(A_nx,A_ny))

  ! set coords, interpolated function
  do i1=1,A_nx
     do i2=1,A_ny
        
        ! Set source coordinates as 0 to 360
        x_coord(i1,i2) = REAL(i1-1)*A_dx
        y_coord(i1,i2) = -90. + (REAL(i2-1)*A_dy + 0.5*A_dy)

        ! set src data 
        lon = x_coord(i1,i2)
        lat = y_coord(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set src data
        data(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
  enddo


  ! Scatter X coords
  call ESMF_GridGetCoord(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=x_coord, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! Scatter Y coords
  call ESMF_GridGetCoord(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                         array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=y_coord, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! Scatter data
  call ESMF_FieldGet(srcField, array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=data, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! Deallocate tmp arrays
  deallocate(x_coord)
  deallocate(y_coord)
  deallocate(data)


  ! Fill dst Coords
  allocate(x_coord(B_nx,B_ny))
  allocate(y_coord(B_nx,B_ny))
  allocate(data(B_nx,B_ny))
  allocate(xdata(B_nx,B_ny))

  ! set coords, interpolated function
  do i1=1,B_nx
     do i2=1,B_ny
        
        ! Set source coordinates as 0 to 360
        x_coord(i1,i2) = REAL(i1-1)*B_dx
        y_coord(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)

        ! set src data 
        lon = x_coord(i1,i2)
        lat = y_coord(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! Set exact value
        xdata(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! Init dst data
        data(i1,i2) = 0.0
     enddo
  enddo


  ! Scatter X coords
  call ESMF_GridGetCoord(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                         array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=x_coord, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! Scatter Y coords
  call ESMF_GridGetCoord(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                         array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=y_coord, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! Scatter xdata
  call ESMF_FieldGet(xdstField, array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=xdata, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  ! Scatter data
  call ESMF_FieldGet(dstField, array=tmpArray, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_ArrayScatter(tmpArray, farray=data, rootPet=0, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  ! Deallocate tmp arrays
  deallocate(x_coord)
  deallocate(y_coord)
  deallocate(data)
  deallocate(xdata)


#if 0
  ! Output Mesh
  call ESMF_GridWriteVTK(srcGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, filename="srcGrid", &
                         rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Output Mesh
  call ESMF_GridWriteVTK(dstGrid, staggerLoc=ESMF_STAGGERLOC_CENTER, filename="dstGrid", &
                         rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routehandle=routehandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check interpolation
  max_rel_error=0.0
  do lDE=0,localDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           rel_error=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2)
        else
           rel_error=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        if (rel_error > max_rel_error) max_rel_error=rel_error

        if (rel_error .gt. 0.001) then
           write(*,*) i1,i2," ",farrayPtr(i1,i2),xfarrayPtr(i1,i2)
           correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


! For Debugging
!  write(*,*) "Max_rel_error=",max_rel_error


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


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridDELOCAL


 subroutine test_regridIrreg(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays
  real(ESMF_KIND_R8) :: dx,dy

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy

  real(ESMF_KIND_R8) :: lon, lat, theta, phi, DEG2RAD
  
  integer :: spherical_grid

  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  src_nx = 25
  src_ny = 30

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 80
  dst_ny = 80

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! degree to rad conversion
  DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8

  ! setup dest. grid
  srcGrid=ESMF_GridCreate1PeriDim(countsPerDeDim1=(/10,15/), countsPerDeDim2=(/14,16/), &
                                  indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                  indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   srcField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
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
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif



  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
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

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx

        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)
!        farrayPtrYC(i1,i2) = -90. + REAL(i2-1)*src_dy 

        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact src data
        !farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        farrayPtr(i1,i2) = 1.0
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     !! get coords
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


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)
  
        ! init exact answer
        lon = farrayPtrXC(i1,i2)
        lat = farrayPtrYC(i1,i2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        ! set exact dst data
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        xfarrayPtr(i1,i2) = 1.0

        ! initialize destination field
        farrayPtr(i1,i2)=0.0

     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


  ! Check results
  do lDE=0,dstlocalDECount-1

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        ! if working everything should be really close to 20.0
        if (abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2)) .gt. 0.001) then
            correct=.false.
	endif
     enddo
     enddo

  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.false., isLatLonDeg=.true., filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       isSphere=.true., isLatLonDeg=.true., filename="dstGrid", array1=dstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
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
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#if 0
  ! Free the srcDistgrid
  call ESMF_DistgridDestroy(srcDistgrid, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif
#endif

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

 end subroutine test_regridIrreg



 subroutine test_regridTetMeshToGrid3D(rc)
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:,:),farrayPtrZC(:,:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:,:),farrayPtr2(:,:,:)
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
  logical :: at_least_one_interp

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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

  ! If we don't have 1 or 4 PETS then exit successfully
  if ((petCount .ne. 1) .and. (petCount .ne. 4)) then
    rc=ESMF_SUCCESS
    return
  endif

  
  ! setup source Mesh
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=10

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/1,2,3,4,5,6,7,8,9, &
               10/)

     ! Allocate and fill node coordinate array.
     ! Since this is a 3D Mesh the size is 3x the
     ! number of nodes.
     allocate(nodeCoords(3*numNodes))
     nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                  1.0,0.0,0.0, & ! node id 2
                  2.0,0.0,0.0, & ! node id 3
                  0.5,1.0,0.0, & ! node id 4
                  1.5,1.0,0.0, & ! node id 5
                  1.0,2.0,0.0, & ! node id 6
                  0.5,0.5,1.0, & ! node id 7
                  1.0,0.5,1.0, & ! node id 8
                  1.5,0.5,1.0, & ! node id 9
                  1.0,1.5,1.0/)  ! node id 10

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
     elemTypes=ESMF_MESHELEMTYPE_TETRA
                 

     ! Allocate and fill the element connection type array.
     ! Note that entries in this array refer to the 
     ! positions in the nodeIds, etc. arrays and that
     ! the order and number of entries for each element
     ! reflects that given in the Mesh options 
     ! section for the corresponding entry
     ! in the elemTypes array.
     allocate(elemConn(4*numElems))
     elemConn=(/1,2,7,4, &  ! elem id 1
                2,3,9,5, &  ! elem id 2
                2,5,8,4, &  ! elem id 3
                4,5,10,6/)  ! elem id 4

 else if (petCount .eq. 4) then
     ! Setup mesh data depending on PET
    if (localPET .eq. 0) then !!! This part only for PET 0
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/1,2,4,7/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 2D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes)) 
       nodeCoords=(/0.0,0.0,0.0, & ! node id 1
                    1.0,0.0,0.0, & ! node id 2
                    0.5,1.0,0.0, & ! node id 4
                    0.5,0.5,1.0/) ! node id 7
 


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 1
                    0, & ! node id 2
                    0, & ! node id 4
                    0/)  ! node id 7

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/1/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 1

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 1

     else if (localPET .eq. 1) then !!! This part only for PET 1
       ! Set number of nodes
       numNodes=4

       ! Allocate and fill the node id array.
       allocate(nodeIds(numNodes))
       nodeIds=(/2,3,5,9/) 

       ! Allocate and fill node coordinate array.
       ! Since this is a 3D Mesh the size is 2x the
       ! number of nodes.
       allocate(nodeCoords(3*numNodes))
       nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                    2.0,0.0,0.0, & ! node id 3
                    1.5,1.0,0.0, & ! node id 5
                    1.5,0.5,1.0/) ! node id 9


       ! Allocate and fill the node owner array.
       allocate(nodeOwners(numNodes))
       nodeOwners=(/0, & ! node id 2
                    1, & ! node id 3
                    2, & ! node id 5
                    1/)  ! node id 9

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/2/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 2

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 2

    else if (localPET .eq. 2) then !!! This part only for PET 2
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/2,4,5,8/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/1.0,0.0,0.0, & ! node id 2
                  0.5,1.0,0.0, & ! node id 4
                  1.5,1.0,0.0, & ! node id 5
                  1.0,0.5,1.0/) ! node id 8



        ! Allocate and fill the node owner array.
        ! Since this Mesh is all on PET 0, it's just set to all 0.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 2
                     0, & ! node id 4
                     2, & ! node id 5
                     2/)  ! node id 8

       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/3/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 3

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,3,4,2/) ! elem id 3

     else if (localPET .eq. 3) then !!! This part only for PET 3
        ! Set number of nodes
        numNodes=4

        ! Allocate and fill the node id array.
        allocate(nodeIds(numNodes))
        nodeIds=(/4,5,6,10/) 

        ! Allocate and fill node coordinate array.
        ! Since this is a 3D Mesh the size is 2x the
        ! number of nodes.
        allocate(nodeCoords(3*numNodes))
        nodeCoords=(/0.5,1.0,0.0, & ! node id 4
                     1.5,1.0,0.0, & ! node id 5
                     1.0,2.0,0.0, & ! node id 6
                     1.0,1.5,1.0/)  ! node id 10



        ! Allocate and fill the node owner array.
        allocate(nodeOwners(numNodes))
        nodeOwners=(/0, & ! node id 4
                     2, & ! node id 5
                     3, & ! node id 6
                     3/)  ! node id 10
 
       ! Set the number of each type of element, plus the total number.
       numElems=1

       ! Allocate and fill the element id array.
       allocate(elemIds(numElems))
       elemIds=(/4/) 

       ! Allocate and fill the element topology type array.
       allocate(elemTypes(numElems))
       elemTypes=(/ESMF_MESHELEMTYPE_TETRA/) ! elem id 4

       ! Allocate and fill the element connection type array.
       ! Note that entry are local indices
       allocate(elemConn(4*numElems))
       elemConn=(/1,2,4,3/) ! elem id 4

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
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   srcField = ESMF_FieldCreate(srcMesh, arrayspec, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  
  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, computationalLBound=clbnd(1:1), computationalUBound=cubnd(1:1), &
                     farrayPtr=farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

!  write(*,*) localPet,"[",clbnd(1:1),cubnd(1:1),"]"

!  write(*,*) localPet, "0: fptr=",farrayPtr1D

  ! set interpolated function
  i2=1
  do i1=1,numNodes

     if (nodeOwners(i1) .eq. localPet) then
        ! Get coordinates
        x=nodeCoords(3*i1-2)
        y=nodeCoords(3*i1-1)
        z=nodeCoords(3*i1)

        ! Set source function
        farrayPtr1D(i2) = 20.0+x+y+z

!        write(*,*) localPet,"i2",i2,"i1",i1,"id:",nodeIds(i1)," Own:", &
!                   nodeOwners(i1), farrayPtr1D(i2)

        ! Advance to next owner
        i2=i2+1
     endif
  enddo

 ! write(*,*) localPet, "1: fptr=",farrayPtr1D


   ! deallocate node data
   deallocate(nodeIds)
   deallocate(nodeCoords)
   deallocate(nodeOwners)

   ! deallocate elem data
   deallocate(elemIds)
   deallocate(elemTypes)
   deallocate(elemConn)


 !  call ESMF_MeshWrite(srcMesh,"srcMesh",rc=localrc)


  ! Establish the resolution of the dst grids
  dst_nx = 10
  dst_ny = 10
  dst_nz = 5

  ! Establish the coordinates of the dst grids
  dst_minx = 0.0
  dst_miny = 0.0
  dst_minz = 0.0
  
  dst_maxx = 2.0
  dst_maxy = 2.0
  dst_maxz = 1.0

  ! setup dest. grid
  dstGrid=ESMF_GridCreateNoPeriDim(minIndex=(/1,1,1/),maxIndex=(/dst_nx,dst_ny,dst_nz/), &
!              coordSys=ESMF_COORDSYS_CART, regDecomp=(/petCount,1,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
       coordSys=ESMF_COORDSYS_CART, regDecomp=(/2,2,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
! DOESN'T WORK WITH 4 PETS? coordSys=ESMF_COORDSYS_CART, regDecomp=(/2,2,1/), indexflag=ESMF_INDEX_GLOBAL, rc=localrc)

  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

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
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER_VCENTER, coordDim=3, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif



     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        ! Set source coordinates
        farrayPtrXC(i1,i2,i3) = ((dst_maxx-dst_minx)*REAL(i1-1)/REAL(dst_nx-1))+dst_minx
        farrayPtrYC(i1,i2,i3) = ((dst_maxy-dst_miny)*REAL(i2-1)/REAL(dst_ny-1))+dst_miny
        farrayPtrZC(i1,i2,i3) = ((dst_maxz-dst_minz)*REAL(i3-1)/REAL(dst_nz-1))+dst_minz

        ! initialize destination field
        farrayPtr(i1,i2,i3)=0.0

     enddo
     enddo
     enddo

  enddo    ! lDE


!  write(*,*) localPet, "2: fptr=",farrayPtr1D

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
	  unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
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


#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif

  ! Check error
  at_least_one_interp=.false.
  do lDE=0,localDECount-1


     !! get coords
     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_GridGetCoord(dstGrid, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=3, &
                            farrayPtr=farrayPtrZC, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif
   
     !! check error
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
     do i3=clbnd(3),cubnd(3)

        !! skip if hasn't been interpolated to 
        !! (All interpolated values should be 20.0 or above)
        if (farrayPtr(i1,i2,i3) <1.0) cycle

        !! mark that at least one point has been interpolated
        at_least_one_interp=.true.

	!! if error is too big report an error
	if (abs(farrayPtr(i1,i2,i3)-(20.0+farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+ &
                                     farrayPtrZC(i1,i2,i3))) > 0.0001) then

!           write(*,*) localPet," error",abs(farrayPtr(i1,i2,i3)), &
!                    abs(20.0+farrayPtrXC(i1,i2,i3)+farrayPtrYC(i1,i2,i3)+farrayPtrZC(i1,i2,i3))
           correct=.false.	
	endif	
     enddo
     enddo
     enddo

  enddo    ! lDE

  ! If we haven't interpolated at least one point, return an error
  ! (This protects against no interpolation happening resulting in a PASS)
  if (.not. at_least_one_interp) then
     correct=.false.
!     write(*,*) "Nothing interpolated"
  endif


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

 end subroutine test_regridTetMeshToGrid3D

      subroutine test_regridExtraFieldDim(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: srcXtraField
  type(ESMF_Field) :: dstXtraField
  type(ESMF_Field) :: xdstXtraField
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: srcXtraPtr(:,:,:),dstXtraPtr(:,:,:)
  real(ESMF_KIND_R8), pointer :: xdstXtraPtr(:,:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(3),fcubnd(3)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  real(ESMF_KIND_R8) :: coord(2)
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  integer :: num_extra

  real(ESMF_KIND_R8) :: theta, phi
  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
    ! degree to rad conversion
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount

  ! result code
  integer :: finalrc
  
  ! init success flag
  correct=.true.

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
  src_nx = 27
  src_ny = 27

  src_dx=360.0/src_nx
  src_dy=180.0/src_ny

  dst_nx = 20
  dst_ny = 20

  dst_dx=360.0/dst_nx
  dst_dy=180.0/dst_ny

  ! extra dimensions for field
  num_extra=5

  
  ! setup source grid
  srcGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  dstGrid=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                coordSys=ESMF_COORDSYS_SPH_DEG, indexflag=ESMF_INDEX_GLOBAL, &
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


   dstField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! Create source/destination fields with extra dimensions
  call ESMF_ArraySpecSet(arrayspec, 3, ESMF_TYPEKIND_R8, rc=rc)

   srcXtraField = ESMF_FieldCreate(srcGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", &
                         ungriddedLBound=(/1/), ungriddedUBound=(/num_extra/), &
                         gridToFieldMap=(/2,3/), &
                         rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


   dstXtraField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", &
                         ungriddedLBound=(/1/), ungriddedUBound=(/num_extra/),&
                         gridToFieldMap=(/2,3/), &
                         rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  xdstXtraField = ESMF_FieldCreate(dstGrid, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", &
                         ungriddedLBound=(/1/), ungriddedUBound=(/num_extra/), &
                         gridToFieldMap=(/2,3/), &
                         rc=localrc)
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

  call ESMF_GridAddCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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
  ! arrayB
  call ESMF_FieldGet(dstField, array=arrayB, rc=localrc)
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


  ! Construct 2D Grid A
  ! (Get memory and set coords for src)
  do lDE=0,localDECount-1
 
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

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*src_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*src_dy + 0.5*src_dy)

     enddo
     enddo

     ! get src pointer
     call ESMF_FieldGet(srcXtraField, lDE, srcXtraPtr,  &
          computationalLBound=fclbnd, computationalUBound=fcubnd, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     !! set interpolated function
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)
     do i3=fclbnd(3),fcubnd(3)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i2,i3))
        phi = DEG2RAD*(90.-farrayPtrYC(i2,i3))

        srcXtraPtr(i1,i2,i3) = 10.0*REAL(i1)*(2. + cos(theta)**2.*cos(2.*phi))

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

     !! set coords
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Set source coordinates as 0 to 360
        farrayPtrXC(i1,i2) = REAL(i1-1)*dst_dx
        farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*dst_dy + 0.5*dst_dy)

     enddo
     enddo


     call ESMF_FieldGet(dstXtraField, lDE, dstXtraPtr,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     call ESMF_FieldGet(xdstXtraField, lDE, xdstXtraPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif


     !! set coords
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)
     do i3=fclbnd(3),fcubnd(3)

        ! initialize destination field
        dstXtraPtr(i1,i2,i3)=0.0

       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(farrayPtrXC(i2,i3))
        phi = DEG2RAD*(90.-farrayPtrYC(i2,i3))

        xdstXtraPtr(i1,i2,i3) = 10.0*REAL(i1)*(2. + cos(theta)**2.*cos(2.*phi))

     enddo
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif


  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", &
       rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
  endif
#endif


  ! Regrid store
  ! Calculate routeHandle on 2D fields
  call ESMF_FieldRegridStore( &
	  srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid on fields with extra dimension
  call ESMF_FieldRegrid(srcXtraField, dstXtraField, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif


  ! Check results
  do lDE=0,localDECount-1

     ! Get interpolated dst field
     call ESMF_FieldGet(dstXtraField, lDE, dstXtraPtr, rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     call ESMF_FieldGet(xdstXtraField, lDE, xdstXtraPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
     endif

     ! Make sure everthing looks ok
     do i1=fclbnd(1),fcubnd(1)
     do i2=fclbnd(2),fcubnd(2)
     do i3=fclbnd(3),fcubnd(3)

        if (xdstXtraPtr(i1,i2,i3) .ne. 0.0) then
           if (abs(dstXtraPtr(i1,i2,i3)-xdstXtraPtr(i1,i2,i3))/abs(dstXtraPtr(i1,i2,i3)) &
                .gt. 0.05) then
              correct=.false.
!              write(*,*) i1,i2,i3,"::",(dstXtraPtr(i1,i2,i3)-xdstXtraPtr(i1,i2,i3))/xdstXtraPtr(i1,i2,i3)
           endif
        else
           if (abs(dstXtraPtr(i1,i2,i3)-xdstXtraPtr(i1,i2,i3)) &
                .gt. 0.05) then
              correct=.false.
           endif
        endif
     enddo
     enddo
     enddo

  enddo    ! lDE


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

   call ESMF_FieldDestroy(srcXtraField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(dstXtraField, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldDestroy(xdstXtraField, rc=localrc)
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


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_regridExtraFieldDim




end program ESMF_FieldRegridUTest
