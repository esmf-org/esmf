! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research,
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

#if defined (ESMF_LAPACK)
#if defined (ESMF_LAPACK_INTERNAL)
#include "ESMF_LapackBlas.inc"
#endif
#endif

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

    integer :: virtMemPet, physMemPet

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
      '$Id$'

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
 
! This #if surrounds all the tests to enable turning on just one test
#if 1
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Bilinear regrid a cubed sphere Grid with regular decomposition"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_bilinear_regrid_csgrid(.true.,rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Bilinear regrid a cubed sphere Grid with irregular decomposition"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_bilinear_regrid_csgrid(.false.,rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Bilinear regrid a cubed sphere Grid with ESMF_COORDSYS_SPH_RAD"

      ! initialize 
      rc=ESMF_SUCCESS

      ! do test
      call test_bilinear_regrid_csgrid_sph_rad(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Patch regrid a cubed sphere Grid"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_patch_regrid_csgrid(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Nearest neighbor regrid a cubed sphere Grid"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_nearest_regrid_csgrid(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Conservative regrid a cubed sphere Grid with regular decomposition"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_conserve_regrid_csgrid(.true., rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Conservative regrid a cubed sphere Grid with irregular decomposition"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_conserve_regrid_csgrid(.false.,rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Bilinear regrid a cubed sphere Mesh"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_bilinear_regrid_csmesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Patch regrid a cubed sphere Mesh"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_patch_regrid_csmesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Nearest neighbor regrid a cubed sphere Mesh"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_nearest_regrid_csmesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with cubed sphere mesh
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Conservative regrid a cubed sphere Mesh"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_conserve_regrid_csmesh(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with Cubed Sphere grid defined in a mosaic file
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Bilinear regrid a regularly decomposed cubed sphere Grid defined in GRIDSPEC Mosaic file"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_bilinear_regrid_csmosaic(.true.,rc)

      ! return results depending on the presence of the NetCDF library
#ifdef ESMF_NETCDF
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
      write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
      call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with Cubed Sphere grid defined in a mosaic file
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Bilinear regrid an irregularly decomposed cubed sphere Grid defined in GRIDSPEC Mosaic file"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_bilinear_regrid_csmosaic(.false.,rc)

      ! return results depending on the presence of the NetCDF library
#ifdef ESMF_NETCDF
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
      write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
      call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
      !------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with Cubed Sphere grid defined in a mosaic file
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Conservative regrid a regularly decomposed cubed sphere Grid defined in a GRIDSPEC Mosaic file"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_conserve_regrid_csmosaic(.true., rc)

      ! return results depending on the presence of the NetCDF library
#ifdef ESMF_NETCDF
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
      write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
      call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
      !------------------------------------------------------------------------

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with Cubed Sphere grid defined in a mosaic file
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Conservative regrid a irregularly decomposed cubed sphere Grid defined in a GRIDSPEC Mosaic file"

      ! initialize 
      rc=ESMF_SUCCESS
       
      ! do test
      call test_conserve_regrid_csmosaic(.false., rc)

      ! return results depending on the presence of the NetCDF library
#ifdef ESMF_NETCDF
      call ESMF_Test((rc==ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#else
      write(failMsg, *) "Did not return ESMF_RC_LIB_NOT_PRESENT"
      call ESMF_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, result, ESMF_SRCLINE)
#endif
      !------------------------------------------------------------------------

#endif
#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

contains 

 subroutine test_bilinear_regrid_csgrid(isregular, rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_bilinear_regrid_csgrid"
  logical, intent(in)   :: isregular
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  integer :: localPet, petCount
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 
  integer :: decomptile(2,6)
  integer :: countsPerDEDim1(3,6), countsPerDEDim2(2,6)
  integer :: i
  
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
  src_tile_size=20

  dst_nx = 47
  dst_ny = 47

  if (isregular) then
    ! Set up decomposition for src Grid
    decomptile(:,1)=(/2,2/)
    decomptile(:,2)=(/2,2/)
    decomptile(:,3)=(/2,2/)
    decomptile(:,4)=(/2,2/)
    decomptile(:,5)=(/2,2/)
    decomptile(:,6)=(/2,2/)

    ! Create Src Grid
    srcGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       regDecompPTile=decomptile, &
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       indexflag = ESMF_INDEX_GLOBAL, &
       rc=localrc)
    if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  else 
    do i=1,3
       CountsPerDeDim1(:,i)=(/10,5,5/)
       CountsPerDeDim1(:,i+3)=(/10,10,0/)
       CountsPerDeDim2(:,i)=(/12,8/)
       CountsPerDeDim2(:,i+3)=(/20,0/)
    enddo
    ! Create Src Grid
    srcGrid=ESMF_GridCreateCubedSphere(src_tile_size, &
       CountsPerDeDim1, CountsPerDeDim2, &	
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       indexflag = ESMF_INDEX_GLOBAL, &
       rc=localrc)
    if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
   endif
  
  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create source/destination fields
   srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        farrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! initialize destination field
        farrayPtr(i1,i2)=0.0
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Check results
  maxRelErr=0.0  
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.001) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo
  enddo    ! lDE

  ! output maxRelErr
  !write(*,*) "maxRelErr=",maxRelErr

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(errField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_bilinear_regrid_csgrid
 
 subroutine test_bilinear_regrid_csgrid_sph_rad(rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_bilinear_regrid_csgrid_sph_rad"
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  real(ESMF_KIND_R4) :: coordsR4(2)
  integer :: localPet, petCount
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 
  integer :: decomptile(2,6)
  
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
  src_tile_size=20

  dst_nx = 47
  dst_ny = 47

  ! Set up decomposition for src Grid
  decomptile(:,1)=(/2,2/)
  decomptile(:,2)=(/2,2/)
  decomptile(:,3)=(/2,2/)
  decomptile(:,4)=(/2,2/)
  decomptile(:,5)=(/2,2/)
  decomptile(:,6)=(/2,2/)

  ! Create Src Grid
  srcGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       regDecompPTile=decomptile, &
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       indexflag = ESMF_INDEX_GLOBAL, &
       coordSys = ESMF_COORDSYS_SPH_RAD, &
       coordTypeKind = ESMF_TYPEKIND_R4, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8*ESMF_COORDSYS_DEG2RAD,-90.0_ESMF_KIND_R8*ESMF_COORDSYS_DEG2RAD/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8*ESMF_COORDSYS_DEG2RAD,90.0_ESMF_KIND_R8*ESMF_COORDSYS_DEG2RAD/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       coordSys = ESMF_COORDSYS_SPH_RAD, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create source/destination fields
   srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coordsR4, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        theta = coordsR4(1)
        phi = 90.0_ESMF_KIND_R4*ESMF_COORDSYS_DEG2RAD-coordsR4(2)
     
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        farrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        theta = coords(1)
        phi = 90.0_ESMF_KIND_R8*ESMF_COORDSYS_DEG2RAD-coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! initialize destination field
        farrayPtr(i1,i2)=0.0
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Check results
  maxRelErr=0.0  
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.001) then
            correct=.false.
            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo
  enddo    ! lDE

  ! output maxRelErr
  !write(*,*) "maxRelErr=",maxRelErr

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(errField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_bilinear_regrid_csgrid_sph_rad

 subroutine test_patch_regrid_csgrid(rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_patch_regrid_csgrid"
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  integer :: localPet, petCount
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 
  integer :: decomptile(2,6)
  
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
  src_tile_size=20

  dst_nx = 47
  dst_ny = 47

  ! Set up decomposition for src Grid
  decomptile(:,1)=(/2,2/)
  decomptile(:,2)=(/2,2/)
  decomptile(:,3)=(/2,2/)
  decomptile(:,4)=(/2,2/)
  decomptile(:,5)=(/2,2/)
  decomptile(:,6)=(/2,2/)

  ! Create Src Grid
  srcGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       regDecompPTile=decomptile, &
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       indexflag = ESMF_INDEX_DELOCAL, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create source/destination fields
   srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        farrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! initialize destination field
        farrayPtr(i1,i2)=0.0
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Check results
  maxRelErr=0.0  
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.001) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo
  enddo    ! lDE

  ! output maxRelErr
  !write(*,*) "maxRelErr=",maxRelErr

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(errField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_patch_regrid_csgrid

 subroutine test_nearest_regrid_csgrid(rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_nearest_regrid_csgrid"
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  integer :: localPet, petCount
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 
  integer :: decomptile(2,6)
  
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
  src_tile_size=20

  dst_nx = 47
  dst_ny = 47

  ! Set up decomposition for src Grid
  decomptile(:,1)=(/2,2/)
  decomptile(:,2)=(/2,2/)
  decomptile(:,3)=(/2,2/)
  decomptile(:,4)=(/2,2/)
  decomptile(:,5)=(/2,2/)
  decomptile(:,6)=(/2,2/)

  ! Create Src Grid
  srcGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       regDecompPTile=decomptile, &
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create Dst Grid
  ! (create an identical grid to the source grid in terms of coordinates 
  !  but with a different distribution to make checking
  !  the accuracty of the nearest neighbor easy)
  dstGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create source/destination fields
   srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        farrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! initialize destination field
        farrayPtr(i1,i2)=0.0
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Check results
  maxRelErr=0.0  
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.001) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo
  enddo    ! lDE

  ! output maxRelErr
  !write(*,*) "maxRelErr=",maxRelErr

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(errField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_nearest_regrid_csgrid

 subroutine test_conserve_regrid_csgrid(isregular, rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_conserve_regrid_csgrid"
  logical, intent(in)   :: isregular
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Field) :: srcArea
  type(ESMF_Field) :: dstArea
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: dstIntegral, srcIntegral
  real(ESMF_KIND_R8) :: localSrcIntegral(1), globalSrcIntegral(1)
  real(ESMF_KIND_R8) :: localDstIntegral(1), globalDstIntegral(1)
  real(ESMF_KIND_R8) :: localMaxRelErr(1), globalMaxRelErr(1)
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 
  integer :: decomptile(2,6)
  integer :: countsPerDEDim1(3,6), countsPerDEDim2(2,6)
  integer :: i

  
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
  src_tile_size=20

  dst_nx = 47
  dst_ny = 47

  if (isregular) then
    ! Set up decomposition for src Grid
    decomptile(:,1)=(/2,2/)
    decomptile(:,2)=(/2,2/)
    decomptile(:,3)=(/2,2/)
    decomptile(:,4)=(/2,2/)
    decomptile(:,5)=(/2,2/)
    decomptile(:,6)=(/2,2/)

    ! Create Src Grid
    srcGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       regDecompPTile=decomptile, &
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
    if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  else 
    do i=1,3
       CountsPerDeDim1(:,i)=(/10,5,5/)
       CountsPerDeDim1(:,i+3)=(/10,10,0/)
       CountsPerDeDim2(:,i)=(/12,8/)
       CountsPerDeDim2(:,i+3)=(/20,0/)
    enddo
    ! Create Src Grid
    srcGrid=ESMF_GridCreateCubedSphere(src_tile_size, &
       CountsPerDeDim1, CountsPerDeDim2, &	
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       indexflag = ESMF_INDEX_GLOBAL, &
       rc=localrc)
    if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
   endif

   ! create src fields
   srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   srcFracField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   srcArea = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get memory and set fields for src
  do lDE=0,srclocalDECount-1
 
     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        farrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


   ! Create dst fields
   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   dstFracField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   dstArea = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get arrays
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get memory and set fields for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! initialize destination field
        farrayPtr(i1,i2)=0.0
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          normType=ESMF_NORMTYPE_FRACAREA, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get the src cell areas
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get the dst cell areas
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

 ! XMRKX

  ! Check results
  maxRelErr=0.0  
  dstIntegral=0.0
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute total integral
        ! NOTE: DO need to include dstFrac here, because the frac has been included in the weights 
        dstIntegral = dstIntegral + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*farrayPtr(i1,i2)
       ! dstIntegral = dstIntegral + dstAreaptr(i1,i2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.005) then
            correct=.false.
            !write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr
     enddo
     enddo
  enddo    ! lDE


  ! Compute src integral
  srcIntegral=0.0
  do lDE=0,srcLocalDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcIntegral = srcIntegral + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*farrayPtr(i1,i2)
   !     srcIntegral = srcIntegral + srcAreaptr(i1,i2)
     enddo
     enddo
  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif

  ! Get info across PETs
  globalSrcIntegral(1) = 0.0
  globalDstIntegral(1) = 0.0
  
  localSrcIntegral(1)=srcIntegral
  call ESMF_VMAllReduce(vm, localSrcIntegral, globalSrcIntegral, 1, &
       ESMF_REDUCE_SUM, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  localDstIntegral(1)=dstIntegral
  call ESMF_VMAllReduce(vm, localDstIntegral, globalDstIntegral, 1, &
       ESMF_REDUCE_SUM, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  localMaxRelErr(1)=maxRelErr
  call ESMF_VMAllReduce(vm, localMaxRelErr, globalMaxRelErr, 1, &
       ESMF_REDUCE_MAX, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Uncomment these calls to see regrid results
#if 0
  if (localPet == 0) then
    write(*,*) "=== Cubed Sphere Grid ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(globalDstIntegral(1)-globalSrcIntegral(1))/globalSrcIntegral(1)
    write(*,*) "SRC Integral = ", globalSrcIntegral(1)
    write(*,*) "DST Integral = ", globalDstIntegral(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", globalMaxRelErr(1)
    write(*,*) " "
  endif
#endif

  ! return answer based on values
  if (ABS(globalDstIntegral(1)-globalSrcIntegral(1))/globalSrcIntegral(1) > 1.0E-14)  correct=.false.
  if (globalMaxRelErr(1) > 10E-2) correct=.false.


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_conserve_regrid_csgrid

 subroutine test_bilinear_regrid_csmesh(rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_bilinear_regrid_csmesh"
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  real(ESMF_KIND_R8),pointer :: ownedElemCoords(:)
  integer :: numOwnedElems
  integer :: localPet, petCount
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 

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
  src_tile_size=50

  dst_nx = 47
  dst_ny = 47

  ! Create Src Mesh
  srcMesh=ESMF_MeshCreateCubedSphere(tileSize=src_tile_size, &
       nx=1, ny=1, & ! Decomposition per Tile
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, typekind=ESMF_TYPEKIND_R8, &
                         meshloc=ESMF_MESHLOC_ELEMENT, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  
  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Set interpolated function
  call ESMF_MeshGet(srcMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

    ! Set interpolated function
  call ESMF_MeshGet(srcMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! loop through and set field
  do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)
     
     ! Set the source to be a function of the x,y,z coordinate
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)
     
     ! set src data
     farrayPtr1D(i1) = x+y+z+15.0
  enddo


   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Create destination fields
   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get memory and set fields for dest
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

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
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Check results
  maxRelErr=0.0  
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.005) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo
  enddo    ! lDE

  ! output maxRelErr
  !write(*,*) "maxRelErr=",maxRelErr

#if 0
  call ESMF_MeshWrite(srcMesh, "srcMesh", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_bilinear_regrid_csmesh

 subroutine test_patch_regrid_csmesh(rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_patch_regrid_csmesh"
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  real(ESMF_KIND_R8),pointer :: ownedElemCoords(:)
  integer :: numOwnedElems
  integer :: localPet, petCount
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 

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
  src_tile_size=50

  dst_nx = 47
  dst_ny = 47

  ! Create Src Mesh
  srcMesh=ESMF_MeshCreateCubedSphere(tileSize=src_tile_size, &
       nx=1, ny=1, & ! Decomposition per Tile
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, typekind=ESMF_TYPEKIND_R8, &
                         meshloc=ESMF_MESHLOC_ELEMENT, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Set interpolated function
  call ESMF_MeshGet(srcMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

    ! Set interpolated function
  call ESMF_MeshGet(srcMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! loop through and set field
  do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)
     
     ! Set the source to be a function of the x,y,z coordinate
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)
     
     ! set src data
     farrayPtr1D(i1) = x+y+z+15.0
  enddo


   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Create destination fields
   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get memory and set fields for dest
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

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
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_PATCH, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Check results
  maxRelErr=0.0  
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.005) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo
  enddo    ! lDE

  ! output maxRelErr
  !write(*,*) "maxRelErr=",maxRelErr

#if 0
  call ESMF_MeshWrite(srcMesh, "srcMesh", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_patch_regrid_csmesh

 subroutine test_nearest_regrid_csmesh(rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_nearest_regrid_csmesh"
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  real(ESMF_KIND_R8),pointer :: ownedElemCoords(:)
  integer :: numOwnedElems
  integer :: localPet, petCount
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 

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
  src_tile_size=50

  dst_nx = 47
  dst_ny = 47

  ! Create Src Mesh
  srcMesh=ESMF_MeshCreateCubedSphere(tileSize=src_tile_size, &
       nx=1, ny=1, & ! Decomposition per Tile
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Create source field
   srcField = ESMF_FieldCreate(srcMesh, typekind=ESMF_TYPEKIND_R8, &
                         meshloc=ESMF_MESHLOC_ELEMENT, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Set interpolated function
  call ESMF_MeshGet(srcMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

    ! Set interpolated function
  call ESMF_MeshGet(srcMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! loop through and set field
  do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)
     
     ! Set the source to be a function of the x,y,z coordinate
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)
     
     ! set src data
     farrayPtr1D(i1) = x+y+z+15.0
  enddo


   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Create Dst Grid
  ! (create an identical csgrid in terms of coordinates to make checking 
  !  nearest neighbor easier)
  dstGrid=ESMF_GridCreateCubedSphere(tileSize=src_tile_size, &
       staggerLocList = (/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Create destination fields
   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get memory and set fields for dest
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

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
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Check results
  maxRelErr=0.0  
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.005) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo
  enddo    ! lDE

  ! output maxRelErr
  !write(*,*) "maxRelErr=",maxRelErr

#if 0
  call ESMF_MeshWrite(srcMesh, "srcMesh", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_nearest_regrid_csmesh

 subroutine test_conserve_regrid_csmesh(rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_conserve_regrid_csmesh"
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: srcMesh
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Field) :: srcArea
  type(ESMF_Field) :: dstArea
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: srcfarrayPtr(:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:), dstFracptr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: clbnd1D(1),cubnd1D(1)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: dstIntegral, srcIntegral
  real(ESMF_KIND_R8) :: localSrcIntegral(1), globalSrcIntegral(1)
  real(ESMF_KIND_R8) :: localDstIntegral(1), globalDstIntegral(1)
  real(ESMF_KIND_R8) :: localMaxRelErr(1), globalMaxRelErr(1)
  real(ESMF_KIND_R8),pointer :: ownedElemCoords(:)
  integer :: numOwnedElems
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 
  
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
  src_tile_size=50

  dst_nx = 47
  dst_ny = 47


  ! Create Src Mesh
  srcMesh=ESMF_MeshCreateCubedSphere(tileSize=src_tile_size, &
       nx=1, ny=1, & ! Decomposition per Tile
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create src Fields
   srcField = ESMF_FieldCreate(srcMesh, typekind=ESMF_TYPEKIND_R8, &
                         meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   srcFracField = ESMF_FieldCreate(srcMesh, typekind=ESMF_TYPEKIND_R8, &
                         meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   srcArea = ESMF_FieldCreate(srcMesh, typekind=ESMF_TYPEKIND_R8, &
                         meshloc=ESMF_MESHLOC_ELEMENT, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return



  ! Load test data into the source Field
  ! Should only be 1 localDE
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr,  rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Set interpolated function
  call ESMF_MeshGet(srcMesh, numOwnedElements=numOwnedElems, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

   ! Allocate space for coordinates
   allocate(ownedElemCoords(2*numOwnedElems))

    ! Set interpolated function
  call ESMF_MeshGet(srcMesh, ownedElemCoords=ownedElemCoords, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! loop through and set field
  do i1=1,numOwnedElems

      ! Get coords
     lon=ownedElemCoords(2*i1-1)
     lat=ownedElemCoords(2*i1)
     
     ! Set the source to be a function of the x,y,z coordinate
     theta = DEG2RAD*(lon)
     phi = DEG2RAD*(90.-lat)

     x = cos(theta)*sin(phi)
     y = sin(theta)*sin(phi)
     z = cos(phi)
     
     ! set src data
     srcFarrayPtr(i1) = x+y+z+15.0
  enddo


   ! Deallocate space for coordinates
   deallocate(ownedElemCoords)


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


   ! Create dst fields
   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   dstFracField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   dstArea = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get arrays
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get memory and set fields for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

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
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          normType=ESMF_NORMTYPE_FRACAREA, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get the src cell areas
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get the dst cell areas
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

 ! XMRKX

  ! Check results
  maxRelErr=0.0  
  dstIntegral=0.0
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! Loop over DE
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute total integral
        ! NOTE: DO need to include dstFrac here, because the frac has been included in the weights 
        dstIntegral = dstIntegral + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.005) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr
     enddo
     enddo
  enddo    ! lDE


  ! Compute src integral
  srcIntegral=0.0

  ! get src pointer
  call ESMF_FieldGet(srcField, 0, srcFarrayPtr, &
            computationalLBound=clbnd1D, computationalUBound=cubnd1D,&
            rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  
  ! get src Field
  call ESMF_FieldGet(srcArea, 0, srcAreaptr,  rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
  ! get frac Field
  call ESMF_FieldGet(srcFracField, 0, srcFracptr,  rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
  do i1=clbnd1D(1),cubnd1D(1)
     srcIntegral = srcIntegral + srcFracptr(i1)*srcAreaptr(i1)*srcFarrayPtr(i1)
  enddo


#if 0
  call ESMF_MeshWrite(srcMesh, filename="srcMesh", &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif

  ! Get info across PETs
  globalSrcIntegral(1) = 0.0
  globalDstIntegral(1) = 0.0
  
  localSrcIntegral(1)=srcIntegral
  call ESMF_VMAllReduce(vm, localSrcIntegral, globalSrcIntegral, 1, &
       ESMF_REDUCE_SUM, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  localDstIntegral(1)=dstIntegral
  call ESMF_VMAllReduce(vm, localDstIntegral, globalDstIntegral, 1, &
       ESMF_REDUCE_SUM, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  localMaxRelErr(1)=maxRelErr
  call ESMF_VMAllReduce(vm, localMaxRelErr, globalMaxRelErr, 1, &
       ESMF_REDUCE_MAX, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Uncomment these calls to see regrid results
#if 0
  if (localPet == 0) then
    write(*,*) "=== Cubed Sphere Mesh ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(globalDstIntegral(1)-globalSrcIntegral(1))/globalSrcIntegral(1)
    write(*,*) "SRC Integral = ", globalSrcIntegral(1)
    write(*,*) "DST Integral = ", globalDstIntegral(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", globalMaxRelErr(1)
    write(*,*) " "
  endif
#endif

  ! return answer based on values
  if (ABS(globalDstIntegral(1)-globalSrcIntegral(1))/globalSrcIntegral(1) > 1.0E-14)  correct=.false.
  if (globalMaxRelErr(1) > 10E-2) correct=.false.


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the meshs and grids
  call ESMF_MeshDestroy(srcMesh, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_conserve_regrid_csmesh

 subroutine test_bilinear_regrid_csmosaic(isregular, rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_bilinear_regrid_csgrid"
  logical :: isregular
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  integer :: localPet, petCount, i
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 
  integer :: decomptile(2,6)
  integer :: countsPerDEDim1(3,6), countsPerDEDim2(2,6)
  character(len=ESMF_MAXPATHLEN) :: filename
  
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

  filename = "data/C48_mosaic.nc"

  dst_nx = 47
  dst_ny = 47

  if (isregular) then
    ! Set up decomposition for src Grid
    decomptile(:,1)=(/2,2/)
    decomptile(:,2)=(/2,2/)
    decomptile(:,3)=(/2,2/)
    decomptile(:,4)=(/2,2/)
    decomptile(:,5)=(/2,2/)
    decomptile(:,6)=(/2,2/)

    ! Create Src Grid
    srcGrid=ESMF_GridCreateMosaic(filename=trim(filename), &
       tileFilePath="./data/", regDecompPTile=decomptile, &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
       indexflag=ESMF_INDEX_GLOBAL, &
       rc=localrc)
    if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  else 
    ! Setup irregular decomposition
    do i=1,3
      CountsPerDeDim1(:,i)=(/24,12,12/)
      CountsPerDeDim1(:,i+3)=(/24,24,0/)
      CountsPerDeDim2(:,i)=(/36,12/)
      CountsPerDeDim2(:,i+3)=(/48,0/)
    enddo
    srcGrid=ESMF_GridCreateMosaic(trim(filename), &
       CountsPerDeDim1, CountsPerDeDim2, &	
       tileFilePath="./data/",           &
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       indexflag = ESMF_INDEX_GLOBAL, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  endif
      
  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Create source/destination fields
   srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Construct Src Grid
  ! (Get memory and set coords for src)
  do lDE=0,srclocalDECount-1
 
     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        farrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Get memory and set coords for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! initialize destination field
        farrayPtr(i1,i2)=0.0
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Check results
  maxRelErr=0.0  
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.001) then
            correct=.false.
!            write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr

     enddo
     enddo
  enddo    ! lDE

  ! output maxRelErr
  !write(*,*) "maxRelErr=",maxRelErr

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(xdstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(errField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_bilinear_regrid_csmosaic

 subroutine test_conserve_regrid_csmosaic(isregular, rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_conserve_regrid_csmosaic"
  logical, intent(in)   :: isregular
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Field) :: xdstField
  type(ESMF_Field) :: errField
  type(ESMF_Field) :: srcArea
  type(ESMF_Field) :: dstArea
  type(ESMF_Field) :: srcFracField
  type(ESMF_Field) :: dstFracField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: errArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:), farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: srcAreaptr(:,:), dstAreaptr(:,:)
  real(ESMF_KIND_R8), pointer :: srcFracptr(:,:), dstFracptr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_tile_size, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  integer :: localPet, petCount
  real(ESMF_KIND_R8) :: dstIntegral, srcIntegral
  real(ESMF_KIND_R8) :: localSrcIntegral(1), globalSrcIntegral(1)
  real(ESMF_KIND_R8) :: localDstIntegral(1), globalDstIntegral(1)
  real(ESMF_KIND_R8) :: localMaxRelErr(1), globalMaxRelErr(1)
  real(ESMF_KIND_R8), parameter ::  DEG2RAD = &
                3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8 
  integer :: decomptile(2,6)
  integer :: countsPerDEDim1(3,6), countsPerDEDim2(2,6)
  integer :: i
  character(len=ESMF_MAXPATHLEN) :: filename
  
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
  filename = "data/C48_mosaic.nc"

  dst_nx = 47
  dst_ny = 47

  if (isregular) then
    ! Set up decomposition for src Grid
    decomptile(:,1)=(/2,2/)
    decomptile(:,2)=(/2,2/)
    decomptile(:,3)=(/2,2/)
    decomptile(:,4)=(/2,2/)
    decomptile(:,5)=(/2,2/)
    decomptile(:,6)=(/2,2/)

    ! Create Src Grid
    srcGrid=ESMF_GridCreateMosaic(filename=trim(filename), &
       tileFilePath="./data/", regDecompPTile=decomptile, &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
       indexflag=ESMF_INDEX_GLOBAL, &
       rc=localrc)
    if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  else 
    ! Setup irregular decomposition
    do i=1,3
      CountsPerDeDim1(:,i)=(/24,12,12/)
      CountsPerDeDim1(:,i+3)=(/24,24,0/)
      CountsPerDeDim2(:,i)=(/36,12/)
      CountsPerDeDim2(:,i+3)=(/48,0/)
    enddo
    srcGrid=ESMF_GridCreateMosaic(trim(filename), &
       CountsPerDeDim1, CountsPerDeDim2, &	
       tileFilePath="./data/",           &
       staggerLocList = (/ESMF_STAGGERLOC_CORNER, ESMF_STAGGERLOC_CENTER/), &
       indexflag = ESMF_INDEX_GLOBAL, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  endif

   ! create src fields
   srcField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   srcFracField = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   srcArea = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get memory and set fields for src
  do lDE=0,srclocalDECount-1
 
     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(srcGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)

        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! set src data
        farrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
     enddo
     enddo

  enddo    ! lDE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Destination grid
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


   ! Create dst fields
   dstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  xdstField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  errField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="xdest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   dstFracField = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


   dstArea = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get arrays
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldGet(errField, array=errArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get memory and set fields for dst
  do lDE=0,dstlocalDECount-1
 
     ! get dst pointer
     call ESMF_FieldGet(dstField, lDE, farrayPtr, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Get exact dst pointer
     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     !! dst data
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        ! init exact answer
        lon = coords(1)
        lat = coords(2)
     
       ! Set the source to be a function of the x,y,z coordinate
        theta = DEG2RAD*(lon)
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! farrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)
        ! set exact dst data
        xfarrayPtr(i1,i2) = x+y+z+15.0

        ! This one seems to do a weird thing around the pole with a cubed sphere
        !xfarrayPtr(i1,i2) = 2. + cos(theta)**2.*cos(2.*phi)

        ! initialize destination field
        farrayPtr(i1,i2)=0.0
     enddo
     enddo

  enddo    ! lDE

#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return
  
#endif

  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
          dstFracField=dstFracField, &
          srcFracField=srcFracField, &
          normType=ESMF_NORMTYPE_FRACAREA, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Do regrid
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get the src cell areas
  call ESMF_FieldRegridGetArea(srcArea, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get the dst cell areas
  call ESMF_FieldRegridGetArea(dstArea, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

 ! XMRKX

  ! Check results
  maxRelErr=0.0  
  dstIntegral=0.0
  do lDE=0,dstlocalDECount-1
     
     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(xdstField, lDE, xfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! get dst area Field
     call ESMF_FieldGet(dstArea, lDE, dstAreaptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     ! get frac Field
     call ESMF_FieldGet(dstFracField, lDE, dstFracptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(errField, lDE, errfarrayPtr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! make sure we're not using any bad points
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Compute total integral
        ! NOTE: DO need to include dstFrac here, because the frac has been included in the weights 
        dstIntegral = dstIntegral + dstFracptr(i1,i2)*dstAreaptr(i1,i2)*farrayPtr(i1,i2)

        ! Compute relative error
        if (xfarrayPtr(i1,i2) .ne. 0.0) then
           relErr=abs((farrayPtr(i1,i2)-xfarrayPtr(i1,i2))/xfarrayPtr(i1,i2))
        else
           relErr=abs(farrayPtr(i1,i2)-xfarrayPtr(i1,i2))
        endif

        ! if working everything should be close to exact answer
        if (relErr .gt. 0.005) then
            correct=.false.
            !write(*,*) "relErr=",relErr,farrayPtr(i1,i2),xfarrayPtr(i1,i2)
        endif

        ! Calc max
        if (relErr > maxRelErr) then
           maxRelErr=relErr
        endif      

        ! put in error field
        errfarrayPtr(i1,i2)=relErr
     enddo
     enddo
  enddo    ! lDE


  ! Compute src integral
  srcIntegral=0.0
  do lDE=0,srcLocalDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=clbnd, &
                             computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! get src Field
     call ESMF_FieldGet(srcArea, lDE, srcAreaptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! get frac Field
     call ESMF_FieldGet(srcFracField, lDE, srcFracptr,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        srcIntegral = srcIntegral + srcFracptr(i1,i2)*srcAreaptr(i1,i2)*farrayPtr(i1,i2)
     enddo
     enddo
  enddo    ! lDE


#if 0
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", array1=srcArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_GridWriteVTK(dstGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="dstGrid", array1=dstArray, array2=errArray, &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif

  ! Get info across PETs
  globalSrcIntegral(1) = 0.0
  globalDstIntegral(1) = 0.0
  
  localSrcIntegral(1)=srcIntegral
  call ESMF_VMAllReduce(vm, localSrcIntegral, globalSrcIntegral, 1, &
       ESMF_REDUCE_SUM, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  localDstIntegral(1)=dstIntegral
  call ESMF_VMAllReduce(vm, localDstIntegral, globalDstIntegral, 1, &
       ESMF_REDUCE_SUM, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  localMaxRelErr(1)=maxRelErr
  call ESMF_VMAllReduce(vm, localMaxRelErr, globalMaxRelErr, 1, &
       ESMF_REDUCE_MAX, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Uncomment these calls to see regrid results
#if 0
  if (localPet == 0) then
    write(*,*) "=== Cubed Sphere Grid ==="
    write(*,*) "Conservation:"
    write(*,*) "Rel Error = ", ABS(globalDstIntegral(1)-globalSrcIntegral(1))/globalSrcIntegral(1)
    write(*,*) "SRC Integral = ", globalSrcIntegral(1)
    write(*,*) "DST Integral = ", globalDstIntegral(1)
    write(*,*) " "
    write(*,*) "Interpolation:"
    write(*,*) "Max Error = ", globalMaxRelErr(1)
    write(*,*) " "
  endif
#endif

  ! return answer based on values
  if (ABS(globalDstIntegral(1)-globalSrcIntegral(1))/globalSrcIntegral(1) > 1.0E-14)  correct=.false.
  if (globalMaxRelErr(1) > 10E-2) correct=.false.


  ! Destroy the Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free the grids
  call ESMF_GridDestroy(srcGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  call ESMF_GridDestroy(dstGrid, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! return answer based on correct flag
  if (correct) then
    rc=ESMF_SUCCESS
  else
    rc=ESMF_FAILURE
  endif

 end subroutine test_conserve_regrid_csmosaic


end program ESMF_FieldRegridUTest


