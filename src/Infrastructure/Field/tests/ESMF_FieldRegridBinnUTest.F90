! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
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
      write(name, *) "Binning regridding between spherical Grids."

      ! initialize
      rc=ESMF_SUCCESS

      ! do test
      call test_binning_regrid_sph_grids(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      !------------------------------------------------------------------------
#endif
#endif
      
    call ESMF_TestEnd(ESMF_SRCLINE)

contains

 subroutine test_binning_regrid_sph_grids(rc)
#undef ESMF_METHOD
#define ESMF_METHOD "test_binning_regrid_sph_grids"
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: srcGrid
  type(ESMF_Grid) :: dstGrid
  type(ESMF_Field) :: srcFieldLon,srcFieldLat
  type(ESMF_Field) :: dstFieldLon,dstFieldLat
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
  real(ESMF_KIND_R8), pointer :: farrayPtrLon(:,:), farrayPtrLat(:,:)
  real(ESMF_KIND_R8), pointer :: xfarrayPtr(:,:)
  real(ESMF_KIND_R8), pointer :: errfarrayPtr(:,:)
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, srclocalDECount, dstlocalDECount
  real(ESMF_KIND_R8) :: coord(2),x,y,z
  character(len=ESMF_MAXSTR) :: string
  integer src_nx, src_ny, dst_nx, dst_ny
  real(ESMF_KIND_R8) :: lon, lat, theta, phi, relErr
  real(ESMF_KIND_R8) :: coords(2), maxRelErr
  real(ESMF_KIND_R8),parameter :: DEG2RAD = 3.141592653589793_ESMF_KIND_R8/180.0_ESMF_KIND_R8
  integer :: localPet, petCount
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
  src_nx=180
  src_ny=90

  dst_nx=src_nx
  dst_ny=src_ny
       

  ! Create source Grid
  srcGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/src_nx,src_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Create source fields
   srcFieldLon = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   srcFieldLat = ESMF_FieldCreate(srcGrid, typekind=ESMF_TYPEKIND_R8, &
        staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Get number of local DEs
  call ESMF_GridGet(srcGrid, localDECount=srclocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! Construct Src Field
  do lDE=0,srclocalDECount-1

     ! get src pointer
     call ESMF_FieldGet(srcFieldLon, lDE, farrayPtrLon, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     ! get src pointer
     call ESMF_FieldGet(srcFieldLat, lDE, farrayPtrLat, rc=localrc)
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
        
        ! Init data
        farrayPtrLon(i1,i2) = coords(1)
        farrayPtrLat(i1,i2) = coords(2)

     enddo
     enddo

  enddo    ! lDE

    
  ! Create Dst Grid
  dstGrid=ESMF_GridCreate1PeriDimUfrm(maxIndex=(/dst_nx,dst_ny/), &
       minCornerCoord=(/0.0_ESMF_KIND_R8,-90.0_ESMF_KIND_R8/), &
       maxCornerCoord=(/360.0_ESMF_KIND_R8,90.0_ESMF_KIND_R8/), &
       staggerLocList=(/ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER/), &
       rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  
  ! Create destination fields
   dstFieldLon = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   dstFieldLat = ESMF_FieldCreate(dstGrid, typekind=ESMF_TYPEKIND_R8, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get number of local DEs
  call ESMF_GridGet(dstGrid, localDECount=dstlocalDECount, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   
  ! Init destination to 0.0
  do lDE=0,dstlocalDECount-1

     ! Get lon dst pointer
     call ESMF_FieldGet(dstFieldLon, lDE, farrayPtrLon, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     
     ! Get lat dst pointer
     call ESMF_FieldGet(dstFieldLat, lDE, farrayPtrLat, &
          computationalLBound=clbnd, computationalUBound=cubnd, rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     ! Init to 0.0
     farrayPtrLon=0.0
     farrayPtrLat=0.0
     
  enddo    ! lDE


#if 0
  ! DEBUG OUTPUT
  
  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstFieldLon, array=dstArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return


  ! srcArray
  call ESMF_FieldGet(srcFieldLon, array=srcArray, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  
  call ESMF_GridWriteVTK(srcGrid,staggerloc=ESMF_STAGGERLOC_CENTER, &
       filename="srcGrid", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

#endif

  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcFieldLon, &
          dstField=dstFieldLon, &
          routeHandle=routeHandle, &
          unmappedAction=ESMF_UNMAPPEDACTION_ERROR, &
          regridmethod=ESMF_REGRIDMETHOD_BINNING, &
          rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Do regrid
  call ESMF_FieldRegrid(srcFieldLon, dstFieldLon, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  call ESMF_FieldRegrid(srcFieldLat, dstFieldLat, routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return


  ! Free routeHandle
  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  
  ! Check results
  correct=.true.
  do lDE=0,dstlocalDECount-1

     call ESMF_FieldGet(dstFieldLon, lDE, farrayPtrLon, computationalLBound=clbnd, &
          computationalUBound=cubnd,  rc=localrc)
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

     call ESMF_FieldGet(dstFieldLat, lDE, farrayPtrLat, rc=localrc) 
     if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return


     !! Make sure that all the points are being mapped within the correct cell
     !! by checking that they are very close to the center point 
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)

        ! Get coords
        call ESMF_GridGetCoord(dstGrid, staggerloc=ESMF_STAGGERLOC_CENTER, &
             localDE=lDE, index=(/i1,i2/), coord=coords, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
             ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

        lon=coords(1)
        lat=coords(2)

        ! DEBUG
        !write(*,*) localPEt,"# orig=(",lon,lat,") map=(",farrayPtrLon(i1,i2),farrayPtrLat(i1,i2),")"

        ! Since it's a 2deg grid, if points are within 0.25 of the center, they are definitely
        ! within the cell
        if (abs(farrayPtrLon(i1,i2)-lon) > 0.25) then
           correct=.false.
        endif

        if (abs(farrayPtrLat(i1,i2)-lat) > 0.25) then
           correct=.false.
        endif        
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
   call ESMF_FieldDestroy(srcFieldLon, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(srcFieldLat, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstFieldLon, rc=localrc)
   if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   call ESMF_FieldDestroy(dstFieldLat, rc=localrc)
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

 end subroutine test_binning_regrid_sph_grids

end program ESMF_FieldRegridUTest
