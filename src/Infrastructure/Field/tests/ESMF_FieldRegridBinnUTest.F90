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

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test regrid with masks
      write(failMsg, *) "Test unsuccessful"
      write(name, *) "Binning regridding between a LocStream and a Mesh with >1 point per cell."

      ! initialize
      rc=ESMF_SUCCESS

      ! do test
      call test_bin_LS_to_M(rc)

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


 ! Test binning regridding between a LocStream and a Mesh.
 ! Also, make sure the summing works with more than one
 ! value per cell. 
 ! Here is the mesh that's used:
 ! 
 !  2.1   7 ------- 8 ------- 9
 !        |         |         |
 !        |    4    |    5    |
 !        |         |         |
 !  1.0   4 ------- 5 ------- 6
 !        |         |  \   3  |
 !        |    1    |    \    |
 !        |         |  2   \  |
 ! -0.1   1 ------- 2 ------- 3
 !
 !      -0.1       1.0        2.1 
 ! 
 !        Node Id labels at corners
 !       Element Id labels in centers
  
 subroutine test_bin_LS_to_M(rc)
  integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Mesh) :: dstMesh
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:), farrayPtr1D(:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  integer :: i1,i2,i3, index(2)
  integer :: lDE, localDECount
  integer :: cl,cu

  real(ESMF_KIND_R8) :: x,y

  integer :: localPet, petCount

  integer, allocatable :: nodeIds(:),nodeOwners(:)
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
  integer, allocatable :: elemIds(:),elemTypes(:),elemConn(:)
  integer :: numNodes, numElems
  integer :: numQuadElems,numTriElems, numTotElems

  type(ESMF_LocStream) :: srcLocStream
  integer :: numLocationsOnThisPet
  real(ESMF_KIND_R8), pointer :: Xarray(:),Yarray(:)
  integer(ESMF_KIND_I4) :: maskValues(2)
  integer(ESMF_KIND_I4), pointer :: maskArray(:)

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
    rc=ESMF_FAILURE
    return
  endif

  ! Setup Src LocStream
  if (petCount .eq. 1) then
    numLocationsOnThisPet=6
  else
    if (localpet .eq. 0) then
      numLocationsOnThisPet=3
    else if (localpet .eq. 1) then
      numLocationsOnThisPet=2
    else if (localpet .eq. 2) then
      numLocationsOnThisPet=1
    else if (localpet .eq. 3) then
      numLocationsOnThisPet=0
    endif
  endif

  !-------------------------------------------------------------------
  ! Create the LocStream:  Allocate space for the LocStream object,
  ! define the number and distribution of the locations.
  !-------------------------------------------------------------------
  srcLocStream=ESMF_LocStreamCreate(name="Equatorial Measurements", &
                                   localCount=numLocationsOnThisPet, &
                                   coordSys=ESMF_COORDSYS_CART, &
                                   indexflag=ESMF_INDEX_GLOBAL, &
                                   rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating locStream'
    rc=ESMF_FAILURE
    return
  endif


  !-------------------------------------------------------------------
  ! Add key data (internally allocating memory).
  !-------------------------------------------------------------------
  call ESMF_LocStreamAddKey(srcLocStream,                    &
                            keyName="ESMF:Y",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Ydimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for Y'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamAddKey(srcLocStream,                    &
                            keyName="ESMF:X",                &
                            KeyTypeKind=ESMF_TYPEKIND_R8, &
                            keyUnits="Units",           &
                            keyLongName="Xdimension", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble adding LocStream key for X'
    rc=ESMF_FAILURE
    return
  endif
  !-------------------------------------------------------------------
  ! Get key data.
  !-------------------------------------------------------------------
  call ESMF_LocStreamGetKey(srcLocStream,                    &
                            keyName="ESMF:Y",                &
                            farray=Yarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for Y coordinate'
    rc=ESMF_FAILURE
    return
  endif
  call ESMF_LocStreamGetKey(srcLocStream,                    &
                            keyName="ESMF:X",                &
                            farray=Xarray,                   &
                            rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble getting LocStream key for X coordinate'
    rc=ESMF_FAILURE
    return
  endif

  ! Create Src field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

  srcField = ESMF_FieldCreate(srcLocStream, arrayspec, &
                        name="src", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    print*,'ERROR:  trouble creating field on locStream'
    rc=ESMF_FAILURE
    return
  endif


  ! Get src field pointer
  call ESMF_FieldGet(srcField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif


  !-------------------------------------------------------------------
  ! Set key data & src field data
  !-------------------------------------------------------------------
  if (petCount .eq. 1) then
    Xarray(1)=0.1  ! elem 1
    Xarray(2)=0.2  ! elem 4
    Xarray(3)=1.25 ! elem 2
    Xarray(4)=0.5  ! elem 4
    Xarray(5)=1.4  ! elem 2
    Xarray(6)=0.9  ! elem 4

    Yarray(1)=0.1  ! elem 1
    Yarray(2)=1.1  ! elem 4
    Yarray(3)=0.25 ! elem 2
    Yarray(4)=1.5  ! elem 4
    Yarray(5)=0.1  ! elem 2
    Yarray(6)=1.1  ! elem 4

    farrayPtr1D(1)=1.0 ! elem 1
    farrayPtr1D(2)=2.0 ! elem 4
    farrayPtr1D(3)=4.0 ! elem 2
    farrayPtr1D(4)=8.0 ! elem 4
    farrayPtr1D(5)=16.0 ! elem 2
    farrayPtr1D(6)=32.0 ! elem 4   
  else
     if (localpet .eq. 0) then
        Xarray(1)=0.1  ! elem 1
        Xarray(2)=0.2  ! elem 4
        Xarray(3)=1.25 ! elem 2
        
        Yarray(1)=0.1  ! elem 1
        Yarray(2)=1.1  ! elem 4
        Yarray(3)=0.25 ! elem 2
        
        farrayPtr1D(1)=1.0 ! elem 1
        farrayPtr1D(2)=2.0 ! elem 4
        farrayPtr1D(3)=4.0 ! elem 2
     else if (localpet .eq. 1) then
        Xarray(4)=0.5  ! elem 4
        Xarray(5)=1.4  ! elem 2

        Yarray(4)=1.5  ! elem 4
        Yarray(5)=0.1  ! elem 2

        farrayPtr1D(4)=8.0 ! elem 4
        farrayPtr1D(5)=16.0 ! elem 2        
    else if (localpet .eq. 2) then
       Xarray(6)=0.9  ! elem 4
       
       Yarray(6)=1.1  ! elem 4

       farrayPtr1D(6)=32.0 ! elem 4
       
    !else if (localpet .eq. 3) then
       ! None
    endif
  endif


  ! Setup Dst
  if (petCount .eq. 1) then
     ! Set number of nodes
     numNodes=9

     ! Allocate and fill the node id array.
     allocate(nodeIds(numNodes))
     nodeIds=(/100,20,30,40,50,60,70,80,90/)

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
       nodeIds=(/100,20,40,50/)

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
       nodeIds=(/20,30,50,60/)

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
        nodeIds=(/40,50,70,80/)

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
        nodeIds=(/50,60,80,90/)

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
   dstMesh=ESMF_MeshCreate(parametricDim=2,spatialDim=2, &
        coordSys=ESMF_COORDSYS_CART, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
   endif

  ! Create dest field
  call ESMF_ArraySpecSet(arrayspec, 1, ESMF_TYPEKIND_R8, rc=rc)

   dstField = ESMF_FieldCreate(dstMesh, arrayspec, meshLoc=ESMF_MESHLOC_ELEMENT, &
                        name="source", rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif

  ! Get dest field pointer
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif

  ! Init to 0.0
  farrayPtr1D=0.0
    

  !!! Regrid forward from the mesh to the locstream
  ! Regrid store
  call ESMF_FieldRegridStore( &
          srcField, &
          dstField=dstField, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BINNING, &
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


  ! Get destination field pointer
  call ESMF_FieldGet(dstField, 0, farrayPtr1D,  rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
        rc=ESMF_FAILURE
        return
  endif
  
  ! Check output
  correct=.true.
  if (petCount .eq. 1) then
    if(abs(farrayPtr1D(1)-1.0) > 1.0E-14) correct=.false. ! elem 1
    if(abs(farrayPtr1D(2)-20.0) > 1.0E-14) correct=.false. ! elem 2
    if(abs(farrayPtr1D(3)-0.0) > 1.0E-14) correct=.false. ! elem 3
    if(abs(farrayPtr1D(4)-42.0) > 1.0E-14) correct=.false. ! elem 4
    if(abs(farrayPtr1D(5)-0.0) > 1.0E-14) correct=.false. ! elem 5
  else
     if (localpet .eq. 0) then
        if(abs(farrayPtr1D(1)-1.0) > 1.0E-14) correct=.false. ! elem 1
     else if (localpet .eq. 1) then
        if(abs(farrayPtr1D(1)-20.0) > 1.0E-14) correct=.false. ! elem 2
        if(abs(farrayPtr1D(2)-0.0) > 1.0E-14) correct=.false. ! elem 3        
    else if (localpet .eq. 2) then
        if(abs(farrayPtr1D(1)-42.0) > 1.0E-14) correct=.false. ! elem 4       
    else if (localpet .eq. 3) then
       if(abs(farrayPtr1D(1)-0.0) > 1.0E-14) correct=.false. ! elem 5
    endif
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
  call ESMF_LocStreamDestroy(srcLocStream, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
  endif

  call ESMF_MeshDestroy(dstMesh, rc=localrc)
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

 end subroutine test_bin_LS_to_M


 
end program ESMF_FieldRegridUTest
