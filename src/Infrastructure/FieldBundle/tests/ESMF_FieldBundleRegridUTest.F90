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
program ESMF_FieldBundleRegridUTest

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
#include "ESMF_Macros.inc"
!
!==============================================================================
!BOPI
! !PROGRAM: ESMF_FieldBundleRegridUTest - This test verifies FieldBundleRegrid functionality.
!
! !DESCRIPTION:
!
! The code in this file specializes on testing the usage of FiledSMM.
!EOPI
!
!-----------------------------------------------------------------------------
! !USES:
    use ESMF_TestMod     ! test methods
    use ESMF
  
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id$'
!------------------------------------------------------------------------------

    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0

    ! individual test result code
    integer :: rc = ESMF_SUCCESS

#ifdef ESMF_TESTEXHAUSTIVE
    ! individual test name
    character(ESMF_MAXSTR) :: name

    ! individual test failure messages
    character(ESMF_MAXSTR*2) :: failMsg
#endif

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    if(rc /= ESMF_SUCCESS) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE

        !------------------------------------------------------------------------
        !EX_UTest
        call test_regrid180vs360_bundle(rc)
        write(failMsg, *) ""
        write(name, *) "FieldBundleRegrid between a 0 to 360 sphere and a -180 to 180 sphere"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest
        call test_regrid180vs360_bundleget(rc)
        write(failMsg, *) ""
        write(name, *) "FieldBundleRegrid between a 0 to 360 sphere and a -180 to 180 sphere"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

 
       !------------------------------------------------------------------------
       !EX_UTest
       ! Test regrid with masks
       write(failMsg, *) "Test unsuccessful"
       write(name, *) "FieldBundleRegrid sphere with mask"

      ! initialize 
       rc=ESMF_SUCCESS
      
      ! do test
      call test_regridSphSrcMask(rc)

      ! return result
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        !------------------------------------------------------------------------
        !EX_UTest
        call test_regridLocStreamBundle(rc)
        write(failMsg, *) ""
        write(name, *) "FieldBundleRegrid between a 0 to 360 sphere and a LocStream"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)



#endif
    call ESMF_TestEnd(ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE

contains

#undef ESMF_METHOD
#define ESMF_METHOD "test_regrid180vs360_bundle"
 
      subroutine test_regrid180vs360_bundle(rc)
        integer, intent(out)  :: rc
        logical :: correct
        integer :: localrc
        type(ESMF_Grid) :: grid360
        type(ESMF_Grid) :: grid180
        type(ESMF_FieldBundle) :: srcFieldBundle360
        type(ESMF_FieldBundle) :: dstFieldBundle360
        type(ESMF_FieldBundle) :: fieldBundle180
        type(ESMF_Field) :: srcField360(6)
        type(ESMF_Field) :: dstField360(6)
        type(ESMF_Field) :: field180(6)
        type(ESMF_Field) :: errorField(6)
        type(ESMF_Array) :: lonArray360
        type(ESMF_RouteHandle) :: routeHandle
        type(ESMF_ArraySpec) :: arrayspec
        type(ESMF_VM) :: vm
        real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
        real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
        real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:),errorfarrayPtr(:,:)
        integer :: clbnd(2),cubnd(2)
        integer :: fclbnd(2),fcubnd(2)
        integer :: i1,i2
        integer :: lDE, localDECount
        character(len=1) :: xstring
        integer src_nx, src_ny, dst_nx, dst_ny
        integer :: num_arrays, NFIELDS=6, i
      
        real(ESMF_KIND_R8) :: src_dx, src_dy
        real(ESMF_KIND_R8) :: dst_dx, dst_dy
        real(ESMF_KIND_R8) :: theta, x, y, z
        real(ESMF_KIND_R8) :: DEG2RAD, lat, lon, phi
        real(ESMF_KIND_R8) :: rangle
        real(ESMF_KIND_R8) :: RAD2DEG
      
      
        integer :: localPet, petCount
      
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
                                    indexflag=ESMF_INDEX_GLOBAL, &
                                    rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
      
      
        ! setup dest. grid
        grid180=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                    indexflag=ESMF_INDEX_GLOBAL, &
                                    rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
      
      
        ! Create source/destination fields
        call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
      
        do i = 1, NFIELDS
             write(xstring, '(i1)') i
             srcField360(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="src360_"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
             dstField360(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="dst360_"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
             errorField(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="error"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
          
             field180(i) = ESMF_FieldCreate(grid180, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="dst180_"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
        enddo 

        srcFieldBundle360 = ESMF_FieldBundleCreate(fieldList=srcField360, rc=localrc)        
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
        dstFieldBundle360 = ESMF_FieldBundleCreate(fieldList=dstField360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
        fieldBundle180 = ESMF_FieldBundleCreate(fieldList=field180, rc=localrc)
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
      
           do i = 1, NFIELDS
               ! get src pointer
               call ESMF_FieldGet(srcField360(i), lDE, farrayPtr, computationalLBound=fclbnd, &
                                       computationalUBound=fcubnd,  rc=localrc)
               if (localrc /=ESMF_SUCCESS) then
                  rc=ESMF_FAILURE
                  return
               endif
          
          
               ! get src destination Field pointer
               call ESMF_FieldGet(dstField360(i), lDE, farrayPtr2,   rc=localrc)
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
                  farrayPtr(i1,i2) = x+y+z+15.0*i
          
                   ! initialize src destination field
                   farrayPtr2(i1,i2)=0.0
          
               enddo
               enddo
          enddo  ! NFIELDS
      
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
      
           do i = 1, NFIELDS 
               call ESMF_FieldGet(field180(i), lDE, farrayPtr, computationalLBound=fclbnd, &
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
            enddo
        enddo    ! lDE
      
        !!! Regrid forward from the 0 to 360 grid to the -180 to 180 grid
        ! Regrid store
        call ESMF_FieldBundleRegridStore(srcFieldBundle360, dstFieldBundle=fieldBundle180, &
                routeHandle=routeHandle, &
                regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
      
        ! Do regrid
        call ESMF_FieldBundleRegrid(srcFieldBundle360, fieldBundle180, routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        call ESMF_FieldBundleRegridRelease(routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        !!!!!!!! Regrid back from the -180 to 180 grid to the 0 to 360 grid
        ! Regrid store
        call ESMF_FieldBundleRegridStore(fieldBundle180, dstFieldBundle=dstFieldBundle360, &
                routeHandle=routeHandle, &
                regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
      
        ! Do regrid
        call ESMF_FieldBundleRegrid(fieldBundle180, dstFieldBundle360, routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        call ESMF_FieldBundleRegridRelease(routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        ! Check if the values are close
        do lDE=0,localDECount-1
      
         do i = 1, NFIELDS
           ! get src Field
           call ESMF_FieldGet(srcField360(i), lDE, farrayPtr, computationalLBound=clbnd, &
                                   computationalUBound=cubnd,  rc=localrc)
           if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
           endif
      
      
           ! get src destination Field
           call ESMF_FieldGet(dstField360(i), lDE, farrayPtr2,  rc=localrc)
           if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
           endif
      
      
           ! get src Field
           call ESMF_FieldGet(errorField(i), lDE, errorfarrayPtr,  rc=localrc)
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
         enddo   ! NFIELDS 
        enddo    ! lDE
      
      
        ! Uncomment these calls to see some actual regrid results
      
        ! Destroy the Fields
        do i = 1, NFIELDS
         call ESMF_FieldDestroy(srcField360(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
      
         call ESMF_FieldDestroy(dstField360(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
      
      
         call ESMF_FieldDestroy(errorField(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
      
         call ESMF_FieldDestroy(field180(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
        enddo
      
        call ESMF_FieldBundleDestroy(srcFieldBundle360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
        endif
        call ESMF_FieldBundleDestroy(dstFieldBundle360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
        endif
        call ESMF_FieldBundleDestroy(fieldBundle180, rc=localrc)
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
      
      end subroutine test_regrid180vs360_bundle

      subroutine test_regridSphSrcMask(rc)
        integer, intent(out)  :: rc
  logical :: correct
  integer :: localrc
  type(ESMF_Grid) :: gridA
  type(ESMF_Grid) :: gridB 
  type(ESMF_FieldBundle) :: fieldBundleA
  type(ESMF_FieldBundle) :: fieldBundleB
  integer, parameter :: numFields=6
  integer :: f
  type(ESMF_Field) :: fieldA(numFields)
  type(ESMF_Field) :: fieldB(numFields)
  type(ESMF_Array) :: arrayB
  type(ESMF_Array) :: arrayBPAtch
  type(ESMF_Array) :: lonArrayA
  type(ESMF_Array) :: srcArrayA
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandleTile
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  integer(ESMF_KIND_I4), pointer :: maskB(:,:), maskA(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrTile(:,:)
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
  
  character(len=1) :: xstring
  
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
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
    rc=ESMF_FAILURE
    return
  endif


  ! setup dest. grid
  gridB=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/B_nx,B_ny/),regDecomp=(/1,petCount/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
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


  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)

   ! Create Field List
   do f=1,numFields
      write(xstring, '(i1)') f
      fieldA(f) = ESMF_FieldCreate(gridA, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source"//xstring, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif
   enddo

   ! Create Field List
   do f=1,numFields
      write(xstring, '(i1)') f
      fieldB(f) = ESMF_FieldCreate(gridB, arrayspec, &
           staggerloc=ESMF_STAGGERLOC_CENTER, name="dest"//xstring, rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif
   enddo


   ! Make FieldBundles
   fieldBundleA = ESMF_FieldBundleCreate(fieldList=fieldA, rc=localrc)        
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif

   ! Make FieldBundles
   fieldBundleB = ESMF_FieldBundleCreate(fieldList=fieldB, rc=localrc)        
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

  
  ! Construct Grid A
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

     ! set coords, mask value
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
           else
              maskA(i1,i2) = 0
           endif
        enddo
     enddo

     ! get Field pointers and init
     do f=1,numFields
        call ESMF_FieldGet(fieldA(f), lDE, farrayPtr,  rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
        endif

        !! set data based on mask
        do i1=clbnd(1),cubnd(1)
           do i2=clbnd(2),cubnd(2)
              if (maskA(i1,i2) == 2) then
                 farrayPtr(i1,i2) = -1000.0 
              else
                 farrayPtr(i1,i2) = 20.0 
              endif
           enddo
        enddo
     enddo ! f=1,numFields

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

     !! set coords
     do i1=clbnd(1),cubnd(1)
        do i2=clbnd(2),cubnd(2)

           ! Set source coordinates as 0 to 360
           farrayPtrXC(i1,i2) = REAL(i1-1)*B_dx
           farrayPtrYC(i1,i2) = -90. + (REAL(i2-1)*B_dy + 0.5*B_dy)
        enddo
     enddo



     ! get Field pointers and init
     do f=1,numFields

        call ESMF_FieldGet(fieldB(f), lDE, farrayPtr,  rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
        endif

        ! Set to 0.0
        farrayPtr(:,:) = 0.0 

     enddo ! f=1,numFields
  enddo    ! lDE


  !!! Regrid forward from the A grid to the B grid
  ! Regrid store
  call ESMF_FieldBundleRegridStore( &
          fieldBundleA, srcMaskValues=(/1,2/), &
          dstFieldBundle=fieldBundleB, &
          unmappedaction=ESMF_UNMAPPEDACTION_IGNORE, &
          routeHandle=routeHandle, &
          regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
          rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  ! Do regrid
  call ESMF_FieldBundleRegrid(fieldBundleA, fieldBundleB, routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

  call ESMF_FieldBundleRegridRelease(routeHandle, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
   endif

   
   ! Check if we're using any of the bad source points
   do lDE=0,localDECount-1

      do f=1,numFields
         call ESMF_FieldGet(fieldB(f), lDE, farrayPtr, computationalLBound=clbnd, &
              computationalUBound=cubnd,  rc=localrc)
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
            enddo
         enddo
      enddo ! f=1,numFields

   enddo    ! lDE





  ! Destroy the FieldBundles
   call ESMF_FieldBundleDestroy(fieldBundleA, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   call ESMF_FieldBundleDestroy(fieldBundleB, rc=localrc)
   if (localrc /=ESMF_SUCCESS) then
     rc=ESMF_FAILURE
     return
   endif

   
   ! Destroy the Fields
   do f=1, numFields
      call ESMF_FieldDestroy(fieldA(f), rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif

      call ESMF_FieldDestroy(fieldB(f), rc=localrc)
      if (localrc /=ESMF_SUCCESS) then
         rc=ESMF_FAILURE
         return
      endif
   enddo


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

      subroutine test_regrid180vs360_bundleget(rc)
        integer, intent(out)  :: rc
        logical :: correct
        integer :: localrc
        type(ESMF_Grid) :: grid360
        type(ESMF_Grid) :: grid180
        type(ESMF_FieldBundle) :: srcFieldBundle360
        type(ESMF_FieldBundle) :: dstFieldBundle360
        type(ESMF_FieldBundle) :: fieldBundle180
        type(ESMF_Field) :: srcField360(6)
        type(ESMF_Field) :: dstField360(6)
        type(ESMF_Field) :: field180(6)
        type(ESMF_Field) :: errorField(6)
        type(ESMF_Array) :: lonArray360
        type(ESMF_RouteHandle) :: routeHandle
        type(ESMF_ArraySpec) :: arrayspec
        type(ESMF_VM) :: vm
        real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
        real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
        real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:),errorfarrayPtr(:,:)
        integer :: clbnd(2),cubnd(2)
        integer :: fclbnd(2),fcubnd(2)
        integer :: i1,i2
        integer :: lDE, localDECount
        character(len=1) :: xstring
        integer src_nx, src_ny, dst_nx, dst_ny
        integer :: num_arrays, NFIELDS=6, i
      
        real(ESMF_KIND_R8) :: src_dx, src_dy
        real(ESMF_KIND_R8) :: dst_dx, dst_dy
        real(ESMF_KIND_R8) :: theta, x, y, z
        real(ESMF_KIND_R8) :: DEG2RAD, lat, lon, phi
        real(ESMF_KIND_R8) :: rangle
        real(ESMF_KIND_R8) :: RAD2DEG
      
      
        integer :: localPet, petCount
        character(1)      :: srcid(6)=(/'L', 'G', 'X', 'A', 'C', 'H'/)
        character(1)      :: dstid(6)=(/'B', 'Y', 'T', 'S', 'P', 'N'/)
        character(1)      :: midid(6)=(/'J', 'U', 'E', 'X', 'Q', 'Y'/)
        !character(1)      :: srcid(6)=(/'A', 'B', 'C', 'D', 'E', 'F'/)
        !character(1)      :: dstid(6)=(/'A', 'B', 'C', 'D', 'E', 'F'/)
        !character(1)      :: midid(6)=(/'A', 'B', 'C', 'D', 'E', 'F'/)
        character(64)     :: sfname, dfname
      
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
                                    indexflag=ESMF_INDEX_GLOBAL, &
                                    rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
      
      
        ! setup dest. grid
        grid180=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                                    indexflag=ESMF_INDEX_GLOBAL, &
                                    rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
      
      
        ! Create source/destination fields
        call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
      
        do i = 1, NFIELDS
             write(xstring, '(i1)') i
             srcField360(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="src360_"//srcid(i), rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
             dstField360(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="dst360_"//dstid(i), rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
             errorField(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="error"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
          
             field180(i) = ESMF_FieldCreate(grid180, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="dst180_"//midid(i), rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
        enddo 

        srcFieldBundle360 = ESMF_FieldBundleCreate(fieldList=srcField360, rc=localrc)        
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
        dstFieldBundle360 = ESMF_FieldBundleCreate(fieldList=dstField360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
        fieldBundle180 = ESMF_FieldBundleCreate(fieldList=field180, rc=localrc)
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
      
           do i = 1, NFIELDS
               ! get src pointer
               call ESMF_FieldGet(srcField360(i), lDE, farrayPtr, computationalLBound=fclbnd, &
                                       computationalUBound=fcubnd,  rc=localrc)
               if (localrc /=ESMF_SUCCESS) then
                  rc=ESMF_FAILURE
                  return
               endif
          
          
               ! get src destination Field pointer
               call ESMF_FieldGet(dstField360(i), lDE, farrayPtr2,   rc=localrc)
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
                  farrayPtr(i1,i2) = x+y+z+15.0*i
          
                   ! initialize src destination field
                   farrayPtr2(i1,i2)=0.0
          
               enddo
               enddo
          enddo  ! NFIELDS
      
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
      
           do i = 1, NFIELDS 
               call ESMF_FieldGet(field180(i), lDE, farrayPtr, computationalLBound=fclbnd, &
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
            enddo
        enddo    ! lDE
      
        !!! Regrid forward from the 0 to 360 grid to the -180 to 180 grid
        ! Regrid store
        call ESMF_FieldBundleRegridStore(srcFieldBundle360, dstFieldBundle=fieldBundle180, &
                routeHandle=routeHandle, &
                regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
      
        ! Do regrid
        call ESMF_FieldBundleRegrid(srcFieldBundle360, fieldBundle180, routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        call ESMF_FieldBundleRegridRelease(routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        !!!!!!!! Regrid back from the -180 to 180 grid to the 0 to 360 grid
        ! Regrid store
        call ESMF_FieldBundleRegridStore(fieldBundle180, dstFieldBundle=dstFieldBundle360, &
                routeHandle=routeHandle, &
                regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
      
        ! Do regrid
        call ESMF_FieldBundleRegrid(fieldBundle180, dstFieldBundle360, routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        call ESMF_FieldBundleRegridRelease(routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        ! Check if the values are close
        do lDE=0,localDECount-1
      
         do i = 1, NFIELDS
           ! get src Field
           call ESMF_FieldGet(srcField360(i), lDE, farrayPtr, computationalLBound=clbnd, &
                                   computationalUBound=cubnd,  rc=localrc)
           if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
           endif
           call ESMF_FieldGet(srcField360(i), name=sfname,  rc=localrc)
           print *, 'utest src field name = ', sfname
      
      
           ! get src destination Field
           call ESMF_FieldGet(dstField360(i), lDE, farrayPtr2,  rc=localrc)
           if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
           endif
           call ESMF_FieldGet(dstField360(i), name=dfname,  rc=localrc)
           print *, 'utest dst field name = ', dfname
      
      
           ! get src Field
           call ESMF_FieldGet(errorField(i), lDE, errorfarrayPtr,  rc=localrc)
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
         enddo   ! NFIELDS 
        enddo    ! lDE
      
      
        ! Uncomment these calls to see some actual regrid results
      
        ! Destroy the Fields
        do i = 1, NFIELDS
         call ESMF_FieldDestroy(srcField360(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
      
         call ESMF_FieldDestroy(dstField360(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
      
      
         call ESMF_FieldDestroy(errorField(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
      
         call ESMF_FieldDestroy(field180(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
        enddo
      
        call ESMF_FieldBundleDestroy(srcFieldBundle360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
        endif
        call ESMF_FieldBundleDestroy(dstFieldBundle360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
        endif
        call ESMF_FieldBundleDestroy(fieldBundle180, rc=localrc)
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
      
      end subroutine test_regrid180vs360_bundleget

#undef ESMF_METHOD
#define ESMF_METHOD "test_regridLocStreamBundle"
 
      subroutine test_regridLocStreamBundle(rc)
        integer, intent(out)  :: rc
        logical :: correct
        integer :: localrc
        type(ESMF_Grid) :: grid360
        type(ESMF_LocStream) :: dstLocStream
        type(ESMF_FieldBundle) :: srcFieldBundle360
        type(ESMF_FieldBundle) :: dstFieldBundle
        type(ESMF_Field) :: srcField360(6)
        type(ESMF_Field) :: dstField(6)
        type(ESMF_Field) :: errorField(6)
        type(ESMF_Array) :: lonArray360
        type(ESMF_RouteHandle) :: routeHandle
        type(ESMF_ArraySpec) :: arrayspec
        type(ESMF_VM) :: vm
        real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
        real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
        real(ESMF_KIND_R8), pointer :: farrayPtr(:,:),farrayPtr2(:,:),errorfarrayPtr(:,:)
        real(ESMF_KIND_R8), pointer :: latArray(:),lonArray(:)
        real(ESMF_KIND_R8), pointer :: farrayPtr1D(:)
        integer :: clbnd(2),cubnd(2)
        integer :: fclbnd(2),fcubnd(2)
        integer :: i1,i2
        integer ::  numLocationsOnThisPet
        integer :: lDE, localDECount
        character(len=1) :: xstring
        integer src_nx, src_ny
        integer :: num_arrays, NFIELDS=6, i
      
        real(ESMF_KIND_R8) :: src_dx, src_dy
        real(ESMF_KIND_R8) :: theta, x, y, z, expected
        real(ESMF_KIND_R8) :: DEG2RAD, lat, lon, phi
        real(ESMF_KIND_R8) :: rangle
        real(ESMF_KIND_R8) :: RAD2DEG
      
      
        integer :: localPet, petCount
      
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
      
        src_nx = 90
        src_ny = 50
      
        ! setup source grid
        grid360=ESMF_GridCreate1PeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                                    indexflag=ESMF_INDEX_GLOBAL, &
                                    rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
      
      
        ! Create source/destination fields
        call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
      
        do i = 1, NFIELDS
             write(xstring, '(i1)') i
             srcField360(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="src360_"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
             errorField(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="error"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
        enddo 

        srcFieldBundle360 = ESMF_FieldBundleCreate(fieldList=srcField360, rc=localrc)        
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
      
        ! Get number of local DEs
        call ESMF_GridGet(grid360, localDECount=localDECount, rc=localrc)
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
      
           do i = 1, NFIELDS
               ! get src pointer
               call ESMF_FieldGet(srcField360(i), lDE, farrayPtr, computationalLBound=fclbnd, &
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
                  farrayPtr(i1,i2) = x+y+z+15.0*i
          
               enddo
               enddo
          enddo  ! NFIELDS
      
        enddo    ! lDE
      
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Destination LocStream
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Create a LocStream

        if (petCount .eq. 1) then
          numLocationsOnThisPet=7
        else
          if (localpet .eq. 0) then
            numLocationsOnThisPet=2
          else if (localpet .eq. 1) then
            numLocationsOnThisPet=2
          else if (localpet .eq. 2) then
            numLocationsOnThisPet=2
          else if (localpet .eq. 3) then
            numLocationsOnThisPet=1
          endif
        endif

        dstLocStream=ESMF_LocStreamCreate(name="Global Temperatures", &
                                   localCount=numLocationsOnThisPet, &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif

        !-------------------------------------------------------------------
        ! Add key data (internally allocating memory).
        !-------------------------------------------------------------------
        call ESMF_LocStreamAddKey(dstLocStream,                 &
                                  keyName="ESMF:Lat",           &
                                  KeyTypeKind=ESMF_TYPEKIND_R8, &
                                  keyUnits="degrees",           &
                                  keyLongName="Latitude", rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          print*,'ERROR:  trouble adding LocStream key for latitude'
          rc=ESMF_FAILURE
          return
        endif
        call ESMF_LocStreamAddKey(dstLocStream,                 &
                                  keyName="ESMF:Lon",           &
                                  KeyTypeKind=ESMF_TYPEKIND_R8, &
                                  keyUnits="degrees",           &
                                  keyLongName="Longitude", rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          print*,'ERROR:  trouble adding LocStream key for longitude'
          rc=ESMF_FAILURE
          return
        endif
      
      
        !-------------------------------------------------------------------
        ! Get key data.
        !-------------------------------------------------------------------
        call ESMF_LocStreamGetKey(dstLocStream,                    &
                                  localDE=0,                    &
                                  keyName="ESMF:Lat",                &
                                  farray=latArray,                   &
                                  rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          print*,'ERROR:  trouble getting LocStream key for latitude'
          rc=ESMF_FAILURE
          return
        endif
        call ESMF_LocStreamGetKey(dstLocStream,                    &
                                  localDE=0,                    &
                                  keyName="ESMF:Lon",                &
                                  farray=lonArray,                   &
                                  rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          print*,'ERROR:  trouble getting LocStream key for longitude'
          rc=ESMF_FAILURE
          return
        endif

       !-------------------------------------------------------------------
        ! Set key data.
        !-------------------------------------------------------------------
        if (petCount .eq. 1) then
          latArray = (/-87.75, -56.25, -26.5, 0.0, 26.5, 56.25, 87.75 /)
          lonArray = (/51.4, 102.8, 154.2, 205.6, 257.0, 308.4, 359.8 /)
        else
          if (localpet .eq. 0) then
            latArray = (/ -87.75, -56.25 /)
            lonArray = (/ 51.4, 102.8 /)
          else if (localpet .eq.1) then
            latArray = (/ -26.5, 0.0 /)
            lonArray = (/ 154.2, 205.6 /)
          else if (localpet .eq.2) then
            latArray = (/ 26.5, 56.25 /)
            lonArray = (/ 257.0, 308.4 /)
          else if (localpet .eq.3) then
            latArray = (/ 87.75 /)
            lonArray = (/ 359.8 /)
          endif
        endif

        ! Set ArraySpec
        call ESMF_ArraySpecSet(arrayspec, rank=1, typekind=ESMF_TYPEKIND_R8, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif

        ! Create a couple of Fields
        do i = 1, NFIELDS
          dstField(i)=ESMF_FieldCreate(locstream=dstLocStream, arrayspec=arrayspec, rc=localrc)
          if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
          endif
        enddo

        ! clear destination Fields
        do lDE=0,localDECount-1
          do i = 1, NFIELDS
            call ESMF_FieldGet(dstField(i), lDE, farrayPtr1D,  rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
            farrayPtr1D=0.0
          enddo
        enddo

        ! Try creating a bundle of these
        dstFieldBundle=ESMF_FieldBundleCreate(fieldList=dstField,rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
      
        !!! Regrid forward from the 0 to 360 grid to the LocStream
        ! Regrid store
        call ESMF_FieldBundleRegridStore(srcFieldBundle360, dstFieldBundle=dstFieldBundle, &
                routeHandle=routeHandle, &
                regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        ! Do regrid
        call ESMF_FieldBundleRegrid(srcFieldBundle360, dstFieldBundle, routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif
      
        call ESMF_FieldBundleRegridRelease(routeHandle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif


        ! Check if the values are close
        do lDE=0,localDECount-1
      
         do i = 1, NFIELDS
      
           ! get destination Field
           call ESMF_FieldGet(dstField(i), lDE, farrayPtr1D,  rc=localrc)
           if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
           endif
      
           ! loop through nodes and make sure interpolated values are reasonable
           do i1=1,numLocationsOnThisPet
             lon=lonArray(i1)
             lat=latArray(i1)

             ! get the x,y,z coordinates
             theta = DEG2RAD*(lon)
             phi = DEG2RAD*(90.-lat)
             x = cos(theta)*sin(phi)
             y = sin(theta)*sin(phi)
             z = cos(phi)

             ! determine validation data
             expected = x+y+z+15.0*i

             ! if error is too big report an error
             if ( abs( farrayPtr1D(i1)-(expected) )/expected > 0.001) then
               print*,'ERROR: larger than expected difference, expected ',expected, &
                      '  got ',farrayPtr1D(i1),'  diff= ',abs(farrayPtr1D(i1)-expected), &
                      '  rel diff= ',abs(farrayPtr1D(i1)-expected)/expected
               correct=.false.
             endif
           enddo


         enddo   ! NFIELDS 
        enddo    ! lDE

      
        ! Destroy the Fields
        do i = 1, NFIELDS
         call ESMF_FieldDestroy(srcField360(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif

         call ESMF_FieldDestroy(dstField(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
      
         call ESMF_FieldDestroy(errorField(i), rc=localrc)
         if (localrc /=ESMF_SUCCESS) then
           rc=ESMF_FAILURE
           return
         endif
      
        enddo
      
        call ESMF_FieldBundleDestroy(srcFieldBundle360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
        endif

        call ESMF_FieldBundleDestroy(dstFieldBundle, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
        endif
      
        ! Free the grid
        call ESMF_GridDestroy(grid360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
            rc=ESMF_FAILURE
            return
         endif

        ! Destroy LocStream
        call ESMF_LocStreamDestroy(dstLocStream, rc=localrc)
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
      
      end subroutine test_regridLocStreamBundle


#endif

end program ESMF_FieldBundleRegridUTest
