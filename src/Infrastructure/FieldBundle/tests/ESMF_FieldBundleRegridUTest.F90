! $Id: ESMF_FieldBundleRegridUTest.F90,v 1.2 2010/11/03 04:58:48 theurich Exp $
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
    use ESMF_FieldBundleRegridMod
  
    implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
    character(*), parameter :: version = &
    '$Id: ESMF_FieldBundleRegridUTest.F90,v 1.2 2010/11/03 04:58:48 theurich Exp $'
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
        call ESMF_Finalize(terminationflag=ESMF_ABORT)

#ifdef ESMF_TESTEXHAUSTIVE

        !------------------------------------------------------------------------
        !EX_UTest_Multi_Proc_Only
        call test_regrid180vs360_bundle(rc)
        write(failMsg, *) ""
        write(name, *) "FieldBundleRegrid between a 0 to 360 sphere and a -180 to 180 sphere"
        call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
#endif
    call ESMF_TestEnd(result, ESMF_SRCLINE)

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
        real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
        real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
        real(ESMF_KIND_R8), pointer :: fptr(:,:),fptr2(:,:),errorfptr(:,:)
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
              if (ESMF_LogMsgFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rc)) return
      
        call ESMF_VMGet(vm, petCount=petCount, localPet=localpet, rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                  ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rc)) return
      
        ! Establish the resolution of the grids
      
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
      
        do i = 1, NFIELDS
             write(xstring, '(i1)') i
             srcField360(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="source"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
             dstField360(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="source"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
             errorField(i) = ESMF_FieldCreate(grid360, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="source"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
          
          
             field180(i) = ESMF_FieldCreate(grid180, arrayspec, &
                                   staggerloc=ESMF_STAGGERLOC_CENTER, name="dest"//xstring, rc=localrc)
            if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
            endif
        enddo 

        srcFieldBundle360 = ESMF_FieldBundleCreate(6, srcField360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
        dstFieldBundle360 = ESMF_FieldBundleCreate(6, dstField360, rc=localrc)
        if (localrc /=ESMF_SUCCESS) then
          rc=ESMF_FAILURE
          return
        endif
        fieldBundle180 = ESMF_FieldBundleCreate(6, field180, rc=localrc)
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
      
           do i = 1, NFIELDS
               ! get src pointer
               call ESMF_FieldGet(srcField360(i), lDE, fptr, computationalLBound=fclbnd, &
                                       computationalUBound=fcubnd,  rc=localrc)
               if (localrc /=ESMF_SUCCESS) then
                  rc=ESMF_FAILURE
                  return
               endif
          
          
               ! get src destination Field pointer
               call ESMF_FieldGet(dstField360(i), lDE, fptr2,   rc=localrc)
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
      
           do i = 1, NFIELDS 
               call ESMF_FieldGet(field180(i), lDE, fptr, computationalLBound=fclbnd, &
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
            enddo
        enddo    ! lDE
      
        !!! Regrid forward from the 0 to 360 grid to the -180 to 180 grid
        ! Regrid store
        call ESMF_FieldBundleRegridStore(srcFieldBundle360, dstFieldBundle=fieldBundle180, &
                routeHandle=routeHandle, &
                regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
                regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
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
                regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
                regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
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
           call ESMF_FieldGet(srcField360(i), lDE, fptr, computationalLBound=clbnd, &
                                   computationalUBound=cubnd,  rc=localrc)
           if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
           endif
      
      
           ! get src destination Field
           call ESMF_FieldGet(dstField360(i), lDE, fptr2,  rc=localrc)
           if (localrc /=ESMF_SUCCESS) then
              rc=ESMF_FAILURE
              return
           endif
      
      
           ! get src Field
           call ESMF_FieldGet(errorField(i), lDE, errorfptr,  rc=localrc)
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
#endif

end program ESMF_FieldBundleRegridUTest
