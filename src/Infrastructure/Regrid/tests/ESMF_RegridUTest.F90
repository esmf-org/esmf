! $Id: ESMF_RegridUTest.F90,v 1.26 2008/04/05 03:38:52 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_RegridUTest

!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_RegridUTest - Test the regrid
!
! !DESCRIPTION:
!
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_TestMod     ! test methods
  use ESMF_Mod
  use ESMF_RegridMod

  use ESMF_FieldGetMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_RegridUTest.F90,v 1.26 2008/04/05 03:38:52 cdeluca Exp $'
!------------------------------------------------------------------------------
    
  ! cumulative result: count failures; no failures equals "all pass"
  integer :: result = 0

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: name, failMsg

  logical :: correct
  type(ESMF_Grid) :: gridSrc
  type(ESMF_Grid) :: gridDst
  type(ESMF_Field) :: srcField
  type(ESMF_Field) :: dstField
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_ArraySpec) :: arrayspec
  type(ESMF_VM) :: vm
  real(ESMF_KIND_R8), pointer :: fptrXC(:,:)
  real(ESMF_KIND_R8), pointer :: fptrYC(:,:)
  real(ESMF_KIND_R8), pointer :: fptr(:,:)
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
  real(ESMF_KIND_R8) :: theta, d2rad, xtmp, x, y

  integer, pointer :: larrayList(:)

  !-----------------------------------------------------------------------------
  call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
  !-----------------------------------------------------------------------------

  ! get global VM
  call ESMF_VMGetGlobal(vm, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_UTest
  write(name, *) "Test GridToMesh"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! Establish the resolution of the grids
  src_nx = 100;
  src_ny = 100;

  dst_nx = 75;
  dst_ny = 50;

  ! Source mesh covers [0,1]x[0,2]
  src_dx = 1. / (REAL(src_nx)+1.)
  src_dy = 1. / (REAL(src_ny)+1.)

  dst_dx = 0.5 / (REAL(dst_nx)+1.)
  dst_dy = 0.5 / (REAL(dst_ny)+1.)

  ! if petCount >1, setup petMap
  gridSrc=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  write(failMsg, *) "ESMF_GridCreateShapeTile fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  gridDst=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  write(failMsg, *) "ESMF_GridCreateShapeTile fail"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcField = ESMF_FieldCreate(gridSrc, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=rc)
  write(failMsg, *) "ESMF_FieldCreate"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

   dstField = ESMF_FieldCreate(gridDst, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=rc)
  write(failMsg, *) "ESMF_FieldCreate"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! Allocate coordinates
  call ESMF_GridAllocCoord(gridSrc, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridAllocCoord(gridDst, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get number of local DEs
  call ESMF_GridGet(gridSrc, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    
  write(failMsg, *) "ESMF_FieldGet"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    
  write(failMsg, *) "ESMF_FieldGet"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10




  ! Get memory and set coords for src
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridSrc, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    
     call ESMF_GridGetCoord(gridSrc, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

      call ESMF_FieldGetDataPtr(srcField, fptr, lDE, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
      write(failMsg, *) "ESMF_FieldGetDataPtr"
      call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      if (localrc .ne. ESMF_SUCCESS) goto 10

    write(*,*) lDE," ::",clbnd,":",cubnd
    write(*,*) lDE," ::",fclbnd,":",fcubnd

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        fptrXC(i1,i2) = REAL((i1-1)*src_dx)
        fptrYC(i1,i2) = REAL((i2-1)*src_dx)
        x = fptrXC(i1, i2)
        y = fptrYC(i1,i2)
     
       ! Function
        fptr(i1, i2) = sin(x*10*3.145)+cos(y*4*3.145)
     enddo
     enddo

  enddo    ! lDE

  ! Get number of local DEs
  call ESMF_GridGet(gridDst, localDECount=localDECount, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE


  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

     call ESMF_FieldGetDataPtr(dstField, fptr, lDE, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     write(failMsg, *) "ESMF_FieldGetDataPtr"
     call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
     if (localrc .ne. ESMF_SUCCESS) goto 10

    write(*,*) lDE," ::",clbnd,":",cubnd
    write(*,*) lDE," ::",fclbnd,":",fcubnd

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords 
     d2rad = 0.01745329251994329547
     theta = 45.

     ctheta = cos(theta*d2rad)
     stheta = sin(theta*d2rad)
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        x = REAL((i1-1)*dst_dx)
        y = REAL((i2-1)*dst_dy)
        fptrXC(i1,i2) = x-0.25
        !fptrYC(i1,i2) = REAL((i2-1)*dst_dy)-0.1*cos(fptrYC(i1,i2)*2.*3.145/0.5)*cos(fptrXC(i1,i2)*2*3.145/0.5)-0.25
        fptrYC(i1,i2) = y-0.03*cos(y*3.145/0.5)*cos(x*2*3.145/0.5)-0.25

        !! Now apply the transformation
        xtmp = fptrXC(i1,i2)
        fptrXC(i1,i2) = ctheta*fptrXC(i1,i2)-stheta*fptrYC(i1,i2)+0.5
        fptrYC(i1,i2) = stheta*xtmp+ctheta*fptrYC(i1,i2)+0.5
        fptr(i1,i2) = 0.    ! set destination field to zero
     enddo
     enddo

     ! Set field values

  enddo    ! lDE


  call ESMF_RegridCreate(srcField, dstField, routeHandle, localrc)
  write(failMsg, *) "RegridCreate"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! Test the sparse matrix multiply
  call ESMF_ArraySparseMatMul(srcArray=srcArray, dstArray=dstArray, &
      routehandle=routeHandle, rc=rc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

   ! Check the results
  ! Get number of local DEs
!  call ESMF_GridGet(gridDst, localDECount=localDECount, rc=localrc)
  !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  ! Get memory and set coords for dst
  !do lDE=0,localDECount-1
    !call ESMF_FieldGetDataPtr(dstField, fptr, lDE, computationalLBound=fclbnd, &
                              !computationalUBound=fcubnd,  rc=localrc)
    !write(failMsg, *) "ESMF_FieldGetDataPtr"
    !call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    !if (localrc .ne. ESMF_SUCCESS) goto 10
 !
    !do i1=fclbnd(1),fcubnd(1)
    !do i2=fclbnd(2),fcubnd(2)
       !x = REAL((i1-1)*dst_dx)
       !y = REAL((i2-1)*dst_dy)
       !print *, i1, i2, ':', fptr(i1,i2)
    !enddo
    !enddo
 !
  !enddo ! lDe

  ! Write results to a mesh
  num_arrays = 1
  write(name, *) "srcmesh"
  call c_ESMC_MeshIO(vm, GridSrc, ESMF_STAGGERLOC_CENTER, &
               num_arrays, name, localrc, &
               srcArray)
  write(name, *) "dstmesh"
  call c_ESMC_MeshIO(vm, Griddst, ESMF_STAGGERLOC_CENTER, &
               num_arrays, name, localrc, &
               dstArray)


  call ESMF_GridDestroy(gridSrc, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

  call ESMF_GridDestroy(gridDst, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    


10   continue


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
end program ESMF_RegridUTest
