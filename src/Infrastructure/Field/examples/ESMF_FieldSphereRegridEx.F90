! $Id: ESMF_FieldSphereRegridEx.F90,v 1.5 2008/04/29 20:11:11 dneckels Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
program ESMF_FieldSphereRegridEx

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================




!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldSphereRegridEx - Test the regrid
!
! !DESCRIPTION:
!
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_Mod
  use ESMF_TestMod     ! test methods
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_GridUtilMod

  use ESMF_FieldGetMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_FieldSphereRegridEx.F90,v 1.5 2008/04/29 20:11:11 dneckels Exp $'
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
  real(ESMF_KIND_R8) :: theta, d2rad, x, y, z
  real(ESMF_KIND_R8) :: DEG2RAD, a, lat, lon, phi
  real(ESMF_KIND_R8) :: rangle, xtmp, ytmp, ztmp
  real(ESMF_KIND_R8) :: RAD2DEG

  integer :: spherical_grid

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
  !NEX_Ex
  write(name, *) "Test GridToMesh"
  write(failMsg, *) "Did not return ESMF_SUCCESS"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! Establish the resolution of the grids
  !dst_nx = 284
  dst_nx = 254
  dst_ny = 200

  src_nx = 75
  src_ny = 50

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
  call ESMF_GridAddCoord(gridSrc, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE

  call ESMF_GridAddCoord(gridDst, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
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


  ! Write results to a mesh
  num_arrays = 1

!
  !call ESMF_GridDestroy(gridSrc, rc=localrc)
  !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    
!
  !call ESMF_GridDestroy(gridDst, rc=localrc)
  !if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

! Test interpolation on the sphere
! Set the source grid coordinates to be a standard lat lon grid

  src_dx = 360./src_nx
  src_dy = 180./src_ny

  DEG2RAD = 3.14159265/180.0
  RAD2DEG = 1./DEG2RAD

  ! Get memory and set coords for src
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridSrc, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    
     call ESMF_GridGetCoord(gridSrc, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

      call ESMF_FieldGet(srcField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
      write(failMsg, *) "ESMF_FieldGet"
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
        fptrXC(i1,i2) = -180. + REAL((i1-1)*src_dx)
        fptrYC(i1,i2) = -90. + REAL((i2-1)*src_dy + 0.5*src_dy)
        x = fptrXC(i1, i2)
        y = fptrYC(i1,i2)
     
       ! Function.  The X component of a windfield over a

        !fptr(i1, i2) = cos(2*x)*cos(a) + sin(2*x)*cos(3*y)*sin(a)
        theta = DEG2RAD*x
        phi = DEG2RAD*(90.-y)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        fptr(i1,i2) = sin(3*3.14*x) + cos(2*3.14*y) + z*z;


     enddo
     enddo

  enddo    ! lDE

  dst_dx = 360./dst_nx
  dst_dy = 180./dst_ny

  rangle = DEG2RAD*20.

  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    
      call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc .ne. ESMF_SUCCESS) rc=ESMF_FAILURE    

    write(*,*) lDE," ::",clbnd,":",cubnd
    write(*,*) lDE," ::",fclbnd,":",fcubnd

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        lon = -180. + REAL((i1-1)*dst_dx)
        lat = -90. + REAL((i2-1)*dst_dy + 0.5*dst_dy)

        ! to 3d
        theta = DEG2RAD*lon
        phi = DEG2RAD*(90.-lat)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        ! 3d rotation about x-axis
        ! x is preserved
        ytmp = cos(rangle)*y + sin(rangle)*z 
        ztmp = -sin(rangle)*y + cos(rangle)*z
        y = ytmp
        z = ztmp

        ! now back to lat lon
        fptrXC(i1,i2) = atan2(y,x)*RAD2DEG
        fptrYC(i1,i2) = asin(z)*RAD2DEG

        fptr(i1,i2) = 0.
     
     enddo
     enddo

  enddo    ! lDE


!BOE
!
!\subsubsection{Creating a Regrid Operator from two Fields}
! To create the sparse matrix regrid operator we call the
! {\tt ESMF\_FieldSphereRegridStore()} routine.  In this example we
! choose the {\tt ESMF_REGRID_METHOD_BILIONEAR} regridding method.  Other
! methods are available and more we will be added in the future.
! This method creates two meshes, and a Rendezvous decomposition of these
! meshes is computed.  An octree search is performed, followed by a determination
! of which source cell each destination gridpoint is in.  Bilinear weights
! are then computed locally on each cell.  This matrix of weights is, finally,
! sent back to the destination grid's row decomposition and declared as a 
! sparse matrix.  This matrix is embedded in the routeHandle object.
!EOE

!BOC
  routeHandle = ESMF_FieldRegridStore(srcField, dstField, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
!EOC

  write(failMsg, *) "FieldRegridStore"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

!BOE
!
!\subsubsection{Applying the Regrid Operator to a pair of Fields}
! The {\tt ESMF\_FieldSphereRegridRun} subroutine calls {\tt ESMF\_ArraySparseMatMul}
! and performs a regrid from source to destination field.
!EOE

  ! Test the regrid application
!BOC
  call ESMF_FieldRegridRun(srcField, dstField, routeHandle, localrc)
!EOC
  write(failMsg, *) "FieldRegridRun"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

!BOE
! 
!\subsubsection{Destroy a Regrid Operator}
!EOE

!BOC
  call ESMF_FieldRegridDestroy(routeHandle, rc=localrc)
!EOC
  write(failMsg, *) "FieldRegridDestroy"
  call ESMF_Test((localrc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
  if (localrc .ne. ESMF_SUCCESS) goto 10

  ! Uncomment these calls to see some actual regrid results
  spherical_grid = 1
!  call ESMF_MeshIO(vm, GridSrc, ESMF_STAGGERLOC_CENTER, &
!               "srcmesh", srcArray, rc=localrc, &
!               spherical=spherical_grid)
!  call ESMF_MeshIO(vm, Griddst, ESMF_STAGGERLOC_CENTER, &
!               "dstmesh", dstArray, rc=localrc, &
!               spherical=spherical_grid)


10   continue


  !-----------------------------------------------------------------------------
  call ESMF_TestEnd(result, ESMF_SRCLINE)
  !-----------------------------------------------------------------------------
end program ESMF_FieldSphereRegridEx
