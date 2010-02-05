! $Id: ESMF_FieldSphereRegridEx.F90,v 1.22.2.1 2010/02/05 19:55:38 svasquez Exp $
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
program ESMF_FieldSphereRegridEx

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================




!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

!==============================================================================
!BOP
! !PROGRAM: ESMF_FieldSphereRegridEx - Example of spherical regridding
!
! !DESCRIPTION:
!
!
!-----------------------------------------------------------------------------
! !USES:
  use ESMF_Mod
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_GridUtilMod

  use ESMF_FieldGetMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_FieldSphereRegridEx.F90,v 1.22.2.1 2010/02/05 19:55:38 svasquez Exp $'
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
  type(ESMF_Field) :: dstField1
  type(ESMF_Array) :: dstArray
  type(ESMF_Array) :: dstArray1
  type(ESMF_Array) :: srcArray
  type(ESMF_RouteHandle) :: routeHandle
  type(ESMF_RouteHandle) :: routeHandle1
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

    ! result code
  integer :: finalrc
  
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_Ex
  write(name, *) "Sphere Regrid Example"

  ! init success flag
  correct=.true.
  rc=ESMF_SUCCESS

  ! Establish the resolution of the grids
  !dst_nx = 284
  dst_nx = 154
  dst_ny = 100

  src_nx = 75
  src_ny = 50

  ! if petCount >1, setup petMap
  gridSrc=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  gridDst=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)

   srcField = ESMF_FieldCreate(gridSrc, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   dstField = ESMF_FieldCreate(gridDst, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   dstField1 = ESMF_FieldCreate(gridDst, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="dest1", rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Allocate coordinates
  call ESMF_GridAddCoord(gridSrc, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridAddCoord(gridDst, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Get number of local DEs
  call ESMF_GridGet(gridSrc, localDECount=localDECount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_FieldGet(dstField1, array=dstArray1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  ! Write results to a mesh
  num_arrays = 1

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
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

     call ESMF_GridGetCoord(gridSrc, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

     call ESMF_FieldGet(srcField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

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
     
       ! Set the source to be a sinusoidal function of x,y,z

        theta = DEG2RAD*x
        phi = DEG2RAD*(90.-y)
        x = cos(theta)*sin(phi)
        y = sin(theta)*sin(phi)
        z = cos(phi)

        fptr(i1,i2) = sin(3*3.14*x) + cos(2*3.14*y) + z*z;


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
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrXC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, fptr=fptrYC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

     call ESMF_FieldGet(dstField, lDE, fptr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error dst clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error dst clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error dst cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error dst cubnd != fcubnd'

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
! choose the {\tt ESMF_REGRID_METHOD_BILIONEAR} regridding method for
! our first example, and {\tt ESMF_REGRID_METHOD_PATCH} for our
! second example (The underlying C++ code process both matrices
! simultaneously, but we do not yet have fortran interfaces for this).
! This method creates two meshes, and a Rendezvous decomposition of these
! meshes is computed.  An octree search is performed, followed by a determination
! of which source cell each destination gridpoint is in.  Bilinear weights
! are then computed locally on each cell.  This matrix of weights is, finally,
! sent back to the destination grid's row decomposition and declared as a 
! sparse matrix.  This matrix is embedded in the routeHandle object.
! the {\tt ESMF\_REGRID\_SCHEME\_FULL3D} option is used since the grid
! represents a full sphere.  This causes the grids to be represented
! as 3D manifolds, and avoids pole and branch cut issues usually
! associated with regridding on the sphere.
!EOE

!BOC
  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
          routeHandle=routeHandle, &
          regridMethod=ESMF_REGRID_METHOD_BILINEAR, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! The patch recovery interpolant
#ifdef ESMF_LAPACK
  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField1, &
          routeHandle=routeHandle1, &
          regridMethod=ESMF_REGRID_METHOD_PATCH, &
          regridScheme=ESMF_REGRID_SCHEME_FULL3D, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
#endif

!BOE
!
!\subsubsection{Applying the Regrid Operator to a pair of Fields}
! The {\tt ESMF\_FieldSphereRegrid} subroutine calls {\tt ESMF\_ArraySparseMatMul}
! and performs a regrid from source to destination field.
!EOE

  ! Test the regrid application
!BOC
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

#ifdef ESMF_LAPACK
  call ESMF_FieldRegrid(srcField, dstField1, routeHandle1, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
#endif

!BOE
! 
!\subsubsection{Release a Regrid Operator}
!EOE

!BOC
  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
#ifdef ESMF_LAPACK
  call ESMF_FieldRegridRelease(routeHandle1, rc=localrc)
#endif
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!  ! Uncomment these calls to see some actual regrid results
!  spherical_grid = 1
!  call ESMF_MeshIO(vm, GridSrc, ESMF_STAGGERLOC_CENTER, &
!               "srcmesh", srcArray, rc=localrc, &
!               spherical=spherical_grid)
!  call ESMF_MeshIO(vm, Griddst, ESMF_STAGGERLOC_CENTER, &
!               "dstmesh", dstArray, dstArray1, rc=localrc, &
!               spherical=spherical_grid)

   ! Free Fields
   call ESMF_FieldDestroy(srcField, rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   call ESMF_FieldDestroy(dstField, rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   call ESMF_FieldDestroy(dstField1, rc=localrc)
   if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


  ! Free the grids
  call ESMF_GridDestroy(GridSrc, rc=localrc)

  call ESMF_GridDestroy(GridDst, rc=localrc)

10   continue

  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_FieldSphereRegridEx.F90"
  else 
    print *, "FAIL: ESMF_FieldSphereRegridEx.F90" 
  endif



end program ESMF_FieldSphereRegridEx
