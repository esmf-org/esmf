! $Id: ESMF_FieldRegridEx.F90,v 1.28.2.1 2010/02/05 19:55:38 svasquez Exp $
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
program ESMF_FieldRegridEx

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================




!------------------------------------------------------------------------------

#include "ESMF_Macros.inc"

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
    '$Id: ESMF_FieldRegridEx.F90,v 1.28.2.1 2010/02/05 19:55:38 svasquez Exp $'
!------------------------------------------------------------------------------
    

  ! individual test result code
  integer :: localrc, rc, petCount,localPet

  ! individual test failure message
  character(ESMF_MAXSTR) :: name

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
  integer :: clbnd(2),cubnd(2)
  integer :: fclbnd(2),fcubnd(2)
  integer :: i1,i2
  integer :: lDE, localDECount
  integer src_nx, src_ny, dst_nx, dst_ny
  integer num_arrays

  real(ESMF_KIND_R8) :: src_dx, src_dy
  real(ESMF_KIND_R8) :: dst_dx, dst_dy
  real(ESMF_KIND_R8) :: ctheta, stheta
  real(ESMF_KIND_R8) :: theta, d2rad, xtmp, x, y

  integer(ESMF_KIND_I4), pointer :: indicies(:,:)
  real(ESMF_KIND_R8), pointer    :: weights(:)
  integer :: spherical_grid

  ! result code
  integer :: finalrc

  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  !-----------------------------------------------------------------------------
  !NEX_Ex
  write(name, *) "Test GridToMesh"

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
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  gridDst=ESMF_GridCreateShapeTile(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                              gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,0/), &
                              indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   srcField = ESMF_FieldCreate(gridSrc, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

   dstField = ESMF_FieldCreate(gridDst, arrayspec, &
                  staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
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

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


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
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)


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

!BOE
!\subsubsection{Field Regrid}
! 
! The Field regrid operation moves data between Fields which lie on different Grids. In order to do this the data in
! the source Field is interpolated to the destination Grid and then put into the destination Field. In ESMF the regrid operation
! is implemented as a sparse matrix multiply. The {\tt ESMF\_FieldRegridStore()} call generates the sparse matrix for
! the regrid operation. This matrix may be either retrieved in a factor and index raw form, or may be retrieved in the form
! of a routeHandle which contains an internal representation of the communication and mathermatical operations necessary to 
! perform the regrid. The routeHandle can then be used in an {\tt ESMF\_FieldRegrid()} call to perform the interpolation
! between two Fields. Note that the operation can be multiple times using the same routeHandle, and may be performed between
! any two fields which lie on the same Grids as were used to generate the routeHandle. When its no longer needed the routeHandle
! should be destroyed by using {\tt ESMF\_FieldRegridRelease()} to free the memory it's using.  
!
! There are two options for accessing ESMF regridding functionality: online and offline.  Online regridding means that the 
! weights are generated via subroutine calls during the execution of the users code. This is the method described in the following 
! sections. Offline regridding means that the weights are generated by a seperate application from the user code. Please see 
! Section~\ref{sec:regrid:offline} for a description of the offline regridding application and the options it supports.
! 
! ESMF currently supports regridding only on a subset of the full range of Grids and Meshes it supports. 
! 
!
! In 2D ESMF supports regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of any combination of triangles and quadralaterals (e.g. rectangles)
! \end{itemize}
! In addition the user may use the {\tt ESMF\_REGRID\_SCHEME\_FULL3D}  option in {\tt ESMF\_FieldRegridStore()} to
! map two single patch logically rectangular Grids onto the sphere and regrid between them in that representation. 
!
!
! In 3D ESMF supports regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of hexahedrons (e.g. cubes). 
! \end{itemize}
! Note that regridding involving tetrahedra is currently NOT supported. 
!
!
! In terms of masking, ESMF regrid currently supports masking for Fields built on structured Grids. The user may mask out points in 
! the source Field or destination Field or both. The user also has the option to return an error for unmapped destination points or
! to ignore them. At this point ESMF does not support extrapolation to destination points outside the unmasked source Field. 
!
! ESMF currently supports two options for interpolation: bilinear and patch. Bilinear interpolation calculates the value for the 
! destination point as a combination of multiple linear interpolations, one for each dimension of the Grid. Note that for ease of 
! use, the term bilinear interpolation is used for 3D interpolation in ESMF as well, although it should more properly be referred 
! to as trilinear interpolation.
!
! Patch (or higher-order) interpolation is the ESMF version of a techique called ``patch recovery'' commonly
! used in finite element modeling~\cite{PatchInterp1}~\cite{PatchInterp2}. It typically results in better approximations to 
! values and derivatives when compared to bilinear interpolation.
! Patch interpolation works by constructing multiple polynomial patches to represent
! the data in a source cell. For 2D grids, these polynomials
! are currently 2nd degree 2D polynomials. One patch is constructed for each corner of the source cell, and the patch is constructed 
! by doing a least squared fit through the data in the cells surrounding the corner. The interpolated value at the destination point is 
! then a weighted average of the values of the patches at that point.
!
! The following sections give examples of using the regridding functionality.
!
!\subsubsection{Creating a Regrid Operator from two Fields}
! To create the sparse matrix regrid operator we call the
! {\tt ESMF\_FieldRegridStore()} routine.  In this example we
! choose the {\tt ESMF\_REGRID\_METHOD\_BILINEAR} regridding method.  Other
! methods are available and more we will be added in the future.
! This method creates two meshes, and a Rendezvous decomposition of these
! meshes is computed.  An octree search is performed, followed by a determination
! of which source cell each destination gridpoint is in.  Bilinear weights
! are then computed locally on each cell.  This matrix of weights is, finally,
! sent back to the destination grid's row decomposition and declared as a 
! sparse matrix.  This matrix is embedded in the routeHandle object.
!EOE

!BOC
  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
                  routeHandle=routeHandle, &
                  indicies=indicies, weights=weights, &
                  regridMethod=ESMF_REGRID_METHOD_BILINEAR, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
!
!\subsubsection{Applying the Regrid Operator to a pair of Fields}
! The {\tt ESMF\_FieldRegrid} subroutine calls {\tt ESMF\_ArraySparseMatMul}
! and performs a regrid from source to destination field.
!EOE

  ! Test the regrid application
!BOC
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

!BOE
! 
!\subsubsection{Release a Regrid Operator}
!EOE

!BOC
  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  ! Write results to a mesh
  num_arrays = 1
  spherical_grid = 0

  ! Uncomment these calls to see some actual regrid results
  call ESMF_MeshIO(vm, GridSrc, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, &
               spherical=spherical_grid, rc=localrc)
  call ESMF_MeshIO(vm, Griddst, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, &
               spherical=spherical_grid, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(terminationflag=ESMF_ABORT)

  call ESMF_GridDestroy(gridSrc, rc=localrc)

  call ESMF_GridDestroy(gridDst, rc=localrc)


  ! Uncomment print statement to print the weights
  if (associated(indicies)) then
    do i1 = 1, size(indicies,1)

    !print *, indicies(i1,1), indicies(i1,2) , ':', weights(i1)
    
    enddo
  endif


10   continue
  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_FieldRegridEx.F90"
  else
    print *, "FAIL: ESMF_FieldRegridEx.F90"
  endif

end program ESMF_FieldRegridEx
