! $Id: ESMF_FieldRegridEx.F90,v 1.70 2012/08/20 21:19:07 oehmke Exp $
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
program ESMF_FieldRegridEx

!==============================================================================
!ESMF_MULTI_PROC_EXAMPLE        String used by test script to count examples.
!==============================================================================




!------------------------------------------------------------------------------
#include "ESMF.h"
#include "ESMF_Macros.inc"

! !USES:
  use ESMF
  use ESMF_TestMod     ! test methods
  use ESMF_RegridMod
  use ESMF_FieldMod
  use ESMF_GridUtilMod

  use ESMF_FieldGetMod

  implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter :: version = &
    '$Id: ESMF_FieldRegridEx.F90,v 1.70 2012/08/20 21:19:07 oehmke Exp $'
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
  real(ESMF_KIND_R8), pointer :: farrayPtrXC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtrYC(:,:)
  real(ESMF_KIND_R8), pointer :: farrayPtr(:,:)
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

  integer(ESMF_KIND_I4), pointer :: indices(:,:)
  real(ESMF_KIND_R8), pointer    :: weights(:)
  integer :: spherical_grid

  ! result code
  integer :: finalrc, result

  character(ESMF_MAXSTR) :: testname
  character(ESMF_MAXSTR) :: failMsg

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

  write(failMsg, *) "Example failure"
  write(testname, *) "Example ESMF_FieldRegridEx"


! ------------------------------------------------------------------------------
! ------------------------------------------------------------------------------


  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, defaultlogfilename="FieldRegridEx.Log", &
                    logkindflag=ESMF_LOGKIND_MULTI, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
  gridSrc=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/src_nx,src_ny/),regDecomp=(/petCount,1/), &
                               indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  gridDst=ESMF_GridCreateNoPeriDim(minIndex=(/1,1/),maxIndex=(/dst_nx,dst_ny/),regDecomp=(/1,petCount/), &
                               indexflag=ESMF_INDEX_GLOBAL, &
                              rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create source/destination fields
  call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc=rc)
  if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   srcField = ESMF_FieldCreate(gridSrc, arrayspec, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, name="source", rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

   dstField = ESMF_FieldCreate(gridDst, arrayspec, &
                  staggerloc=ESMF_STAGGERLOC_CENTER, name="dest", rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Allocate coordinates
  call ESMF_GridAddCoord(gridSrc, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridAddCoord(gridDst, staggerloc=ESMF_STAGGERLOC_CENTER, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get number of local DEs
  call ESMF_GridGet(gridSrc, localDECount=localDECount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Get arrays
  ! dstArray
  call ESMF_FieldGet(dstField, array=dstArray, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! srcArray
  call ESMF_FieldGet(srcField, array=srcArray, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  ! Get memory and set coords for src
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridSrc, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
     call ESMF_GridGetCoord(gridSrc, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

      call ESMF_FieldGet(srcField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
      if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (clbnd(1) .ne. fclbnd(1)) print *, 'Error clbnd != fclbnd'
    if (clbnd(2) .ne. fclbnd(2)) print *, 'Error clbnd != fclbnd'
    if (cubnd(1) .ne. fcubnd(1)) print *, 'Error cubnd != fcubnd'
    if (cubnd(2) .ne. fcubnd(2)) print *, 'Error cubnd != fcubnd'

     !! set coords, interpolated function
     do i1=clbnd(1),cubnd(1)
     do i2=clbnd(2),cubnd(2)
        farrayPtrXC(i1,i2) = REAL((i1-1)*src_dx)
        farrayPtrYC(i1,i2) = REAL((i2-1)*src_dx)
        x = farrayPtrXC(i1, i2)
        y = farrayPtrYC(i1,i2)
     
       ! Function
        farrayPtr(i1, i2) = sin(x*10*3.145)+cos(y*4*3.145)
     enddo
     enddo

  enddo    ! lDE

  ! Get number of local DEs
  call ESMF_GridGet(gridDst, localDECount=localDECount, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)


  ! Get memory and set coords for dst
  do lDE=0,localDECount-1
 
     !! get coord 1
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=1, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrXC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
     call ESMF_GridGetCoord(gridDst, localDE=lDE, staggerLoc=ESMF_STAGGERLOC_CENTER, coordDim=2, &
                            computationalLBound=clbnd, computationalUBound=cubnd, farrayPtr=farrayPtrYC, rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

     call ESMF_FieldGet(dstField, lDE, farrayPtr, computationalLBound=fclbnd, &
                             computationalUBound=fcubnd,  rc=localrc)
     if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

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
        farrayPtrXC(i1,i2) = x-0.25
        farrayPtrYC(i1,i2) = y-0.03*cos(y*3.145/0.5)*cos(x*2*3.145/0.5)-0.25

        !! Now apply the transformation
        xtmp = farrayPtrXC(i1,i2)
        farrayPtrXC(i1,i2) = ctheta*farrayPtrXC(i1,i2)-stheta*farrayPtrYC(i1,i2)+0.5
        farrayPtrYC(i1,i2) = stheta*xtmp+ctheta*farrayPtrYC(i1,i2)+0.5
        farrayPtr(i1,i2) = 0.    ! set destination field to zero
     enddo
     enddo

     ! Set field values

  enddo    ! lDE

!BOE
!\subsubsection{Field regridding}\label{sec:fieldregrid}
!
! This section describes the regridding methods provided by ESMF. Regridding, also called remapping or interpolation, is 
! the process of changing the grid that underlies data values while preserving qualities of the original data. Different 
! kinds of transformations are appropriate for different problems. Regridding may be needed when communicating data between
! Earth system model components such as land and atmosphere, or between different data sets to support operations such as visualization.
!
! Regridding can be broken into two stages. The first stage is generation of an interpolation weight matrix that describes how points in 
! the source grid contribute to points in the destination grid. The second stage is the multiplication of values on the source grid by the
! interpolation weight matrix to produce values on the destination grid. This occurs through a parallel sparse matrix multiply.
!
! There are two options for accessing ESMF regridding functionality: offline and integrated. Offline regridding is a process whereby interpolation 
! weights are generated by a separate ESMF application, not within the user code. The ESMF offline regridding application also only generates the interpolation 
! matrix, the user is responsible for reading in this matrix and doing the actual interpolation (multiplication by the sparse matrix) in their code.
! Please see Section~\ref{sec:ESMF_RegridWeightGen} for a description of the offline regridding application and the options it supports. 
! In constrast to offline regridding, integrated regridding is a process whereby interpolation weights are generated via subroutine calls during the
! execution of the user's code. The integrated regridding can also perform the parallel sparse 
! matrix multiply. In other words, ESMF integrated regridding allows a user to perform the whole process of interpolation within their code.
! The rest of this section further describes ESMF integrated regridding. Figure~\ref{Regriddingcapabilities} shows a comparison of the capabilities 
! of offline and integrated regridding. 
!
! The basic flow of using ESMF integerated regridding is as follows. First a source and destination grid object are created, both can be either a Grid or Mesh. 
! Coordinates are set during Mesh creation, but for the Grid they must be set separately using the {\tt ESMF\_GridAddCoord()} and {\tt ESMF\_GridGetCoord()} methods. 
! Next Fields are built on the source and destination grid objects. These Fields are then passed into {\tt ESMF\_FieldRegridStore()}. The user can either get a 
! sparse matrix from this call and/or a {\tt routeHandle}. If the user gets the sparse matrix then they are responsible for deallocating it, but other than that
! can use it as they wish. The {\tt routeHandle} can be used in the {\tt ESMF\_FieldRegrid()} call to perform the actual interpolation of data from the source 
! to the destination field. This interpolation can be repeated for the same set of Fields as long as the coordinates at the staggerloc involved in the
! regridding in the associated grid object don't change. The same {\tt routeHandle} can also be used between any pair of Fields which is weakly congruent 
! to the pair used to create the {\tt routeHandle}.  Congruent Fields possess matching DistGrids and the shape of the 
! local array tiles matches between the Fields for every DE. For weakly congruent Fields the sizes                                                               
! of the undistributed dimensions, that vary faster with memory than the first distributed dimension,                                                            
! are permitted to be different. This means that the same routehandle can be applied to a large class                                                            
! of similar Fields that differ in the number of elements in the left most undistributed dimensions.             
! You can apply the routehandle between any set of Fields weakly congruent to the original Fields used to create the routehandle without 
! incurring an error. However, if you want                                     
! the routehandle to be the same interpolation between the grid objects upon which the Fields are built as was calculated                                        
! with the original {\tt ESMF\_FieldRegridStore()} call, then there                                                                                              
! are additional constraints on the grid objects. To be the same interpolation, the grid objects upon which the                                                  
! Fields are build must contain the same coordinates at the stagger locations involved in the regridding as                                                      
! the original source and destination Fields used in the {\tt ESMF\_FieldRegridStore()} call.                                                                    
! The routehandle represents the interpolation between the grid objects as they were during the {\tt ESMF\_FieldRegridStore()} call.                             
! So if the coordinates at the stagger location in the grid objects change, a new call to {\tt ESMF\_FieldRegridStore()}                                         
! is necessary to compute the interpolation between that new set of coordinates. When finished with the {\tt routeHandle} 
! {\tt ESMF\_FieldRegridRelease()} should be used to 
! free the associated memory. 
!
! In the case that the Grid is on a sphere ({\tt coordSys=ESMF\_COORDSYS\_SPH\_DEG or ESMF\_COORDSYS\_SPH\_RAD})
! then the coordinates given in the Grid are interpretted as latitude and longitude values. The coordinates can either be in degrees or radians as indicated by the 
! {\tt coordSys} flag set during Grid creation. As is true with many global models, this application currently assumes the latitude and longitude refer to positions on a 
! perfect sphere, as opposed to a more complex and accurate representation of the earth's true shape such as would be used in a GIS system. (ESMF's current user base doesn't 
! require this level of detail in representing the earth's shape, but it could be added in the future if necessary.)
!
! For Grids on a sphere, the regridding occurs in 3D Cartesian to avoid
! problems with periodicity and with the pole singularity. This library
! supports four options for handling the pole region (i.e. the empty area above the top row of the source grid or below
! the bottom row of the source grid).  Note that all of these pole options currently only work for the Fields build on the Grid class and not for those built on 
! the Mesh class. The first option is to leave the pole region empty ({\tt polemethod=ESMF\_POLEMETHOD\_NONE}), in this 
! case if a destination point lies above or below the 
! top row of the source grid, it will fail to map, yielding an error (unless {\tt unmappedaction=ESMF\_UNMAPPEDACTION\_IGNORE} is specified).  
! With the next two options, the pole region is handled by constructing 
! an artificial pole in the center of the top and bottom row of grid points and then filling
! in the region from this pole to the edges of the source grid with triangles. 
! The pole is located at the average of the position of the points surrounding
! it, but moved outward to be at the same radius as the rest of the points
! in the grid. The difference between these two artificial pole options is what value is used at the pole. 
! The default pole option ({\tt polemethod=ESMF\_POLEMETHOD\_ALLAVG}) sets the value at the pole to be the average of the values
! of all of the grid points surrounding the pole. For the other option ({\tt polemethod=ESMF\_POLEMETHOD\_NPNTAVG}), the user chooses
! a number N from 1 to the number of source grid points around the pole. The value N is set via the argument {\tt regridPoleNPnts}. For
! each destination point, the value at the pole is then the average of the N source points
! surrounding that destination point. For the last pole option ({\tt polemethod=ESMF\_POLEMETHOD\_TEETH}) no artificial pole is constructed, instead the
! pole region is covered by connecting points across the top and bottom row of the source Grid into triangles. As 
! this makes the top and bottom of the source sphere flat, for a big enough difference between the size of
! the source and destination pole regions, this can still result in unmapped destination points.  
! Only pole option {\tt ESMF\_POLEMETHOD\_NONE} is currently supported with the conservative interpolation method 
!(i.e. {\tt regridmethod=ESMF\_REGRIDMETHOD\_CONSERVE}). 
!
! Masking is the process whereby parts of a grid can be marked to be ignored during an operation, such as regridding. Masking can be used on a source grid to 
! indicate that certain portions of the grid should not be used to generate regridded data. This is useful, for example, if a portion of source grid contains 
! unusable values. Masking can also be used on a destination grid to indicate that the portion of the field built on that part of the Grid should not receive 
! regridded data. This is useful, for example, when part of the grid isn't being used (e.g. the land portion of an ocean grid).
!
! ESMF currently supports masking for Fields built on structured Grids and element masking for Fields built on unstructured Meshes. The user may mask out points 
! in the source Field or destination Field or both. To do masking the user sets mask information in the Grid (see~\ref{sec:usage:items}) or 
! Mesh (see~\ref{sec:mesh:mask}) upon which the Fields passed into the 
! {\tt ESMF\_FieldRegridStore()} call are built. The `srcMaskValues' and `dstMaskValues' arguments to that call can then be used to specify which values in that mask 
! information indicate that a location should be masked out. For example, if `dstMaskValues' is set to (/1,2/), then any location that has a value of 1 or 2 in 
! the mask information of the Grid or Mesh upon which the destination Field is built will be masked out.
!
! Masking behavior differs slightly between regridding methods. For non-conservative regridding methods (e.g. bilinear or high-order patch), masking is done on
! points. For these methods, masking a destination point means that the point won't participate in regridding (e.g. won't be interpolated to). For these methods, 
! masking a source point means that the entire source cell using that point is masked out. In other words, if any corner point making up a source cell is masked 
! then the cell is masked. For conservative regridding methods (e.g. first-order conservative) masking is done on cells. Masking a destination cell means that the 
! cell won't participate in regridding (e.g. won't be interpolated to). Similarly, masking a source cell means that the cell won't participate in regridding 
! (e.g. won't be interpolated from). For any type of interpolation method (conservative or non-conservative) the masking is set on the location upon which the 
! Fields passed into the regridding call are built. For example, if Fields built on {\tt ESMF\_STAGGERLOC\_CENTER} are passed into the {\tt ESMF\_FieldRegridStore()} 
! call then the masking should also be set in {\tt ESMF\_STAGGERLOC\_CENTER}.
!
! If a destination point can't be mapped to a location in the source grid, the user has two options. The user may ignore those destination points
! that can't be mapped by setting the {\tt unmappedaction} argument to {\tt ESMF\_UNMAPPEDACTION\_IGNORE}. (Ignored points won't be included in
! the sparse matrix or routeHandle output from the {\tt ESMF\_FieldRegridStore()} call.)   The user also has the option to return
! an error if unmapped destination points exist. This is the default behavior, so the user can either not set the {\tt unmappedaction} argument
! or the user can set it to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. At this point ESMF does not support extrapolation to destination points outside 
! the unmasked source Field. 
!
! ESMF currently supports three options for interpolation: bilinear, patch, and conservative. 
! Bilinear interpolation calculates the value for the 
! destination point as a combination of multiple linear interpolations, one for each dimension of the Grid. Note that for ease of 
! use, the term bilinear interpolation is used for 3D interpolation in ESMF as well, although it should more properly be referred 
! to as trilinear interpolation.
!
!\smallskip
!
! In 2D, ESMF supports bilinear regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of any combination of triangles and quadrilaterals (e.g. rectangles)
! \end{itemize}
!
!\smallskip
!
! In 3D, ESMF supports bilinear regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of hexahedrons (e.g. cubes)
! \end{itemize}
!
!\smallskip
!
! To use the bilinear method the user may created their Fields on any stagger location for Grids or the node location ({\tt ESMF\_MESHLOC\_NODE}) for Meshes.
! For Grids, the stagger location upon which the Field was built must contain coordinates. 
!
! Patch (or higher-order) interpolation is the ESMF version of a technique called ``patch recovery'' commonly
! used in finite element modeling~\cite{PatchInterp1}~\cite{PatchInterp2}. It typically results in better approximations to 
! values and derivatives when compared to bilinear interpolation.
! Patch interpolation works by constructing multiple polynomial patches to represent
! the data in a source cell. For 2D grids, these polynomials
! are currently 2nd degree 2D polynomials. One patch is constructed for each corner of the source cell, and the patch is constructed 
! by doing a least squared fit through the data in the cells surrounding the corner. The interpolated value at the destination point is 
! then a weighted average of the values of the patches at that point. The patch method has a larger
! stencil than the bilinear, for this reason the patch weight matrix can be correspondingly larger
! than the bilinear matrix (e.g. for a quadrilateral grid the patch matrix is around 4x the size of
! the bilinear matrix). This can be an issue when performing a regrid operation close to the memory
! limit on a machine.  
!
!\smallskip
!
! In 2D, ESMF supports patch regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of any combination of triangles and quadrilaterals (e.g. rectangles)
! \end{itemize}
!
!\smallskip
!
! Patch regridding is currently not supported in 3D.
!
!\smallskip
!
! To use the patch method the user may created their Fields on any stagger location for Grids or the node location ({\tt ESMF\_MESHLOC\_NODE}) for Meshes.
! For Grids, the stagger location upon which the Field was built must contain coordinates. 
!
! First-order conservative interpolation~\cite{ConservativeOrder1} is also available as a regridding method. This method 
! will typically have  
! a larger local interpolation error than the previous two methods, but will do a much better job of preserving the value
! of the  integral of data between the source and destination grid. In this method the value across each source cell
! is treated as a constant. The weights for a particular destination cell are the area of intersection of each 
! source cell with the destination cell divided by the area of the destination cell. For cartesian grids, the area of a grid cell is the typical cartesian area. 
! For grids on a sphere, cell areas are calculated by connecting the corner coordinates of each grid cell with great circles. If the user doesn't specify
! cell areas in the involved Grids or Meshes, then the conservation will hold for the areas as calculated by 
! ESMF. This means the following equation will hold:  sum-over-all-source-cells(Vsi*Asi) = sum-over-all-destination-cells(Vdj*A'dj), where
! V is the variable being regridded and A' is the area of a cell as calculated by ESMF.  The subscripts s and d refer to source and destination values, and the i and j are the source 
! and destination grid cell indices (flattening the arrays to 1 dimension). If the user does specify the area's in the Grid or Mesh, then the conservation will be adjusted to work for the areas 
! provided by the user. This means the following equation will hold:  sum-over-all-source-cells(Vsi*Asi) = sum-over-all-destination-cells(Vdj*Adj),
! where A is the area of a cell as provided by the user. 
!
! The user should be aware that because of the conservation relationship between the source and destination fields, the more the total source area
! differs from the total destination area the more the values of the source field will differ from the corresponding values of the destination field, likely giving a higher 
! interpolation error. It is best to have the total source and destination areas the same (this will automatically be true if no user areas are specified). For source and destination grids 
! which only partially overlap the areas which should be the same are the areas of the overlapping regions of the source and destination. 
!
! Note that for grids on a sphere the conservative interpolation assumes great circle edges to cells. This means that the
! edges of a cell won't necessarily be
! the same as a straight line in latitude longitude. For small edges, this difference will be small, but for long edges it
! could be significant. This means if
! the user expects cell edges as straight lines in latitude longitude space, they should avoid using one large cell with 
! long edges to compute an average over a region (e.g. over an ocean basin). The 
! user should also avoid using cells which contain one edge that runs half way or more around the earth, because the 
! regrid weight calculation assumes the 
! edge follows the shorter great circle path. Also, there isn't a unique great circle edge defined between points on the 
! exact opposite side of the earth from one another (antipodal points). 
! However, the user can work around both of these problem by breaking the long edge into two smaller edges by inserting 
! an extra node, or by breaking the large target grid cells 
! into two or more smaller grid cells. This allows the application to resolve the ambiguity in edge direction. 
!
! It is important to note that the current implementation of conservative regridding doesn't normalize the interpolation 
! weights by the destination fraction. This means that for a destination
! grid which only partially overlaps the source grid the destination field which is output from the regrid operation 
! should be divided by the corresponding destination fraction to yield the 
! true interpolated values for cells which are only partially covered by the source grid. The fraction also needs to be 
! included when computing the total source and destination integrals. 
!
! The following pseudo-code shows how to compute the total source integral ({\tt src\_total}) given the source field values
! ({\tt src\_field}), the source area ({\tt src\_area}) from the {\tt ESMF\_FieldRegridGetArea()} call, and
! the source fraction ({\tt src\_frac}) from the {\tt ESMF\_FieldRegridStore()} call:
!
!\begin{verbatim}
! src_total=0.0
! for each source element i
!    src_total=src_total+src_field(i)*src_area(i)*src_frac(i)
! end for
!\end{verbatim}
!
! The following pseudo-code shows how to compute the total destination integral ({\tt dst\_total}) given the
! destination field values ({\tt dst\_field}) resulting
! from the {\tt ESMF\_FieldRegrid()} call, the destination area ({\tt dst\_area}) from the {\tt ESMF\_FieldRegridGetArea()}
! call,
! and the destination fraction ({\tt dst\_frac}) from the {\tt ESMF\_FieldRegridStore()} call. It also 
! shows how to adjust the destination field ({\tt dst\_field}) resulting from the {\tt ESMF\_FieldRegrid()} call by the
! fraction ({\tt dst\_frac}) from the {\tt ESMF\_FieldRegridStore()} call: 
!
!\begin{verbatim}
!
! dst_total=0.0
! for each destination element i
!    if (dst_frac(i) not equal to 0.0) then
!       dst_total=dst_total+dst_field(i)*dst_area(i) 
!       dst_field(i)=dst_field(i)/dst_frac(i)
!       ! If mass computed here after dst_field adjust, would need to be:
!       ! dst_total=dst_total+dst_field(i)*dst_area(i)*dst_frac(i) 
!    end if
! end for
!\end{verbatim}
!
!\smallskip
!
! In 2D, ESMF supports conservative regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of any combination of triangles and quadrilaterals (e.g. rectangles)
! \end{itemize}
!
!\smallskip
!
! In 3D, ESMF supports conservative regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of hexahedrons (e.g. cubes) and tetrahedras.
! \end{itemize}
!
!\smallskip
!
! To use the conservative method the user must have created their Fields on the center 
! stagger location ({\tt ESMF\_STAGGERLOC\_CENTER} in 2D or {\tt ESMF\_STAGGERLOC\_CENTER\_VCENTER} in 3D) for Grids  or the element location ({\tt ESMF\_MESHLOC\_ELEMENT}) for Meshes.
! For Grids, the corner stagger location ({\tt ESMF\_STAGGERLOC\_CORNER} in 2D or {\tt ESMF\_STAGGERLOC\_CORNER\_VFACE} in 3D) must contain coordinates describing the outer perimeter of the Grid cells. 
!
!\begin{table}[ht]
!\centering
!\vspace{0.2cm}
!\begin{tabular}{| l | l | c c |}
!\hline
!& & Online & Offline \\ [0.5ex]
!\hline
!2D Polygons & Triangles & $\surd$ & $\surd$ \\
!& Quadrilaterals & $\surd$ & $\surd$ \\
!\hline
!3D Polygons & Hexahedrons & $\surd$ & $\surd$ \\
!\hline
!Regridding & Bilinear & $\surd$ & $\surd$ \\
!& Patch & $\surd$ & $\surd$ \\
!& Conservative (1st order) & $\surd$ & $\surd$ \\
!\hline
!Masking & Destination & $\surd$ & $\surd$ \\
!& Source & $\surd$ &  $\surd$ \\
!& Unmapped points & $\surd$ & $\surd$ \\
!\hline
!Pole Options & Full circle average & $\surd$ & $\surd$ \\
!& N-point average & $\surd$ & $\surd$ \\
!& Teeth pole & $\surd$ & $\surd$ \\[1ex]
!\hline
!\end{tabular}
!\label{Regriddingcapabilities}
!\caption{Comparison of the offline vs. online regridding capabilities of ESMF}
!\end{table}
!
! The following sections give examples of using the regridding functionality.
!
!\subsubsection{Precompute a regridding operation between two Fields}
! To create the sparse matrix regrid operator we call the
! {\tt ESMF\_FieldRegridStore()} routine.  In this example we
! choose the {\tt ESMF\_REGRIDMETHOD\_BILINEAR} regridding method.  Other
! methods are available and more we will be added in the future.
! This method creates two meshes, and a Rendezvous decomposition of these
! meshes is computed.  An octree search is performed, followed by a determination
! of which source cell each destination gridpoint is in.  Bilinear weights
! are then computed locally on each cell.  This matrix of weights is, finally,
! sent back to the destination grid's row decomposition and declared as a 
! sparse matrix.  This matrix is embedded in the routeHandle object.
! Note the coordinates of the source and destination grids upon which the source and destination fields are 
! defined should be in degrees.  
!EOE

!BOC
  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
                  routeHandle=routeHandle, &
                  regridmethod=ESMF_REGRIDMETHOD_BILINEAR, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
!
!\subsubsection{Apply a regridding operation between a pair of Fields}
! The {\tt ESMF\_FieldRegrid} subroutine calls {\tt ESMF\_ArraySparseMatMul}
! and performs a regrid from source to destination field.
!EOE

  ! Test the regrid application
!BOC
  call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

!BOE
! 
!\subsubsection{Release the stored information for a regridding operation}
!EOE

!BOC
  call ESMF_FieldRegridRelease(routeHandle, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#if 0
  ! Write results to a mesh
  num_arrays = 1
  spherical_grid = 0

  ! Uncomment these calls to see some actual regrid results
  call ESMF_MeshIO(vm, GridSrc, ESMF_STAGGERLOC_CENTER, &
               "srcmesh", srcArray, &
               spherical=spherical_grid, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_MeshIO(vm, Griddst, ESMF_STAGGERLOC_CENTER, &
               "dstmesh", dstArray, &
               spherical=spherical_grid, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#endif

  call ESMF_GridDestroy(gridSrc, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_GridDestroy(gridDst, rc=localrc)
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

10   continue
  ! IMPORTANT: ESMF_STest() prints the PASS string and the # of processors in the log
  ! file that the scripts grep for.
  call ESMF_STest((finalrc.eq.ESMF_SUCCESS), testname, failMsg, result, ESMF_SRCLINE)

  call ESMF_Finalize(rc=rc)

  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_FieldRegridEx.F90"
  else
    print *, "FAIL: ESMF_FieldRegridEx.F90"
  endif

end program ESMF_FieldRegridEx
