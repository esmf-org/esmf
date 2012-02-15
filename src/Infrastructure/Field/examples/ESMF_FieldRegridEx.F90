! $Id: ESMF_FieldRegridEx.F90,v 1.60 2012/02/15 23:11:38 svasquez Exp $
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
    '$Id: ESMF_FieldRegridEx.F90,v 1.60 2012/02/15 23:11:38 svasquez Exp $'
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
! The rest of this section further describes ESMF integrated regridding.
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
! the routehandle to be the same interpolation between the grid objects upon which the Fields are build as was calculated                                        
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
! ESMF currently supports regridding only on a subset of the full range of Grids and Meshes it supports. 
! 
!
! In 2D, ESMF supports regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of any combination of triangles and quadralaterals (e.g. rectangles)
! \end{itemize}
!
!
! In 3D, ESMF supports bilinear or conservative regridding between any combination of the following:
! \begin{itemize}
! \item Structured Grids composed of a single logically rectangular patch
! \item Unstructured Meshes composed of hexahedrons (e.g. cubes). 
! \end{itemize}
! Note that regridding involving tetrahedra is currently NOT supported. 
!
! In the case that the Grid is on a sphere (coordSys=ESMF\_COORDSYS\_SPH\_DEG or ESMF\_COORDSYS\_SPH\_DEG)
! then the coordinates given in the Grid are interpretted as latitude and longitude values. The coordinates can either be in degrees or radians as indicated by the 
! {\tt coordSys} flag set during Grid creation. As is true with many global models, this application currently assumes the latitude and longitude refer to positions on a 
! perfect sphere, as opposed to a more complex and accurate representation of the earth's true shape such as would be used in a GIS system. (ESMF's current user base doesn't 
! require this level of detail in representing the earth's shape, but it could be added in the future if necessary.)
!
! In terms of masking, ESMF regrid currently supports masking for Fields built on structured Grids. The user may mask out points in 
! the source Field or destination Field or both. The user also has the option to return an error for unmapped destination points or
! to ignore them. At this point ESMF does not support extrapolation to destination points outside the unmasked source Field. 
!
! ESMF currently supports three options for interpolation: bilinear, patch, and conservative. 
! Bilinear interpolation calculates the value for the 
! destination point as a combination of multiple linear interpolations, one for each dimension of the Grid. Note that for ease of 
! use, the term bilinear interpolation is used for 3D interpolation in ESMF as well, although it should more properly be referred 
! to as trilinear interpolation.
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
!
! First-order conservative interpolation~\cite{ConservativeOrder1} is also available as a regridding method. This method will 
! typically have  a larger interpolation error than the previous two methods, but will do a much better job of preserving the 
! value of the integral of data between the source and destination grid. In this method the value across each source cell
! is treated as a constant. The weights for a particular destination cell, are the area of intersection of each 
! source cell with the destination cell divided by the area of the destination cell.
! Areas in this case are the great circle areas of the polygons which make up the cells (the cells around each center are 
! defined by the corner coordinates in the grid file). To use this method the user must have created their Fields on the center 
! stagger location ({\tt ESMF\_STAGGERLOC\_CENTER}) for Grids  or the element location ({\tt ESMF\_MESHLOC\_ELEMENT}) for Meshes.
! For Grids, the corner stagger location ({\tt ESMF\_STAGGERLOC\_CORNER}) must contain coordinates describing the outer perimeter of the
! Grid cells. Currently conservative interpolation is only supported for 2D Grids and Meshes. 
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
!3D Polygons & Hexahedrons & $\surd$ & \\
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
