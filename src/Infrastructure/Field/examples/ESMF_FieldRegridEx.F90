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
    '$Id$'
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
! This section describes the Field regrid methods. For an in depth description of ESMF regridding and the options available
! please see Section~\ref{sec:regrid}. 
!
! The basic flow of ESMF Field regridding is as follows. First a source and destination geometry object are created, depending on 
! the regrid method they can be either a Grid, a Mesh, or a LocStream. 
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
! The following example demonstrates doing a regrid operation between two Fields.
!
!EOE

!BOC

  ! (Create source Grid, Mesh, or LocStream.)
  ! (Create srcField on the above.)

  ! (Create destination Grid, Mesh, or LocStream.)
  ! (Create dstField on the above.)
  
  ! Create the routeHandle which encodes the communication and
  ! information necessary for the regrid sparse matrix multiply.
  call ESMF_FieldRegridStore(srcField=srcField, dstField=dstField, &
                             routeHandle=routeHandle, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOB

!BOC
 
  ! Can loop here regridding from srcField to dstField 
  ! do i=1,....

       ! (Put data into srcField)

       ! Use the routeHandle to regrid data from srcField to dstField.
       ! As described above, the same routeHandle can be used to 
       ! regrid any source and destination Fields which are weakly
       ! congruent to the original srcField and dstField. 
       call ESMF_FieldRegrid(srcField, dstField, routeHandle, rc=localrc)
!EOC
  if (localrc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)
!BOC

  !    (Use data in dstField)

  ! enddo 


  ! Free the buffers and data associated with the routeHandle. 
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
