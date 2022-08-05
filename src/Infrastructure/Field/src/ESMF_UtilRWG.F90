! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
#define ESMF_FILENAME "ESMF_UtilRWG.F90"

module ESMF_UtilRWGMod

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

  ! !USES:
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_UtilTypesMod
  use ESMF_LogErrMod
  use ESMF_VMMod

  use ESMF_GridMod
  use ESMF_MeshMod
  use ESMF_FieldMod
  use ESMF_FieldGatherMod
  use ESMF_FieldCreateMod
  use ESMF_FieldRegridMod
  use ESMF_StaggerLocMod

  implicit none

  private

  public :: computeAreaGrid
  public :: computeAreaMesh

contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeAreaGrid"

! For now just put into a 1D array on PET 0. When PIO array write is done, then
! do it in parallel
! AREA ONLY VALID ON PET 0
subroutine computeAreaGrid(grid, petNo, area, regridScheme, rc)
  type(ESMF_Grid) :: grid
  integer :: petNo
  real (ESMF_KIND_R8), pointer :: area(:)
  integer :: regridScheme
  integer :: rc

  type(ESMF_Field) :: areaField
  integer :: minIndex(2), maxIndex(2), gridDims(2)
  real (ESMF_KIND_R8), pointer :: area2D(:,:)
  integer :: i, start, ntiles
  integer :: localrc

  ! Create a field on the grid to hold the areas
  areaField=ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                 staggerloc=ESMF_STAGGERLOC_CENTER, name="area", rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! compute areas
  call ESMF_FieldRegridGetArea(areaField, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
 if (localrc /=ESMF_SUCCESS) then
     rc=localrc
     return
  endif

  ! Get number of tiles
  call ESMF_GridGet(grid, tileCount = ntiles, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
       ESMF_ERR_PASSTHRU, &
       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get size of Grid
  call ESMF_GridGet(grid, tile=1, staggerloc=ESMF_STAGGERLOC_CENTER, &
        minIndex=minIndex, maxIndex=maxIndex, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return
  if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
  endif

  ! Grid size
  gridDims(1)=maxIndex(1)-minIndex(1)+1
  gridDims(2)=maxIndex(2)-minIndex(2)+1

  ! Allocate memory for area
  allocate(area2D(gridDims(1),gridDims(2)))

  ! Only do this part on PET 0
  if (petNo .eq. 0) then
     ! Allocate memory for area
     allocate(area(gridDims(1)*gridDims(2)*ntiles))
  endif

  ! Get area onto PET 0
  start=1
  do i=1,ntiles
    call ESMF_FieldGather(areaField, farray=area2D, rootPet=0, tile=i, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

     ! copy to 1D array
     if (PetNo == 0) then
       ! flatten area
       area(start:start+gridDims(1)*gridDims(2)-1)=RESHAPE(area2D,(/gridDims(1)*gridDims(2)/))
       start= start+gridDims(1)*gridDims(2)
     endif
  enddo

  ! deallocate memory for 2D area
  deallocate(area2D)

end subroutine computeAreaGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "computeAreaMesh"

! For now just put into a 1D array on PET 0. When PIO array write is done, then
! do it in parallel
! AREA ONLY VALID ON PET 0
subroutine computeAreaMesh(mesh, vm, petNo, petCnt, area, rc)
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: VM
  integer :: petNo,petCnt
  real (ESMF_KIND_R8), pointer :: area(:)
  integer :: rc
  real (ESMF_KIND_R8), pointer :: localArea(:)
  integer :: localrc
  integer :: localElemCount,i
  integer (ESMF_KIND_I4) :: localCount(1)
  integer (ESMF_KIND_I4),pointer :: globalCount(:),globalDispl(:)
  integer :: totalCount

   ! Get local size of mesh areas
   call ESMF_MeshGet(mesh, numOwnedElements=localElemCount, &
          rc=localrc)
   if (ESMF_LogFoundError(localrc, &
                       ESMF_ERR_PASSTHRU, &
                       ESMF_CONTEXT, rcToReturn=rc)) return

  ! allocate space for areas
  allocate(localArea(localElemCount))

  ! Get local Areas
  call ESMF_MeshGetElemArea(mesh, areaList=localArea, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                       ESMF_ERR_PASSTHRU, &
                       ESMF_CONTEXT, rcToReturn=rc)) return

  ! Allocate List of counts
  allocate(globalCount(petCnt))

  ! Get List of counts
  localCount(1)=localElemCount
  call ESMF_VMGather(vm,localCount,globalCount,count=1,rootPet=0,rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

  ! Calculate Displacements
  allocate(globalDispl(petCnt))
  if (petNo==0) then
     globalDispl(1)=0
     do i=2,petCnt
        globalDispl(i)=globalDispl(i-1)+globalCount(i-1)
     enddo
  else
    globalDispl=0
  endif


  ! Sum size
  if (petNo==0) then
    totalCount=0
    do i=1,petCnt
       totalCount=totalCount+globalCount(i)
    enddo
  else
    totalCount=1 ! Because I'm not sure what happens
                 ! if array is not allocated in VM
  endif

  ! Allocate final area list
  allocate(area(totalCount))

  ! Gather all areas
  call ESMF_VMGatherV(vm,sendData=localArea, sendCount=localElemCount,&
         recvData=area,recvCounts=globalCount,recvOffsets=globalDispl,&
         rootPet=0, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
                         ESMF_ERR_PASSTHRU, &
                         ESMF_CONTEXT, rcToReturn=rc)) return

  ! Get rid of helper variables
  deallocate(localArea)
  deallocate(globalCount)
  deallocate(globalDispl)
  if (petNo .ne. 0) deallocate(area)

end subroutine computeAreaMesh


end module ESMF_UtilRWGMod

