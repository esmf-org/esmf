! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMFIO.F90"
!
! ESMFIO Base Module
module ESMFIOMod

!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMFIO - Read/Write methods for 2D fields on multi-tile grids
!
! !DESCRIPTION:
!
! The code in this file provide additional I/O APIs for fields created on
! grids with single and multiple tiles. I/O is performed to/from individual
! files (1/tile) using 1 PET/tile.
!
!------------------------------------------------------------------------------
! !USES:

  use ESMF_CompMod
  use ESMF_DistGridMod
  use ESMF_FieldMod
  use ESMF_GridMod
  use ESMF_LogErrMod
  use ESMF_UtilTypesMod
  use ESMF_VMMod
  use ESMF_ArrayMod
  use ESMF_GeomBaseMod
  use ESMF_StaggerLocTypeMod
  use ESMF_StateMod
  use ESMF_ClockMod
  use ESMF_GridCompMod
  use ESMF_FieldGetMod
#ifdef ESMF_NETCDF
  use netcdf
#endif

  implicit none

  type ESMFIOLayout
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
#endif
    logical :: localIOflag
    integer :: tile
    integer :: ncid
    type(ESMF_GridComp) :: taskComp
  end type ESMFIOLayout

  type ioData
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
#endif
    type(ESMFIOLayout), pointer :: IOLayout(:) => null()
  end type ioData

  type ioWrapper
    type(ioData), pointer :: IO => null()
  end type ioWrapper


  private

  public :: ESMFIO_Create
  public :: ESMFIO_Destroy
  public :: ESMFIO_Read
  public :: ESMFIO_Write

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

contains
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMFIO_Create()"

  function ESMFIO_Create(grid, keywordEnforcer, rc)
    type(ESMF_Grid), intent(in)            :: grid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,         intent(out), optional :: rc

    type(ESMF_GridComp) :: ESMFIO_Create

    ! -- local variables
    integer             :: localrc
    integer             :: i, localDe, localDeCount, localpe, peCount, npe
    integer             :: tile, tileCount
    integer, dimension(:), allocatable :: localTile, tileToPet, pes, recvpes
    type(ESMF_GridComp) :: IOComp, taskComp
    type(ESMF_VM)       :: vm
    type(ioWrapper)     :: is
    type(ioData), pointer :: IO => null()

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    nullify(IO)

    call ESMF_GridGet(grid, localDeCount=localDeCount, tileCount=tileCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_VMGet(vm, localPet=localpe, petCount=peCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    allocate(recvpes(peCount), pes(peCount), stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, &
      msg="Unable to allocate internal memory for ESMFIO initialization", &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    pes = 0
    pes(localpe+1) = -localDeCount

    call ESMF_VMAllReduce(vm, pes, recvpes, peCount, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    pes = -1
    npe = 0
    do i = 1, peCount
      if (recvpes(i) < 0) then
        npe = npe + 1 
        pes(npe) = i - 1
      end if
    end do

    ! -- create IO component on this PET
    IOComp = ESMF_GridCompCreate(name="io_comp", petList=pes(1:npe), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    deallocate(recvpes, pes, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_GridCompSetServices(IOComp, IOCompSetServices, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (ESMF_GridCompIsPetLocal(IOComp)) then

      allocate(IO, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, &
        msg="Unable to allocate internal memory for ESMFIO initialization", &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
    
      allocate(IO % IOLayout(0:localDeCount-1), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, &
        msg="Unable to allocate internal memory for ESMFIO initialization", &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      is % IO => IO

    else

      is % IO => null()

    end if

    ! -- set internal state for IO component
    call ESMF_GridCompSetInternalState(IOComp, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ! -- save grid object in IO component
    call ESMF_GridCompSet(IOComp, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    allocate(localTile(tileCount), tileToPet(tileCount*peCount), stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, &
      msg="Unable to allocate internal memory for ESMFIO initialization", &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ! -- store which tiles are assigned to this PET
    localTile = -1
    do localDe = 0, localDeCount-1
      call ESMF_GridGet(grid, localDE=localDe, tile=tile, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      localTile(tile) = localpe
      is % IO % IOLayout(localDe) % tile = tile
      is % IO % IOLayout(localDe) % ncid = 0
    end do

    tileToPet = -1
    call ESMF_VMAllGather(vm, localTile, tileToPet, tileCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    deallocate(localTile, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ! -- extract the list of PETs assigned to each tile and create MPI groups
    allocate(pes(peCount), stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, &
      msg="Unable to allocate internal memory for ESMFIO initialization", &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ! -- gather PET list for each tile and create tile-specific VMs
    pes = -1
    do tile = 1, tileCount
      npe = 0
      do i = tile, tileCount*peCount, tileCount
        if (tileToPet(i) > -1) then
          npe = npe + 1
          pes(npe) = tileToPet(i)
        end if
      end do

      taskComp = ESMF_GridCompCreate(petList=pes(1:npe), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_GridCompSetServices(taskComp, IOCompSetServices, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      do localDe = 0, localDeCount-1
        call ESMF_GridGet(grid, localDE=localDe, tile=i, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        if (tile == i) then
          ! -- create new VM for tile
          is % IO % IOLayout(localDe) % taskComp = taskComp
        end if
      end do
    end do

    deallocate(pes, tileToPet, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ! -- flag PET if local I/O must be performed
    do localDe = 0, localDeCount - 1
      call ESMF_GridCompGet(is % IO % IOLayout(localDe) % taskComp, vm=vm, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      call ESMF_VMGet(vm, localPet=localpe, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      is % IO % IOLayout(localDe) % localIOflag = (localpe == 0)
    end do

    ESMFIO_Create = IOComp

  end function ESMFIO_Create

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMFIO_Destroy()"

  subroutine ESMFIO_Destroy(IOComp, keywordEnforcer, rc)
    type(ESMF_GridComp)            :: IOComp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer, intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: localDe
    type(ioWrapper) :: is

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (ESMF_GridCompIsCreated(IOComp)) then
      call ESMF_GridCompGetInternalState(IOComp, is, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      if (associated(is % IO)) then
        if (associated(is % IO % IOLayout)) then
          do localDe = 0, size(is % IO % IOLayout) - 1
            if (ESMF_GridCompIsCreated(is % IO % IOLayout(localDe) % taskComp)) then
              call ESMF_GridCompDestroy(is % IO % IOLayout(localDe) % taskComp, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
            end if
          end do

          deallocate(is % IO % IOLayout, stat=localrc)
          if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
          nullify(is % IO % IOLayout)

          call ESMF_GridCompDestroy(IOComp, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        end if
        nullify(is % IO)
      end if
    end if
    
  end subroutine ESMFIO_Destroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMFIO_Write()"

  subroutine ESMFIO_Write(IOComp, fileName, fieldList, keywordEnforcer, &
    filePath, iofmt, rc)
    type(ESMF_GridComp),   intent(inout)         :: IOComp
    character(len=*),      intent(in)            :: fileName
    type(ESMF_Field),      intent(in)            :: fieldList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),      intent(in),  optional :: filePath
    type(ESMF_IOFmt_flag), intent(in),  optional :: iofmt
    integer,               intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: item, localDe, localDeCount
    type(ioWrapper) :: is
    type(ESMF_IOFmt_flag) :: liofmt

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (.not.ESMF_GridCompIsPetLocal(IOComp)) return

    liofmt = ESMF_IOFMT_NETCDF
    if (present(iofmt)) liofmt = iofmt

    call ESMF_GridCompGetInternalState(IOComp, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (.not.associated(is % IO)) return
    if (.not.associated(is % IO % IOLayout)) return

    localDeCount = size(is % IO % IOLayout)

    if (liofmt == ESMF_IOFMT_NETCDF) then
      do localDe = 0, localDeCount - 1
        call IONCCreate(IOComp, fileName, filePath=filePath, fieldList=fieldList, &
          localDe=localDe, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      end do
      do item = 1, size(fieldList)
        call ESMFIO_FieldAccess(IOComp, fieldList(item), 'write', iofmt=liofmt, rc=localrc)
      end do
      do localDe = 0, localDeCount - 1
        call IONCClose(IOComp, localDe=localDe, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        call IONCClose(IOComp, localDe=localDe, rc=localrc)
      end do
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="I/O format not implemented", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail out
    end if

  end subroutine ESMFIO_Write

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMFIO_Read()"

  subroutine ESMFIO_Read(IOComp, fileName, fieldList, keywordEnforcer, &
    filePath, iofmt, rc)
    type(ESMF_GridComp),   intent(inout)         :: IOComp
    character(len=*),      intent(in)            :: fileName
    type(ESMF_Field),      intent(in)            :: fieldList(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),      intent(in),  optional :: filePath
    type(ESMF_IOFmt_flag), intent(in),  optional :: iofmt
    integer,               intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: item, localDe, localDeCount
    type(ioWrapper) :: is
    type(ESMF_IOFmt_flag) :: liofmt

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (.not.ESMF_GridCompIsPetLocal(IOComp)) return

    call ESMF_GridCompGetInternalState(IOComp, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (.not.associated(is % IO)) return
    if (.not.associated(is % IO % IOLayout)) return

    localDeCount = size(is % IO % IOLayout)

    liofmt = ESMF_IOFMT_NETCDF
    if (present(iofmt)) liofmt = iofmt

    if (liofmt == ESMF_IOFMT_NETCDF) then
      do localDe = 0, localDeCount - 1
        call IONCOpen(IOComp, fileName, filePath=filePath, localDe=localDe, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      end do
      do item = 1, size(fieldList)
        call ESMFIO_FieldAccess(IOComp, fieldList(item), 'read', iofmt=liofmt, rc=localrc)
      end do
      do localDe = 0, localDeCount - 1
        call IONCClose(IOComp, localDe=localDe, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        call IONCClose(IOComp, localDe=localDe, rc=localrc)
      end do
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="I/O format not implemented", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail out
    end if

  end subroutine ESMFIO_Read

!------------------------------------------------------------------------------
! Private methods below
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMFIO_FieldAccess()"

  subroutine ESMFIO_FieldAccess(IOComp, field, action, keywordEnforcer, &
    iofmt, rc)
    type(ESMF_GridComp),   intent(in)            :: IOComp
    type(ESMF_Field),      intent(in)            :: field
    character(len=*),      intent(in)            :: action
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_IOFmt_flag), intent(in),  optional :: iofmt
    integer,               intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: localDe, localDeCount, rank
    integer :: de, deCount, dimCount, tile, tileCount
    integer, dimension(:), allocatable :: deToTileMap, localDeToDeMap
    integer, dimension(:,:), allocatable :: minIndexPDe, maxIndexPDe
    integer, dimension(:,:), allocatable :: minIndexPTile, maxIndexPTile
    type(ioWrapper) :: is
    type(ESMF_Grid) :: grid, iogrid
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Array) :: array
    type(ESMF_VM) :: vm
    type(ESMF_GeomType_flag)      :: geomtype
    type(ESMF_StaggerLoc)         :: staggerloc
    type(ESMF_TypeKind_Flag)      :: typekind

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (.not.ESMF_GridCompIsPetLocal(IOComp)) return

    call ESMF_GridCompGet(IOComp, grid=iogrid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (geomtype == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, rank=rank, &
        staggerloc=staggerloc, typekind=typekind, localDeCount=localDeCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      if (grid /= iogrid) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="I/O fields and I/O component must be defined on the same grid", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail out
      end if
      if (rank /= 2) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Only 2D fields are supported.", &
          ESMF_CONTEXT, rcToReturn=rc)
        return  ! bail ou
      end if
      ! -- get domain decomposition
      call ESMF_GridGet(grid, staggerloc, distgrid=distgrid, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_DistGridGet(distgrid, deCount=deCount, dimCount=dimCount, &
        tileCount=tileCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      allocate(minIndexPDe(dimCount, deCount), maxIndexPDe(dimCount, deCount),  &
        minIndexPTile(dimCount, tileCount), maxIndexPTile(dimCount, tileCount), &
        deToTileMap(deCount), localDeToDeMap(localDeCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_DistGridGet(distgrid, &
        minIndexPDe=minIndexPDe, maxIndexPDe=maxIndexPDe, &
        minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_FieldGet(field, array=array, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_ArrayGet(array, deToTileMap=deToTileMap, &
        localDeToDeMap=localDeToDeMap, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_GridCompGetInternalState(IOComp, is, localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      do localDe = 0, localDeCount-1
        de   = localDeToDeMap(localDe+1) + 1
        tile = deToTileMap(de)

        call ESMF_GridCompGet(is % IO % IOLayout(localDe) % taskComp, vm=vm, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

        select case (trim(action))
          case('r','read')
            call IORead2D(vm, field, &
              minIndexPDe(:,de), maxIndexPDe(:,de), &
              minIndexPTile(:,tile), maxIndexPTile(:,tile), &
              iofmt=iofmt, localDe=localDe, &
              ncid=is % IO % IOLayout(localDe) % ncid, &
              rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
          case('w','write')
            call IOWrite2D(vm, field, &
              minIndexPDe(:,de), maxIndexPDe(:,de), &
              minIndexPTile(:,tile), maxIndexPTile(:,tile), &
              iofmt=iofmt, localDe=localDe, &
              ncid=is % IO % IOLayout(localDe) % ncid, &
              rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
          case default
              ! -- do nothing
        end select
      end do

      deallocate(minIndexPDe, maxIndexPDe, minIndexPTile, maxIndexPTile, &
        deToTileMap, localDeToDeMap, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="I/O fields can only be defined on Grid objects.", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail ou
    end if

  end subroutine ESMFIO_FieldAccess

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "IOWrite2D()"

  subroutine IOWrite2D(vm, field, &
    minIndexPDe, maxIndexPDe, minIndexPTile, maxIndexPTile, keywordEnforcer, &
    fileName, iofmt, localDe, ncid, rc)
    type(ESMF_VM),         intent(in)            :: vm
    type(ESMF_Field),      intent(in)            :: field
    integer, dimension(:), intent(in)            :: minIndexPDe
    integer, dimension(:), intent(in)            :: maxIndexPDe
    integer, dimension(:), intent(in)            :: minIndexPTile
    integer, dimension(:), intent(in)            :: maxIndexPTile
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),      intent(in),  optional :: fileName
    type(ESMF_IOFmt_flag), intent(in),  optional :: iofmt
    integer,               intent(in),  optional :: localDe
    integer,               intent(in),  optional :: ncid
    integer,               intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: ilen, jlen, lbuf, lde, localpe
    integer :: lncid, varId, ncStatus
    integer, dimension(2) :: elb, eub
    integer(ESMF_KIND_I4), dimension(:),   allocatable :: recvbuf_i4
    integer(ESMF_KIND_I4), dimension(:,:), allocatable :: buf_i4
    integer(ESMF_KIND_I4), dimension(:,:), pointer     :: fp_i4 => null()
    real(ESMF_KIND_R4),    dimension(:),   allocatable :: recvbuf_r4
    real(ESMF_KIND_R4),    dimension(:,:), allocatable :: buf_r4
    real(ESMF_KIND_R4),    dimension(:,:), pointer     :: fp_r4 => null()
    real(ESMF_KIND_R8),    dimension(:),   allocatable :: recvbuf_r8
    real(ESMF_KIND_R8),    dimension(:,:), allocatable :: buf_r8
    real(ESMF_KIND_R8),    dimension(:,:), pointer     :: fp_r8 => null()
    character(len=ESMF_MAXSTR) :: fieldName
    type(ESMF_TypeKind_Flag) :: typekind

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    lde = 0
    if (present(localDe)) lde = localDe

    call ESMF_FieldGet(field, name=fieldName, typekind=typekind, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ilen = maxIndexPTile(1)-minIndexPTile(1)+1
    jlen = maxIndexPTile(2)-minIndexPTile(2)+1
    lbuf = ilen * jlen

    if      (typekind == ESMF_TYPEKIND_I4) then

      call ESMF_FieldGet(field, localDe=lde, farrayPtr=fp_i4, &
        exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      allocate(buf_i4(minIndexPTile(1):maxIndexPTile(1), &
                      minIndexPTile(2):maxIndexPTile(2)), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_i4 = 0_ESMF_KIND_I4
      buf_i4(minIndexPDe(1):maxIndexPDe(1), &
             minIndexPDe(2):maxIndexPDe(2)) = fp_i4(elb(1):eub(1),elb(2):eub(2))

      nullify(fp_i4)

      allocate(recvbuf_i4(lbuf), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_VMReduce(vm, reshape(buf_i4, (/lbuf/)), recvbuf_i4, lbuf, &
        ESMF_REDUCE_SUM, 0, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_i4 = reshape(recvbuf_i4, (/ilen, jlen/))

      deallocate(recvbuf_i4, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    else if (typekind == ESMF_TYPEKIND_R4) then

      call ESMF_FieldGet(field, localDe=lde, farrayPtr=fp_r4, &
        exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      allocate(buf_r4(minIndexPTile(1):maxIndexPTile(1), &
                      minIndexPTile(2):maxIndexPTile(2)), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_r4 = 0._ESMF_KIND_R4
      buf_r4(minIndexPDe(1):maxIndexPDe(1), &
             minIndexPDe(2):maxIndexPDe(2)) = fp_r4(elb(1):eub(1),elb(2):eub(2))

      nullify(fp_r4)

      allocate(recvbuf_r4(lbuf), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_VMReduce(vm, reshape(buf_r4, (/lbuf/)), recvbuf_r4, lbuf, &
        ESMF_REDUCE_SUM, 0, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_r4 = reshape(recvbuf_r4, (/ilen, jlen/))

      deallocate(recvbuf_r4, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    else if (typekind == ESMF_TYPEKIND_R8) then

      call ESMF_FieldGet(field, localDe=lde, farrayPtr=fp_r8, &
        exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      allocate(buf_r8(minIndexPTile(1):maxIndexPTile(1), &
                      minIndexPTile(2):maxIndexPTile(2)), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_r8 = 0._ESMF_KIND_R8
      buf_r8(minIndexPDe(1):maxIndexPDe(1), &
             minIndexPDe(2):maxIndexPDe(2)) = fp_r8(elb(1):eub(1),elb(2):eub(2))

      nullify(fp_r8)

      allocate(recvbuf_r8(lbuf), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_VMReduce(vm, reshape(buf_r8, (/lbuf/)), recvbuf_r8, lbuf, &
        ESMF_REDUCE_SUM, 0, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_r8 = reshape(recvbuf_r8, (/ilen, jlen/))

      deallocate(recvbuf_r8, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    else

      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Field: "//trim(fieldName)//" - typekind not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return  ! bail out
    end if

    call ESMF_VMGet(vm, localPet=localpe, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (localpe == 0) then
      if (iofmt == ESMF_IOFMT_NETCDF) then
#ifdef ESMF_NETCDF
        lncid = 0
        if (present(ncid)) then
          lncid = ncid
        else if (present(fileName)) then
          ncStatus = nf90_open(trim(fileName), NF90_WRITE, lncid)
          if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
            msg="Field "//trim(fieldName)//" not defined in NetCDF data set "//trim(fileName), &
            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        end if

        ! -- add data
        if (lncid /= 0) then
          ncStatus = nf90_inq_varid(lncid, trim(fieldName), varId)
          if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
            msg="Field "//trim(fieldName)//" not defined in NetCDF data set "//trim(fileName), &
            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        
          if (typekind == ESMF_TYPEKIND_I4) then
            ncStatus = nf90_put_var(lncid, varId, buf_i4)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
              msg="Error writing field "//trim(fieldName)//" to NetCDF data set "//trim(fileName), &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
          else if (typekind == ESMF_TYPEKIND_R4) then
            ncStatus = nf90_put_var(lncid, varId, buf_r4)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
              msg="Error writing field "//trim(fieldName)//" to NetCDF data set "//trim(fileName), &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
          else if (typekind == ESMF_TYPEKIND_R8) then
            ncStatus = nf90_put_var(lncid, varId, buf_r8)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
              msg="Error writing field "//trim(fieldName)//" to NetCDF data set "//trim(fileName), &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
          end if
        end if
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
#endif
      end if
    end if

    if (typekind == ESMF_TYPEKIND_I4) then
      deallocate(buf_i4, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
    else if (typekind == ESMF_TYPEKIND_R4) then
      deallocate(buf_r4, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
    else if (typekind == ESMF_TYPEKIND_R8) then
      deallocate(buf_r8, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
    end if

  end subroutine IOWrite2D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "IORead2D()"

  subroutine IORead2D(vm, field, &
    minIndexPDe, maxIndexPDe, minIndexPTile, maxIndexPTile, keywordEnforcer, &
    fileName, iofmt, localDe, ncid, rc)
    type(ESMF_VM),         intent(in)            :: vm
    type(ESMF_Field),      intent(in)            :: field
    integer, dimension(:), intent(in)            :: minIndexPDe
    integer, dimension(:), intent(in)            :: maxIndexPDe
    integer, dimension(:), intent(in)            :: minIndexPTile
    integer, dimension(:), intent(in)            :: maxIndexPTile
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),      intent(in),  optional :: fileName
    type(ESMF_IOFmt_flag), intent(in),  optional :: iofmt
    integer,               intent(in),  optional :: localDe
    integer,               intent(in),  optional :: ncid
    integer,               intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: ilen, jlen, lbuf, lde, localpe, rank
    integer :: lncid, varId, ncStatus, ndims, xtype
    integer, dimension(2) :: elb, eub
    integer(ESMF_KIND_I4), dimension(:),   allocatable :: bcstbuf_i4
    integer(ESMF_KIND_I4), dimension(:,:), allocatable :: buf_i4
    integer(ESMF_KIND_I4), dimension(:,:), pointer     :: fp_i4 => null()
    real(ESMF_KIND_R4),    dimension(:),   allocatable :: bcstbuf_r4
    real(ESMF_KIND_R4),    dimension(:,:), allocatable :: buf_r4
    real(ESMF_KIND_R4),    dimension(:,:), pointer     :: fp_r4 => null()
    real(ESMF_KIND_R8),    dimension(:),   allocatable :: bcstbuf_r8
    real(ESMF_KIND_R8),    dimension(:,:), allocatable :: buf_r8
    real(ESMF_KIND_R8),    dimension(:,:), pointer     :: fp_r8 => null()
    character(len=ESMF_MAXSTR) :: fieldName
    type(ESMF_TypeKind_Flag) :: typekind

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    lde = 0
    if (present(localDe)) lde = localDe

    call ESMF_FieldGet(field, name=fieldName, rank=rank, &
      typekind=typekind, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ilen = maxIndexPTile(1)-minIndexPTile(1)+1
    jlen = maxIndexPTile(2)-minIndexPTile(2)+1
    lbuf = ilen * jlen

    if      (typekind == ESMF_TYPEKIND_I4) then
      allocate(buf_i4(minIndexPTile(1):maxIndexPTile(1), &
                      minIndexPTile(2):maxIndexPTile(2)), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
       buf_i4 = 0_ESMF_KIND_I4
    else if (typekind == ESMF_TYPEKIND_R4) then
      allocate(buf_r4(minIndexPTile(1):maxIndexPTile(1), &
                      minIndexPTile(2):maxIndexPTile(2)), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      buf_r4 = 0._ESMF_KIND_R4
    else if (typekind == ESMF_TYPEKIND_R8) then
      allocate(buf_r8(minIndexPTile(1):maxIndexPTile(1), &
                      minIndexPTile(2):maxIndexPTile(2)), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      buf_r8 = 0._ESMF_KIND_R8
    end if

    call ESMF_VMGet(vm, localPet=localpe, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (localpe == 0) then
      if (iofmt == ESMF_IOFMT_NETCDF) then
#ifdef ESMF_NETCDF
        lncid = 0
        if (present(ncid)) then
          lncid = ncid
        else if (present(fileName)) then
          ncStatus = nf90_open(trim(fileName), NF90_NOWRITE, lncid)
          if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
            msg="Field "//trim(fieldName)//" not defined in NetCDF data set "//trim(fileName), &
            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        end if

        ! -- add data
        if (lncid /= 0) then
          ncStatus = nf90_inq_varid(lncid, trim(fieldName), varId)
          if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
            msg="Field "//trim(fieldName)//" not defined in NetCDF data set "//trim(fileName), &
            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        
          ncStatus = nf90_inquire_variable(lncid, varId, xtype=xtype, ndims=ndims)
          if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
            msg="Error inquiring variable "//trim(fieldName)//" in NetCDF data set "//trim(fileName), &
            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

          if (ndims /= rank) then
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg="Variable "//trim(fieldName)//" has different rank than Field", &
              ESMF_CONTEXT, rcToReturn=rc)
            return  ! bail out
          end if

          if      (typekind == ESMF_TYPEKIND_I4) then

            if (xtype /= NF90_INT) then
              call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
                msg="Variable "//trim(fieldName)//" has different typekind than Field", &
                ESMF_CONTEXT, rcToReturn=rc)
              return  ! bail out
            end if

            ncStatus = nf90_get_var(lncid, varId, buf_i4)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
              msg="Error reading field "//trim(fieldName)//" from NetCDF data set "//trim(fileName), &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

          else if (typekind == ESMF_TYPEKIND_R4) then

            if (xtype /= NF90_FLOAT) then
              call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
                msg="Variable "//trim(fieldName)//" has different typekind than Field", &
                ESMF_CONTEXT, rcToReturn=rc)
              return  ! bail out
            end if

            ncStatus = nf90_get_var(lncid, varId, buf_r4)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
              msg="Error reading field "//trim(fieldName)//" from NetCDF data set "//trim(fileName), &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

          else if (typekind == ESMF_TYPEKIND_R8) then

            if (xtype /= NF90_DOUBLE) then
              call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
                msg="Variable "//trim(fieldName)//" has different typekind than Field", &
                ESMF_CONTEXT, rcToReturn=rc)
              return  ! bail out
            end if

            ncStatus = nf90_get_var(lncid, varId, buf_r8)
            if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
              msg="Error reading field "//trim(fieldName)//" from NetCDF data set "//trim(fileName), &
              ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

          else

            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg="Field: "//trim(fieldName)//" - typekind not supported", &
              ESMF_CONTEXT, rcToReturn=rc)
            return  ! bail out

          end if

        end if
#else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
#endif
      end if
    end if

    if      (typekind == ESMF_TYPEKIND_I4) then

      allocate(bcstbuf_i4(lbuf), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      bcstbuf_i4 = reshape(buf_i4, (/lbuf/))

      call ESMF_VMBroadcast(vm, bcstbuf_i4, lbuf, 0, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_FieldGet(field, localDe=lde, farrayPtr=fp_i4, &
        exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_i4 = reshape(bcstbuf_i4, (/ilen,jlen/))

      fp_i4(elb(1):eub(1),elb(2):eub(2)) = buf_i4(minIndexPDe(1):maxIndexPDe(1), &
                                                  minIndexPDe(2):maxIndexPDe(2))

      deallocate(buf_i4, bcstbuf_i4, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    else if (typekind == ESMF_TYPEKIND_R4) then

      allocate(bcstbuf_r4(lbuf), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      bcstbuf_r4 = reshape(buf_r4, (/lbuf/))

      call ESMF_VMBroadcast(vm, bcstbuf_r4, lbuf, 0, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_FieldGet(field, localDe=lde, farrayPtr=fp_r4, &
        exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_r4 = reshape(bcstbuf_r4, (/ilen,jlen/))

      fp_r4(elb(1):eub(1),elb(2):eub(2)) = buf_r4(minIndexPDe(1):maxIndexPDe(1), &
                                                  minIndexPDe(2):maxIndexPDe(2))

      deallocate(buf_r4, bcstbuf_r4, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    else if (typekind == ESMF_TYPEKIND_R8) then

      allocate(bcstbuf_r8(lbuf), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      bcstbuf_r8 = reshape(buf_r8, (/lbuf/))

      call ESMF_VMBroadcast(vm, bcstbuf_r8, lbuf, 0, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      call ESMF_FieldGet(field, localDe=lde, farrayPtr=fp_r8, &
        exclusiveLBound=elb, exclusiveUBound=eub, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      buf_r8 = reshape(bcstbuf_r8, (/ilen,jlen/))

      fp_r8(elb(1):eub(1),elb(2):eub(2)) = buf_r8(minIndexPDe(1):maxIndexPDe(1), &
                                                  minIndexPDe(2):maxIndexPDe(2))

      deallocate(buf_r8, bcstbuf_r8, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    end if

  end subroutine IORead2D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "IOFilenameGet()"

  subroutine IOFilenameGet(fullName, fileName, keywordEnforcer, tile, filePath)
    character(len=*), intent(out)          :: fullName
    character(len=*), intent(in)           :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,          intent(in), optional :: tile
    character(len=*), intent(in), optional :: filePath

    ! -- local variables
    integer :: iext, lstr
    character(len=ESMF_MAXSTR) :: fname
    character(len=*), parameter :: defext = ".nc"

    ! -- begin
    fname = ""
    fullName = ""

    if (present(tile)) then
      iext = index(fileName, ".", back=.true.)
      if (iext > 0) then
        write(fname, '(a,"tile",i0,a)') fileName(1:iext), tile, trim(fileName(iext:))
      else
        write(fname, '(a,"tile",i0,a)') trim(fileName), tile, defext
      end if
    else
      fname = fileName
    end if

    if (present(filePath)) then
      lstr = len_trim(filePath)
      if (filePath(lstr:lstr) == "/") then
        fullName = trim(filePath) // trim(fname)
      else
        fullName = trim(filePath) // "/" // trim(fname)
      end if
    else
      fullName = trim(fname)
    end if

  end subroutine IOFilenameGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "IONCCreate()"

  subroutine IONCCreate(IOComp, fileName, keywordEnforcer, filePath, &
    fieldList, localDe, rc)
    type(ESMF_GridComp), intent(inout)         :: IOComp
    character(len=*),    intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),    intent(in),  optional :: filePath
    type(ESMF_Field),    intent(in),  optional :: fieldList(:)
    integer,             intent(in),  optional :: localDe
    integer,             intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: dimCount, item, sloc
    integer :: ncid, ncStatus, varId, xtype
    integer :: de, dimLen, tile, staggerlocCount, tileCount
    character(len=ESMF_MAXSTR) :: dimName, fieldName
    character(len=ESMF_MAXPATHLEN) :: fullName
    logical, dimension(:),   allocatable :: staggerlocList
    integer, dimension(:,:), allocatable :: dimIds
    integer, dimension(:,:), allocatable :: minIndexPTile, maxIndexPTile
    type(ioWrapper)          :: is
    type(ESMF_Grid)          :: grid
    type(ESMF_DistGrid)      :: distgrid
    type(ESMF_StaggerLoc)    :: staggerloc
    type(ESMF_TypeKind_Flag) :: typekind

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (.not.ESMF_GridCompIsPetLocal(IOComp)) return

#ifdef ESMF_NETCDF
    de = 0
    if (present(localDe)) de = localDe

    call ESMF_GridCompGetInternalState(IOComp, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (.not.is % IO % IOLayout(de) % localIOflag) return

    call ESMF_GridCompGet(IOComp, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_GridGet(grid, dimCount=dimCount, &
      staggerlocCount=staggerlocCount, tileCount=tileCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (tileCount > 1) then
      call IOFilenameGet(fullName, fileName, filePath=filePath, &
        tile=is % IO % IOLayout(de) % tile)
    else
      call IOFilenameGet(fullName, fileName, filePath=filePath)
    end if

    ! -- collect staggerloc values
    allocate(staggerlocList(0:staggerlocCount-1), stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    staggerlocList = .false.
    if (present(fieldList)) then
      do item = 1, size(fieldList)
        call ESMF_FieldGet(fieldList(item), staggerloc=staggerloc, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        staggerlocList(staggerloc % staggerloc) = .true.
      end do
    else
      ! -- set default staggerloc as ESMF_STAGGERLOC_CENTER
      staggerlocList(ESMF_STAGGERLOC_CENTER % staggerloc) = .true.
    end if

    ncStatus = nf90_create(trim(fullName), NF90_CLOBBER, ncid)
    if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
      msg="Error opening NetCDF data set: "//trim(fileName), &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    allocate(dimIds(dimCount, 0:staggerlocCount-1), stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, &
      msg="Unable to allocate internal memory for IOCreate", &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ! -- define dimensions
    dimIds = 0
    do sloc = 0, staggerlocCount-1

      if (staggerlocList(sloc)) then

        call ESMF_GridGet(grid, staggerloc=ESMF_StaggerLoc(sloc), &
          distgrid=distgrid, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

        call ESMF_DistgridGet(distgrid, dimCount=dimCount, &
          tileCount=tileCount, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

        allocate(minIndexPTile(dimCount, tileCount), &
          maxIndexPTile(dimCount, tileCount), stat=localrc)
        if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

        call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
          maxIndexPTile=maxIndexPTile, rc=localrc)
        if (ESMF_LogFoundAllocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

        do item = 1, dimCount
          tile = is % IO % IOLayout(de) % tile
          dimLen = maxIndexPTile(item, tile) - minIndexPTile(item, tile) + 1
          dimName = ""
          write(dimName, '("x",2i0)') sloc, item
          ncStatus = nf90_def_dim(ncid, trim(dimName), dimLen, dimIds(item,sloc))
          if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
            msg="Error defining dimension "//trim(dimName), &
            ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
        end do

        deallocate(minIndexPTile, maxIndexPTile, stat=localrc)
        if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

      end if

    end do

    deallocate(staggerlocList, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    ! -- define Field variables
    if (present(fieldList)) then
      do item = 1, size(fieldList)

        call ESMF_FieldGet(fieldList(item), name=fieldName, &
          staggerloc=staggerloc, typekind=typekind, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

        if      (typekind == ESMF_TYPEKIND_I4) then
          xtype = NF90_INT
        else if (typekind == ESMF_TYPEKIND_R4) then
          xtype = NF90_FLOAT
        else if (typekind == ESMF_TYPEKIND_R8) then
          xtype = NF90_DOUBLE
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Field: "//trim(fieldName)//" - typekind not supported", &
            ESMF_CONTEXT, rcToReturn=rc)
          return  ! bail out
        end if

        ncStatus = nf90_def_var(ncid, trim(fieldName), xtype, &
          dimIds(:, staggerloc % staggerloc), varId)
        if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
          msg="Error defining NetCDF variable: "//trim(fieldName), &
          ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      end do
    end if

    ncStatus = nf90_enddef(ncid)
    if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
      msg="Error defining NetCDF data set: "//trim(fileName), &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    deallocate(dimIds, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, &
      msg="Unable to deallocate internal memory for IONCCreate", &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    is % IO % IOLayout(de) % ncid = ncid
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
#endif

  end subroutine IONCCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "IONCOpen()"

  subroutine IONCOpen(IOComp, fileName, keywordEnforcer, filePath, localDe, rc)
    type(ESMF_GridComp), intent(inout)         :: IOComp
    character(len=*),    intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),    intent(in),  optional :: filePath
    integer,             intent(in),  optional :: localDe
    integer,             intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: ncStatus
    integer :: de, tileCount
    character(len=ESMF_MAXPATHLEN) :: fullName
    type(ioWrapper) :: is
    type(ESMF_Grid) :: grid

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (.not.ESMF_GridCompIsPetLocal(IOComp)) return

#ifdef ESMF_NETCDF
    de = 0
    if (present(localDe)) de = localDe

    call ESMF_GridCompGetInternalState(IOComp, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_GridCompGet(IOComp, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    call ESMF_GridGet(grid, tileCount=tileCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (is % IO % IOLayout(de) % localIOflag) then

      if (tileCount > 1) then
        call IOFilenameGet(fullName, fileName, filePath=filePath, &
          tile=is % IO % IOLayout(de) % tile)
      else
        call IOFilenameGet(fullName, fileName, filePath=filePath)
      end if

      ncStatus = nf90_open(trim(fullName), NF90_NOWRITE, is % IO % IOLayout(de) % ncid)
      if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
        msg="Error opening NetCDF data set: "//trim(fileName), &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    end if
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
#endif

  end subroutine IONCOpen

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "IONCClose()"

  subroutine IONCClose(IOComp, keywordEnforcer, localDe, rc)
    type(ESMF_GridComp), intent(inout)         :: IOComp
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(in),  optional :: localDe
    integer,             intent(out), optional :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, ncStatus
    type(ioWrapper) :: is

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (.not.ESMF_GridCompIsPetLocal(IOComp)) return

#ifdef ESMF_NETCDF
    de = 0
    if (present(localDe)) de = localDe

    call ESMF_GridCompGetInternalState(IOComp, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out

    if (.not.associated(is % IO)) return
    if (.not.associated(is % IO % IOLayout)) return

    if (is % IO % IOLayout(de) % ncid /= 0) then
      ncStatus = nf90_close(is % IO % IOLayout(de) % ncid)
      if (ESMF_LogFoundNetCDFError(ncerrToCheck=ncStatus, &
        msg="Error closing NetCDF data set", &
        ESMF_CONTEXT, rcToReturn=rc)) return  ! bail out
      is % IO % IOLayout(de) % ncid = 0
    end if
#else
    call ESMF_LogSetError(rcToCheck=ESMF_RC_LIB_NOT_PRESENT, &
                 msg="- ESMF_NETCDF not defined when lib was compiled", &
                 ESMF_CONTEXT, rcToReturn=rc)
#endif

  end subroutine IONCClose

!------------------------------------------------------------------------------

  subroutine IOCompNoOp(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    rc = ESMF_SUCCESS
  end subroutine IOCompNoOp

!------------------------------------------------------------------------------

  subroutine IOCompSetServices(IOComp, rc)
    type(ESMF_GridComp)  :: IOComp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call ESMF_GridCompSetEntryPoint(IOComp, ESMF_METHOD_INITIALIZE, IOCompNoOp, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT)) return  ! bail out

    call ESMF_GridCompSetEntryPoint(IOComp, ESMF_METHOD_RUN, IOCompNoOp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT)) return  ! bail out

    call ESMF_GridCompSetEntryPoint(IOComp, ESMF_METHOD_FINALIZE, IOCompNoOp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT)) return  ! bail out
  
  end subroutine IOCompSetServices

end module ESMFIOMod
