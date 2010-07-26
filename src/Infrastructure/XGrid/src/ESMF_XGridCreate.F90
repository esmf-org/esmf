! $Id: ESMF_XGridCreate.F90,v 1.4 2010/07/26 19:38:00 feiliu Exp $
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
#define ESMF_FILENAME "ESMF_XGridCreate.F90"
!==============================================================================
!
!     ESMF XGridCreate module
module ESMF_XGridCreateMod
!
!==============================================================================
!
! This file contains the XGridCreate APIs
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_XGridCreateMod - APIs to create XGrid
!
! !DESCRIPTION:
!
! Implements XGridCreate APIs
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_DistGridMod
  use ESMF_GridMod
  use ESMF_XGridMod
  use ESMF_InitMacrosMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
    public ESMF_XGridCreate                 ! Create
    public ESMF_XGridDestroy                ! Destroy
!
!
!EOPI
   
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_XGridCreate.F90,v 1.4 2010/07/26 19:38:00 feiliu Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_XGridCreate - Create an XGrid
!
! !INTERFACE:
    interface ESMF_XGridCreate
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_XGridCreateRaw
!        module procedure ESMF_XGridCreateOnline


! !DESCRIPTION:
!    Create an XGrid from raw input parameters
 
!EOPI
      end interface
!
!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridCreateOnline()"
!BOPI
! !IROUTINE:  ESMF_XGridCreateOnline - Create an XGrid online from user input

! !INTERFACE:

type(ESMF_XGrid) function ESMF_XGridCreateOnline(sideA, sideB, sideAPriority, &
sideBPriority, storeOverlay, name, rc)
type(ESMF_Grid), intent(in)     :: sideA(:), sideB(:)
integer, intent(in), optional   :: sideAPriority(:), sideBPriority(:)
logical, intent(in), optional   :: storeOverlay
character(len=*), intent(in), optional :: name
integer, intent(out), optional  :: rc
!
! !DESCRIPTION:
!      Create an XGrid onine from user input
!
!     The arguments are:
!     \begin{description}
!     \item [sideA]
!           2D Grids on side A
!     \item [sideB]
!           2D Grids on side B
!     \item [{[sideAPriority]}]
!           Priority array of Grids on sideA during overlay generation.
!           The priority arrays describe the priorities of Grids at the overlapping region.
!           Flux contributions at the overlapping region are computed from the Grid of the
!           highest priority.
!     \item [{[sideBPriority]}]
!           priority of Grids on sideB during overlay generation
!           The priority arrays describe the priorities of Grids at the overlapping region.
!           Flux contributions at the overlapping region are computed from the Grid of the
!           highest priority.
!     \item [{[storeOverlay]}]
!           Setting the storeOverlay optional argument to .false. (default) 
!           allows a user to bypass internal calculation of the fully 
!           unstructured grid and its storage.
!     \item [{[name]}]
!           name of the xgrid object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOPI
    integer :: localrc, ngrid_a, ngrid_b, i
    type(ESMF_XGridType), pointer :: xgtype

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input Grids
    ngrid_a = size(sideA, 1)
    ngrid_b = size(sideB, 1)
    if(ngrid_a .le. 0 .or. ngrid_b .le. 0) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- number of Grids are invalid on one side of the XGrid", &
           ESMF_CONTEXT, rc) 
        return
    endif
    do i = 1, ngrid_a
        ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideA(i),rc)
    enddo
    do i = 1, ngrid_b
        ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideB(i),rc)
    enddo

    ! initialize XGridType object and its base object
    nullify(xgtype)
    nullify(ESMF_XGridCreateOnline%xgtypep)
    call ESMF_XGridConstructBaseObj(xgtype, name, localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                "Constructing xgtype base object ", &
                                ESMF_CONTEXT, rc)) return

    ! copy the Grids
    allocate(xgtype%sideA(ngrid_a), xgtype%sideB(ngrid_b), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
        "- Allocating xgtype%grids ", &
        ESMF_CONTEXT, rc)) return
    xgtype%sideA = sideA
    xgtype%sideB = sideB

    !TODO: call into online regridding to compute the distgrids

    ! Finalize XGrid Creation
    xgtype%status = ESMF_STATUS_READY
    ESMF_XGridCreateOnline%xgtypep => xgtype 
    ESMF_INIT_SET_CREATED(ESMF_XGridCreateOnline)

    call ESMF_XGridValidate(ESMF_XGridCreateOnline, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridCreateOnline


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridCreateRaw()"
!BOPI
! !IROUTINE:  ESMF_XGridCreateRaw - Create an XGrid from raw input parameters

! !INTERFACE:

type(ESMF_XGrid) function ESMF_XGridCreateRaw(sideA, sideB, area, centroid, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, &
    name, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_Grid), intent(in)     :: sideA(:), sideB(:)
real*8, intent(in), optional    :: area(:)
real*8, intent(in), optional    :: centroid(:,:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatA2X(:), sparseMatX2A(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatB2X(:), sparseMatX2B(:)
character (len=*), intent(in), optional :: name
integer, intent(out), optional  :: rc 
!
! !DESCRIPTION:
!      Create an XGrid directly from raw input parameters
!
!     The arguments are:
!     \begin{description}
!     \item [sideA]
!           2D Grids on side A
!     \item [sideB]
!           2D Grids on side B
!     \item [{[area]}]
!           area of the xgrid cells
!     \item [{[centroid]}]
!           coordinates at the area weighted center of the xgrid cells
!     \item [{[sparseMatA2X]}]
!           indexlist from a Grid index space on side A to xgrid index space
!           indexFactorlist from a Grid index space on side A to xgrid index space
!     \item [{[sparseMatX2A]}]
!           indexlist from xgrid index space to a Grid index space on side A
!           indexFactorlist from xgrid index space to a Grid index space on side A
!     \item [{[sparseMatB2X]}]
!           indexlist from a Grid index space on side B to xgrid index space
!           indexFactorlist from a Grid index space on side B to xgrid index space
!     \item [{[sparseMatX2B]}]
!           indexlist from xgrid index space to a Grid index space on side B
!           indexFactorlist from xgrid index space to a Grid index space on side B
!     \item [{[name]}]
!           name of the xgrid object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOPI

    integer :: localrc, ngrid_a, ngrid_b, n_idx_a2x, n_idx_x2a, n_idx_b2x, n_idx_x2b
    integer :: n_wgts_a, n_wgts_b, ndim, ncells, i
    type(ESMF_XGridType), pointer :: xgtype

    ! clearly, srcIdxList should be 1D, but what about distgridM_idxlist??
    integer, allocatable :: srcIdxList(:), distgridM_idxlist(:)


    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input Grids
    ngrid_a = size(sideA, 1)
    ngrid_b = size(sideB, 1)
    if(ngrid_a .le. 0 .or. ngrid_b .le. 0) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- number of Grids are invalid on one side of the XGrid", &
           ESMF_CONTEXT, rc) 
        return
    endif
    do i = 1, ngrid_a
        ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideA(i),rc)
    enddo
    do i = 1, ngrid_b
        ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideB(i),rc)
    enddo

    ! initialize XGridType object and its base object
    nullify(xgtype)
    nullify(ESMF_XGridCreateRaw%xgtypep)
    call ESMF_XGridConstructBaseObj(xgtype, name, localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
                                "Constructing xgtype base object ", &
                                ESMF_CONTEXT, rc)) return

    ! copy the Grids
    allocate(xgtype%sideA(ngrid_a), xgtype%sideB(ngrid_b), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
        "- Allocating xgtype%grids ", &
        ESMF_CONTEXT, rc)) return
    xgtype%sideA = sideA
    xgtype%sideB = sideB

    ! copy area and centroid
    if(present(area)) then
        ncells = size(area, 1)
        allocate(xgtype%area(ncells), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Allocating xgtype%area ", &
            ESMF_CONTEXT, rc)) return
        xgtype%area = area
    endif

    if(present(centroid)) then
        ncells = size(centroid, 1)
        ndim = size(centroid, 2)
        allocate(xgtype%centroid(ncells, ndim), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Allocating xgtype%centroid ", &
            ESMF_CONTEXT, rc)) return
        xgtype%area = area
    endif

    ! check and copy all the sparse matrix spec structures
    if(present(sparseMatA2X)) then
        call ESMF_SparseMatca(sparseMatA2X, xgtype%sparseMatA2X, ngrid_a, 'sparseMatA2X', rc=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Initializing xgtype%sparseMatX2A ", &
            ESMF_CONTEXT, rc)) return
    endif

    if(present(sparseMatX2A)) then
        call ESMF_SparseMatca(sparseMatX2A, xgtype%sparseMatX2A, ngrid_a, 'sparseMatX2A', rc=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Initializing xgtype%sparseMatX2A ", &
            ESMF_CONTEXT, rc)) return
    endif

    ! TODO:
    ! if both A2X and X2A are present, check the sequence index list of X are identical
    ! this checking will be collective since the indices needs to be gathered
    ! if(present(sparseMatA2X) .and. present(sparseMatX2A)) then
    ! endif

    if(present(sparseMatB2X)) then
        call ESMF_SparseMatca(sparseMatB2X, xgtype%sparseMatB2X, ngrid_b, 'sparseMatB2X', rc=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Initializing xgtype%sparseMatB2X ", &
            ESMF_CONTEXT, rc)) return
    endif

    if(present(sparseMatX2B)) then
        call ESMF_SparseMatca(sparseMatX2B, xgtype%sparseMatX2B, ngrid_b, 'sparseMatX2B', rc=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Initializing xgtype%sparseMatX2B ", &
            ESMF_CONTEXT, rc)) return
    endif

    ! TODO:
    ! if both B2X and X2B are present, check the sequence index list of X are identical
    ! this checking will be collective since the indices needs to be gathered
    ! if(present(sparseMatA2X) .and. present(sparseMatX2A)) then
    ! endif

    ! create the distgrids
    call ESMF_XGridDistGrids(xgtype, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    ! Finalize XGrid Creation
    xgtype%status = ESMF_STATUS_READY
    ESMF_XGridCreateRaw%xgtypep => xgtype 
    ESMF_INIT_SET_CREATED(ESMF_XGridCreateRaw)

    call ESMF_XGridValidate(ESMF_XGridCreateRaw, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridCreateRaw

! modularize this to have a subroutine to compute union of integer sets
! this will make debuging easier
subroutine ESMF_XGridDistGrids(xgtype, rc)

    type(ESMF_XGridType), intent(inout) :: xgtype
    integer, intent(out), optional      :: rc

    integer :: i, ngrid, localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! create the A side distgrids
    if(associated(xgtype%sparseMatA2X)) then
        ngrid = size(xgtype%sideA, 1)
        allocate(xgtype%distgridA(ngrid), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Allocating xgtype%distgridA(ngrid) ", &
            ESMF_CONTEXT, rc)) return
        do i = 1, ngrid
            call ESMF_XGridDG(xgtype%sideA(i), xgtype%distgridA(i), &
                xgtype%sparseMatA2X(i)%factorIndexList, 2, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo
    endif

    ! if A2X is not provided and X2A is provided
    ! compute A side distgrids based on X2A 
    if(.not. associated(xgtype%sparseMatA2X) .and. &
        associated(xgtype%sparseMatX2A)) then
        ngrid = size(xgtype%sideA, 1)
        allocate(xgtype%distgridA(ngrid), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Allocating xgtype%distgridA(ngrid) ", &
            ESMF_CONTEXT, rc)) return
        do i = 1, ngrid
            call ESMF_XGridDG(xgtype%sideA(i), xgtype%distgridA(i), &
                xgtype%sparseMatX2A(i)%factorIndexList, 1, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo
    endif

    ! create the B side distgrids
    if(associated(xgtype%sparseMatB2X)) then
        ngrid = size(xgtype%sideB, 1)
        allocate(xgtype%distgridB(ngrid), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Allocating xgtype%distgridB(ngrid) ", &
            ESMF_CONTEXT, rc)) return
        do i = 1, ngrid
            call ESMF_XGridDG(xgtype%sideB(i), xgtype%distgridB(i), &
                xgtype%sparseMatB2X(i)%factorIndexList, 2, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo
    endif

    ! if B2X is not provided and X2B is provided
    ! compute B side distgrids based on X2B 
    if(.not. associated(xgtype%sparseMatB2X) .and. &
        associated(xgtype%sparseMatX2B)) then
        ngrid = size(xgtype%sideB, 1)
        allocate(xgtype%distgridB(ngrid), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Allocating xgtype%distgridB(ngrid) ", &
            ESMF_CONTEXT, rc)) return
        do i = 1, ngrid
            call ESMF_XGridDG(xgtype%sideB(i), xgtype%distgridB(i), &
                xgtype%sparseMatX2B(i)%factorIndexList, 1, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
        enddo
    endif

    ! use the union of A2X indices to create the balanced distgrid
    if(associated(xgtype%sparseMatA2X)) then
        xgtype%distgridM = ESMF_XGridDGOverlay(xgtype%sparseMatA2X, 2, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    ! if A2X is not provided and X2A is provided
    ! use the union of X2A indices to create the balanced distgrid
    if(.not. associated(xgtype%sparseMatA2X) .and. &
        associated(xgtype%sparseMatX2A)) then
        xgtype%distgridM = ESMF_XGridDGOverlay(xgtype%sparseMatX2A, 1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    ! use the union of B2X indices to create the balanced distgrid
    if(.not. associated(xgtype%sparseMatA2X) .and. &
       .not. associated(xgtype%sparseMatX2A) .and. &
        associated(xgtype%sparseMatB2X)) then
        xgtype%distgridM = ESMF_XGridDGOverlay(xgtype%sparseMatB2X, 2, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    ! use the union of X2B indices to create the balanced distgrid
    if(.not. associated(xgtype%sparseMatA2X) .and. &
       .not. associated(xgtype%sparseMatX2A) .and. &
       .not. associated(xgtype%sparseMatB2X) .and. &
        associated(xgtype%sparseMatX2B)) then
        xgtype%distgridM = ESMF_XGridDGOverlay(xgtype%sparseMatX2B, 1, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    if(.not. associated(xgtype%sparseMatA2X) .and. &
       .not. associated(xgtype%sparseMatX2A) .and. &
       .not. associated(xgtype%sparseMatB2X) .and. &
       .not. associated(xgtype%sparseMatX2B)) then
        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
           "- one of the sparse matrix arguments must be specified", &
           ESMF_CONTEXT, rc) 
        return

    endif

    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridDistGrids

!------------------------------------------------------------------------------
type(ESMF_DistGrid) function ESMF_XGridDGOverlay(sparseMat, dim, rc)
    type(ESMF_XGridSpec), intent(in), pointer   :: sparseMat(:)
    integer, intent(in)                         :: dim
    integer, intent(out), optional              :: rc

    integer :: i, j, ii, ngrid, localrc, nidx, nidx_tot, l, u
    integer :: minidx, maxidx, minidx1, maxidx1, minidx_n, maxidx_n
    integer, allocatable :: indices(:), indices_diff(:), indices_union(:)
    integer, allocatable :: iarray(:), iarray_t(:)

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ngrid = size(sparseMat, 1)

    ! generate the union of indices from all the A2X factorIndexLists
    ! generate the initial array that has the index positions marked '1'
    minidx = minval(sparseMat(1)%factorIndexList(dim,:))
    maxidx = maxval(sparseMat(1)%factorIndexList(dim,:))
    allocate(iarray(minidx:maxidx), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
        "- Allocating iarray(minidx:maxidx) ", &
        ESMF_CONTEXT, rc)) return
    iarray = 0
    do i = minidx, maxidx
        iarray(sparseMat(1)%factorIndexList(dim,i)) = 1
    enddo

    do i = 2, ngrid

        minidx1 = minval(sparseMat(i)%factorIndexList(dim,:))
        maxidx1 = maxval(sparseMat(i)%factorIndexList(dim,:))
        minidx_n = min(minidx, minidx1)
        maxidx_n = max(maxidx, maxidx1)

        allocate(iarray_t(minidx_n:maxidx_n), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Allocating iarray_t(minidx_n:maxidx_n) ", &
            ESMF_CONTEXT, rc)) return
        ! copy the old index position array
        do j = minidx, maxidx
            iarray_t(j) = iarray(j)
        enddo
        ! toggle the index position array with the new index list
        l = lbound(sparseMat(i)%factorIndexList, dim)
        u = ubound(sparseMat(i)%factorIndexList, dim)
        do j = l, u
            iarray_t(sparseMat(i)%factorIndexList(dim,j)) = 1
        enddo

        minidx = minidx_n
        maxidx = maxidx_n

        ! reset the index posity array, swap the temp one over
        deallocate(iarray)         
        allocate(iarray(minidx:maxidx), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
            "- Allocating iarray(minidx:maxidx) ", &
            ESMF_CONTEXT, rc)) return
        do j = minidx, maxidx
            iarray(j) = iarray_t(j)
        enddo
        deallocate(iarray_t)
    enddo

    ! compress the iarray into the index list
    ! first count how many 1s are thyere
    nidx = 0
    do i = minidx, maxidx
        if(iarray(i) .eq. 1) nidx = nidx + 1
    enddo

    allocate(indices(nidx), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
        "- Allocating indices(nidx) ", &
        ESMF_CONTEXT, rc)) return

    ! every marked position means that index exists
    ! add that index to indices array
    ii = 1
    do i = minidx, maxidx
        if(iarray(i) .eq. 1) then
            indices(ii) = i
            ii = ii + 1
        endif
    enddo

    ESMF_XGridDGOverlay = ESMF_DistGridCreate(indices, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    
    deallocate(iarray, indices)

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridDGOverlay

!------------------------------------------------------------------------------
subroutine ESMF_XGridDG(grid, distgrid, factorIndexList, dim, rc)

    type(ESMF_Grid), intent(in)                 :: grid
    type(ESMF_DistGrid), intent(inout)          :: distgrid
    integer, intent(in), pointer                :: factorIndexList(:,:)
    integer, intent(in)                         :: dim
    integer, intent(out), optional              :: rc

    integer                                     :: localrc, nidx_src, nidx_dst

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    !print *, dim, size(factorIndexList, 2), factorIndexList(dim, :)

    distgrid = ESMF_DistGridCreate(factorIndexList(dim,:), rc=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
        "- Creating distgrid from factorIndexList", &
        ESMF_CONTEXT, rc)) return
    
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridDG

!------------------------------------------------------------------------------
subroutine ESMF_SparseMatca(sparseMats, sparseMatd, ngrid, tag, rc)

    type(ESMF_XGridSpec), intent(in)    :: sparseMats(:)
    type(ESMF_XGridSpec), intent(out), pointer   :: sparseMatd(:)
    integer, intent(in)                 :: ngrid
    character(len=*), intent(in)        :: tag
    integer, intent(out), optional      :: rc

    integer                             :: i, localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    if(size(sparseMats,1) /= ngrid) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
           "- number of Grids different from size of sparseMat for "//tag, &
           ESMF_CONTEXT, rc) 
        return
    endif

    do i = 1, ngrid
        if(.not. associated(sparseMats(i)%factorIndexList) .or. &
          .not. associated(sparseMats(i)%factorList)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
               "- sparseMat not initiailzed properly for "//tag, &
               ESMF_CONTEXT, rc) 
            return
        endif

        if(size(sparseMats(i)%factorIndexList, 2) /= size(sparseMats(i)%factorList, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
               "- sparseMat factorIndexList and factorList sizes not consistent "//tag, &
               ESMF_CONTEXT, rc) 
            return
        endif
    enddo
        
    allocate(sparseMatd(ngrid), stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
        "Allocating xgtype%"//tag, &
        ESMF_CONTEXT, rc)) return
    sparseMatd = sparseMats

    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_SparseMatca

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridConstructBaseObj()"
!BOPI
! !IROUTINE:  ESMF_XGridConstructBaseObj - Allocate xgtype pointer and its base object

! !INTERFACE:
subroutine ESMF_XGridConstructBaseObj(xgtype, name, rc)

!
! !ARGUMENTS:
    type(ESMF_XGridType), intent(inout), pointer :: xgtype
    character (len=*), intent(in), optional      :: name
    integer, intent(out), optional               :: rc 

!
! !DESCRIPTION:
!      Set one xgrid structure equal to another
!
!     The arguments are:
!     \begin{description}
!     \item [xgtype]
!           XGridType pointer to be constructed
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOPI

    integer                                     :: localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    allocate(xgtype, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, &
        "- Allocating XGrid Type", &
        ESMF_CONTEXT, rc)) return
    call ESMF_XGridInitialize(xgtype, rc=localrc) 
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    call ESMF_BaseCreate(xgtype%base, "XGrid", name, 0, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridConstructBaseObj
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridDestroy"
!BOPI
! !IROUTINE: ESMF_XGridDestroy - Free all resources associated with a XGrid
! !INTERFACE:
  subroutine ESMF_XGridDestroy(xgrid, rc)
!
! !ARGUMENTS:
    type(ESMF_XGrid) :: xgrid       
    integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
! Releases all resources associated with the {\tt ESMF\_XGrid}.
! 
! The arguments are:
! \begin{description}
! \item [xgrid]
!       {\tt ESMF\_XGrid} object.
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! Local variables
    integer :: localrc, i
    type(ESMF_Status) :: xgridstatus

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    call ESMF_XGridValidate(xgrid, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    if (.not.associated(xgrid%xgtypep)) then 
      call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
        "Uninitialized or already destroyed XGrid: xgtypep unassociated", &
        ESMF_CONTEXT, rc)
      return
    endif 

    ! Destruct all xgrid internals and then free xgrid memory.
    call ESMF_BaseGetStatus(xgrid%xgtypep%base, xgridstatus, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
        
    if (xgridstatus .eq. ESMF_STATUS_READY) then

      if((xgrid%xgtypep%is_proxy)) then

        if(associated(xgrid%xgtypep%distgridA)) then
          do i = 1, size(xgrid%xgtypep%distgridA,1)
            call ESMF_DistGridDestroy(xgrid%xgtypep%distgridA(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
          enddo
        endif
        if(associated(xgrid%xgtypep%distgridB)) then
          do i = 1, size(xgrid%xgtypep%distgridB,1)
            call ESMF_DistGridDestroy(xgrid%xgtypep%distgridB(i), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
          enddo
        endif

        !call ESMF_DistGridDestroy(xgrid%xgtypep%distgridM, rc=localrc)
        !if (ESMF_LogMsgFoundError(localrc, &
        !    ESMF_ERR_PASSTHRU, &
        !    ESMF_CONTEXT, rc)) return

        if(associated(xgrid%xgtypep%centroid)) then
            deallocate(xgrid%xgtypep%centroid)
        endif
        if(associated(xgrid%xgtypep%area)) then
            deallocate(xgrid%xgtypep%area)
        endif

        if(associated(xgrid%xgtypep%sparseMatA2X)) then
            deallocate(xgrid%xgtypep%sparseMatA2X)
        endif
        if(associated(xgrid%xgtypep%sparseMatX2A)) then
            deallocate(xgrid%xgtypep%sparseMatX2A)
        endif
        if(associated(xgrid%xgtypep%sparseMatB2X)) then
            deallocate(xgrid%xgtypep%sparseMatB2X)
        endif
        if(associated(xgrid%xgtypep%sparseMatX2B)) then
            deallocate(xgrid%xgtypep%sparseMatX2B)
        endif

      endif
      
    endif

    ! mark object invalid
    call ESMF_BaseSetStatus(xgrid%xgtypep%base, ESMF_STATUS_INVALID, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    ESMF_INIT_SET_DELETED(xgrid)

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_XGridDestroy

end module ESMF_XGridCreateMod
