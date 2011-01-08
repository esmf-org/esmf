! $Id: ESMF_XGridGet.F90,v 1.17 2011/01/08 16:22:39 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_XGridGet.F90"
!==============================================================================
!
!     ESMF XGridGet module
module ESMF_XGridGetMod
!
!==============================================================================
!
! This file contains the XGridGet APIs
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_XGridGetMod - APIs to get information from XGrid
!
! !DESCRIPTION:
!
! Implements XGridGet APIs
!
!------------------------------------------------------------------------------
! !USES:
  use ESMF_UtilTypesMod
  use ESMF_UtilMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_DistGridMod
  use ESMF_DELayoutMod
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
    public ESMF_XGridGet                 ! Get
!
!
!EOPI
   
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_XGridGet.F90,v 1.17 2011/01/08 16:22:39 svasquez Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_XGridGet - Get information from an XGrid
!
! !INTERFACE:
    interface ESMF_XGridGet
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_XGridGetDefault
        module procedure ESMF_XGridGetDG
        module procedure ESMF_XGridGetEle
        module procedure ESMF_XGridGetSMMSpec


! !DESCRIPTION:
!    Get default information from XGrid
 
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
#define ESMF_METHOD "ESMF_XGridGetDefault()"
!BOP
! !IROUTINE:  ESMF_XGridGet - Get default information from an XGrid

! !INTERFACE: ESMF_XGridGet
! ! Private name; call using ESMF_XGridGet()

subroutine ESMF_XGridGetDefault(xgrid, &
    sideA, sideB, ngridA, ngridB, area, centroid, &
    distgridA, distgridB, distgridM, &
    dimCount, localDECount, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, &
    name, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid), intent(in)                 :: xgrid
type(ESMF_Grid), intent(out), optional       :: sideA(:), sideB(:)
integer, intent(out), optional               :: ngridA, ngridB
real*8, intent(out), optional                :: area(:)
real*8, intent(out), optional                :: centroid(:,:)
type(ESMF_DistGrid), intent(out), optional   :: distgridA(:)
type(ESMF_DistGrid), intent(out), optional   :: distgridB(:)
type(ESMF_DistGrid), intent(out), optional   :: distgridM
integer, intent(out), optional               :: dimCount
integer, intent(out), optional               :: localDECount
type(ESMF_XGridSpec), intent(out), optional  :: sparseMatA2X(:), sparseMatX2A(:)
type(ESMF_XGridSpec), intent(out), optional  :: sparseMatB2X(:), sparseMatX2B(:)
character (len=*), intent(out), optional     :: name
integer, intent(out), optional               :: rc 
!
! !DESCRIPTION:
!      Get information about XGrid
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The xgrid object used to retrieve information from.
!     \item [{[sideA]}]
!           2D Grids on side A
!     \item [{[sideB]}]
!           2D Grids on side B
!     \item [{[ngridA]}]
!           Number of grids on the A side
!     \item [{[ngridB]}]
!           Number of grids on the B side
!     \item [{[area]}]
!           area of the xgrid cells
!     \item [{[centroid]}]
!           coordinates at the area weighted center of the xgrid cells
!     \item [{[distgridA]}]
!           list of distgrids whose sequence index list is an overlap between a Grid
!           on sideA and the xgrid object.
!     \item [{[distgridB]}]
!           list of distgrids whose sequence index list is an overlap between a Grid
!           on sideB and the xgrid object.
!     \item [{[distgridM]}]
!           the distgrid whose sequence index list fully describes the xgrid object.
!     \item [{[dimCount]}]
!           dimension of the xgrid 
!     \item [{[localDECount]}]
!           number of local DEs on local PET
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
!EOP

    integer :: localrc, ngrid_a, ngrid_b, n_idx_a2x, n_idx_x2a, n_idx_b2x, n_idx_x2b
    integer :: n_wgts_a, n_wgts_b, ndim, ncells, i
    type(ESMF_XGridType), pointer :: xgtypep
    type(ESMF_DELayout)           :: delayout

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    if(present(ngridA)) then
        ngridA = size(xgtypep%sideA, 1)
    endif

    if(present(ngridB)) then
        ngridB = size(xgtypep%sideB, 1)
    endif

    if(present(sideA)) then
        ngrid_a = size(sideA, 1)
        if(ngrid_a /= size(xgtypep%sideA, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               "- size of sideA doesn't match the size of sideA in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_a
            sideA(i) = xgtypep%sideA(i)
        enddo 
    endif
    if(present(sideB)) then
        ngrid_b = size(sideB, 1)
        if(ngrid_b /= size(xgtypep%sideB, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               "- size of sideB doesn't match the size of sideB in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_b
            sideB(i) = xgtypep%sideB(i)
        enddo 
    endif

    if(present(area)) then
        ncells = size(area,1)
        if(.not. associated(xgtypep%area)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized area in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ncells /= size(xgtypep%area, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
               "- size of area doesn't match the size of area in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif
        area = xgtypep%area
    endif 
    if(present(centroid)) then
        ndim = size(centroid, 1)
        ncells = size(centroid, 2)
        if(.not. associated(xgtypep%centroid)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized centroid in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ncells /= size(xgtypep%centroid, 2) .or. &
           ndim  /= size(xgtypep%centroid, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
               "- size of centroid doesn't match the size of centroid in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif
        centroid = xgtypep%centroid
    endif 

    if(present(distgridA)) then
        ngrid_a = size(distgridA)
        if(ngrid_a /= size(xgtypep%distgridA, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               "- size of distgridA doesn't match the size of distgridA in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_a
            distgridA(i) = xgtypep%distgridA(i)
        enddo 
    endif

    if(present(distgridB)) then
        ngrid_b = size(distgridB)
        if(ngrid_b /= size(xgtypep%distgridB, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               "- size of distgridB doesn't match the size of distgridB in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_b
            distgridB(i) = xgtypep%distgridB(i)
        enddo 
    endif

    if(present(distgridM)) then
        distgridM = xgtypep%distgridM
    endif

    if(present(sparseMatA2X)) then
        ngrid_a = size(sparseMatA2X, 1)
        if(.not. associated(xgtypep%sparseMatA2X)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized sparseMatA2X in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatA2X, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               "- size of sparseMatA2X doesn't match the size of sparseMatA2X in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_a
            sparseMatA2X(i) = xgtypep%sparseMatA2X(i)
        enddo 
    endif

    if(present(sparseMatX2A)) then
        ngrid_a = size(sparseMatX2A, 1)
        if(.not. associated(xgtypep%sparseMatX2A)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized sparseMatX2A in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatX2A, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               "- size of sparseMatX2A doesn't match the size of sparseMatX2A in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_a
            sparseMatX2A(i) = xgtypep%sparseMatX2A(i)
        enddo 
    endif

    if(present(sparseMatB2X)) then
        ngrid_a = size(sparseMatB2X, 1)
        if(.not. associated(xgtypep%sparseMatB2X)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized sparseMatB2X in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatB2X, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               "- size of sparseMatB2X doesn't match the size of sparseMatB2X in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_a
            sparseMatB2X(i) = xgtypep%sparseMatB2X(i)
        enddo 
    endif

    if(present(sparseMatX2B)) then
        ngrid_a = size(sparseMatX2B, 1)
        if(.not. associated(xgtypep%sparseMatX2B)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized sparseMatX2B in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatX2B, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               "- size of sparseMatX2B doesn't match the size of sparseMatX2B in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_a
            sparseMatX2B(i) = xgtypep%sparseMatX2B(i)
        enddo 
    endif
    
    if (present(name)) then
        call c_ESMC_GetName(xgtypep%base, name, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    if(present(dimCount)) then
        dimCount = 1
    endif

    if(present(localDECount)) then
        call ESMF_DistGridGet(xgtypep%distgridM, delayout=delayout, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_DELayoutGet(delayout, localDECount=localDECount, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    ! success
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGetDefault

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetSMMSpec()"
!BOPI
! !IROUTINE:  ESMF_XGridGet - Get an individual SparseMatSpec

! !INTERFACE: ESMF_XGridGet

subroutine ESMF_XGridGetSMMSpec(xgrid, sparseMat, srcSide, srcGridIndex, &
    dstSide, dstGridIndex, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid), intent(in)                 :: xgrid
type(ESMF_XGridSpec), intent(out)            :: sparseMat
type(ESMF_XGridSide), intent(in)             :: srcSide
integer, intent(in)                          :: srcGridIndex
type(ESMF_XGridSide), intent(in)             :: dstSide
integer, intent(in)                          :: dstGridIndex
integer, intent(out), optional               :: rc 
!
! !DESCRIPTION:
!      Get information about XGrid
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The xgrid object used to retrieve information from.
!     \item [distgrid]
!       Distgrid whose sequence index list is an overlap between gridIndex-th Grid
!       on xgridSide and the xgrid object.
!     \item [{[srcSide]}] 
!       Side of the XGrid from (either ESMF\_XGRID\_SIDEA,
!       ESMF\_XGRID\_SIDEB, or ESMF\_XGRID\_BALANCED).
!     \item [{[srcGridIndex]}] 
!       If xgridSide is  ESMF\_XGRID\_SIDEA or ESMF\_XGRID\_SIDEB then this index tells which Grid on
!       that side.
!     \item [{[dstSide]}] 
!       Side of the XGrid from (either ESMF\_XGRID\_SIDEA,
!       ESMF\_XGRID\_SIDEB, or ESMF\_XGRID\_BALANCED).
!     \item [{[dstGridIndex]}] 
!       If xgridSide is  ESMF\_XGRID\_SIDEA or ESMF\_XGRID\_SIDEB then this index tells which Grid on
!       that side.
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!       is created.
!     \end{description}
!
!EOPI

    type(ESMF_XGridType), pointer :: xgtypep

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    if(srcSide .eq. ESMF_XGRID_SIDEA .and. dstSide .eq. ESMF_XGRID_BALANCED) then
        sparseMat = xgtypep%SparseMatA2X(srcGridIndex)
    endif

    if(srcSide .eq. ESMF_XGRID_SIDEB .and. dstSide .eq. ESMF_XGRID_BALANCED) then
        sparseMat = xgtypep%SparseMatB2X(srcGridIndex)
    endif

    if(srcSide .eq. ESMF_XGRID_BALANCED .and. dstSide .eq. ESMF_XGRID_SIDEA) then
        sparseMat = xgtypep%SparseMatX2A(dstGridIndex)
    endif

    if(srcSide .eq. ESMF_XGRID_BALANCED .and. dstSide .eq. ESMF_XGRID_sideB) then
        sparseMat = xgtypep%SparseMatX2B(dstGridIndex)
    endif

    ! success
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGetSMMSpec

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetDG()"
!BOP
! !IROUTINE:  ESMF_XGridGet - Get an individual DistGrid

! !INTERFACE: ESMF_XGridGet
! ! Private name; call using ESMF_XGridGet()

subroutine ESMF_XGridGetDG(xgrid, distgrid, xgridSide, gridIndex, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid), intent(in)                 :: xgrid
type(ESMF_DistGrid), intent(out)             :: distgrid
type(ESMF_XGridSide), intent(in), optional   :: xgridSide
integer, intent(in), optional                :: gridIndex
integer, intent(out), optional               :: rc 
!
! !DESCRIPTION:
!      Get a distgrid from XGrid from a specific side. 
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The xgrid object used to retrieve information from.
!     \item [distgrid]
!       Distgrid whose sequence index list is an overlap between gridIndex-th Grid
!       on xgridSide and the xgrid object.
!     \item [{[xgridSide]}] 
!       \begin{sloppypar}
!       Which side of the XGrid to retrieve the distgrid from (either ESMF\_XGRID\_SIDEA,
!       ESMF\_XGRID\_SIDEB, or ESMF\_XGRID\_BALANCED). If not passed in then
!       defaults to ESMF\_XGRID\_BALANCED.
!       \end{sloppypar}
!     \item [{[xgridIndex]}] 
!       If xgridSide is ESMF\_XGRID\_SIDEA or ESMF\_XGRID\_SIDEB then this index 
!       selects the Distgrid associated with the Grid on
!       that side. If not provided, defaults to 1. 
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!       is created.
!     \end{description}
!
!EOP

    type(ESMF_XGridType), pointer :: xgtypep
    type(ESMF_XGridSide)          :: l_xgridSide
    integer                       :: l_gridIndex

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    if(present(xgridSide)) then
        l_xgridSide = xgridSide
    else                   
        l_xgridSide = ESMF_XGRID_BALANCED
    endif

    if(present(gridIndex)) then
        l_gridIndex = gridIndex
    else                   
        l_gridIndex = 1
    endif

    if(l_gridIndex .lt. 0) then
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
           "- gridIndex cannot be less than 0", &
           ESMF_CONTEXT, rc) 
        return
    endif

    if(l_xgridSide .eq. ESMF_XGRID_BALANCED) then
        distgrid = xgtypep%distgridM
    endif

    if(l_xgridSide .eq. ESMF_XGRID_SIDEA) then
        if(l_gridIndex .gt. size(xgtypep%distgridA, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
"- gridIndex cannot be greater than the size of distgridA in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        distgrid = xgtypep%distgridA(l_gridIndex)
    endif

    if(l_xgridSide .eq. ESMF_XGRID_SIDEB) then
        if(l_gridIndex .gt. size(xgtypep%distgridB, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
"- gridIndex cannot be greater than the size of distgridB in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        distgrid = xgtypep%distgridB(l_gridIndex)
    endif

    ! success
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGetDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetEle()"
!BOP
! !IROUTINE:  ESMF_XGridGet - Get information about an XGrid

! !INTERFACE: ESMF_XGridGet
! ! Private name; call using ESMF_XGridGet()

subroutine ESMF_XGridGetEle(xgrid, &
    localDE, elementCount, &
    exclusiveCount, exclusiveLBound, exclusiveUBound, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid), intent(in)                 :: xgrid
integer, intent(in)                          :: localDE
integer, intent(out), optional               :: elementCount
integer, intent(out), optional               :: exclusiveCount
integer, intent(out), optional               :: exclusiveLBound
integer, intent(out), optional               :: exclusiveUBound
integer, intent(out), optional               :: rc 
!
! !DESCRIPTION:
!      Get localDE specific information about XGrid
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The xgrid object used to retrieve information from.
!     \item [localDE]
!       Local DE for which information is requested.
!          [0,..,localDeCount-1]
!     \item [{[elementCount]}]
!          Number of elements in exclusive region per DE
!     \item [{[exclusiveLBound]}]
!          Lower bound of sequence indices in exclusive region per DE
!     \item [{[exclusiveUBound]}]
!          Upper bound of sequence indices in exclusive region per DE
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOP

    type(ESMF_XGridType), pointer   :: xgtypep
    type(ESMF_DELayout)             :: delayout
    integer                         :: deCount, localrc
    integer, allocatable            :: minIndex(:,:), maxIndex(:,:)

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    if(present(elementCount)) then
        call ESMF_DistGridGet(xgtypep%distgridM, localDE, elementCount=elementCount, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    call ESMF_DistGridGet(xgtypep%distgridM, delayout=delayout, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    call ESMF_DELayoutGet(delayout, deCount=deCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return

    allocate(minIndex(1, deCount))
    call ESMF_DistGridGet(xgtypep%distgridM, minIndexPDimPDe=minIndex, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    if(present(exclusiveLBound)) then
        exclusiveLBound = minIndex(1,localDE+1)
    endif

    allocate(maxIndex(1, deCount))
    call ESMF_DistGridGet(xgtypep%distgridM, maxIndexPDimPDe=maxIndex, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    if(present(exclusiveUBound)) then
        exclusiveUBound = maxIndex(1,localDE+1)
    endif

    if(present(exclusiveCount)) then
        exclusiveCount = maxIndex(1,localDE+1) - minIndex(1,localDE+1) + 1
    endif

    deallocate(minIndex)
    deallocate(maxIndex)

    if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_XGridGetEle

!------------------------------------------------------------------------------ 
end module ESMF_XGridGetMod
