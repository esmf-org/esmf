! $Id: ESMF_XGridGet.F90,v 1.1 2010/07/20 21:10:20 feiliu Exp $
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
    '$Id: ESMF_XGridGet.F90,v 1.1 2010/07/20 21:10:20 feiliu Exp $'

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
        module procedure ESMF_XGridGetEle


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
!BOPI
! !IROUTINE:  ESMF_XGridGetDefault - Get default information from XGrid

! !INTERFACE:

subroutine ESMF_XGridGetDefault(xgrid, &
    sideA, sideB, area, centroid, &
    distgridA, distgridB, distgridM, &
    dimCount, localDECount, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, &
    name, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid), intent(in)                 :: xgrid
type(ESMF_Grid), intent(out), optional       :: sideA(:), sideB(:)
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
!EOPI

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

    if(present(sideA)) then
        ngrid_a = size(sideA, 1)
        if(ngrid_a /= size(xgtypep%sideA, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
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
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
               "- size of sideB doesn't match the size of sideB in the XGrid", &
               ESMF_CONTEXT, rc) 
            return
        endif
        do i = 1, ngrid_a
            sideB(i) = xgtypep%sideB(i)
        enddo 
    endif

    if(present(area)) then
        ncells = size(area,1)
        if(.not. associated(xgtypep%area)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized area in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ncells /= size(xgtypep%area, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
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
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized centroid in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ncells /= size(xgtypep%centroid, 2) .or. &
           ndim  /= size(xgtypep%centroid, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
               "- size of centroid doesn't match the size of centroid in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif
        centroid = xgtypep%centroid
    endif 

    if(present(distgridA)) then
        ngrid_a = size(distgridA)
        if(ngrid_a /= size(xgtypep%distgridA, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
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
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
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
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized sparseMatA2X in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatA2X, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
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
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized sparseMatX2A in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatX2A, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
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
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized sparseMatB2X in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatB2X, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
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
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, &
               "- uninitialized sparseMatX2B in the XGrid", &
               ESMF_CONTEXT, rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatX2B, 1)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_WRONG, & 
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
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    if(present(dimCount)) then
        dimCount = 1
    endif

    if(present(localDECount)) then
        call ESMF_DistGridGet(xgtypep%distgridM, delayout=delayout, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_DELayoutGet(delayout, localDECount=localDECount, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    ! success
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGetDefault

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetEle()"
!BOPI
! !IROUTINE:  ESMF_XGridGetEle - Get information about XGrid

! !INTERFACE:

subroutine ESMF_XGridGetEle(xgrid, &
    localDE, elementCount, &
    exclusiveLBound, exclusiveUBound, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid), intent(in)                 :: xgrid
integer, intent(in)                          :: localDE
integer, intent(out), optional               :: elementCount
integer, intent(out), optional               :: exclusiveLBound(ESMF_MAXDIM)
integer, intent(out), optional               :: exclusiveUBound(ESMF_MAXDIM)
integer, intent(out), optional               :: rc 
!
! !DESCRIPTION:
!      Get information about XGrid
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
!EOPI

    type(ESMF_XGridType), pointer   :: xgtypep
    type(ESMF_DELayout)             :: delayout
    integer                         :: ldeCount, localrc
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
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
    endif

    if(present(exclusiveLBound)) then
        call ESMF_DistGridGet(xgtypep%distgridM, delayout=delayout, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_DELayoutGet(delayout, localDECount=ldeCount, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        allocate(minIndex(1, ldeCount))
        call ESMF_DistGridGet(xgtypep%distgridM, delayout=delayout, &
            minIndexPDimPDe=minIndex, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        exclusiveLBound(1) = minIndex(1,localDE+1)
        deallocate(minIndex)
    endif

    if(present(exclusiveUBound)) then
        call ESMF_DistGridGet(xgtypep%distgridM, delayout=delayout, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        call ESMF_DELayoutGet(delayout, localDECount=ldeCount, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        allocate(maxIndex(1, ldeCount))
        call ESMF_DistGridGet(xgtypep%distgridM, delayout=delayout, &
            maxIndexPDimPDe=maxIndex, &
            rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rc)) return
        exclusiveUBound(1) = maxIndex(1,localDE+1)
        deallocate(maxIndex)
    endif

    if(present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_XGridGetEle

!------------------------------------------------------------------------------ 
end module ESMF_XGridGetMod
