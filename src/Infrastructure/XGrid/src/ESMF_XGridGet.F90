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
  use ESMF_StaggerLocMod
  use ESMF_ArrayMod
  use ESMF_GridMod
  use ESMF_MeshMod
  use ESMF_XGridGeomBaseMod
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
    '$Id$'

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
        module procedure ESMF_XGridGetEle
        module procedure ESMF_XGridGetDefault
        module procedure ESMF_XGridGetDG
        module procedure ESMF_XGridGetGB
        module procedure ESMF_XGridGetGeomObj
        module procedure ESMF_XGridGetSMMSpecFrac


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
! !IROUTINE:  ESMF_XGridGet - Get object-wide information from an XGrid

! !INTERFACE: ESMF_XGridGet
! ! Private name; call using ESMF_XGridGet()

subroutine ESMF_XGridGetDefault(xgrid, keywordEnforcer, &
    sideAGridCount, sideBGridCount, sideAMeshCount, sideBMeshCount, &
    dimCount, elementCount, &
    sideAGrid, sideBGrid, sideAMesh, sideBMesh, &
    mesh, &
    area, centroid, &
    distgridA, distgridB, distgridM, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, &
    name, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid),     intent(in)            :: xgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
integer,              intent(out), optional :: sideAGridCount, sideBGridCount
integer,              intent(out), optional :: sideAMeshCount, sideBMeshCount
integer,              intent(out), optional :: dimCount
integer,              intent(out), optional :: elementCount
type(ESMF_Grid),      intent(out), optional :: sideAGrid(:), sideBGrid(:)
type(ESMF_Mesh),      intent(out), optional :: sideAMesh(:), sideBMesh(:)
type(ESMF_Mesh),      intent(out), optional :: mesh
real(ESMF_KIND_R8),   intent(out), optional :: area(:)
real(ESMF_KIND_R8),   intent(out), optional :: centroid(:,:)
type(ESMF_DistGrid),  intent(out), optional :: distgridA(:)
type(ESMF_DistGrid),  intent(out), optional :: distgridB(:)
type(ESMF_DistGrid),  intent(out), optional :: distgridM
type(ESMF_XGridSpec), intent(out), optional :: sparseMatA2X(:)
type(ESMF_XGridSpec), intent(out), optional :: sparseMatX2A(:)
type(ESMF_XGridSpec), intent(out), optional :: sparseMatB2X(:)
type(ESMF_XGridSpec), intent(out), optional :: sparseMatX2B(:)
character (len=*),    intent(out), optional :: name
integer,              intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Get information about XGrid
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The {\tt ESMF\_XGrid} object used to retrieve information from.
!     \item [{[sideAGridCount]}]
!           Total Number of Grids on the A side.
!     \item [{[sideBGridCount]}]
!           Total Number of Grids on the B side.
!     \item [{[sideAMeshCount]}]
!           Total Number of Meshes on the A side.
!     \item [{[sideBMeshCount]}]
!           Total Number of Meshes on the B side.
!     \item [{[dimCount]}]
!           Number of dimension of the xgrid.
!     \item [{[elementCount]}]
!          Number of elements in exclusive region of the xgrid on this PET.
!     \item [{[sideAGrid]}]
!           List of 2D Grids on side A. Must enter with shape(sideAGrid)=(/sideAGridCount/).
!     \item [{[sideBGrid]}]
!           List of 2D Grids on side B. Must enter with shape(sideBGrid)=(/sideBGridCount/).
!     \item [{[sideAMesh]}]
!           List of 2D Meshes on side A. Must enter with shape(sideAMesh)=(/sideAMeshCount/).
!     \item [{[sideBMesh]}]
!           List of 2D Meshes on side B. Must enter with shape(sideBMesh)=(/sideBMeshCount/).
!     \item [{[mesh]}]
!           Super mesh stored in XGrid when storeOverlay is set true during XGrid creation.
!     \item [{[area]}]
!           Area of the xgrid cells on this PET. Must enter with shape(area)=(/elementCount/).
!     \item [{[centroid]}]
!           Coordinates at the area weighted center of the xgrid cells on this PET. Must enter with shape(centroid)=(/dimCount, elementCount/).
!     \item [{[distgridA]}]
!           List of distgrids whose sequence index list is an overlap between a Grid
!           on sideA and the xgrid object. Must enter with shape(distgridA)=(/sideAGridCount+sideAMeshCount/).
!     \item [{[distgridB]}]
!           List of distgrids whose sequence index list is an overlap between a Grid
!           on sideB and the xgrid object. Must enter with shape(distgridB)=(/sideBGridCount+sideBMeshCount/).
!     \item [{[distgridM]}]
!           The distgrid whose sequence index list fully describes the xgrid object.
!     \item [{[sparseMatA2X]}]
!           Indexlist from a Grid index space on side A to xgrid index space; 
!           indexFactorlist from a Grid index space on side A to xgrid index space. Must enter with shape(sparsematA2X)=(/sideAGridCount+sideAMeshCount/).
!     \item [{[sparseMatX2A]}]
!           Indexlist from xgrid index space to a Grid index space on side A; 
!           indexFactorlist from xgrid index space to a Grid index space on side A. Must enter with shape(sparsematX2A)=(/sideAGridCount+sideAMeshCount/).
!     \item [{[sparseMatB2X]}]
!           Indexlist from a Grid index space on side B to xgrid index space; 
!           indexFactorlist from a Grid index space on side B to xgrid index space. Must enter with shape(sparsematB2X)=(/sideBGridCount+sideBMeshCount/).
!     \item [{[sparseMatX2B]}]
!           Indexlist from xgrid index space to a Grid index space on side B; 
!           indexFactorlist from xgrid index space to a Grid index space on side B. Must enter with shape(sparsematX2B)=(/sideBGridCount+sideBMeshCount/).
!     \item [{[name]}]
!           Name of the xgrid object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOP

    integer :: localrc, ngrid_a, ngrid_b, n_idx_a2x, n_idx_x2a, n_idx_b2x, n_idx_x2b
    integer :: n_wgts_a, n_wgts_b, ndim, ncells, i, count
    type(ESMF_XGridType), pointer :: xgtypep
    type(ESMF_DELayout)           :: delayout
    type(ESMF_XGridGeomType_Flag) :: xggt

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    if(present(sideAGridCount)) then
        count = 0
        do i = 1, size(xgtypep%sideA, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideA(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_GRID) count = count + 1
        enddo
        sideAGridCount = count
    endif
    if(present(sideBGridCount)) then
        count = 0
        do i = 1, size(xgtypep%sideB, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideB(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_GRID) count = count + 1
        enddo
        sideBGridCount = count
    endif
    if(present(sideAMeshCount)) then
        count = 0
        do i = 1, size(xgtypep%sideA, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideA(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_MESH) count = count + 1
        enddo
        sideAMeshCount = count
    endif
    if(present(sideBMeshCount)) then
        count = 0
        do i = 1, size(xgtypep%sideB, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideB(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_MESH) count = count + 1
        enddo
        sideBMeshCount = count
    endif

    if(present(sideAGrid)) then
        ngrid_a = size(sideAGrid, 1)
        count = 0
        do i = 1, size(xgtypep%sideA, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideA(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_GRID) count = count + 1
        enddo
        if(ngrid_a /= count) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of sideAGrid doesn't match the number Grids on sideA in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        count = 0
        do i = 1, size(xgtypep%sideA, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideA(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_GRID) then
            count = count + 1
            sideAGrid(count) = xgtypep%sideA(i)%gbcp%grid
          endif
        enddo 
    endif
    if(present(sideAMesh)) then
        ngrid_a = size(sideAMesh, 1)
        count = 0
        do i = 1, size(xgtypep%sideA, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideA(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_MESH) count = count + 1
        enddo
        if(ngrid_a /= count) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of sideAMesh doesn't match the number Meshes on sideA in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        count = 0
        do i = 1, size(xgtypep%sideA, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideA(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_MESH) then
            count = count + 1
            sideAMesh(count) = xgtypep%sideA(i)%gbcp%mesh
          endif
        enddo 
    endif

    if(present(sideBGrid)) then
        ngrid_b = size(sideBGrid, 1)
        count = 0
        do i = 1, size(xgtypep%sideB, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideB(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_GRID) count = count + 1
        enddo
        if(ngrid_b /= count) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of sideBGrid doesn't match the number Grids on sideB in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        count = 0
        do i = 1, size(xgtypep%sideB, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideB(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_GRID) then
            count = count + 1
            sideBGrid(count) = xgtypep%sideB(i)%gbcp%grid
          endif
        enddo 
    endif
    if(present(sideBMesh)) then
        ngrid_b = size(sideBMesh, 1)
        count = 0
        do i = 1, size(xgtypep%sideB, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideB(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_MESH) count = count + 1
        enddo
        if(ngrid_b /= count) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of sideBMesh doesn't match the number Meshes on sideB in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        count = 0
        do i = 1, size(xgtypep%sideB, 1)
          call ESMF_XGridGeomBaseGet(xgtypep%sideB(i), geomtype=xggt, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
          if(xggt == ESMF_XGRIDGEOMTYPE_MESH) then
            count = count + 1
            sideBMesh(count) = xgtypep%sideB(i)%gbcp%mesh
          endif
        enddo 
    endif

    if(present(mesh)) then
      if(.not. xgtypep%storeOverlay) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
             msg="- Cannot retrieve super mesh when storeOverylay is false.", &
             ESMF_CONTEXT, rcToReturn=rc)
          return
      endif    
      mesh = xgtypep%mesh
    endif

    if(present(area)) then
        ncells = size(area,1)
        if(.not. associated(xgtypep%area)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- uninitialized area in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
        endif    
        if(ncells /= size(xgtypep%area, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- size of area doesn't match the size of area in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
        endif
        area = xgtypep%area
    endif 
    if(present(centroid)) then
        ndim = size(centroid, 1)
        ncells = size(centroid, 2)
        if(.not. associated(xgtypep%centroid)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- uninitialized centroid in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
        endif    
        if(ncells /= size(xgtypep%centroid, 2) .or. &
           ndim  /= size(xgtypep%centroid, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- size of centroid doesn't match the size of centroid in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
        endif
        centroid = xgtypep%centroid
    endif 

    if(present(distgridA)) then
        ngrid_a = size(distgridA)
        if(ngrid_a /= size(xgtypep%distgridA, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of distgridA doesn't match the size of distgridA in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        do i = 1, ngrid_a
            distgridA(i) = xgtypep%distgridA(i)
        enddo 
    endif

    if(present(distgridB)) then
        ngrid_b = size(distgridB)
        if(ngrid_b /= size(xgtypep%distgridB, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of distgridB doesn't match the size of distgridB in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
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
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- uninitialized sparseMatA2X in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatA2X, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of sparseMatA2X doesn't match the size of sparseMatA2X in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        do i = 1, ngrid_a
            sparseMatA2X(i) = xgtypep%sparseMatA2X(i)
        enddo 
    endif

    if(present(sparseMatX2A)) then
        ngrid_a = size(sparseMatX2A, 1)
        if(.not. associated(xgtypep%sparseMatX2A)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- uninitialized sparseMatX2A in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatX2A, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of sparseMatX2A doesn't match the size of sparseMatX2A in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        do i = 1, ngrid_a
            sparseMatX2A(i) = xgtypep%sparseMatX2A(i)
        enddo 
    endif

    if(present(sparseMatB2X)) then
        ngrid_a = size(sparseMatB2X, 1)
        if(.not. associated(xgtypep%sparseMatB2X)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- uninitialized sparseMatB2X in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatB2X, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of sparseMatB2X doesn't match the size of sparseMatB2X in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        do i = 1, ngrid_a
            sparseMatB2X(i) = xgtypep%sparseMatB2X(i)
        enddo 
    endif

    if(present(sparseMatX2B)) then
        ngrid_a = size(sparseMatX2B, 1)
        if(.not. associated(xgtypep%sparseMatX2B)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- uninitialized sparseMatX2B in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc)
            return
        endif    
        if(ngrid_a /= size(xgtypep%sparseMatX2B, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- size of sparseMatX2B doesn't match the size of sparseMatX2B in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        do i = 1, ngrid_a
            sparseMatX2B(i) = xgtypep%sparseMatX2B(i)
        enddo 
    endif
    
    if (present(name)) then
        call ESMF_GetName(xgtypep%base, name, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if(present(dimCount)) then
        dimCount = 1
    endif
    if(present(elementCount)) then
        call ESMF_DistGridGet(xgtypep%distgridM, localDe=0, elementCount=elementCount, &
            rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! success
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGetDefault

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetGB()"
!BOPI
! !IROUTINE:  ESMF_XGridGet - Get geombase object lists

! !INTERFACE: ESMF_XGridGet
! ! Private name; call using ESMF_XGridGet()

subroutine ESMF_XGridGetGB(xgrid, sideA, sideB, rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid), intent(in)                :: xgrid
type(ESMF_XGridGeomBase), intent(out)       :: sideA(:), sideB(:)
integer, intent(out), optional              :: rc 
!
! !DESCRIPTION:
!      Get information about XGrid
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The {\tt ESMF\_XGrid} object used to retrieve information from.
!     \item [{[sideA]}]
!           2D GeomBase objects on side A
!     \item [{[sideB]}]
!           2D GeomBase objects on side B
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOPI

    integer :: localrc, ngrid_a, ngrid_b, i
    type(ESMF_XGridType), pointer :: xgtypep

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    ngrid_a = size(sideA, 1)
    if(ngrid_a /= size(xgtypep%sideA, 1)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- size of sideA doesn't match the size of sideA in the XGrid", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif
    do i = 1, ngrid_a
        sideA(i) = xgtypep%sideA(i)
    enddo 

    ngrid_b = size(sideB, 1)
    if(ngrid_b /= size(xgtypep%sideB, 1)) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- size of sideB doesn't match the size of sideB in the XGrid", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif
    do i = 1, ngrid_b
        sideB(i) = xgtypep%sideB(i)
    enddo 

    ! success
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGetGB

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetGeomObj()"
!BOPI
! !IROUTINE:  ESMF_XGridGet - Get an individual GeomBase Obj from an XGrid

! !INTERFACE: ESMF_XGridGet
! ! Private name; call using ESMF_XGridGet()

subroutine ESMF_XGridGetGeomObj(xgrid, geombase, keywordEnforcer, &
    xgridside, gridindex, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid),          intent(in)            :: xgrid
type(ESMF_XGridGeomBase),  intent(out)           :: geombase
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
type(ESMF_XGridSide_Flag), intent(in),  optional :: xgridside
integer,                   intent(in),  optional :: gridindex
integer,                   intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Get a distgrid from XGrid from a specific side. 
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The {\tt ESMF\_XGrid} object used to retrieve information from.
!     \item [geombase]
!       Geombase Object referenced by gridIndex-th Grid or Mesh
!       on xgridSide stored in the xgrid object.
!     \item [{[xgridside]}] 
!       \begin{sloppypar}
!       Which side of the XGrid to retrieve the distgrid from (either ESMF\_XGRIDSIDE\_A,
!       ESMF\_XGRIDSIDE\_B, or ESMF\_XGRIDSIDE\_BALANCED). If not passed in then
!       defaults to ESMF\_XGRIDSIDE\_BALANCED.
!       \end{sloppypar}
!     \item [{[gridindex]}] 
!       If xgridSide is ESMF\_XGRIDSIDE\_A or ESMF\_XGRIDSIDE\_B then this index 
!       selects the Distgrid associated with the Grid on
!       that side. If not provided, defaults to 1. 
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!       is created.
!     \end{description}
!
!EOP

    type(ESMF_XGridType), pointer :: xgtypep
    type(ESMF_XGridSide_Flag)     :: l_xgridSide
    integer                       :: l_gridIndex

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    if(present(xgridSide)) then
        l_xgridSide = xgridSide
    else                   
        l_xgridSide = ESMF_XGRIDSIDE_BALANCED
    endif

    if(present(gridIndex)) then
        l_gridIndex = gridIndex
    else                   
        l_gridIndex = 1
    endif

    if(l_gridIndex .lt. 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- gridIndex cannot be less than 0", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif

    if(l_xgridSide .eq. ESMF_XGRIDSIDE_BALANCED) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- XGridSide cannot be Balanced while retrieving geombase obj", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif

    if(l_xgridSide .eq. ESMF_XGRIDSIDE_A) then
        if(l_gridIndex .gt. size(xgtypep%distgridA, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
msg="- gridIndex cannot be greater than the size of distgridA in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        geombase = xgtypep%sideA(l_gridIndex)
    endif

    if(l_xgridSide .eq. ESMF_XGRIDSIDE_B) then
        if(l_gridIndex .gt. size(xgtypep%distgridB, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
msg="- gridIndex cannot be greater than the size of distgridB in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        geombase = xgtypep%sideB(l_gridIndex)
    endif

    ! success
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGetGeomObj

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetSMMSpecFrac()"
!BOPI
! !IROUTINE:  ESMF_XGridGet - Get an individual SparseMatSpec

! !INTERFACE: ESMF_XGridGet

subroutine ESMF_XGridGetSMMSpecFrac(xgrid, srcSide, srcGridIndex, &
    dstSide, dstGridIndex, keywordEnforcer, &
    sparseMat, srcFracArray, dstFracArray, &
    srcFrac2Array, dstFrac2Array, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid),          intent(in)               :: xgrid
type(ESMF_XGridSide_Flag), intent(in)               :: srcSide
integer,                   intent(in)               :: srcGridIndex
type(ESMF_XGridSide_Flag), intent(in)               :: dstSide
integer,                   intent(in)               :: dstGridIndex
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
type(ESMF_XGridSpec),      intent(out),   optional  :: sparseMat
type(ESMF_Array),          intent(inout), optional  :: srcFracArray
type(ESMF_Array),          intent(inout), optional  :: dstFracArray
type(ESMF_Array),          intent(inout), optional  :: srcFrac2Array
type(ESMF_Array),          intent(inout), optional  :: dstFrac2Array
integer,                   intent(out),   optional  :: rc 
!
! !DESCRIPTION:
!      Get information about XGrid
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The {\tt ESMF\_XGrid} object used to retrieve information from.
!     \item [srcSide] 
!       Side of the XGrid from (either ESMF\_XGRIDSIDE\_A,
!       ESMF\_XGRIDSIDE\_B, or ESMF\_XGRIDSIDE\_BALANCED).
!     \item [srcGridIndex] 
!       If xgridSide is  ESMF\_XGRIDSIDE\_A or ESMF\_XGRIDSIDE\_B then this index tells which Grid on
!       that side.
!     \item [dstSide]
!       Side of the XGrid from (either ESMF\_XGRIDSIDE\_A,
!       ESMF\_XGRIDSIDE\_B, or ESMF\_XGRIDSIDE\_BALANCED).
!     \item [dstGridIndex] 
!       If xgridSide is  ESMF\_XGRIDSIDE\_A or ESMF\_XGRIDSIDE\_B then this index tells which Grid on
!       that side.
!     \item [{[sparseMat]}]
!       SparseMat corresponding to the src and dst Grid or Mesh.
!     \item [{[srcFracArray]}]
!       src Frac Array corresponding to the src Grid or Mesh.
!     \item [{[dstFracArray]}]
!       dst Frac Array corresponding to the dst Grid or Mesh.
!     \item [{[srcFrac2Array]}]
!       src Frac2 Array corresponding to the src Grid or Mesh.
!     \item [{[dstFrac2Array]}]
!       dst Frac2 Array corresponding to the dst Grid or Mesh.
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!       is created.
!     \end{description}
!
!EOPI

    type(ESMF_XGridType), pointer :: xgtypep
    integer                       :: localrc

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    if(srcSide .eq. ESMF_XGRIDSIDE_A .and. dstSide .eq. ESMF_XGRIDSIDE_BALANCED) then
        if(present(sparseMat)) sparseMat = xgtypep%SparseMatA2X(srcGridIndex)

        if(present(srcFracArray)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot query srcFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(srcFracArray, xgtypep%fracA2X(srcGridIndex), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(present(dstFracArray)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot query dstFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(dstFracArray, xgtypep%fracX, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(srcFrac2Array)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot query srcFrac2Array for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(srcFrac2Array, xgtypep%frac2A(srcGridIndex), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(present(dstFrac2Array)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot query dstFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(dstFrac2Array, xgtypep%fracX, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
    endif

    if(srcSide .eq. ESMF_XGRIDSIDE_B .and. dstSide .eq. ESMF_XGRIDSIDE_BALANCED) then
        if(present(sparseMat)) sparseMat = xgtypep%SparseMatB2X(srcGridIndex)
        if(present(srcFracArray)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot query srcFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(srcFracArray, xgtypep%fracB2X(srcGridIndex), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(present(dstFracArray)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot query dstFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(dstFracArray, xgtypep%fracX, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(srcFrac2Array)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot query srcFrac2Array for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(srcFrac2Array, xgtypep%frac2B(srcGridIndex), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(present(dstFrac2Array)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot query dstFrac2Array for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(dstFrac2Array, xgtypep%fracX, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
    endif

    if(srcSide .eq. ESMF_XGRIDSIDE_BALANCED .and. dstSide .eq. ESMF_XGRIDSIDE_A) then
        if(present(sparseMat)) sparseMat = xgtypep%SparseMatX2A(dstGridIndex)
        if(present(srcFracArray)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot cannot query srcFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(srcFracArray, xgtypep%fracX, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(present(dstFracArray)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot cannot query dstFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(dstFracArray, xgtypep%fracX2A(dstGridIndex), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(srcFrac2Array)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot cannot query srcFrac2Array for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(srcFrac2Array, xgtypep%fracX, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(present(dstFrac2Array)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot cannot query dstFrac2Array for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(dstFrac2Array, xgtypep%frac2A(dstGridIndex), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
    endif

    if(srcSide .eq. ESMF_XGRIDSIDE_BALANCED .and. dstSide .eq. ESMF_XGRIDSIDE_B) then
        if(present(sparseMat)) sparseMat = xgtypep%SparseMatX2B(dstGridIndex)
        if(present(srcFracArray)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot cannot query srcFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(srcFracArray, xgtypep%fracX, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(present(dstFracArray)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot cannot query dstFracArray for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(dstFracArray, xgtypep%fracX2B(dstGridIndex), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        if(present(srcFrac2Array)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot cannot query srcFrac2Array for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(srcFrac2Array, xgtypep%fracX, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        if(present(dstFrac2Array)) then
          if(xgtypep%online == 0) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- Cannot cannot query dstFrac2Array for xgrid created offline", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
          call ESMF_ArrayCopy(dstFrac2Array, xgtypep%frac2B(dstGridIndex), rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif
    endif

    ! success
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridGetSMMSpecFrac

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetDG()"
!BOPI
! !IROUTINE:  ESMF_XGridGet - Get an individual DistGrid from an XGrid

! !INTERFACE: ESMF_XGridGet
! ! Private name; call using ESMF_XGridGet()

subroutine ESMF_XGridGetDG(xgrid, distgrid, keywordEnforcer, &
    xgridside, gridindex, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid),          intent(in)            :: xgrid
type(ESMF_DistGrid),       intent(out)           :: distgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
type(ESMF_XGridSide_Flag), intent(in),  optional :: xgridside
integer,                   intent(in),  optional :: gridindex
integer,                   intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Get a distgrid from XGrid from a specific side. 
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The {\tt ESMF\_XGrid} object used to retrieve information from.
!     \item [distgrid]
!       Distgrid whose sequence index list is an overlap between gridIndex-th Grid
!       on xgridSide and the xgrid object.
!     \item [{[xgridside]}] 
!       \begin{sloppypar}
!       Which side of the XGrid to retrieve the distgrid from (either ESMF\_XGRIDSIDE\_A,
!       ESMF\_XGRIDSIDE\_B, or ESMF\_XGRIDSIDE\_BALANCED). If not passed in then
!       defaults to ESMF\_XGRIDSIDE\_BALANCED.
!       \end{sloppypar}
!     \item [{[gridindex]}] 
!       If xgridSide is ESMF\_XGRIDSIDE\_A or ESMF\_XGRIDSIDE\_B then this index 
!       selects the Distgrid associated with the Grid on
!       that side. If not provided, defaults to 1. 
!     \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!       is created.
!     \end{description}
!
!EOPI

    type(ESMF_XGridType), pointer :: xgtypep
    type(ESMF_XGridSide_Flag)     :: l_xgridSide
    integer                       :: l_gridIndex

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input XGrid
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    xgtypep => xgrid%xgtypep

    if(present(xgridSide)) then
        l_xgridSide = xgridSide
    else                   
        l_xgridSide = ESMF_XGRIDSIDE_BALANCED
    endif

    if(present(gridIndex)) then
        l_gridIndex = gridIndex
    else                   
        l_gridIndex = 1
    endif

    if(l_gridIndex .lt. 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- gridIndex cannot be less than 0", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif

    if(l_xgridSide .eq. ESMF_XGRIDSIDE_BALANCED) then
        distgrid = xgtypep%distgridM
    endif

    if(l_xgridSide .eq. ESMF_XGRIDSIDE_A) then
        if(l_gridIndex .gt. size(xgtypep%distgridA, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
msg="- gridIndex cannot be greater than the size of distgridA in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
        distgrid = xgtypep%distgridA(l_gridIndex)
    endif

    if(l_xgridSide .eq. ESMF_XGRIDSIDE_B) then
        if(l_gridIndex .gt. size(xgtypep%distgridB, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
msg="- gridIndex cannot be greater than the size of distgridB in the XGrid", &
               ESMF_CONTEXT, rcToReturn=rc) 
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
!BOPI
! !IROUTINE:  ESMF_XGridGet - Get DE-local information from an XGrid

! !INTERFACE: ESMF_XGridGet
! ! Private name; call using ESMF_XGridGet()

subroutine ESMF_XGridGetEle(xgrid, localDE, keywordEnforcer, &
    elementCount, exclusiveCount, exclusiveLBound, exclusiveUBound, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_XGrid), intent(in)            :: xgrid
integer,          intent(in)            :: localDE
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
integer,          intent(out), optional :: elementCount
integer,          intent(out), optional :: exclusiveCount
integer,          intent(out), optional :: exclusiveLBound
integer,          intent(out), optional :: exclusiveUBound
integer,          intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Get localDE specific information about XGrid
!
!     The arguments are:
!     \begin{description}
!     \item [xgrid]
!       The {\tt ESMF\_XGrid} object used to retrieve information from.
!     \item [localDE]
!       Local DE for which information is requested. Correct value is an element of
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
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    call ESMF_DistGridGet(xgtypep%distgridM, delayout=delayout, &
        rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_DELayoutGet(delayout, deCount=deCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(minIndex(1, deCount))
    call ESMF_DistGridGet(xgtypep%distgridM, minIndexPDe=minIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(present(exclusiveLBound)) then
        exclusiveLBound = minIndex(1,localDE+1)
    endif

    allocate(maxIndex(1, deCount))
    call ESMF_DistGridGet(xgtypep%distgridM, maxIndexPDe=maxIndex, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
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
