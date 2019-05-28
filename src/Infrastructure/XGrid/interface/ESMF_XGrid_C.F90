!  $Id$
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
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable 
!   arrays, or ...
!
!==============================================================================
#define ESMF_FILENAME "ESMF_XGrid_C.F90"
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id$'
!==============================================================================

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_xgridcreate"
  subroutine f_esmf_xgridcreate(xgrid, &
       num_sideAGrid, sideAGrid, &
       num_sideAMesh, sideAMesh, &
       num_sideBGrid, sideBGrid, &
       num_sideBMesh, sideBMesh, &
       num_sideAGridPriority, sideAGridPriority, &
       num_sideAMeshPriority, sideAMeshPriority, &
       num_sideBGridPriority, sideBGridPriority, &
       num_sideBMeshPriority, sideBMeshPriority, &
       num_sideAMaskValues, sideAMaskValues, &
       num_sideBMaskValues, sideBMaskValues, &
       storeOverlay, &
!       name, &  DON'T WORRY ABOUT NAME RIGHT NOW
       rc)

    use ESMF_XGridMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_GridMod
    use ESMF_MeshMod

    implicit none

    ! Arguments
    type(ESMF_XGrid)               :: xgrid
    integer, intent(in)            :: num_sideAGrid
    type(ESMF_Pointer)             :: sideAGrid(1:num_sideAGrid)
    integer, intent(in)            :: num_sideAMesh
    type(ESMF_Pointer)             :: sideAMesh(1:num_sideAMesh)
    integer, intent(in)            :: num_sideBGrid
    type(ESMF_Pointer)             :: sideBGrid(1:num_sideBGrid)
    integer, intent(in)            :: num_sideBMesh
    type(ESMF_Pointer)             :: sideBMesh(1:num_sideBMesh)

    integer, intent(in) :: num_sideAGridPriority
    integer             :: sideAGridPriority(1:num_sideAGridPriority)
    integer, intent(in) :: num_sideAMeshPriority
    integer             :: sideAMeshPriority(1:num_sideAMeshPriority)
    integer, intent(in) :: num_sideBGridPriority
    integer             :: sideBGridPriority(1:num_sideBGridPriority)
    integer, intent(in) :: num_sideBMeshPriority
    integer             :: sideBMeshPriority(1:num_sideBMeshPriority)

    integer, intent(in)            :: num_sideAMaskValues
    integer(ESMF_KIND_I4),intent(in) :: sideAMaskValues(1:num_sideAMaskValues)
    integer, intent(in)            :: num_sideBMaskValues
    integer(ESMF_KIND_I4),intent(in) :: sideBMaskValues(1:num_sideBMaskValues)

    integer, intent(in) :: storeOverlay
!    character(len=*),intent(in)    :: name    DON'T WORRY ABOUT NAME RIGHT NOW
    integer, intent(out)           :: rc              

    ! Local variables
    type(ESMF_Grid)             :: localSideAGrid(1:num_sideAGrid)
    type(ESMF_Mesh)             :: localSideAMesh(1:num_sideAMesh)
    type(ESMF_Grid)             :: localSideBGrid(1:num_sideBGrid)
    type(ESMF_Mesh)             :: localSideBMesh(1:num_sideBMesh)
    logical :: localStoreOverlay
    integer :: localrc
    integer :: i

    ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL

    !! Create ESMF acceptable versions of class arrays !!

    ! sideAGrid
    do i=1,num_sideAGrid
       localSideAGrid(i)%this=sideAGrid(i)
       ESMF_INIT_SET_CREATED(localSideAGrid(i))
    enddo

    ! sideAMesh
    do i=1,num_sideAMesh
       localSideAMesh(i)=ESMF_MeshCreate(sideAMesh(i), rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! sideBGrid
    do i=1,num_sideBGrid
       localSideBGrid(i)%this=sideBGrid(i)
       ESMF_INIT_SET_CREATED(localSideBGrid(i))
    enddo

    ! sideBMesh
    do i=1,num_sideBMesh
       localSideBMesh(i)=ESMF_MeshCreate(sideBMesh(i), rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    enddo


    ! Translate storeOverlay
    localStoreOverlay=.false.
    if (storeOverlay .ne. 0) localStoreOverlay=.true.


    ! Call into ESMF public interface
    xgrid=ESMF_XGridCreate(&
         sideAGrid=localSideAGrid, &
         sideAMesh=localSideAMesh, &
         sideBGrid=localSideBGrid, &
         sideBMesh=localSideBMesh, &
         sideAGridPriority=sideAGridPriority, & 
         sideAMeshPriority=sideAMeshPriority, &
         sideBGridPriority=sideBGridPriority, &
         sideBMeshPriority=sideBMeshPriority, &
         sideAMaskValues=sideAMaskValues, &
         sideBMaskValues=sideBMaskValues, &
         storeOverlay=localstoreOverlay, &
         ! name, &  DON'T WORRY ABOUT NAME RIGHT NOW
         rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridcreate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_xgriddestroy"
  subroutine f_esmf_xgriddestroy(xgrid, rc)

    use ESMF_XGridMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod

    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(out)           :: rc     
  
  ! initialize return code; assume routine not implemented
    rc = ESMF_RC_NOT_IMPL
  
    call ESMF_XGridDestroy(xgrid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgriddestroy


#undef  ESMF_METHOD 
#define ESMF_METHOD "f_esmf_xgridgetside1gridcount"
  subroutine f_esmf_xgridgetsideagridcount(xgrid, count, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod


    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(out)           :: count              
    integer, intent(out)           :: rc              

    ! Init rc
    rc = ESMF_RC_NOT_IMPL

    ! Get info
    call ESMF_XGridGet(xgrid, sideAGridCount=count, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set rc to success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetsideagridcount



#undef  ESMF_METHOD 
#define ESMF_METHOD "f_esmf_xgridgetsideameshcount"
  subroutine f_esmf_xgridgetsideameshcount(xgrid, count, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod


    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(out)           :: count              
    integer, intent(out)           :: rc              

    ! Init rc
    rc = ESMF_RC_NOT_IMPL

    ! Get info
    call ESMF_XGridGet(xgrid, sideAMeshCount=count, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set rc to success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetsideameshcount


#undef  ESMF_METHOD 
#define ESMF_METHOD "f_esmf_xgridgetsideBgridcount"
  subroutine f_esmf_xgridgetsidebgridcount(xgrid, count, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod


    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(out)           :: count              
    integer, intent(out)           :: rc              

    ! Init rc
    rc = ESMF_RC_NOT_IMPL

    ! Get info
    call ESMF_XGridGet(xgrid, sideBGridCount=count, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set rc to success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetsidebgridcount


#undef  ESMF_METHOD 
#define ESMF_METHOD "f_esmf_xgridgetsidebmeshcount"
  subroutine f_esmf_xgridgetsidebmeshcount(xgrid, count, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod


    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(out)           :: count              
    integer, intent(out)           :: rc              

    ! Init rc
    rc = ESMF_RC_NOT_IMPL

    ! Get info
    call ESMF_XGridGet(xgrid, sideBMeshCount=count, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set rc to success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetsidebmeshcount


#undef  ESMF_METHOD 
#define ESMF_METHOD "f_esmf_xgridgetdimcount"
  subroutine f_esmf_xgridgetdimcount(xgrid, count, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod


    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(out)           :: count              
    integer, intent(out)           :: rc              

    ! Init rc
    rc = ESMF_RC_NOT_IMPL

    ! Get info
    call ESMF_XGridGet(xgrid, dimCount=count, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set rc to success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetdimcount



#undef  ESMF_METHOD 
#define ESMF_METHOD "f_esmf_xgridgetelementcount"
  subroutine f_esmf_xgridgetelementcount(xgrid, count, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod


    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(out)           :: count              
    integer, intent(out)           :: rc              

    ! Init rc
    rc = ESMF_RC_NOT_IMPL

    ! Get info
    call ESMF_XGridGet(xgrid, elementCount=count, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set rc to success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetelementcount

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_xgridgetmesh"
  subroutine f_esmf_xgridgetmesh(xgrid, meshp, &
       parametricDim, spatialDim, coordSys, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
    use ESMF_MeshMod

    implicit none

    type(ESMF_XGrid),intent(inout)        :: xgrid
    type(ESMF_Pointer)                    :: meshp
    integer, intent(out)                  :: parametricDim              
    integer, intent(out)                  :: spatialDim              
    type(ESMF_CoordSys_Flag), intent(out) :: coordSys
    integer, intent(out)                  :: rc              

    type(ESMF_Mesh)             :: mesh

    rc = ESMF_RC_NOT_IMPL

    ! Get Mesh
    call ESMF_XGridGet(xgrid, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get internal pointer
    call ESMF_MeshGetIntPtr(mesh, meshp, rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get other information
    call ESMF_MeshGet(mesh, &
         parametricDim=parametricDim, &
         spatialDim=spatialDim, &
         coordSys=coordSys, &
         rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetmesh

#undef  ESMF_METHOD 
#define ESMF_METHOD "f_esmf_xgridgetarea"
  subroutine f_esmf_xgridgetarea(xgrid, num_area, area, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
! XMRKX !
    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(in)            :: num_area
    real(ESMF_KIND_R8)             :: area(1:num_area)             
    integer, intent(out)           :: rc              

    ! Init rc
    rc = ESMF_RC_NOT_IMPL

    ! Get info
    call ESMF_XGridGet(xgrid, area=area, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set rc to success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetarea

#undef  ESMF_METHOD 
#define ESMF_METHOD "f_esmf_xgridgetcentroid"
  subroutine f_esmf_xgridgetcentroid(xgrid, elemCount, dimCount, centroid, rc)

    use ESMF_XGridMod
    use ESMF_XGridGetMod
    use ESMF_XGridCreateMod
    use ESMF_XGridGeomBaseMod
    use ESMF_UtilTypesMod
    use ESMF_BaseMod
    use ESMF_LogErrMod
! XMRKX !
    implicit none

    type(ESMF_XGrid)               :: xgrid
    integer, intent(in)            :: elemCount
    integer, intent(in)            :: dimCount
    real(ESMF_KIND_R8)             :: centroid(1:elemCount,1:dimCount)
    integer, intent(out)           :: rc              

    ! Init rc
    rc = ESMF_RC_NOT_IMPL

    ! Get info
    call ESMF_XGridGet(xgrid, centroid=centroid, rc=rc)
    if (ESMF_LogFoundError(rc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set rc to success
    rc = ESMF_SUCCESS
  
  end subroutine f_esmf_xgridgetcentroid



#if 0
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_xgridget"
  subroutine f_esmf_xgridget(xgrid, &
                  sideAGridCount, &
                  sideBGridCount, &
                  rc)

    ! Arguments
    type(ESMF_XGrid)               :: xgrid
    integer, intent(out) :: sideAGridCount
    integer, intent(out) :: sideBGridCount
    integer, intent(out)           :: rc              


    



  end subroutine f_esmf_xgridget
#endif

#if 0

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
!           Coordinates at the area weighted center of the xgrid cells on this PET. Must enter with shape(centroid)=(/elementCount, dimCount/).
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
#endif
