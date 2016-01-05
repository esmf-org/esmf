! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
  use ESMF_ArrayMod
  use ESMF_GridMod
  use ESMF_GridUtilMod
  use ESMF_MeshMod
  use ESMF_StaggerLocMod
  use ESMF_XGridGeomBaseMod
  use ESMF_XGridMod
  use ESMF_InitMacrosMod
  use ESMF_VMMod
  use ESMF_F90InterfaceMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

  ! temporarily store the weights while F90 arrays are alloc'ed
  type ESMF_TempWeights 
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
    type(ESMF_Pointer) :: this
  end type

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! - ESMF-public methods:
    public ESMF_XGridCreate                 ! Create
    public ESMF_XGridCreateFromSparseMat    ! Create
    public ESMF_XGridIsCreated
    public ESMF_XGridDestroy                ! Destroy
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
!
!
  interface ESMF_XGridGetFracInt
    module procedure ESMF_XGridGetFracIntGrid
    module procedure ESMF_XGridGetFracIntMesh
  end interface
  interface ESMF_XGridGetFrac2Int
    module procedure ESMF_XGridGetFrac2IntGrid
    module procedure ESMF_XGridGetFrac2IntMesh
  end interface


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!===============================================================================
! XGridOperator() interface documentation (must be before creates)
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_XGridAssignment(=) - XGrid assignment
!
! !INTERFACE:
!   interface assignment(=)
!   xgrid1 = xgrid2
!
! !ARGUMENTS:
!   type(ESMF_XGrid) :: xgrid1
!   type(ESMF_XGrid) :: xgrid2
!
!
! !DESCRIPTION:
!   Assign xgrid1 as an alias to the same ESMF XGrid object in memory
!   as xgrid2. If xgrid2 is invalid, then xgrid1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[xgrid1]
!     The {\tt ESMF\_XGrid} object on the left hand side of the assignment.
!   \item[xgrid2]
!     The {\tt ESMF\_XGrid} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_XGridOperator(==) - XGrid equality operator
!
! !INTERFACE:
!   interface operator(==)
!   if (xgrid1 == xgrid2) then ... endif
!             OR
!   result = (xgrid1 == xgrid2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_XGrid), intent(in) :: xgrid1
!   type(ESMF_XGrid), intent(in) :: xgrid2
!
!
! !DESCRIPTION:
!   Test whether xgrid1 and xgrid2 are valid aliases to the same ESMF
!   XGrid object in memory. For a more general comparison of two ESMF XGrids,
!   going beyond the simple alias test, the ESMF\_XGridMatch() function (not yet
!   implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[xgrid1]
!     The {\tt ESMF\_XGrid} object on the left hand side of the equality
!     operation.
!   \item[xgrid2]
!     The {\tt ESMF\_XGrid} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
!    module procedure ESMF_XGridEQ
!
!    end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_XGridOperator(/=) - XGrid not equal operator
!
! !INTERFACE:
!   interface operator(/=)
!   if (xgrid1 /= xgrid2) then ... endif
!             OR
!   result = (xgrid1 /= xgrid2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_XGrid), intent(in) :: xgrid1
!   type(ESMF_XGrid), intent(in) :: xgrid2
!
!
! !DESCRIPTION:
!   Test whether xgrid1 and xgrid2 are {\it not} valid aliases to the
!   same ESMF XGrid object in memory. For a more general comparison of two ESMF
!   XGrids, going beyond the simple alias test, the ESMF\_XGridMatch() function
!   (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[xgrid1]
!     The {\tt ESMF\_XGrid} object on the left hand side of the non-equality
!     operation.
!   \item[xgrid2]
!     The {\tt ESMF\_XGrid} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
!    module procedure ESMF_XGridNE
!
!    end interface
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridCreate()"
!BOP
! !IROUTINE:  ESMF_XGridCreate - Create an XGrid from lists of Grids and Meshes

! !INTERFACE:

function ESMF_XGridCreate(keywordEnforcer, &
    sideAGrid,              sideAMesh, &
    sideBGrid,              sideBMesh, &
    sideAGridPriority,      sideAMeshPriority, &
    sideBGridPriority,      sideBMeshPriority, &
    sideAMaskValues,        sideBMaskValues, &
    storeOverlay, &
    name, rc)
!
! !RETURN VALUE:
  type(ESMF_XGrid)                           :: ESMF_XGridCreate
!
! !ARGUMENTS:
  type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  type(ESMF_Grid),      intent(in), optional :: sideAGrid(:)
  type(ESMF_Mesh),      intent(in), optional :: sideAMesh(:)
  type(ESMF_Grid),      intent(in), optional :: sideBGrid(:)
  type(ESMF_Mesh),      intent(in), optional :: sideBMesh(:)
  integer,              intent(in), optional :: sideAGridPriority(:)
  integer,              intent(in), optional :: sideAMeshPriority(:)
  integer,              intent(in), optional :: sideBGridPriority(:)
  integer,              intent(in), optional :: sideBMeshPriority(:)
  integer(ESMF_KIND_I4),intent(in), optional :: sideAMaskValues(:)
  integer(ESMF_KIND_I4),intent(in), optional :: sideBMaskValues(:)
  logical,              intent(in), optional :: storeOverlay
  character(len=*),     intent(in), optional :: name
  integer,              intent(out),optional :: rc

!
! !DESCRIPTION:
!      Create an XGrid from user supplied input: the list of Grids or Meshes on side A and side B, 
!  and other optional arguments. A user can supply both Grids and Meshes on one side to create
!  the XGrid. By default, the Grids have a higher priority over Meshes but the order of priority 
!  can be adjusted by the optional GridPriority and MeshPriority arguments. The priority order
!  of Grids and Meshes can also be interleaved by rearranging the optional 
!  GridPriority and MeshPriority arguments accordingly.
!  
!  Sparse matrix multiply coefficients are internally computed and
!  uniquely determined by the Grids or Meshes provided in {\tt sideA} and {\tt sideB}. User can supply
!  a single {\tt ESMF\_Grid} or an array of {\tt ESMF\_Grid} on either side of the 
!  {\tt ESMF\_XGrid}. For an array of {\tt ESMF\_Grid} or {\tt ESMF\_Mesh} in {\tt sideA} or {\tt sideB},
!  a merging process concatenates all the {\tt ESMF\_Grid}s and {\tt ESMF\_Mesh}es 
!  into a super mesh represented
!  by {\tt ESMF\_Mesh}. The super mesh is then used to compute the XGrid. 
!  Grid or Mesh objects in {\tt sideA} and {\tt sideB} arguments must have coordinates defined for
!  the corners of a Grid or Mesh cell. XGrid creation can be potentially memory expensive given the
!  size of the input Grid and Mesh objects. By default, the super mesh is not stored
!  to reduce memory usage. 
!  Once communication routehandles are computed using {\tt ESMF\_FieldRegridStore()} method through
!  XGrid, all memory can be released by destroying the XGrid.
! 
!  If {\tt sideA} and {\tt sideB} have a single 
!  Grid or Mesh object, it's erroneous
!  if the two Grids or Meshes are spatially disjoint. 
!  It is also erroneous to specify Grid or Mesh object in {\tt sideA} or {\tt sideB} 
!  that is spatially disjoint from the {\tt ESMF\_XGrid}.  
!
!  This call is {\em collective} across the current VM. For more details please refer to the description 
!  \ref{sec:xgrid:desc} of the XGrid class. For an example and associated documentation using this method see section 
!  \ref{sec:xgrid:usage:xgrid_create}

!
!     The arguments are:
!     \begin{description}
!     \item [{[sideAGrid]}]
!           Parametric 2D Grids on side A, for example, 
!           these Grids can be either Cartesian 2D or Spherical.
!     \item [{[sideAMesh]}]
!           Parametric 2D Meshes on side A, for example, 
!           these Meshes can be either Cartesian 2D or Spherical.
!     \item [{[sideBGrid]}]
!           Parametric 2D Grids on side B, for example, 
!           these Grids can be either Cartesian 2D or Spherical.
!     \item [{[sideBMesh]}]
!           Parametric 2D Meshes on side B, for example, 
!           these Meshes can be either Cartesian 2D or Spherical.
!     \item [{[sideAGridPriority]}]
!           Priority array of Grids on {\tt sideA} during overlay generation.
!           The priority arrays describe the priorities of Grids at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Grid of the
!           highest priority to the lowest priority.
!     \item [{[sideAMeshPriority]}]
!           Priority array of Meshes on {\tt sideA} during overlay generation.
!           The priority arrays describe the priorities of Meshes at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Mesh of the
!           highest priority to the lowest priority.
!     \item [{[sideBGridPriority]}]
!           Priority of Grids on {\tt sideB} during overlay generation
!           The priority arrays describe the priorities of Grids at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Grid of the
!           highest priority to the lowest priority.
!     \item [{[sideBMeshPriority]}]
!           Priority array of Meshes on {\tt sideB} during overlay generation.
!           The priority arrays describe the priorities of Meshes at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Mesh of the
!           highest priority to the lowest priority.
!     \item [{[sideAMaskValues]}]
!           Mask information can be set in the Grid (see~\ref{sec:usage:items}) or Mesh (see~\ref{sec:mesh:mask}) 
!           upon which the {\tt Field} is built. The {\tt sideAMaskValues} argument specifies the values in that 
!           mask information which indicate a point should be masked out. In other words, a location is masked if and only if the
!           value for that location in the mask information matches one of the values listed in {\tt sideAMaskValues}.  
!           If {\tt sideAMaskValues} is not specified, no masking on side A will occur. 
!     \item [{[sideBMaskValues]}]
!           Mask information can be set in the Grid (see~\ref{sec:usage:items}) or Mesh (see~\ref{sec:mesh:mask}) 
!           upon which the {\tt Field} is built. The {\tt sideBMaskValues} argument specifies the values in that 
!           mask information which indicate a point should be masked out. In other words, a location is masked if and only if the
!           value for that location in the mask information matches one of the values listed in {\tt sideBMaskValues}.  
!           If {\tt sideBMaskValues} is not specified, no masking on side B will occur. 
!     \item [{[storeOverlay]}]
!           Setting the {\tt storeOverlay} optional argument to .false. (default) 
!           allows a user to bypass storage of the {\tt ESMF\_Mesh} used to represent the XGrid.
!           Only a {\tt ESMF\_DistGrid} is stored to allow Field to be built on the XGrid.
!           If the temporary mesh object is of interest, {\tt storeOverlay} can be set to .true.
!           so a user can retrieve it for future use.
!     \item [{[name]}]
!           name of the xgrid object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOP

    integer                       :: localrc, ngrid_a, ngrid_b, i, j
    type(ESMF_XGridType), pointer :: xgtype
    type(ESMF_Mesh)               :: meshA, meshB, mesh, tmpmesh
    type(ESMF_Mesh), allocatable  :: meshAt(:), meshBt(:)
    type(ESMF_Pointer)            :: meshp
    type(ESMF_VM)                 :: vm
    integer(ESMF_KIND_I4), pointer:: indicies(:,:)
    real(ESMF_KIND_R8), pointer   :: weights(:), sidemesharea(:)
    integer                       :: nentries
    type(ESMF_TempWeights)        :: tweights
    integer                       :: AisSphere, BisSphere
    integer                       :: compute_midmesh
    real(ESMF_KIND_R8)            :: fraction = 1.0 ! all newly created Mesh has 1.0 frac2
    type(ESMF_INDEX_FLAG)         :: indexflag
    type(ESMF_DistGrid)           :: distgridTmp
    !real(ESMF_KIND_R8), pointer   :: fracFptr(:,:)
    integer                       :: localElemCount, sdim, pdim
    type(ESMF_XGridGeomType_Flag), allocatable :: xggt_a(:), xggt_b(:)

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Presumably no longer useful
    AisSphere = 0
    BisSphere = 0

    ! check there are enough input to create the XGrid
    if(.not. present(sideAGrid) .and. .not. present(sideAMesh)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- Either Grid or Mesh must be provided on sideA", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif
    if(.not. present(sideBGrid) .and. .not. present(sideBMesh)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- Either Grid or Mesh must be provided on sideB", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    if(present(sideAGrid) .and. present(sideAMesh)) then
      if(size(sideAGrid, 1)+size(sideAMesh, 1) .le. 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- Either Grid or Mesh must be provided on sideA", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
    endif

    if(present(sideBGrid) .and. present(sideBMesh)) then
      if(size(sideBGrid, 1)+size(sideBMesh, 1) .le. 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- Either Grid or Mesh must be provided on sideB", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
    endif

    ! check init status of input Grids
    if(present(sideAGrid)) then
      ngrid_a = size(sideAGrid, 1)
      do i = 1, ngrid_a
          ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideAGrid(i),rc)
      enddo

      ! Can only do conservative on 2D right now
      ! Make sure all Grids are 2 dimensional and 
      ! has enough data points in each dimension for every de-element on a PET to clip
      do i = 1, ngrid_a
        call checkGrid(sideAGrid(i), ESMF_STAGGERLOC_CORNER, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      enddo

      if(present(sideAGridPriority)) then
        if(size(sideAGridPriority, 1) /= ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- Size of sideAGridPriority does not match size of sideAGrid", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
    endif

    if(present(sideBGrid)) then
      ngrid_b = size(sideBGrid, 1)
      do i = 1, ngrid_b
          ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideBGrid(i),rc)
      enddo
      do i = 1, ngrid_b
        call checkGrid(sideBGrid(i), ESMF_STAGGERLOC_CORNER, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      enddo
      if(present(sideBGridPriority)) then
        if(size(sideBGridPriority, 1) /= ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- Size of sideBGridPriority does not match size of sideBGrid", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
    endif

    !TODO: need to check Meshes are initialized properly too.
    if(present(sideAMesh)) then
      ngrid_a = size(sideAMesh, 1)
      if(present(sideAMeshPriority)) then
        if(size(sideAMeshPriority, 1) /= ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- Size of sideAMeshPriority does not match size of sideAMesh", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
    endif
    if(present(sideBMesh)) then
      ngrid_b = size(sideBMesh, 1)
      if(present(sideBMeshPriority)) then
        if(size(sideBMeshPriority, 1) /= ngrid_b) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- Size of sideBMeshPriority does not match size of sideBMesh", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
    endif

    ! Priority for Grid and Mesh must all be present simultaneously
    if( (present(sideAGridPriority) .and. .not. present(sideAGrid)) .or. &
         present(sideAMeshPriority) .and. .not. present(sideAMesh)  .or. &
         present(sideBGridPriority) .and. .not. present(sideBGrid)  .or. &
         present(sideBMeshPriority) .and. .not. present(sideBMesh)) then

      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- Cannot specify Grid or Mesh Priority without actual list of Grids or Meshes", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! At this point the inputs are consistently sized.
    ! Take care of ordering
    ngrid_a = 0
    if(present(sideAGrid)) ngrid_a = size(sideAGrid, 1)
    if(present(sideAMesh)) ngrid_a = ngrid_a + size(sideAMesh, 1)
    ngrid_b = 0
    if(present(sideBGrid)) ngrid_b = size(sideBGrid, 1)
    if(present(sideBMesh)) ngrid_b = ngrid_b + size(sideBMesh, 1)

    ! do some range checking on priority lists
    if(present(sideAGridPriority) .and. present(sideAMeshPriority)) then
      do i = 1, size(sideAGridPriority, 1)
        if(sideAGridPriority(i) .le. 0 .or. sideAGridPriority(i) .gt. ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- sideAGridPriority value out of range", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      enddo
      do i = 1, size(sideAMeshPriority, 1)
        if(sideAMeshPriority(i) .le. 0 .or. sideAMeshPriority(i) .gt. ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- sideAMeshPriority value out of range", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      enddo

      do i = 1, size(sideAGridPriority, 1)
        do j = 1, size(sideAMeshPriority, 1)
          if(sideAGridPriority(i) == sideAMeshPriority(j)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- sideAGridPriority and sideAMeshPriority cannot have duplicate entry", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
        enddo
      enddo
    endif

    if(present(sideBGridPriority) .and. present(sideBMeshPriority)) then
      do i = 1, size(sideBGridPriority, 1)
        if(sideBGridPriority(i) .le. 0 .or. sideBGridPriority(i) .gt. ngrid_b) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- sideBGridPriority value out of range", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      enddo
      do i = 1, size(sideBMeshPriority, 1)
        if(sideBMeshPriority(i) .le. 0 .or. sideBMeshPriority(i) .gt. ngrid_b) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- sideBMeshPriority value out of range", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      enddo

      do i = 1, size(sideBGridPriority, 1)
        do j = 1, size(sideBMeshPriority, 1)
          if(sideBGridPriority(i) == sideBMeshPriority(j)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- sideBGridPriority and sideBMeshPriority cannot have duplicate entry", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
        enddo
      enddo
    endif

    ! at this point, the input priority lists have valid entries within correct range.
    ! initialize XGridType object and its base object
    nullify(xgtype)
    nullify(ESMF_XGridCreate%xgtypep)
    call ESMF_XGridConstructBaseObj(xgtype, name, localrc)
    if (ESMF_LogFoundAllocError(localrc, &
                                msg="Constructing xgtype base object ", &
                                ESMF_CONTEXT, rcToReturn=rc)) return
    
    allocate(xgtype%sideA(ngrid_a), xgtype%sideB(ngrid_b), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
        msg="- Allocating xgtype%sideA or xgtype%sideB ", &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Need to initialize xgtype%sideA based on sideAGrid and/or sideAMesh
    if(present(sideAGrid)) then
      if(present(sideAGridPriority)) then
        do i = 1, size(sideAGrid, 1)
          xgtype%sideA(sideAGridPriority(i)) = ESMF_XGridGeomBaseCreate(sideAGrid(i), &
            ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
      else
        do i = 1, size(sideAGrid, 1)
          xgtype%sideA(i) = ESMF_XGridGeomBaseCreate(sideAGrid(i), &
            ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
      endif

      if(present(sideAMesh)) then
        if(present(sideAMeshPriority)) then
          do i = 1, size(sideAMesh, 1)
            xgtype%sideA(sideAMeshPriority(i)) = ESMF_XGridGeomBaseCreate(sideAMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        else
          do i = 1, size(sideAMesh, 1)
            xgtype%sideA(i+size(sideAGrid, 1)) = ESMF_XGridGeomBaseCreate(sideAMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif
    else ! .not. present(sideAGrid)
      if(present(sideAMesh)) then
        if(present(sideAMeshPriority)) then
          do i = 1, size(sideAMesh, 1)
            xgtype%sideA(sideAMeshPriority(i)) = ESMF_XGridGeomBaseCreate(sideAMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        else
          do i = 1, size(sideAMesh, 1)
            xgtype%sideA(i) = ESMF_XGridGeomBaseCreate(sideAMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif
    endif

    ! Need to initialize xgtype%sideB based on sideBGrid and/or sideBMesh
    if(present(sideBGrid)) then
      if(present(sideBGridPriority)) then
        do i = 1, size(sideBGrid, 1)
          xgtype%sideB(sideBGridPriority(i)) = ESMF_XGridGeomBaseCreate(sideBGrid(i), &
            ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
      else
        do i = 1, size(sideBGrid, 1)
          xgtype%sideB(i) = ESMF_XGridGeomBaseCreate(sideBGrid(i), &
            ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
      endif

      if(present(sideBMesh)) then
        if(present(sideBMeshPriority)) then
          do i = 1, size(sideBMesh, 1)
            xgtype%sideB(sideBMeshPriority(i)) = ESMF_XGridGeomBaseCreate(sideBMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        else
          do i = 1, size(sideBMesh, 1)
            xgtype%sideB(i+size(sideBGrid, 1)) = ESMF_XGridGeomBaseCreate(sideBMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif
    else ! .not. present(sideBGrid)
      if(present(sideBMesh)) then
        if(present(sideBMeshPriority)) then
          do i = 1, size(sideBMesh, 1)
            xgtype%sideB(sideBMeshPriority(i)) = ESMF_XGridGeomBaseCreate(sideBMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        else
          do i = 1, size(sideBMesh, 1)
            xgtype%sideB(i) = ESMF_XGridGeomBaseCreate(sideBMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif
    endif

    ! allocate the temporary meshes
    allocate(meshAt(ngrid_a), meshBt(ngrid_b), &
      xggt_a(ngrid_a), xggt_b(ngrid_b), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="- Allocating temporary meshes for Xgrid creation", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    do i = 1, ngrid_a
      call ESMF_XGridGeomBaseGet(xgtype%sideA(i), geomtype=xggt_a(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if(xggt_a(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        meshAt(i) = ESMF_GridToMesh(xgtype%sideA(i)%gbcp%grid, &
          ESMF_STAGGERLOC_CORNER, AisSphere, &
          maskValues=sideAMaskValues, &
          regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else if(xggt_a(i) == ESMF_XGRIDGEOMTYPE_MESH) then
        meshAt(i) = xgtype%sideA(i)%gbcp%mesh
        if (present(sideAMaskValues)) then
          call ESMF_MeshTurnOnCellMask(meshAt(i), maskValues=sideAMaskValues, rc=localrc);
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        call c_esmc_meshsetfraction(meshAt(i), fraction, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- Invalid sideA xgridgeombase object", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
      if(i == 1) meshA = meshAt(i)
      if(i .ge. 2) then
        ! call into mesh merge with priority taken into account
        ! meshAt is truncated(if necessary) and concatenated onto meshA
        ! and result stored in tmpmesh
        call c_esmc_meshmerge(meshA, meshAt(i), tmpmesh, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        if(i .gt. 2) then
          ! the intermediate meshA is only a pointer type of mesh at this point, 
          ! call the C api to destroy it
          call C_ESMC_MeshDestroy(meshA%this, localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        meshA = tmpmesh
      endif
    enddo

    do i = 1, ngrid_b
      call ESMF_XGridGeomBaseGet(xgtype%sideB(i), geomtype=xggt_b(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if(xggt_b(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        meshBt(i) = ESMF_GridToMesh(xgtype%sideB(i)%gbcp%grid, &
          ESMF_STAGGERLOC_CORNER, BisSphere, &
          maskValues=sideBMaskValues, &
          regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else if(xggt_b(i) == ESMF_XGRIDGEOMTYPE_MESH) then
        meshBt(i) = xgtype%sideB(i)%gbcp%mesh
        if (present(sideBMaskValues)) then
          call ESMF_MeshTurnOnCellMask(meshBt(i), maskValues=sideBMaskValues, rc=localrc);
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
        endif
        call c_esmc_meshsetfraction(meshBt(i), fraction, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- Invalid sideB xgridgeombase object", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
      if( i == 1) meshB = meshBt(i)
      ! call into mesh merge with priority taken into account
      ! meshBt is truncated(if necessary) and concatenated onto meshB 
      ! and result stored in tmpmesh
      if(i .ge. 2) then
        call c_esmc_meshmerge(meshB, meshBt(i), tmpmesh, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        if(i .gt. 2) then
          call C_ESMC_MeshDestroy(meshB%this, localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        endif

        meshB = tmpmesh
      endif
    enddo

    allocate(xgtype%sparseMatA2X(ngrid_a), &
      xgtype%sparseMatX2A(ngrid_a), &
      xgtype%sparseMatB2X(ngrid_b), &
      xgtype%sparseMatX2B(ngrid_b), stat=localrc)
    if(localrc /= 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- Failed to allocate SMM parameters", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! use current VM for communication
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into streamlined Regrid
    ! input:  MeshA: merged mesh on side A
    ! input:  MeshB: merged mesh on side B
    ! output: Meshp: merged mesh in the middle (the super mesh)
    compute_midmesh = 1
    call c_esmc_xgridregrid_create(vm, meshA, meshB, &
      meshp, compute_midmesh, &
      ESMF_REGRIDMETHOD_CONSERVE, &
      ESMF_UNMAPPEDACTION_IGNORE, &
      nentries, tweights, &
      localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    mesh = ESMF_MeshCreate(meshp, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_MeshGet(mesh, numOwnedElements=localElemCount, &
            spatialDim=sdim, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call C_ESMC_MeshGetDimensions(mesh%this, sdim, pdim, localrc);
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    allocate(xgtype%area(localElemCount), xgtype%centroid(localElemCount, sdim), stat=localrc)
    if(localrc /= 0) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- Failed to allocate area or centroid", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    if(localElemCount .gt. 0) then
      call C_ESMC_MeshGetArea(mesh%this, localElemCount, xgtype%area, localrc);
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call C_ESMC_MeshGetCentroid(mesh%this, localElemCount, xgtype%centroid, localrc);
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Create and Retrieve fraction Arrays for store use, only for online
    allocate(xgtype%fracA2X(ngrid_a), xgtype%fracB2X(ngrid_b))
    allocate(xgtype%fracX2A(ngrid_a), xgtype%fracX2B(ngrid_b))
    allocate(xgtype%frac2A(ngrid_a), xgtype%frac2B(ngrid_b))
    do i = 1, ngrid_A
      if(xggt_a(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_GridGet(xgtype%sideA(i)%gbcp%grid, distgrid=distgridTmp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_MeshGet(xgtype%sideA(i)%gbcp%mesh, elementDistgrid=distgridTmp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      xgtype%fracA2X(i) = ESMF_ArrayCreate(distgridTmp, typekind=ESMF_TYPEKIND_R8, &
        indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      xgtype%fracX2A(i) = ESMF_ArrayCreate(distgridTmp, typekind=ESMF_TYPEKIND_R8, &
       indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      xgtype%frac2A(i) = ESMF_ArrayCreate(distgridTmp, typekind=ESMF_TYPEKIND_R8, &
       indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      ! retrieve frac2 Field
      if(xggt_a(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_XGridGetFrac2Int(xgtype%sideA(i)%gbcp%grid, mesh=meshAt(i), array=xgtype%frac2A(i), &
             staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_XGridGetFrac2Int(mesh=meshAt(i), array=xgtype%frac2A(i), &
             meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    enddo
    do i = 1, ngrid_B
      if(xggt_b(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_GridGet(xgtype%sideB(i)%gbcp%grid, distgrid=distgridTmp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_MeshGet(xgtype%sideB(i)%gbcp%mesh, elementDistgrid=distgridTmp, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      xgtype%fracB2X(i) = ESMF_ArrayCreate(distgridTmp, typekind=ESMF_TYPEKIND_R8, &
        indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      xgtype%fracX2B(i) = ESMF_ArrayCreate(distgridTmp, typekind=ESMF_TYPEKIND_R8, &
        indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      xgtype%frac2B(i) = ESMF_ArrayCreate(distgridTmp, typekind=ESMF_TYPEKIND_R8, &
       indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      ! retrieve frac2 Field
      if(xggt_b(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_XGridGetFrac2Int(xgtype%sideB(i)%gbcp%grid, mesh=meshBt(i), array=xgtype%frac2B(i), &
             staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_XGridGetFrac2Int(mesh=meshBt(i), array=xgtype%frac2B(i), &
             meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    enddo

    ! TODO: investigate the possibility of optimization for the general case of multiple Grids.
    ! When there is only 1 grid per side, optimization can be done but it's not clear for multiple Grids.
    compute_midmesh = 0
    do i = 1, ngrid_a
      call c_esmc_xgridregrid_create(vm, meshAt(i), mesh, &
        tmpmesh, compute_midmesh, &
        ESMF_REGRIDMETHOD_CONSERVE, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatA2X(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatA2X(i)%factorList(nentries))
      if(nentries .ge. 1) then
        call c_ESMC_Copy_TempWeights_xgrid(tweights, &
        xgtype%sparseMatA2X(i)%factorIndexList(1,1), &
        xgtype%sparseMatA2X(i)%factorList(1))
      endif
      if(xggt_a(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_XGridGetFracInt(xgtype%sideA(i)%gbcp%grid, mesh=meshAt(i), array=xgtype%fracA2X(i), &
             staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_XGridGetFracInt(mesh=meshAt(i), array=xgtype%fracA2X(i), &
             meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      
      ! Now the reverse direction
      call c_esmc_xgridregrid_create(vm, mesh, meshAt(i), &
        tmpmesh, compute_midmesh, &
        ESMF_REGRIDMETHOD_CONSERVE, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatX2A(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatX2A(i)%factorList(nentries))
      if(nentries .ge. 1) then
        call c_ESMC_Copy_TempWeights_xgrid(tweights, &
        xgtype%sparseMatX2A(i)%factorIndexList(1,1), &
        xgtype%sparseMatX2A(i)%factorList(1))
      endif
      if(xggt_a(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_XGridGetFracInt(xgtype%sideA(i)%gbcp%grid, mesh=meshAt(i), array=xgtype%fracX2A(i), &
             staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_XGridGetFracInt(mesh=meshAt(i), array=xgtype%fracX2A(i), &
             meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    enddo

    ! now do the B side
    do i = 1, ngrid_b
      call c_esmc_xgridregrid_create(vm, meshBt(i), mesh, &
        tmpmesh, compute_midmesh, &
        ESMF_REGRIDMETHOD_CONSERVE, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatB2X(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatB2X(i)%factorList(nentries))
      if(nentries .ge. 1) then
        call c_ESMC_Copy_TempWeights_xgrid(tweights, &
        xgtype%sparseMatB2X(i)%factorIndexList(1,1), &
        xgtype%sparseMatB2X(i)%factorList(1))
      endif
      if(xggt_b(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_XGridGetFracInt(xgtype%sideB(i)%gbcp%grid, mesh=meshBt(i), array=xgtype%fracB2X(i), &
             staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_XGridGetFracInt(mesh=meshBt(i), array=xgtype%fracB2X(i), &
             meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    
      ! Now the reverse direction
      call c_esmc_xgridregrid_create(vm, mesh, meshBt(i), &
        tmpmesh, compute_midmesh, &
        ESMF_REGRIDMETHOD_CONSERVE, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatX2B(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatX2B(i)%factorList(nentries))
      if(nentries .ge. 1) then
        call c_ESMC_Copy_TempWeights_xgrid(tweights, &
        xgtype%sparseMatX2B(i)%factorIndexList(1,1), &
        xgtype%sparseMatX2B(i)%factorList(1))
      endif
      if(xggt_b(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_XGridGetFracInt(xgtype%sideB(i)%gbcp%grid, mesh=meshBt(i), array=xgtype%fracX2B(i), &
             staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call ESMF_XGridGetFracInt(mesh=meshBt(i), array=xgtype%fracX2B(i), &
             meshloc=ESMF_MESHLOC_ELEMENT, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
             ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    enddo

    xgtype%storeOverlay = .false.
    if(present(storeOverlay)) then
      if(storeOverlay) xgtype%storeOverlay = .true.
    endif
      
    ! call into offline xgrid create with the xgrid specs
    call ESMF_XGridConstruct(xgtype, xgtype%sideA, xgtype%sideB, &
      sparseMatA2X=xgtype%sparseMatA2X, sparseMatX2A=xgtype%sparseMatX2A, &
      sparseMatB2X=xgtype%sparseMatB2X, sparseMatX2B=xgtype%sparseMatX2B, &
      offline=.false., &
      mesh=mesh, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! store the middle mesh if needed
    ! and clean up temporary memory used
    if(xgtype%storeOverlay) then
      xgtype%mesh = mesh
    else
      call ESMF_MeshDestroy(mesh, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    do i = 1, ngrid_a
      if(present(sideAMaskValues)) then
        call ESMF_MeshTurnOffCellMask(meshAt(i), rc=localrc);
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      if(xggt_a(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_MeshDestroy(meshAt(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    enddo

    do i = 1, ngrid_b
      if(present(sideBMaskValues)) then
        call ESMF_MeshTurnOffCellMask(meshBt(i), rc=localrc);
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      if(xggt_b(i) == ESMF_XGRIDGEOMTYPE_GRID) then
        call ESMF_MeshDestroy(meshBt(i), rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    enddo

    deallocate(meshAt, meshBt)
    deallocate(xggt_a, xggt_b)

    ! Finalize XGrid Creation
    xgtype%online = 1
    xgtype%status = ESMF_STATUS_READY
    ESMF_XGridCreate%xgtypep => xgtype 
    ESMF_INIT_SET_CREATED(ESMF_XGridCreate)

    !call ESMF_XGridValidate(ESMF_XGridCreate, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !    ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridCreateFromSparseMat()"
!BOP
! !IROUTINE:  ESMF_XGridCreateFromSparseMat an XGrid from raw input parameters

! !INTERFACE:

function ESMF_XGridCreateFromSparseMat(keywordEnforcer, &
    sideAGrid,              sideAMesh, &
    sideBGrid,              sideBMesh, &
    sideAGridPriority,      sideAMeshPriority, &
    sideBGridPriority,      sideBMeshPriority, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, &
    area, centroid, &
    name, &
    rc) 

!
! !RETURN VALUE:
    type(ESMF_XGrid) :: ESMF_XGridCreateFromSparseMat
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
type(ESMF_Grid),      intent(in), optional :: sideAGrid(:)
type(ESMF_Mesh),      intent(in), optional :: sideAMesh(:)
type(ESMF_Grid),      intent(in), optional :: sideBGrid(:)
type(ESMF_Mesh),      intent(in), optional :: sideBMesh(:)
integer,              intent(in), optional :: sideAGridPriority(:)
integer,              intent(in), optional :: sideAMeshPriority(:)
integer,              intent(in), optional :: sideBGridPriority(:)
integer,              intent(in), optional :: sideBMeshPriority(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatA2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2A(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatB2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2B(:)
real(ESMF_KIND_R8),   intent(in), optional :: area(:)
real(ESMF_KIND_R8),   intent(in), optional :: centroid(:,:)
character (len=*),    intent(in), optional :: name
integer,              intent(out),optional :: rc 

!
! !DESCRIPTION:
!      Create an XGrid directly from user supplied sparse matrix parameters. User
!      is responsible to supply all information necessary for communication calculation. 
!      For an example and associated documentation using this method see section 
!      \ref{sec:xgrid:usage:xgrid_createfromsparsemat}
!
!     The arguments are:
!     \begin{description}
!     \item [{[sideAGrid]}]
!           Parametric 2D Grids on side A, for example, 
!           these Grids can be either Cartesian 2D or Spherical.
!     \item [{[sideAMesh]}]
!           Parametric 2D Meshes on side A, for example, 
!           these Meshes can be either Cartesian 2D or Spherical.
!     \item [{[sideBGrid]}]
!           Parametric 2D Grids on side B, for example, 
!           these Grids can be either Cartesian 2D or Spherical.
!     \item [{[sideBMesh]}]
!           Parametric 2D Meshes on side B, for example, 
!           these Meshes can be either Cartesian 2D or Spherical.
!     \item [{[sideAGridPriority]}]
!           Priority array of Grids on {\tt sideA} during overlay generation.
!           The priority arrays describe the priorities of Grids at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Grid of the
!           highest priority to the lowest priority.
!     \item [{[sideAMeshPriority]}]
!           Priority array of Meshes on {\tt sideA} during overlay generation.
!           The priority arrays describe the priorities of Meshes at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Mesh of the
!           highest priority to the lowest priority.
!     \item [{[sideBGridPriority]}]
!           Priority of Grids on {\tt sideB} during overlay generation
!           The priority arrays describe the priorities of Grids at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Grid of the
!           highest priority to the lowest priority.
!     \item [{[sideBMeshPriority]}]
!           Priority array of Meshes on {\tt sideB} during overlay generation.
!           The priority arrays describe the priorities of Meshes at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Mesh of the
!           highest priority to the lowest priority.
!     \item [{[sparseMatA2X]}]
!           indexlist from a Grid index space on side A to xgrid index space;
!           indexFactorlist from a Grid index space on side A to xgrid index space.
!     \item [{[sparseMatX2A]}]
!           indexlist from xgrid index space to a Grid index space on side A;
!           indexFactorlist from xgrid index space to a Grid index space on side A.
!     \item [{[sparseMatB2X]}]
!           indexlist from a Grid index space on side B to xgrid index space;
!           indexFactorlist from a Grid index space on side B to xgrid index space.
!     \item [{[sparseMatX2B]}]
!           indexlist from xgrid index space to a Grid index space on side B;
!           indexFactorlist from xgrid index space to a Grid index space on side B.
!     \item [{[area]}]
!           area of the xgrid cells.
!     \item [{[centroid]}]
!           coordinates at the area weighted center of the xgrid cells.
!     \item [{[name]}]
!           name of the xgrid object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOP

    integer :: localrc, ngrid_a, ngrid_b
    integer :: i, j, ncells, ndim
    type(ESMF_XGridType), pointer :: xgtype

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check there are enough input to create the XGrid
    if(.not. present(sideAGrid) .and. .not. present(sideAMesh)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- Either Grid or Mesh must be provided on sideA", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif
    if(.not. present(sideBGrid) .and. .not. present(sideBMesh)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- Either Grid or Mesh must be provided on sideB", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    if(present(sideAGrid) .and. present(sideAMesh)) then
      if(size(sideAGrid, 1)+size(sideAMesh, 1) .le. 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- Either Grid or Mesh must be provided on sideA", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
    endif

    if(present(sideBGrid) .and. present(sideBMesh)) then
      if(size(sideBGrid, 1)+size(sideBMesh, 1) .le. 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- Either Grid or Mesh must be provided on sideB", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
    endif

    ! check init status of input Grids
    if(present(sideAGrid)) then
      ngrid_a = size(sideAGrid, 1)
      do i = 1, ngrid_a
          ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideAGrid(i),rc)
      enddo

      ! No need to check grid for offline xgrid creation, assume user know what they are doing!
      !do i = 1, ngrid_a
      !  call checkGrid(sideAGrid(i), ESMF_STAGGERLOC_CORNER, rc=localrc)
      !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !      ESMF_CONTEXT, rcToReturn=rc)) return
      !enddo

      if(present(sideAGridPriority)) then
        if(size(sideAGridPriority, 1) /= ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- Size of sideAGridPriority does not match size of sideAGrid", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
    endif

    if(present(sideBGrid)) then
      ngrid_b = size(sideBGrid, 1)
      do i = 1, ngrid_b
          ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideBGrid(i),rc)
      enddo
      ! No need to check grid for offline xgrid creation, assume user know what they are doing!
      !do i = 1, ngrid_b
      !  call checkGrid(sideBGrid(i), ESMF_STAGGERLOC_CORNER, rc=localrc)
      !  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      !      ESMF_CONTEXT, rcToReturn=rc)) return
      !enddo
      if(present(sideBGridPriority)) then
        if(size(sideBGridPriority, 1) /= ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- Size of sideBGridPriority does not match size of sideBGrid", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
    endif

    !TODO: need to check Meshes too.
    if(present(sideAMesh)) then
      ngrid_a = size(sideAMesh, 1)
      if(present(sideAMeshPriority)) then
        if(size(sideAMeshPriority, 1) /= ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- Size of sideAMeshPriority does not match size of sideAMesh", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
    endif
    if(present(sideBMesh)) then
      ngrid_b = size(sideBMesh, 1)
      if(present(sideBMeshPriority)) then
        if(size(sideBMeshPriority, 1) /= ngrid_b) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- Size of sideBMeshPriority does not match size of sideBMesh", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      endif
    endif

    ! Priority for Grid and Mesh must all be present simultaneously
    if( (present(sideAGridPriority) .and. .not. present(sideAGrid)) .or. &
         present(sideAMeshPriority) .and. .not. present(sideAMesh)  .or. &
         present(sideBGridPriority) .and. .not. present(sideBGrid)  .or. &
         present(sideBMeshPriority) .and. .not. present(sideBMesh)) then

      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
         msg="- Cannot specify Grid or Mesh Priority without actual list of Grids or Meshes", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! At this point the inputs are consistently sized.
    ! Take care of ordering
    ngrid_a = 0
    if(present(sideAGrid)) ngrid_a = size(sideAGrid, 1)
    if(present(sideAMesh)) ngrid_a = ngrid_a + size(sideAMesh, 1)
    ngrid_b = 0
    if(present(sideBGrid)) ngrid_b = size(sideBGrid, 1)
    if(present(sideBMesh)) ngrid_b = ngrid_b + size(sideBMesh, 1)

    ! do some range checking on priority lists
    if(present(sideAGridPriority) .and. present(sideAMeshPriority)) then
      do i = 1, size(sideAGridPriority, 1)
        if(sideAGridPriority(i) .le. 0 .or. sideAGridPriority(i) .gt. ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- sideAGridPriority value out of range", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      enddo
      do i = 1, size(sideAMeshPriority, 1)
        if(sideAMeshPriority(i) .le. 0 .or. sideAMeshPriority(i) .gt. ngrid_a) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- sideAMeshPriority value out of range", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      enddo

      do i = 1, size(sideAGridPriority, 1)
        do j = 1, size(sideAMeshPriority, 1)
          if(sideAGridPriority(i) == sideAMeshPriority(j)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- sideAGridPriority and sideAMeshPriority cannot have duplicate entry", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
        enddo
      enddo
    endif

    if(present(sideBGridPriority) .and. present(sideBMeshPriority)) then
      do i = 1, size(sideBGridPriority, 1)
        if(sideBGridPriority(i) .le. 0 .or. sideBGridPriority(i) .gt. ngrid_b) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- sideBGridPriority value out of range", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      enddo
      do i = 1, size(sideBMeshPriority, 1)
        if(sideBMeshPriority(i) .le. 0 .or. sideBMeshPriority(i) .gt. ngrid_b) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
             msg="- sideBMeshPriority value out of range", &
             ESMF_CONTEXT, rcToReturn=rc) 
          return
        endif
      enddo

      do i = 1, size(sideBGridPriority, 1)
        do j = 1, size(sideBMeshPriority, 1)
          if(sideBGridPriority(i) == sideBMeshPriority(j)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- sideBGridPriority and sideBMeshPriority cannot have duplicate entry", &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
          endif
        enddo
      enddo
    endif

    if(.not. present(sparseMatA2X) .and. &
      (.not. present(sparseMatX2A)).and. &
      (.not. present(sparseMatB2X)).and. &
      (.not. present(sparseMatX2B))) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- One of the sparseMat must be set to generate an XGrid", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif

    ! initialize XGridType object and its base object
    nullify(xgtype)
    nullify(ESMF_XGridCreateFromSparseMat%xgtypep)
    call ESMF_XGridConstructBaseObj(xgtype, name, localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="Constructing xgtype base object ", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    allocate(xgtype%sideA(ngrid_a), xgtype%sideB(ngrid_b), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
        msg="- Allocating xgtype%sideA or xgtype%sideB ", &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Need to initialize xgtype%sideA based on sideAGrid and/or sideAMesh
    if(present(sideAGrid)) then
      if(present(sideAGridPriority)) then
        do i = 1, size(sideAGrid, 1)
          xgtype%sideA(sideAGridPriority(i)) = ESMF_XGridGeomBaseCreate(sideAGrid(i), &
            ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
      else
        do i = 1, size(sideAGrid, 1)
          xgtype%sideA(i) = ESMF_XGridGeomBaseCreate(sideAGrid(i), &
            ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
      endif

      if(present(sideAMesh)) then
        if(present(sideAMeshPriority)) then
          do i = 1, size(sideAMesh, 1)
            xgtype%sideA(sideAMeshPriority(i)) = ESMF_XGridGeomBaseCreate(sideAMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        else
          do i = 1, size(sideAMesh, 1)
            xgtype%sideA(i+size(sideAGrid, 1)) = ESMF_XGridGeomBaseCreate(sideAMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif
    else ! .not. present(sideAGrid)
      if(present(sideAMesh)) then
        if(present(sideAMeshPriority)) then
          do i = 1, size(sideAMesh, 1)
            xgtype%sideA(sideAMeshPriority(i)) = ESMF_XGridGeomBaseCreate(sideAMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        else
          do i = 1, size(sideAMesh, 1)
            xgtype%sideA(i) = ESMF_XGridGeomBaseCreate(sideAMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif
    endif

    ! Need to initialize xgtype%sideB based on sideBGrid and/or sideBMesh
    if(present(sideBGrid)) then
      if(present(sideBGridPriority)) then
        do i = 1, size(sideBGrid, 1)
          xgtype%sideB(sideBGridPriority(i)) = ESMF_XGridGeomBaseCreate(sideBGrid(i), &
            ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
      else
        do i = 1, size(sideBGrid, 1)
          xgtype%sideB(i) = ESMF_XGridGeomBaseCreate(sideBGrid(i), &
            ESMF_STAGGERLOC_CENTER, rc=localrc)
          if (ESMF_LogFoundError(localrc, &
              ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
      endif

      if(present(sideBMesh)) then
        if(present(sideBMeshPriority)) then
          do i = 1, size(sideBMesh, 1)
            xgtype%sideB(sideBMeshPriority(i)) = ESMF_XGridGeomBaseCreate(sideBMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        else
          do i = 1, size(sideBMesh, 1)
            xgtype%sideB(i+size(sideBGrid, 1)) = ESMF_XGridGeomBaseCreate(sideBMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif
    else ! .not. present(sideBGrid)
      if(present(sideBMesh)) then
        if(present(sideBMeshPriority)) then
          do i = 1, size(sideBMesh, 1)
            xgtype%sideB(sideBMeshPriority(i)) = ESMF_XGridGeomBaseCreate(sideBMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        else
          do i = 1, size(sideBMesh, 1)
            xgtype%sideB(i) = ESMF_XGridGeomBaseCreate(sideBMesh(i), &
              ESMF_MESHLOC_ELEMENT, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif
    endif

    if(present(area)) then
      ncells = size(area, 1)
      allocate(xgtype%area(ncells), stat=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
          msg="- Allocating xgtype%area ", &
          ESMF_CONTEXT, rcToReturn=rc)) return
      xgtype%area = area
    endif

    if(present(centroid)) then
        ncells = size(centroid, 1)
        ndim = size(centroid, 2)
        allocate(xgtype%centroid(ncells, ndim), stat=localrc)
        if (ESMF_LogFoundAllocError(localrc, &
            msg="- Allocating xgtype%centroid ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
        xgtype%centroid = centroid
    endif

    call ESMF_XGridConstruct(xgtype, xgtype%sideA, xgtype%sideB, &
      sparseMatA2X=sparseMatA2X, sparseMatX2A=sparseMatX2A, &
      sparseMatB2X=sparseMatB2X, sparseMatX2B=sparseMatX2B, offline=.true., rc=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="Constructing xgtype object ", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Finalize XGrid Creation
    xgtype%online = 0
    xgtype%status = ESMF_STATUS_READY
    ESMF_XGridCreateFromSparseMat%xgtypep => xgtype 
    ESMF_INIT_SET_CREATED(ESMF_XGridCreateFromSparseMat)

    call ESMF_XGridValidate(ESMF_XGridCreateFromSparseMat, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridCreateFromSparseMat

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridConstruct()"
!BOPI
! !IROUTINE:  ESMF_XGridConstruct - Construct XGrid from input

! !INTERFACE:
subroutine ESMF_XGridConstruct(xgtype, sideA, sideB, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, offline, &
    mesh, internal_alloc, rc)
!
! !ARGUMENTS:
type(ESMF_XGridType), intent(inout)        :: xgtype
type(ESMF_XGridGeomBase), intent(in)       :: sideA(:), sideB(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatA2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2A(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatB2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2B(:)
logical, intent(in), optional              :: offline
type(ESMF_Mesh), intent(inout), optional   :: mesh
logical, intent(in), optional              :: internal_alloc
integer, intent(out), optional             :: rc 

!
! !DESCRIPTION:
!     Construct internals of xgtype from input
!
!     The arguments are:
!     \begin{description}
!     \item [xgtype]
!           the {ESMF\_XGridType} object.
!     \item [sideA]
!           2D Grids on side A.
!     \item [sideB]
!           2D Grids on side B.
!     \item [{[sparseMatA2X]}]
!           indexlist from a Grid index space on side A to xgrid index space;
!           indexFactorlist from a Grid index space on side A to xgrid index space.
!     \item [{[sparseMatX2A]}]
!           indexlist from xgrid index space to a Grid index space on side A;
!           indexFactorlist from xgrid index space to a Grid index space on side A.
!     \item [{[sparseMatB2X]}]
!           indexlist from a Grid index space on side B to xgrid index space;
!           indexFactorlist from a Grid index space on side B to xgrid index space.
!     \item [{[sparseMatX2B]}]
!           indexlist from xgrid index space to a Grid index space on side B;
!           indexFactorlist from xgrid index space to a Grid index space on side B.
!     \item [{[offline]}]
!           online generation optimization turned on/off (default off)
!     \item [{[mesh]}]
!           online generation with mesh
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

  integer :: localrc, ngrid_a, ngrid_b
  integer :: i
  logical :: l_offline
  real(ESMF_KIND_R8), pointer :: xgrid_frac(:)

  localrc = ESMF_SUCCESS

  ! Initialize return code   
  if(present(rc)) rc = ESMF_RC_NOT_IMPL
  l_offline = .true.
  if(present(offline)) l_offline = offline

  ngrid_a = size(sideA, 1)
  ngrid_b = size(sideB, 1)

  ! check and copy all the sparse matrix spec structures
  if(present(sparseMatA2X) .and. l_offline) then
      call ESMF_SparseMatca(sparseMatA2X, xgtype%sparseMatA2X, ngrid_a, &
        'sparseMatA2X', rc=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
          msg="- Initializing xgtype%sparseMatX2A ", &
          ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  if(present(sparseMatX2A) .and. l_offline) then
      call ESMF_SparseMatca(sparseMatX2A, xgtype%sparseMatX2A, ngrid_a, &
        'sparseMatX2A', rc=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
          msg="- Initializing xgtype%sparseMatX2A ", &
          ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  ! TODO:
  ! if both A2X and X2A are present, check the sequence index list of X are identical
  ! this checking will be collective since the indices needs to be gathered
  ! if(present(sparseMatA2X) .and. present(sparseMatX2A)) then
  ! endif
  ! Another approach is to create 2 distgrids and use distgridMatch to compare
  ! the result Distgrid as discussed.

  if(present(sparseMatB2X) .and. l_offline) then
      call ESMF_SparseMatca(sparseMatB2X, xgtype%sparseMatB2X, ngrid_b, &
        'sparseMatB2X', rc=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
          msg="- Initializing xgtype%sparseMatB2X ", &
          ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  if(present(sparseMatX2B) .and. l_offline) then
      call ESMF_SparseMatca(sparseMatX2B, xgtype%sparseMatX2B, ngrid_b, &
        'sparseMatX2B', rc=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
          msg="- Initializing xgtype%sparseMatX2B ", &
          ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  ! TODO:
  ! if both B2X and X2B are present, check the sequence index list of X are identical
  ! this checking will be collective since the indices needs to be gathered
  ! if(present(sparseMatA2X) .and. present(sparseMatX2A)) then
  ! endif

  ! create the distgrids
  if((.not. l_offline) .and. present(mesh)) then
    call ESMF_XGridDistGridsOnline(xgtype, mesh, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  else
    call ESMF_XGridDistGrids(xgtype, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  ! Create the fracX here because we have a non-vanishing distgridM at this point and
  ! we know its entries should always be 1.0. This could be left for the user but it's 
  ! provided here so regridstore call retrieve this Field directly either as src or dst Frac.
  if(.not. l_offline) then
    xgtype%fracX = ESMF_ArrayCreate(xgtype%distgridM, typekind=ESMF_TYPEKIND_R8, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    ! mesh distgrid should always 1 de/pet
    call ESMF_ArrayGet(xgtype%fracX, localDe=0, farrayPtr=xgrid_frac, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    xgrid_frac = 1.0
  endif

  if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridConstruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ESMF_XGridDistGridsOnline()"
!BOPI
! !IROUTINE:  ESMF_XGridDistGridsOnline - create the distgrids 
!                                         online for the xgridtype object

! !INTERFACE:
subroutine ESMF_XGridDistGridsOnline(xgtype, mesh, rc)

!
! !ARGUMENTS:
    type(ESMF_XGridType), intent(inout) :: xgtype
    type(ESMF_Mesh),      intent(inout) :: mesh
    integer, intent(out), optional      :: rc

!
! !DESCRIPTION:
!      Create the distgrids for the {ESMF\_XGridType} object
!
!     The arguments are:
!     \begin{description}
!     \item [xgtype]
!           the {ESMF\_XGridType} object.
!     \item [{[mesh]}]
!           the {ESMF\_Mesh} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

  integer                                :: localrc, i
  type(ESMF_DistGrid)                    :: distgrid

  ! Initialize
  localrc = ESMF_RC_NOT_IMPL

  ! Initialize return code   
  if(present(rc)) rc = ESMF_RC_NOT_IMPL

  if(xgtype%storeOverlay) then
    call ESMF_MeshGet(mesh, elementDistgrid=xgtype%distgridM, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  else
    call ESMF_MeshGet(mesh, elementDistgrid=distgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    xgtype%distgridM = ESMF_DistGridCreate(distgrid, indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  endif
  allocate(xgtype%distgridA(size(xgtype%sideA)))
  allocate(xgtype%distgridB(size(xgtype%sideB)))
  do i = 1, size(xgtype%sideA)
    call ESMF_XGridGeomBaseGet(xgtype%sideA(i), distgrid=xgtype%distgridA(i), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  enddo
  do i = 1, size(xgtype%sideB)
    call ESMF_XGridGeomBaseGet(xgtype%sideB(i), distgrid=xgtype%distgridB(i), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  enddo

  if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridDistGridsOnline

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ESMF_XGridDistGrids()"
!BOPI
! !IROUTINE:  ESMF_XGridDistGrids - create the distgrids for the xgridtype object

! !INTERFACE:
subroutine ESMF_XGridDistGrids(xgtype, rc)

!
! !ARGUMENTS:
    type(ESMF_XGridType), intent(inout) :: xgtype
    integer, intent(out), optional      :: rc

!
! !DESCRIPTION:
!      Create the distgrids for the {ESMF\_XGridType} object
!
!     The arguments are:
!     \begin{description}
!     \item [xgtype]
!           the {ESMF\_XGridType} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

    integer :: i, ngrid, localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! create the A side distgrids
    if(associated(xgtype%sparseMatA2X)) then
        ngrid = size(xgtype%sideA, 1)
        allocate(xgtype%distgridA(ngrid), stat=localrc)
        if (ESMF_LogFoundAllocError(localrc, &
            msg="- Allocating xgtype%distgridA(ngrid) ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, ngrid
            call ESMF_XGridDG(xgtype%sideA(i), xgtype%distgridA(i), &
                xgtype%sparseMatA2X(i)%factorIndexList, 2, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
    endif

    ! if A2X is not provided and X2A is provided
    ! compute A side distgrids based on X2A 
    if(.not. associated(xgtype%sparseMatA2X) .and. &
        associated(xgtype%sparseMatX2A)) then
        ngrid = size(xgtype%sideA, 1)
        allocate(xgtype%distgridA(ngrid), stat=localrc)
        if (ESMF_LogFoundAllocError(localrc, &
            msg="- Allocating xgtype%distgridA(ngrid) ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, ngrid
            call ESMF_XGridDG(xgtype%sideA(i), xgtype%distgridA(i), &
                xgtype%sparseMatX2A(i)%factorIndexList, 1, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
    endif

    ! create the B side distgrids
    if(associated(xgtype%sparseMatB2X)) then
        ngrid = size(xgtype%sideB, 1)
        allocate(xgtype%distgridB(ngrid), stat=localrc)
        if (ESMF_LogFoundAllocError(localrc, &
            msg="- Allocating xgtype%distgridB(ngrid) ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, ngrid
            call ESMF_XGridDG(xgtype%sideB(i), xgtype%distgridB(i), &
                xgtype%sparseMatB2X(i)%factorIndexList, 2, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
    endif

    ! if B2X is not provided and X2B is provided
    ! compute B side distgrids based on X2B 
    if(.not. associated(xgtype%sparseMatB2X) .and. &
        associated(xgtype%sparseMatX2B)) then
        ngrid = size(xgtype%sideB, 1)
        allocate(xgtype%distgridB(ngrid), stat=localrc)
        if (ESMF_LogFoundAllocError(localrc, &
            msg="- Allocating xgtype%distgridB(ngrid) ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
        do i = 1, ngrid
            call ESMF_XGridDG(xgtype%sideB(i), xgtype%distgridB(i), &
                xgtype%sparseMatX2B(i)%factorIndexList, 1, rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        enddo
    endif

    ! use the union of A2X indices to create the balanced distgrid
    if(associated(xgtype%sparseMatA2X)) then
        xgtype%distgridM = ESMF_XGridDGOverlay(xgtype%sparseMatA2X, 2, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! if A2X is not provided and X2A is provided
    ! use the union of X2A indices to create the balanced distgrid
    if(.not. associated(xgtype%sparseMatA2X) .and. &
        associated(xgtype%sparseMatX2A)) then
        xgtype%distgridM = ESMF_XGridDGOverlay(xgtype%sparseMatX2A, 1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! use the union of B2X indices to create the balanced distgrid
    if(.not. associated(xgtype%sparseMatA2X) .and. &
       .not. associated(xgtype%sparseMatX2A) .and. &
        associated(xgtype%sparseMatB2X)) then
        xgtype%distgridM = ESMF_XGridDGOverlay(xgtype%sparseMatB2X, 2, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! use the union of X2B indices to create the balanced distgrid
    if(.not. associated(xgtype%sparseMatA2X) .and. &
       .not. associated(xgtype%sparseMatX2A) .and. &
       .not. associated(xgtype%sparseMatB2X) .and. &
        associated(xgtype%sparseMatX2B)) then
        xgtype%distgridM = ESMF_XGridDGOverlay(xgtype%sparseMatX2B, 1, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    !if(.not. associated(xgtype%sparseMatA2X) .and. &
    !   .not. associated(xgtype%sparseMatX2A) .and. &
    !   .not. associated(xgtype%sparseMatB2X) .and. &
    !   .not. associated(xgtype%sparseMatX2B)) then
    !    call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
    !       "- one of the sparse matrix arguments must be specified", &
    !       ESMF_CONTEXT, rcToReturn=rc) 
    !    return

    !endif

    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridDistGrids

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridDGOverlay()"
!BOPI
! !IROUTINE:  ESMF_XGridDGOverlay - compute the overlay distgrid from offline input

! !INTERFACE:
function ESMF_XGridDGOverlay(sparseMat, dim, rc)
!
! !RETURN VALUE:
    type(ESMF_DistGrid)                         :: ESMF_XGridDGOverlay
!
! !ARGUMENTS:
    type(ESMF_XGridSpec), pointer               :: sparseMat(:)
    integer, intent(in)                         :: dim
    integer, intent(out), optional              :: rc

!
! !DESCRIPTION:
!      Compute the overlay distgrid from offline input of indices
!
!     The arguments are:
!     \begin{description}
!     \item [sparseMat]
!           the {ESMF\_XGridSpec} object containing indices and weights.
!     \item [dim]
!           dimension of the indices used to retrieve the seq. index list.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

    integer :: i, j, ii, ngrid, localrc, nidx, nidx_tot, l, u
    integer :: minidx, maxidx, minidx1, maxidx1, minidx_n, maxidx_n
    integer, allocatable :: indices(:), indices_diff(:), indices_union(:)
    integer, allocatable :: iarray(:), iarray_t(:)

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ngrid = size(sparseMat, 1)

    ! generate the union of indices from all the factorIndexLists:
    ! generate the initial array that has the index positions marked '1'
    ! Because of the distributed nature of the indices, there may be
    ! duplicate entries in the index union residing on the other PETs
    ! this is currently left to to the SMM engine to detect such an error.
    !
    ! TODO: query the distributed data directory to avoid duplication
    ! and return to user an error as early as possible
    minidx = minval(sparseMat(1)%factorIndexList(dim,:))
    maxidx = maxval(sparseMat(1)%factorIndexList(dim,:))
    allocate(iarray(minidx:maxidx), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
        msg="- Allocating iarray(minidx:maxidx) ", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    iarray = 0
    l = lbound(sparseMat(1)%factorIndexList, dim)
    u = ubound(sparseMat(1)%factorIndexList, dim)
    do j = l, u
        iarray(sparseMat(1)%factorIndexList(dim,j)) = 1
    enddo

    do i = 2, ngrid

        minidx1 = minval(sparseMat(i)%factorIndexList(dim,:))
        maxidx1 = maxval(sparseMat(i)%factorIndexList(dim,:))
        minidx_n = min(minidx, minidx1)
        maxidx_n = max(maxidx, maxidx1)

        allocate(iarray_t(minidx_n:maxidx_n), stat=localrc)
        if (ESMF_LogFoundAllocError(localrc, &
            msg="- Allocating iarray_t(minidx_n:maxidx_n) ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
        ! copy the old index position array
        iarray_t = 0
        do j = minidx, maxidx
            iarray_t(j) = iarray(j)
        enddo
        ! toggle the index position array with the new index list
        ! do local uniqueness checking
        l = lbound(sparseMat(i)%factorIndexList, dim)
        u = ubound(sparseMat(i)%factorIndexList, dim)
        do j = l, u
            if(iarray_t(sparseMat(i)%factorIndexList(dim,j)) == 1) then
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_RANK, &
                msg=" - local duplicate index entry discovered", &
                ESMF_CONTEXT, rcToReturn=rc)
              return
            endif
            iarray_t(sparseMat(i)%factorIndexList(dim,j)) = 1
        enddo

        minidx = minidx_n
        maxidx = maxidx_n

        ! reset the index posity array, swap the temp one over
        deallocate(iarray)         
        allocate(iarray(minidx:maxidx), stat=localrc)
        if (ESMF_LogFoundAllocError(localrc, &
            msg="- Allocating iarray(minidx:maxidx) ", &
            ESMF_CONTEXT, rcToReturn=rc)) return
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
    if (ESMF_LogFoundAllocError(localrc, &
        msg="- Allocating indices(nidx) ", &
        ESMF_CONTEXT, rcToReturn=rc)) return

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
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    deallocate(iarray, indices)

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridDGOverlay

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridDG()"
!BOPI
! !IROUTINE:  ESMF_XGridDG - compute distgrid on the XGrid locally matching the grid

! !INTERFACE:
subroutine ESMF_XGridDG(xgridgeombase, distgrid, factorIndexList, dim, rc)
!
! !DESCRIPTION:
!      Compute distgrid on the XGrid locally matching the grid on the same set of PETs;
!     This allows local SMM optimization. To be completed.
!
!     The arguments are:
!     \begin{description}
!     \item [xgridgeombase]
!           xgridgeombase object spanning a set of PETs.
!     \item [distgrid]
!           distgrid object spanning the same set of PETs.
!     \item [factorIndexList]
!           indices used to construct the arb index list.
!     \item [dim]
!           dimension of the indices used to retrieve the seq. index list.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

    type(ESMF_XGridGeomBase), intent(in)        :: xgridgeombase
    type(ESMF_DistGrid), intent(inout)          :: distgrid
    integer,             pointer                :: factorIndexList(:,:)
    integer, intent(in)                         :: dim
    integer, intent(out), optional              :: rc

    integer                                     :: localrc, nidx_src, nidx_dst

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    !print *, dim, size(factorIndexList, 2), factorIndexList(dim, :)

    distgrid = ESMF_DistGridCreate(factorIndexList(dim,:), rc=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
        msg="- Creating distgrid from factorIndexList", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_SparseMatca()"
!BOPI
! !IROUTINE:  ESMF_SparseMatca - allocate internal SMM parameters and copy from src.

! !INTERFACE:
subroutine ESMF_SparseMatca(sparseMats, sparseMatd, ngrid, tag, rc)

!
! !ARGUMENTS:
    type(ESMF_XGridSpec), intent(in)    :: sparseMats(:)
    type(ESMF_XGridSpec),     pointer   :: sparseMatd(:)
    integer, intent(in)                 :: ngrid
    character(len=*), intent(in)        :: tag
    integer, intent(out), optional      :: rc

!
! !DESCRIPTION:
!      Allocate internal SMM parameters and copy from src.
!
!     The arguments are:
!     \begin{description}
!     \item [sparseMats]
!           the source {\tt ESMF\_XGridSpec} object.
!     \item [sparseMatd]
!           the destination {\tt ESMF\_XGridSpec} object.
!     \item [ngrid]
!           number of grid, redundency check.
!     \item [tag]
!           A string to indicate which one of the 4 SMM parameters is used.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI
    integer                             :: i, localrc

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    if(size(sparseMats,1) /= ngrid) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- number of Grids different from size of sparseMat for "//tag, &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif

    do i = 1, ngrid
        if(.not. associated(sparseMats(i)%factorIndexList) .or. &
           .not. associated(sparseMats(i)%factorList)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- sparseMat not initiailzed properly for "//tag, &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif

        if(size(sparseMats(i)%factorIndexList, 2) /= size(sparseMats(i)%factorList, 1)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
               msg="- sparseMat factorIndexList and factorList sizes not consistent "//tag, &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif
    enddo
        
    allocate(sparseMatd(ngrid), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
        msg="Allocating xgtype%"//tag, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    do i = 1, ngrid
      sparseMatd(i) = sparseMats(i)
    enddo

    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_SparseMatca

!------------------------------------------------------------------------------
! Small subroutine to make sure that Grid doesn't
! contain some of the properties that aren't currently
! allowed in regridding. Slightly enhanced from the version in FieldRegrid.
#undef  ESMF_METHOD
#define ESMF_METHOD "checkGrid()"
!BOPI
! !IROUTINE:  checkGrid - check the grid to make sure it can be used to create XGrid

! !INTERFACE:
subroutine checkGrid(grid,staggerloc,rc)
!
! !ARGUMENTS:
    type (ESMF_Grid) :: grid
    type(ESMF_StaggerLoc) :: staggerloc
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Check the grid to make sure it can be used to create XGrid
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           the {\tt ESMF\_Grid} object.
!     \item [staggerloc]
!           the {\tt ESMF\_STAGGERLOC} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI
    type(ESMF_GridDecompType) :: decompType
    integer :: localDECount, lDE, ec(ESMF_MAXDIM)
    integer :: localrc, i, dimCount

    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Make sure Grid isn't arbitrarily distributed
    call ESMF_GridGetDecompType(grid, decompType, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   ! Error if decompType is ARBITRARY
   if (decompType .eq. ESMF_GRID_ARBITRARY) then
         call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
             msg="- can't currently regrid an arbitrarily distributed Grid", & 
             ESMF_CONTEXT, rcToReturn=rc) 
          return
   endif        

   ! Make sure Grid doesn't contain width 1 DEs
   call ESMF_GridGet(grid,localDECount=localDECount, dimCount=dimCount, &
          rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

   if (dimCount .ne. 2) then
         call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
         msg="- can currently only create xgrid on 2D grids", & 
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif
   
   ! loop through checking DEs
   do lDE=0,localDECount-1
       
       ! Get bounds of DE
       call ESMF_GridGet(grid,staggerloc=staggerloc, localDE=lDE, &
              exclusivecount=ec,rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

       ! loop and make sure they aren't too small in any dimension
       do i=1,dimCount
          if (ec(i) .lt. 2) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
          msg="- can't currently regrid a grid that contains a DE of width less than 2", & 
             ESMF_CONTEXT, rcToReturn=rc) 
          return
          endif
       enddo
   enddo

   if(present(rc)) rc = ESMF_SUCCESS
end subroutine checkGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetFracIntGrid"
!BOPI
! !IROUTINE: ESMF_XGridGetFracInt - Gets the frac of grid cells after a regrid from a Mesh

! !INTERFACE:
      subroutine ESMF_XGridGetFracIntGrid(Grid, Mesh, Array, staggerLoc, &
                 rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in)            :: Grid
      type(ESMF_Mesh), intent(inout)         :: Mesh
      type(ESMF_Array), intent(inout)        :: Array
      type(ESMF_StaggerLoc), intent(in)      :: staggerLoc
      integer, intent(out), optional         :: rc
!
! !DESCRIPTION:
!     The arguments are:
!     \begin{description}
!     \item[Mesh]
!          The mesh.
!     \item[Array]
!          The grid array.
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       type(ESMF_VM)        :: vm
       logical :: isMemFreed

       ! Logic to determine if valid optional args are passed.  

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Make sure the srcMesh has its internal bits in place
       call ESMF_MeshGet(Mesh, isMemFreed=isMemFreed, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (isMemFreed)  then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- Mesh has had its coordinate and connectivity info freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif

       ! Call through to the C++ object that does the work
       call c_ESMC_xgrid_getfrac(Grid, Mesh, Array, staggerLoc, &
                                  localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      rc = ESMF_SUCCESS

      end subroutine ESMF_XGridGetFracIntGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetFrac2IntGrid"
!BOPI
! !IROUTINE: ESMF_XGridGetFrac2Int - Gets the frac2 of grid cells after a regrid from a Mesh

! !INTERFACE:
      subroutine ESMF_XGridGetFrac2IntGrid(Grid, Mesh, Array, staggerLoc, &
                 rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in)            :: Grid
      type(ESMF_Mesh), intent(inout)         :: Mesh
      type(ESMF_Array), intent(inout)        :: Array
      type(ESMF_StaggerLoc), intent(in)      :: staggerLoc
      integer, intent(out), optional         :: rc
!
! !DESCRIPTION:
!     The arguments are:
!     \begin{description}
!     \item[Mesh]
!          The mesh.
!     \item[Array]
!          The grid array.
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       type(ESMF_VM)        :: vm
       logical :: isMemFreed

       ! Logic to determine if valid optional args are passed.  

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Make sure the srcMesh has its internal bits in place
       call ESMF_MeshGet(Mesh, isMemFreed=isMemFreed, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (isMemFreed)  then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- Mesh has had its coordinate and connectivity info freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif

       ! Call through to the C++ object that does the work
       call c_ESMC_xgrid_getfrac2(Grid, Mesh, Array, staggerLoc, &
                                  localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      rc = ESMF_SUCCESS

      end subroutine ESMF_XGridGetFrac2IntGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetFracIntMesh"
!BOPI
! !IROUTINE: ESMF_XGridGetFracInt - Gets the frac of grid cells after a regrid from a Mesh

! !INTERFACE:
      subroutine ESMF_XGridGetFracIntMesh(Mesh, Array, meshloc, &
                 rc)
!
! !ARGUMENTS:
      type(ESMF_Mesh), intent(inout)         :: Mesh
      type(ESMF_Array), intent(inout)        :: Array
      type(ESMF_MeshLoc), intent(in)         :: meshloc
      integer, intent(out), optional         :: rc
!
! !DESCRIPTION:
!     The arguments are:
!     \begin{description}
!     \item[Mesh]
!          The mesh.
!     \item[Array]
!          The grid array.
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       real(ESMF_KIND_R8), pointer :: frac(:)
       type(ESMF_VM)        :: vm
       logical :: isMemFreed

       ! Logic to determine if valid optional args are passed.  

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Make sure the srcMesh has its internal bits in place
       call ESMF_MeshGet(Mesh, isMemFreed=isMemFreed, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (isMemFreed)  then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- Mesh has had its coordinate and connectivity info freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif

       call ESMF_ArrayGet(Array, localDe=0, farrayPtr=frac, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Call through to the C++ object that does the work
       call ESMF_MeshGetElemFrac(Mesh, frac, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      rc = ESMF_SUCCESS

      end subroutine ESMF_XGridGetFracIntMesh

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetFrac2IntMesh"
!BOPI
! !IROUTINE: ESMF_XGridGetFrac2Int - Gets the frac2 of grid cells after a regrid from a Mesh

! !INTERFACE:
      subroutine ESMF_XGridGetFrac2IntMesh(Mesh, Array, meshloc, rc)
!
! !ARGUMENTS:
      type(ESMF_Mesh), intent(inout)         :: Mesh
      type(ESMF_Array), intent(inout)        :: Array
      type(ESMF_MeshLoc), intent(in)         :: meshloc
      integer, intent(out), optional         :: rc
!
! !DESCRIPTION:
!     The arguments are:
!     \begin{description}
!     \item[Mesh]
!          The mesh.
!     \item[Array]
!          The grid array.
!     \item[{rc}]
!          Return code.
!     \end{description}
!EOPI
       integer :: localrc
       type(ESMF_VM)        :: vm
       logical :: isMemFreed
       real(ESMF_KIND_R8), pointer :: frac(:)

       ! Logic to determine if valid optional args are passed.  

       ! Initialize return code; assume failure until success is certain
       localrc = ESMF_RC_NOT_IMPL
       if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Make sure the srcMesh has its internal bits in place
       call ESMF_MeshGet(Mesh, isMemFreed=isMemFreed, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       if (isMemFreed)  then
           call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- Mesh has had its coordinate and connectivity info freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif

       call ESMF_ArrayGet(Array, localDe=0, farrayPtr=frac, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

       ! Call through to the C++ object that does the work
       call ESMF_MeshGetElemFrac2(Mesh, frac, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

      rc = ESMF_SUCCESS

      end subroutine ESMF_XGridGetFrac2IntMesh

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridConstructBaseObj()"
!BOPI
! !IROUTINE:  ESMF_XGridConstructBaseObj - Allocate xgtype pointer and its base object

! !INTERFACE:
subroutine ESMF_XGridConstructBaseObj(xgtype, name, rc)

!
! !ARGUMENTS:
    type(ESMF_XGridType),                pointer :: xgtype
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
    if (ESMF_LogFoundAllocError(localrc, &
        msg="- Allocating XGrid Type", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_XGridInitialize(xgtype, rc=localrc) 
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_BaseCreate(xgtype%base, "XGrid", name, 0, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridConstructBaseObj
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridIsCreated()"
!BOP
! !IROUTINE: ESMF_XGridIsCreated - Check whether a XGrid object has been created

! !INTERFACE:
  function ESMF_XGridIsCreated(xgrid, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_XGridIsCreated
!
! !ARGUMENTS:
    type(ESMF_XGrid), intent(in)            :: xgrid
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt xgrid} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[xgrid]
!     {\tt ESMF\_XGrid} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_XGridIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_XGridGetInit(xgrid)==ESMF_INIT_CREATED) &
      ESMF_XGridIsCreated = .true.
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridDestroy"
!BOP
! !IROUTINE: ESMF_XGridDestroy - Release resources associated with an XGrid
! !INTERFACE:

  subroutine ESMF_XGridDestroy(xgrid, keywordenforcer, &
    rc)
!
! !ARGUMENTS:
    type(ESMF_XGrid), intent(inout)          :: xgrid       
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,          intent(out),  optional :: rc     
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
! Destroys an {\tt ESMF\_XGrid}, releasing the resources associated
! with the object.
! 
! The arguments are:
! \begin{description}
! \item [xgrid]
!       {\tt ESMF\_XGrid} object.
! \item [{[rc]}] 
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    ! Local variables
    integer :: localrc, i
    type(ESMF_Status) :: xgridstatus

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variables
    ESMF_INIT_CHECK_DEEP(ESMF_XGridGetInit,xgrid,rc)

    if (.not. associated(xgrid%xgtypep)) then 
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
        msg="Uninitialized or already destroyed XGrid: xgtypep unassociated", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif 

    if(xgrid%xgtypep%storeOverlay) then
      call ESMF_MeshDestroy(xgrid%xgtypep%mesh, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call ESMF_DistGridDestroy(xgrid%xgtypep%distgridM, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Destruct all xgrid internals and then free xgrid memory.
    call ESMF_BaseGetStatus(xgrid%xgtypep%base, xgridstatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (xgridstatus .eq. ESMF_STATUS_READY) then

      if((xgrid%xgtypep%is_proxy)) then

        if(associated(xgrid%xgtypep%sideA)) then
          do i = 1, size(xgrid%xgtypep%sideA,1)
            call ESMF_XGridGeomBaseDestroy(xgrid%xgtypep%sideA(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
        if(associated(xgrid%xgtypep%sideB)) then
          do i = 1, size(xgrid%xgtypep%sideB,1)
            call ESMF_XGridGeomBaseDestroy(xgrid%xgtypep%sideB(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif

        if(associated(xgrid%xgtypep%distgridA)) then
          do i = 1, size(xgrid%xgtypep%distgridA,1)
            call ESMF_DistGridDestroy(xgrid%xgtypep%distgridA(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
        if(associated(xgrid%xgtypep%distgridB)) then
          do i = 1, size(xgrid%xgtypep%distgridB,1)
            call ESMF_DistGridDestroy(xgrid%xgtypep%distgridB(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif

      endif ! proxy

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

      ! destroy all the fraction arrays for Xgrid created online
      if(xgrid%xgtypep%online == 1) then
        if(associated(xgrid%xgtypep%fracA2X)) then
          do i = 1, size(xgrid%xgtypep%fracA2X,1)
            call ESMF_ArrayDestroy(xgrid%xgtypep%fracA2X(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
        if(associated(xgrid%xgtypep%fracB2X)) then
          do i = 1, size(xgrid%xgtypep%fracB2X,1)
            call ESMF_ArrayDestroy(xgrid%xgtypep%fracB2X(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
        if(associated(xgrid%xgtypep%fracX2A)) then
          do i = 1, size(xgrid%xgtypep%fracX2A,1)
            call ESMF_ArrayDestroy(xgrid%xgtypep%fracX2A(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
        if(associated(xgrid%xgtypep%fracX2B)) then
          do i = 1, size(xgrid%xgtypep%fracX2B,1)
            call ESMF_ArrayDestroy(xgrid%xgtypep%fracX2B(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
        call ESMF_ArrayDestroy(xgrid%xgtypep%fracX, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
        if(associated(xgrid%xgtypep%frac2A)) then
          do i = 1, size(xgrid%xgtypep%frac2A,1)
            call ESMF_ArrayDestroy(xgrid%xgtypep%frac2A(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
        if(associated(xgrid%xgtypep%frac2B)) then
          do i = 1, size(xgrid%xgtypep%frac2B,1)
            call ESMF_ArrayDestroy(xgrid%xgtypep%frac2B(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
      endif ! online

    endif ! valid status

    ! mark object invalid
    call ESMF_BaseSetStatus(xgrid%xgtypep%base, ESMF_STATUS_INVALID, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_INIT_SET_DELETED(xgrid)

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_XGridDestroy

end module ESMF_XGridCreateMod
