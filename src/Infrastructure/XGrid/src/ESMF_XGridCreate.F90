! $Id: ESMF_XGridCreate.F90,v 1.50 2012/03/19 22:24:41 svasquez Exp $
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
  sequence
    type(ESMF_Pointer) :: this
  end type
  type ESMF_FracPtr
  sequence
    real(ESMF_KIND_R8), pointer :: fraclist(:)
  end type

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
    '$Id: ESMF_XGridCreate.F90,v 1.50 2012/03/19 22:24:41 svasquez Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

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
! !IROUTINE:  ESMF_XGridCreate - Create an XGrid from user input

! !INTERFACE:

function ESMF_XGridCreate(sideA, sideB, keywordEnforcer, &
    sideAToXGridScheme, sideBToXGridScheme, &
    sideAPriority,      sideBPriority, &
    sideAMaskValues,    sideBMaskValues, &
    storeOverlay, &
    offline, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, &
    area, centroid, &
    name, rc)
!
! !RETURN VALUE:
  type(ESMF_XGrid)              :: ESMF_XGridCreate
!
! !ARGUMENTS:
  type(ESMF_Grid), intent(in)                 :: sideA(:), sideB(:)
  type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
  integer,              intent(in), optional :: sideAToXGridScheme
  integer,              intent(in), optional :: sideBToXGridScheme
  integer,              intent(in), optional :: sideAPriority(:)
  integer,              intent(in), optional :: sideBPriority(:)
  integer(ESMF_KIND_I4),intent(in), optional :: sideAMaskValues(:)
  integer(ESMF_KIND_I4),intent(in), optional :: sideBMaskValues(:)
  logical,              intent(in), optional :: storeOverlay
  logical,              intent(in), optional :: offline
  type(ESMF_XGridSpec), intent(in), optional :: sparseMatA2X(:)
  type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2A(:)
  type(ESMF_XGridSpec), intent(in), optional :: sparseMatB2X(:)
  type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2B(:)
  real(ESMF_KIND_R8),   intent(in), optional :: area(:)
  real(ESMF_KIND_R8),   intent(in), optional :: centroid(:,:)
  character(len=*),     intent(in), optional :: name
  integer,              intent(out),optional :: rc

!
! !DESCRIPTION:
!      Create an XGrid from user supplied input: the list of Grids on side A and side B, 
!  and other optional arguments. By default, XGrid is created online with user supplied
!  list of Grids, i.e. the sparse matrix multiply coefficients are internally computed.
!  Grid objects in {\tt sideA} and {\tt sideB} arguments must have coordinates defined for
!  the corners of a Grid cell. XGrid created online can be potentially memory expensive, one
!  way to save memory is to destroy XGrid after necesary routehandles are computed from
!  {\tt ESMF\_FieldRegridStore} method.
! 
!  User can also turn on offline creation of XGrid in which case, sparse matrix matmul
!  coefficients need to be supplied by the user. 
!
!  Masking is not fully tested and is not supported right now. Specifying {\tt sideAMaskValues}
!  or {\tt sideBMaskValues} will result in an error returned from this method. 
!
!  It is erroneous to specify identical Grid object in {\tt sideA} and
!  {\tt sideA} arguments.
!
!  This call is {\em collective} across the current VM.
!
!     The arguments are:
!     \begin{description}
!     \item [sideA]
!           Parametric 2D Grids on side A, for example, these Grids can be either Cartesian 2D or Spherical.
!     \item [sideB]
!           Parametric 2D Grids on side B, for example, these Grids can be either Cartesian 2D or Spherical.
!     \item [{[sideAToXGridScheme]}]
!           Specify the geometry and unit of metric of the Grids on A side in relation to XGrid created. 
!           Possible values are
!                            {\tt ESMF\_REGRID\_SCHEME\_FULL3D},
!                            {\tt ESMF\_REGRID\_SCHEME\_NATIVE},
!                            {\tt ESMF\_REGRID\_SCHEME\_REGION3D},
!                            {\tt ESMF\_REGRID\_SCHEME\_FULLTOREG3D},
!                            {\tt ESMF\_REGRID\_SCHEME\_REGTOFULL3D}.
!     \item [{[sideBToXGridScheme]}]
!           Specify the geometry and unit of metric of the Grids on B side in relation to XGrid created. 
!           Possible values are
!                            {\tt ESMF\_REGRID\_SCHEME\_FULL3D},
!                            {\tt ESMF\_REGRID\_SCHEME\_NATIVE},
!                            {\tt ESMF\_REGRID\_SCHEME\_REGION3D},
!                            {\tt ESMF\_REGRID\_SCHEME\_FULLTOREG3D},
!                            {\tt ESMF\_REGRID\_SCHEME\_REGTOFULL3D}.
!     \item [{[sideAPriority]}]
!           Priority array of Grids on {\tt sideA} during overlay generation.
!           The priority arrays describe the priorities of Grids at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Grid of the
!           highest priority to the lowest priority.
!     \item [{[sideBPriority]}]
!           Priority of Grids on {\tt sideB} during overlay generation
!           The priority arrays describe the priorities of Grids at the overlapping region.
!           Flux contributions at the overlapping region are computed in the order from the Grid of the
!           highest priority to the lowest priority.
!     \item [{[storeOverlay]}]
!           Setting the {\tt storeOverlay} optional argument to .false. (default) 
!           allows a user to bypass storage of the ESMF\_Mesh used to represent the XGrid.
!           Only a {\tt ESMF\_DistGrid} is stored to allow Field to be built on the XGrid.
!           If the temporary mesh object is of interest, {\tt storeOverlay} can be set to .true.
!           so a user can retrieve it for future use.
!     \item [{[offline]}]
!           Turn on offline XGrid creation and use user supplied Sparse
!           Matrix Multiply coefficients and index pairs, area, and centroid information.
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

  integer                    :: localrc
  logical                    :: l_offline
  
  l_offline = .false.
  if(present(offline)) l_offline = offline

  if(.not. l_offline) then
    ESMF_XGridCreate = ESMF_XGridCreateDefault(sideA, sideB, & 
      sideAToXGridScheme=sideAToXGridScheme, sideBToXGridScheme=sideBToXGridScheme, &
      sideAPriority=sideAPriority, sideBPriority=sideBPriority, &
      sideAMaskValues=sideAMaskValues, sideBMaskValues=sideBMaskValues, &
      storeOverlay=storeOverlay, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  else
    ESMF_XGridCreate = ESMF_XGridCreateOffline(sideA, sideB, &
      sparseMatA2X=sparseMatA2X, sparseMatX2A=sparseMatX2A, &
      sparseMatB2X=sparseMatB2X, sparseMatX2B=sparseMatX2B, &
      area=area, centroid=centroid, name=name, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridCreateDefault()"
!BOPI
! !IROUTINE:  ESMF_XGridCreateDefault - Create an XGrid online from user input

! !INTERFACE:
! ! Private name; call using ESMF_XGridCreate()

function ESMF_XGridCreateDefault(sideA, sideB, &
    sideAToXGridScheme, sideBToXGridScheme, &
    sideAPriority, sideBPriority, &
    sideAMaskValues, sideBMaskValues, &
    storeOverlay, name, rc)
!
! !RETURN VALUE:
  type(ESMF_XGrid)              :: ESMF_XGridCreateDefault
!
! !ARGUMENTS:
  type(ESMF_Grid),       intent(in)           :: sideA(:), sideB(:)
  integer,               intent(in), optional :: sideAToXGridScheme, sideBToXGridScheme
  integer,               intent(in), optional :: sideAPriority(:), sideBPriority(:)
  integer(ESMF_KIND_I4), intent(in), optional :: sideAMaskValues(:), sideBMaskValues(:)
  logical,               intent(in), optional :: storeOverlay
  character(len=*),      intent(in), optional :: name
  integer,               intent(out),optional :: rc


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
!     \item [{[sideAToXGridScheme]}]
!           Specify the geometry and unit of metric of the Grids on A side. 
!     \item [{[sideBToXGridScheme]}]
!           Specify the geometry and unit of metric of the Grids on B side. 
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
    integer                       :: localrc, ngrid_a, ngrid_b, i, j
    real(ESMF_KIND_R8), pointer   :: area(:), centroid(:,:)
    type(ESMF_XGridType), pointer :: xgtype
    type(ESMF_Mesh)               :: meshA, meshB, mesh, tmpmesh
    type(ESMF_Mesh), allocatable  :: meshAt(:), meshBt(:)
    type(ESMF_Pointer)            :: meshp
    type(ESMF_VM)                 :: vm
    integer(ESMF_KIND_I4), pointer:: indicies(:,:)
    real(ESMF_KIND_R8), pointer   :: weights(:), sidemesharea(:), mesharea(:)
    integer                       :: nentries
    type(ESMF_TempWeights)        :: tweights
    integer                       :: AisSphere, BisSphere, XisSphere, BXisSphere
    logical                       :: AisLatLonDeg, BisLatLonDeg, XisLatLonDeg, BXisLatLonDeg
    integer, allocatable          :: l_sideAPriority(:), l_sideBPriority(:)
    integer                       :: l_sideAToXGridScheme, l_sideBToXGridScheme
    integer                       :: l_XGridToSideAScheme, l_XGridToSideBScheme
    integer                       :: l_SideAToSideBScheme
    integer                       :: compute_midmesh
    real(ESMF_KIND_R8)            :: fraction = 1.0
    type(ESMF_FracPtr), allocatable :: sideAfrac(:), sideBfrac(:)
    type(ESMF_INDEX_FLAG)         :: indexflag
    type(ESMF_DistGrid)           :: distgridTmp
    !real(ESMF_KIND_R8), pointer   :: fracFptr(:,:)

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    if(present(sideAMaskValues) .or. present(sideBMaskValues)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_BAD, &
         msg="- masking is not fully tested and supported with XGrid", &
         ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! check init status of input Grids
    ngrid_a = size(sideA, 1)
    ngrid_b = size(sideB, 1)
    if(ngrid_a .le. 0 .or. ngrid_b .le. 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- number of Grids are invalid on one side of the XGrid", &
           ESMF_CONTEXT, rcToReturn=rc) 
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
    nullify(ESMF_XGridCreateDefault%xgtypep)
    call ESMF_XGridConstructBaseObj(xgtype, name, localrc)
    if (ESMF_LogFoundAllocError(localrc, &
                                msg="Constructing xgtype base object ", &
                                ESMF_CONTEXT, rcToReturn=rc)) return

    ! Can only do conservative on 2D right now
    ! Make sure all Grids are 2 dimensional and 
    ! has enough data points on a PET to clip
    do i = 1, ngrid_a
      call checkGrid(sideA(i), ESMF_STAGGERLOC_CORNER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    do i = 1, ngrid_b
      call checkGrid(sideB(i), ESMF_STAGGERLOC_CORNER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! Assign regrid scheme
    if(present(sideAToXGridScheme)) then
      if(sideAToXGridScheme == ESMF_REGRID_SCHEME_FULLTOREG3D) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
          msg="- side A to XGrid scheme cannot be ESMF_REGRID_SCHEME_FULLTOREG3D", & 
          ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif

      l_sideAToXGridScheme = sideAToXGridScheme
      l_XGridToSideAScheme = sideAToXGridScheme
      if(sideAToXGridScheme == ESMF_REGRID_SCHEME_REGTOFULL3D) &
        l_XGridToSideAScheme = ESMF_REGRID_SCHEME_FULLTOREG3D
    else
      l_sideAToXGridScheme = ESMF_REGRID_SCHEME_NATIVE
      l_XGridToSideAScheme = ESMF_REGRID_SCHEME_NATIVE
    endif
    call schemeToGridType(l_sideAToXGridScheme, AisSphere, AisLatLonDeg, &
      XisSphere, XisLatLonDeg)
    if(present(sideBToXGridScheme)) then
      if(sideAToXGridScheme == ESMF_REGRID_SCHEME_FULLTOREG3D) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
          msg="- side B to XGrid scheme cannot be ESMF_REGRID_SCHEME_FULLTOREG3D", & 
          ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
      l_sideBToXGridScheme = sideBToXGridScheme
      l_XGridToSideBScheme = sideBToXGridScheme
      if(sideBToXGridScheme == ESMF_REGRID_SCHEME_REGTOFULL3D) &
        l_XGridToSideBScheme = ESMF_REGRID_SCHEME_FULLTOREG3D
    else
      l_sideBToXGridScheme = ESMF_REGRID_SCHEME_NATIVE
      l_XGridToSideBScheme = ESMF_REGRID_SCHEME_NATIVE
    endif
    call schemeToGridType(l_sideBToXGridScheme, BisSphere, BisLatLonDeg, &
      BXisSphere, BXisLatLonDeg)
    ! make sure A and B scheme means the same thing
    if( (XisSphere /= BXisSphere) .or. (XisLatLonDeg .neqv. BXisLatLonDeg)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
        msg="- side A to XGrid scheme not compatible with side B to XGrid scheme", &
        ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif
    ! figure out the scheme from sideA to sideB for middle mesh generation
    l_SideAToSideBScheme = SideAToSideB(l_sideAToXGridScheme, &
      l_XGridToSideBScheme, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! compute the necessary mesh merge order accounting for the grid priorities
    allocate(l_sideAPriority(ngrid_a), l_sideBPriority(ngrid_b))
    if(present(sideAPriority)) then
      if(size(sideAPriority) /= ngrid_a) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
         msg="- Number of sideA grids doesn't agree with size of sideAPriority", &
         ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
      do i = 1, ngrid_a
        l_sideAPriority(i) = sideAPriority(i)
      enddo
    else
      do i = 1, ngrid_a
        l_sideAPriority(i) = i
      enddo
    endif
    if(present(sideBPriority)) then
      if(size(sideBPriority) /= ngrid_b) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, & 
         msg="- Number of sideB grids doesn't agree with size of sideBPriority", &
         ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
      do i = 1, ngrid_b
        l_sideBPriority(i) = sideBPriority(i)
      enddo
    else
      do i = 1, ngrid_b
        l_sideBPriority(i) = i
      enddo
    endif

    ! allocate the temporary meshes and frac ptr
    allocate(meshAt(ngrid_a), meshBt(ngrid_b), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="- Allocating temporary meshes for Xgrid creation", &
      ESMF_CONTEXT, rcToReturn=rc)) return
    allocate(sideAfrac(ngrid_a), sideBfrac(ngrid_b), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="- Allocating temporary frac ptr type for Xgrid creation", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !TODO: Create the src/dst Mesh, take care of maskValues
    meshAt(1) = ESMF_GridToMesh(sideA(l_sideAPriority(1)), &
      ESMF_STAGGERLOC_CORNER, AisSphere, AisLatLonDeg, &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call c_esmc_meshsetfraction(meshAt(1), fraction, localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    meshA = meshAt(1)
    ! retrieve frac list, for the highest priority mesh, fraction is always 1.
    allocate(sideAfrac(1)%fraclist(meshAt(1)%numownedelements), stat=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    sideAfrac(1)%fraclist=1.0

    do i = 2, ngrid_a
      meshAt(i) = ESMF_GridToMesh(sideA(l_sideAPriority(i)), &
        ESMF_STAGGERLOC_CORNER, AisSphere, AisLatLonDeg, &
        regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call c_esmc_meshsetfraction(meshAt(i), fraction, localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      ! call into mesh merge with priority taken into account
      ! meshAt is truncated(if necessary) and concatenated onto meshA
      ! and result stored in tmpmesh
      call c_esmc_meshmerge(meshA, meshAt(i), tmpmesh, localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if(i .gt. 2) then
        !call ESMF_MeshDestroy(meshA, rc=localrc)
        ! meshA is only a pointer type of mesh at this point, call the C api to destroy it
        call C_ESMC_MeshDestroy(meshA%this, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! retrieve frac list
      allocate(sideAfrac(i)%fraclist(meshAt(i)%numownedelements), stat=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_MeshGetElemFrac(meshAt(i), sideAfrac(i)%fraclist, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      meshA = tmpmesh
    enddo

    meshBt(1) = ESMF_GridToMesh(sideB(l_sideBPriority(1)), &
      ESMF_STAGGERLOC_CORNER, BisSphere, BisLatLonDeg, &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call c_esmc_meshsetfraction(meshBt(1), fraction, localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    meshB = meshBt(1)
    ! retrieve frac list, for the highest priority mesh, fraction is always 1.
    allocate(sideBfrac(1)%fraclist(meshBt(1)%numownedelements), stat=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    sideBfrac(1)%fraclist=1.0

    do i = 2, ngrid_b
      meshBt(i) = ESMF_GridToMesh(sideB(l_sideBPriority(i)), &
        ESMF_STAGGERLOC_CORNER, BisSphere, BisLatLonDeg, &
        regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call c_esmc_meshsetfraction(meshBt(i), fraction, localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      ! call into mesh merge with priority taken into account
      ! meshBt is truncated(if necessary) and concatenated onto meshB 
      ! and result stored in tmpmesh
      call c_esmc_meshmerge(meshB, meshBt(i), tmpmesh, localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if(i .gt. 2) then
        !call ESMF_MeshDestroy(meshB, rc=localrc)
        call C_ESMC_MeshDestroy(meshB%this, localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! retrieve frac list
      allocate(sideBfrac(i)%fraclist(meshBt(i)%numownedelements), stat=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call ESMF_MeshGetElemFrac(meshBt(i), sideBfrac(i)%fraclist, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return

      meshB = tmpmesh
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
    compute_midmesh = 1
    call c_esmc_xgridregrid_create(vm, meshA, meshB, &
      meshp, compute_midmesh, &
      ESMF_REGRIDMETHOD_CONSERVE, &
      l_SideAToSideBScheme, &
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
    call compute_mesharea(mesh, mesharea, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Retrieve fraction Arrays for store use, only for online
    allocate(xgtype%fracA2X(ngrid_a), xgtype%fracB2X(ngrid_b))
    allocate(xgtype%fracX2A(ngrid_a), xgtype%fracX2B(ngrid_b))
    do i = 1, ngrid_A
      call ESMF_GridGet(sideA(i), distgrid=distgridTmp, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
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
    enddo
    do i = 1, ngrid_B
      call ESMF_GridGet(sideB(i), distgrid=distgridTmp, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
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
    enddo

    !allocate(indicies(2,nentries))
    !allocate(weights(nentries))

    !call c_ESMC_Copy_TempWeights_xgrid(tweights, &
    !  indicies(1,1), weights(1))

    !do i = 1, size(indicies,2)
    !   print *, indicies(1,i), '->', indicies(2,i), weights(i), mesharea(i)
    !enddo
    !deallocate(indicies, weights)

    ! TODO: loop through sideA and sideB to compute the interpolation
    ! Compute regrid weights in 4 directions? (2 directions have constant wgt matrix).
    ! This time we don't need the midmesh which would just be the mesh that 
    ! overlaps with sideA or sideB.
    compute_midmesh = 0
    do i = 1, ngrid_a
      call c_esmc_xgridregrid_create(vm, meshAt(i), mesh, &
        tmpmesh, compute_midmesh, &
        ESMF_REGRIDMETHOD_CONSERVE, &
        l_sideAToXGridScheme, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatA2X(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatA2X(i)%factorList(nentries))
      !allocate(xgtype%sparseMatX2A(i)%factorIndexList(2,nentries))
      !allocate(xgtype%sparseMatX2A(i)%factorList(nentries))
      if(nentries .ge. 1) then
        call c_ESMC_Copy_TempWeights_xgrid(tweights, &
        xgtype%sparseMatA2X(i)%factorIndexList(1,1), &
        xgtype%sparseMatA2X(i)%factorList(1))
      
        ! apply src fraction due to creeping, this quantity is typically 1.
        !do j = 1, nentries
        !  xgtype%sparseMatA2X(i)%factorList(j) = xgtype%sparseMatA2X(i)%factorList(j) * &
        !    sideAfrac(i)%fraclist(xgtype%sparseMatA2X(i)%factorIndexList(1,j))
        !enddo

      ! Now the reverse direction

      !  ! an immediate optimization is to use the A side area to simply invert the weight
      !  call compute_mesharea(meshAt(i), sidemesharea, rc=localrc)
      !  if (ESMF_LogFoundError(localrc, &
      !      ESMF_ERR_PASSTHRU, &
      !      ESMF_CONTEXT, rcToReturn=rc)) return
      !  if(size(sideAfrac(i)%fraclist) /= size(sidemesharea)) then
      !    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
      !       msg="- number of area elements not equal to frac elements", &
      !       ESMF_CONTEXT, rcToReturn=rc) 
      !    return
      !  endif
      !  do j = 1, nentries
      !    xgtype%sparseMatX2A(i)%factorIndexList(1,j) = xgtype%sparseMatA2X(i)%factorIndexList(2,j)
      !    xgtype%sparseMatX2A(i)%factorIndexList(2,j) = xgtype%sparseMatA2X(i)%factorIndexList(1,j)
      !    ! If src area is not completely creeped out, invert the destination area weighted weights.
      !    ! TODO: take care of masking here
      !    if(sideAfrac(i)%fraclist(xgtype%sparseMatX2A(i)%factorIndexList(2,j)) .gt. 0.) then
      !      xgtype%sparseMatX2A(i)%factorList(j) = &
      !        mesharea(xgtype%sparseMatX2A(i)%factorIndexList(1,j))&
      !        /(sidemesharea(xgtype%sparseMatX2A(i)%factorIndexList(2,j)) &
      !        * sideAfrac(i)%fraclist(xgtype%sparseMatX2A(i)%factorIndexList(2,j)))
      !    else
      !      xgtype%sparseMatX2A(i)%factorList(j) = 0.0
      !    endif
      !    ! weight cannot exceed 1., renormalize due to fractional clipping
      !    if( xgtype%sparseMatX2A(i)%factorList(j) .gt. 1.0) xgtype%sparseMatX2A(i)%factorList(j) = 1.0
      !  enddo
      !  deallocate(sidemesharea)
      endif
      call ESMF_XGridGetFracInt(sideA(i), mesh=meshAt(i), array=xgtype%fracA2X(i), &
           staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
           ESMF_CONTEXT, rcToReturn=rc)) return
      
      ! Now the reverse direction
      call c_esmc_xgridregrid_create(vm, mesh, meshAt(i), &
        tmpmesh, compute_midmesh, &
        ESMF_REGRIDMETHOD_CONSERVE, &
        l_XGridToSideAScheme, &
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
      call ESMF_XGridGetFracInt(sideA(i), mesh=meshAt(i), array=xgtype%fracX2A(i), &
           staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
           ESMF_CONTEXT, rcToReturn=rc)) return

      deallocate(sideAfrac(i)%fraclist)
    enddo

    ! now do the B side
    do i = 1, ngrid_b
      call c_esmc_xgridregrid_create(vm, meshBt(i), mesh, &
        tmpmesh, compute_midmesh, &
        ESMF_REGRIDMETHOD_CONSERVE, &
        l_sideBToXGridScheme, &
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
      
        ! apply src fraction due to creeping, this quantity is typically 1.
        !do j = 1, nentries
        !  xgtype%sparseMatB2X(i)%factorList(j) = xgtype%sparseMatB2X(i)%factorList(j) * &
        !    sideBfrac(i)%fraclist(xgtype%sparseMatB2X(i)%factorIndexList(1,j))
        !enddo
      endif
      call ESMF_XGridGetFracInt(sideB(i), mesh=meshBt(i), array=xgtype%fracB2X(i), &
           staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
           ESMF_CONTEXT, rcToReturn=rc)) return
    
      ! Now the reverse direction
      call c_esmc_xgridregrid_create(vm, mesh, meshBt(i), &
        tmpmesh, compute_midmesh, &
        ESMF_REGRIDMETHOD_CONSERVE, &
        l_sideBToXGridScheme, &
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
      call ESMF_XGridGetFracInt(sideB(i), mesh=meshBt(i), array=xgtype%fracX2B(i), &
           staggerloc=ESMF_STAGGERLOC_CORNER, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
           ESMF_CONTEXT, rcToReturn=rc)) return

      !call ESMF_ArrayGet(xgtype%fracX2B(i), localDe=0, farrayPtr=fracFptr, rc=localrc)
      !if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, & 
      !     ESMF_CONTEXT, rcToReturn=rc)) return

      deallocate(sideBfrac(i)%fraclist)
    enddo

    deallocate(sideAfrac, sideBfrac)

    xgtype%storeOverlay = .false.
    if(present(storeOverlay)) then
      if(storeOverlay) xgtype%storeOverlay = .true.
    endif
    
    ! call into offline xgrid create with the xgrid specs
    call ESMF_XGridConstruct(xgtype, sideA, sideB, area=mesharea, &
      sparseMatA2X=xgtype%sparseMatA2X, sparseMatX2A=xgtype%sparseMatX2A, &
      sparseMatB2X=xgtype%sparseMatB2X, sparseMatX2B=xgtype%sparseMatX2B, &
      offline=.false., &
      mesh=mesh, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Should we store routehandle instead for online generation of XGrid?
    ! Routehandle can't (shouldn't) be reconciled. Storing SMM parameters
    ! allows XGrid to be redistributed.

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

    deallocate(l_sideAPriority, l_sideBPriority)

    do i = 1, ngrid_a
      call ESMF_MeshDestroy(meshAt(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    do i = 1, ngrid_b
      call ESMF_MeshDestroy(meshBt(i), rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    deallocate(meshAt, meshBt)
    deallocate(mesharea)

    ! Finalize XGrid Creation
    xgtype%online = 1
    xgtype%status = ESMF_STATUS_READY
    ESMF_XGridCreateDefault%xgtypep => xgtype 
    ESMF_INIT_SET_CREATED(ESMF_XGridCreateDefault)

    !call ESMF_XGridValidate(ESMF_XGridCreateDefault, rc=localrc)
    !if (ESMF_LogFoundError(localrc, &
    !    ESMF_ERR_PASSTHRU, &
    !    ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridCreateDefault


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridCreateOffline()"
!BOPI
! !IROUTINE:  ESMF_XGridCreateOfflineCreate an XGrid from raw input parameters

! !INTERFACE:

function ESMF_XGridCreateOffline(sideA, sideB, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, &
    area, centroid, &
    name, &
    rc) 

!
! !RETURN VALUE:
    type(ESMF_XGrid) :: ESMF_XGridCreateOffline
!
! !ARGUMENTS:
type(ESMF_Grid), intent(in)                :: sideA(:), sideB(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatA2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2A(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatB2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2B(:)
real(ESMF_KIND_R8), intent(in), optional   :: area(:)
real(ESMF_KIND_R8), intent(in), optional   :: centroid(:,:)
character (len=*), intent(in), optional    :: name
integer, intent(out), optional             :: rc 

!
! !DESCRIPTION:
!      Create an XGrid directly from raw input parameters
!
!     The arguments are:
!     \begin{description}
!     \item [sideA]
!           2D Grids on side A.
!     \item [sideB]
!           2D Grids on side B.
!     \item [offline]
!           Indicate this is offline XGrid creation.
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
!EOPI

    integer :: localrc, ngrid_a, ngrid_b
    integer :: i
    type(ESMF_XGridType), pointer :: xgtype

    ! clearly, srcIdxList should be 1D, but what about distgridM_idxlist??
    !integer, allocatable :: srcIdxList(:), distgridM_idxlist(:)


    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input Grids
    ngrid_a = size(sideA, 1)
    ngrid_b = size(sideB, 1)
    if(ngrid_a .le. 0 .or. ngrid_b .le. 0) then
        call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
           msg="- number of Grids are invalid on one side of the XGrid", &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif
    do i = 1, ngrid_a
        ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideA(i),rc)
    enddo
    do i = 1, ngrid_b
        ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,sideB(i),rc)
    enddo

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
    nullify(ESMF_XGridCreateOffline%xgtypep)
    call ESMF_XGridConstructBaseObj(xgtype, name, localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="Constructing xgtype base object ", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_XGridConstruct(xgtype, sideA, sideB, area=area, centroid=centroid, &
      sparseMatA2X=sparseMatA2X, sparseMatX2A=sparseMatX2A, &
      sparseMatB2X=sparseMatB2X, sparseMatX2B=sparseMatX2B, offline=.true., rc=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="Constructing xgtype object ", &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Finalize XGrid Creation
    xgtype%online = 0
    xgtype%status = ESMF_STATUS_READY
    ESMF_XGridCreateOffline%xgtypep => xgtype 
    ESMF_INIT_SET_CREATED(ESMF_XGridCreateOffline)

    call ESMF_XGridValidate(ESMF_XGridCreateOffline, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridCreateOffline

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridConstruct()"
!BOPI
! !IROUTINE:  ESMF_XGridConstruct - Construct XGrid from input

! !INTERFACE:
subroutine ESMF_XGridConstruct(xgtype, sideA, sideB, area, centroid, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, offline, &
    mesh, rc)
!
! !ARGUMENTS:
type(ESMF_XGridType), intent(inout)        :: xgtype
type(ESMF_Grid), intent(in)                :: sideA(:), sideB(:)
real(ESMF_KIND_R8), intent(in), optional   :: area(:)
real(ESMF_KIND_R8), intent(in), optional   :: centroid(:,:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatA2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2A(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatB2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2B(:)
logical, intent(in), optional              :: offline
type(ESMF_Mesh), intent(inout), optional   :: mesh
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
!     \item [{[area]}]
!           area of the xgrid cells.
!     \item [{[centroid]}]
!           coordinates at the area weighted center of the xgrid cells.
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
  integer :: ndim, ncells, i
  logical :: l_offline
  real(ESMF_KIND_R8), pointer :: xgrid_frac(:)

  localrc = ESMF_SUCCESS

  ! Initialize return code   
  if(present(rc)) rc = ESMF_RC_NOT_IMPL
  l_offline = .true.
  if(present(offline)) l_offline = offline

  ngrid_a = size(sideA, 1)
  ngrid_b = size(sideB, 1)
  ! copy the Grids
  allocate(xgtype%sideA(ngrid_a), xgtype%sideB(ngrid_b), stat=localrc)
  if (ESMF_LogFoundAllocError(localrc, &
      msg="- Allocating xgtype%grids ", &
      ESMF_CONTEXT, rcToReturn=rc)) return
  xgtype%sideA = sideA
  xgtype%sideB = sideB

  ! copy area and centroid
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
    call ESMF_GridGet(xgtype%sideA(i), distgrid=xgtype%distgridA(i), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  enddo
  do i = 1, size(xgtype%sideB)
    call ESMF_GridGet(xgtype%sideB(i), distgrid=xgtype%distgridB(i), rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
  enddo

  if(present(rc)) rc = ESMF_SUCCESS

end subroutine

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
subroutine ESMF_XGridDG(grid, distgrid, factorIndexList, dim, rc)
!
! !DESCRIPTION:
!      Compute distgrid on the XGrid locally matching the grid on the same set of PETs;
!     This allows local SMM optimization. To be completed.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           grid object spanning a set of PETs.
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

    type(ESMF_Grid), intent(in)                 :: grid
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
#undef  ESMF_METHOD
#define ESMF_METHOD "schemeToGridType()"
!BOPI
! !IROUTINE:  schemeToGridType - determine src and dst Grid types

! !INTERFACE:
subroutine schemeToGridType(scheme, srcIsSphere, srcIsLatLonDeg, &
  dstIsSphere, dstIsLatLonDeg, rc)

!
! !ARGUMENTS:
  integer, intent(in)              :: scheme
  integer, intent(out)             :: srcIsSphere
  logical, intent(out)             :: srcIsLatLonDeg
  integer, intent(out)             :: dstIsSphere
  logical, intent(out)             :: dstIsLatLonDeg
  integer, intent(out), optional   :: rc
!
! !DESCRIPTION:
!      Determine src and dst Grid types
!
!     The arguments are:
!     \begin{description}
!     \item [scheme]
!           regridding scheme
!     \item [srcIsSphere]
!           is src Grid a sphere.
!     \item [srcIsLatLonDeg]
!           is src Grid a lat lon grid with coordinates in deg.
!     \item [dstIsSphere]
!           is dst Grid a sphere.
!     \item [dstIsLatLonDeg]
!           is dst Grid a lat lon grid with coordinates in deg.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

  if(present(rc)) rc = ESMF_SUCCESS

  ! Set interpretation of grid based on regridScheme
  if (Scheme .eq. ESMF_REGRID_SCHEME_FULL3D) then
     srcIsSphere = 1
     srcIsLatLonDeg=.true.
     dstIsSphere = 1
     dstIsLatLonDeg=.true.
  else if (Scheme .eq. ESMF_REGRID_SCHEME_FULLTOREG3D) then
     srcIsSphere = 1
     srcIsLatLonDeg=.true.
     dstIsSphere = 0
     dstIsLatLonDeg=.true.
  else if (Scheme .eq. ESMF_REGRID_SCHEME_REGTOFULL3D) then
     srcIsSphere = 0
     srcIsLatLonDeg=.true.
     dstIsSphere = 1
     dstIsLatLonDeg=.true.
  else if (Scheme .eq. ESMF_REGRID_SCHEME_REGION3D) then
     srcIsSphere = 0
     srcIsLatLonDeg=.true.
     dstIsSphere = 0
     dstIsLatLonDeg=.true.
  else if (Scheme .eq. ESMF_REGRID_SCHEME_DCON3D) then
     srcIsSphere = 0
     srcIsLatLonDeg=.true.
     dstIsSphere = 0
     dstIsLatLonDeg=.true.
  else if (Scheme .eq. ESMF_REGRID_SCHEME_DCON3DWPOLE) then
     srcIsSphere = 0
     srcIsLatLonDeg=.true.
     dstIsSphere = 0
     dstIsLatLonDeg=.true.
  else  
     srcIsSphere = 0
     srcIsLatLonDeg=.false.
     dstIsSphere = 0
     dstIsLatLonDeg=.false.
  endif
  return
end subroutine schemeToGridType

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "SideAToSideB()"
!BOPI
! !IROUTINE:  SideAToSideB - compute regrid scheme from sideA to sideB

! !INTERFACE:
function SideAToSideB(AtoX, XtoB, rc)
!
! !RETURN VALUE:
  integer                       :: SideAToSideB
!
! !ARGUMENTS:
  integer, intent(in)           :: AtoX
  integer, intent(in)           :: XtoB
  integer, intent(out), optional:: rc
!
! !DESCRIPTION:
!      Compute regrid scheme from sideA to sideB.
!
!     The arguments are:
!     \begin{description}
!     \item [AtoX]
!           regrid scheme from sideA to XGrid
!     \item [XtoB]
!           regrid scheme from XGrid to sideB
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

  logical                       :: match

  match = .false.
  if(present(rc)) rc = ESMF_SUCCESS
  !print *, AtoX, XtoB

  if(AtoX == ESMF_REGRID_SCHEME_NATIVE .and. XtoB == ESMF_REGRID_SCHEME_NATIVE) then
    SideAToSideB = ESMF_REGRID_SCHEME_NATIVE
    match = .true.
  endif
  if(AtoX == ESMF_REGRID_SCHEME_FULL3D .and. XtoB == ESMF_REGRID_SCHEME_FULL3D) then
    SideAToSideB = ESMF_REGRID_SCHEME_FULL3D
    match = .true.
  endif
  if(AtoX == ESMF_REGRID_SCHEME_REGTOFULL3D .and. XtoB == ESMF_REGRID_SCHEME_FULL3D) then
    SideAToSideB = ESMF_REGRID_SCHEME_REGTOFULL3D
    match = .true.
  endif
  if(AtoX == ESMF_REGRID_SCHEME_REGION3D .and. XtoB == ESMF_REGRID_SCHEME_REGION3D) then
    SideAToSideB = ESMF_REGRID_SCHEME_REGION3D
    match = .true.
  endif
  if(AtoX == ESMF_REGRID_SCHEME_DCON3D .and. XtoB == ESMF_REGRID_SCHEME_DCON3D) then
    SideAToSideB = ESMF_REGRID_SCHEME_DCON3D
    match = .true.
  endif
  if(AtoX == ESMF_REGRID_SCHEME_DCON3DWPOLE .and. &
      XtoB == ESMF_REGRID_SCHEME_DCON3DWPOLE) then
    SideAToSideB = ESMF_REGRID_SCHEME_DCON3DWPOLE
    match = .true.
  endif
  if(.not. match) then
    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
       msg="- Incorrect regrid schemes for XGrid generation", &
       ESMF_CONTEXT, rcToReturn=rc) 
    return
  endif        
  if(present(rc)) rc = ESMF_SUCCESS

end function SideAToSideB
    
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
#define ESMF_METHOD "compute_mesharea()"
!BOPI
! !IROUTINE:  compute_mesharea - Compute mesh Area

! !INTERFACE:
subroutine compute_mesharea(mesh, area, rc)

!
! !ARGUMENTS:
type(ESMF_Mesh), intent(inout)             :: mesh
real(ESMF_KIND_R8), pointer                :: area(:)
integer, intent(out), optional             :: rc
!
! !DESCRIPTION:
!      Allocate internal SMM parameters and copy from src.
!
!     The arguments are:
!     \begin{description}
!     \item [mesh]
!           the {\tt ESMF\_Mesh} object.
!     \item [area]
!           the area of the mesh
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

logical :: hasSplitElem
integer :: localrc, localElemCount


  ! Find out if elements are split
  call ESMF_MeshGetElemSplit(mesh, hasSplitElem=hasSplitElem, rc=localrc)
  if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
  endif

  ! Get area depending on split elements
  if (hasSplitElem) then
     ! Get local size of mesh areas before split
     call ESMF_MeshGetElemSplit(mesh, origElemCount=localElemCount, &
            rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
    endif

    ! allocate space for areas
    allocate(Area(localElemCount))

    ! Get local Areas
    call ESMF_MeshGetOrigElemArea(mesh, areaList=Area, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
     endif
  else
     ! Get local size of mesh areas
     call ESMF_MeshGet(mesh, numOwnedElements=localElemCount, &
            rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
    endif

    ! allocate space for areas
    allocate(Area(localElemCount))

    ! Get local Areas
    call ESMF_MeshGetElemArea(mesh, areaList=Area, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=localrc
      return
     endif
  endif

  if(present(rc)) rc = ESMF_SUCCESS

end subroutine compute_mesharea

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridGetFracInt"
!BOPI
! !IROUTINE: ESMF_XGridGetFracInt - Gets the frac of grid cells after a regrid from a Mesh

! !INTERFACE:
      subroutine ESMF_XGridGetFracInt(Grid, Mesh, Array, staggerLoc, &
                 rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in)         :: Grid
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

      end subroutine ESMF_XGridGetFracInt

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

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridDestroy"
!BOP
! !IROUTINE: ESMF_XGridDestroy - Release resources associated with an XGrid
! !INTERFACE:

  subroutine ESMF_XGridDestroy(xgrid, keywordenforcer, rc)
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
            call ESMF_GridDestroy(xgrid%xgtypep%sideA(i), rc=localrc)
            if (ESMF_LogFoundError(localrc, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
          enddo
        endif
        if(associated(xgrid%xgtypep%sideB)) then
          do i = 1, size(xgrid%xgtypep%sideB,1)
            call ESMF_GridDestroy(xgrid%xgtypep%sideB(i), rc=localrc)
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
