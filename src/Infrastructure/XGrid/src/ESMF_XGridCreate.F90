! $Id: ESMF_XGridCreate.F90,v 1.22 2011/04/26 19:50:45 feiliu Exp $
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
  use ESMF_GridUtilMod
  use ESMF_MeshMod
  use ESMF_StaggerLocMod
  use ESMF_XGridMod
  use ESMF_RegridMod
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
    '$Id: ESMF_XGridCreate.F90,v 1.22 2011/04/26 19:50:45 feiliu Exp $'

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
        module procedure ESMF_XGridCreateDefault


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
#define ESMF_METHOD "ESMF_XGridCreateDefault()"
!BOPI
! !IROUTINE:  ESMF_XGridCreateDefault - Create an XGrid online from user input

! !INTERFACE:
! ! Private name; call using ESMF_XGridCreate()

function ESMF_XGridCreateDefault(sideA, sideB, sideAScheme, sideBScheme, sideAPriority, &
sideBPriority, storeOverlay, name, rc)

!
! !ARGUMENTS:
type(ESMF_Grid), intent(in)            :: sideA(:), sideB(:)
integer, intent(in)                    :: sideAScheme, sideBScheme
integer, intent(in), optional          :: sideAPriority(:), sideBPriority(:)
logical, intent(in), optional          :: storeOverlay
character(len=*), intent(in), optional :: name
integer, intent(out), optional         :: rc

!
! !RETURN VALUE:
  type(ESMF_XGrid)              :: ESMF_XGridCreateDefault

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
!     \item [sideAScheme]
!           Specify the geometry and unit of metric of the Grids on A side. 
!           Possible values are ESMF_XGRID_SCHEME_SPHERELATLONDEG, 
!           ESMF_XGRID_SCHEME_CARTESIAN2D 
!     \item [sideBScheme]
!           Specify the geometry and unit of metric of the Grids on B side. 
!           Possible values are ESMF_XGRID_SCHEME_SPHERELATLONDEG, 
!           ESMF_XGRID_SCHEME_CARTESIAN2D 
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
    type(ESMF_Mesh)               :: meshA, meshB, mesh
    type(ESMF_Mesh)               :: meshAt, meshBt, tmpmesh
    type(ESMF_Pointer)            :: meshp
    type(ESMF_VM)                 :: vm
    integer(ESMF_KIND_I4), pointer:: indicies(:,:)
    real(ESMF_KIND_R8), pointer   :: weights(:), sidemesharea(:), mesharea(:)
    integer                       :: nentries
    type(ESMF_TempWeights)        :: tweights
    integer                       :: gridDimCount, AisSphere, BisSphere
    logical                       :: AisLatLonDeg, BisLatLonDeg
    integer, allocatable          :: l_sideAPriority(:), l_sideBPriority(:)
    integer                       :: compute_midmesh

    ! Initialize
    localrc = ESMF_RC_NOT_IMPL

    ! Initialize return code   
    if(present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check init status of input Grids
    ngrid_a = size(sideA, 1)
    ngrid_b = size(sideB, 1)
    if(ngrid_a .le. 0 .or. ngrid_b .le. 0) then
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
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
    ! Make sure all Grids are 2 dimensional
    ! move all these code into checkgrid
    do i = 1, ngrid_a
      call ESMF_GridGet(grid=sideA(i), &
             dimCount=gridDimCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if (gridDimCount .ne. 2) then
           call ESMF_LogSetError(ESMF_RC_ARG_BAD, & 
           msg="- can currently only create xgrid on 2D grids", & 
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
    enddo

    do i = 1, ngrid_b
      call ESMF_GridGet(grid=sideB(i), &
             dimCount=gridDimCount, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      if (gridDimCount .ne. 2) then
           call ESMF_LogSetError(ESMF_RC_ARG_BAD, & 
           msg="- can currently only create xgrid on 2D grids", & 
           ESMF_CONTEXT, rcToReturn=rc) 
        return
      endif
    enddo

    ! Set interpretation of grids based on xgridScheme
    if (sideAScheme .eq. ESMF_XGRID_SCHEME_SPHERELATLONDEG) then
      AisSphere = 1
      AisLatLonDeg = .true.
    else if (sideAScheme .eq. ESMF_XGRID_SCHEME_CARTESIAN2D) then
      AisSphere = 0
      AisLatLonDeg = .false.
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, & 
        msg="- invalid sideA scheme", & 
        ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    if (sideBScheme .eq. ESMF_XGRID_SCHEME_SPHERELATLONDEG) then
      BisSphere = 1
      BisLatLonDeg = .true.
    else if (sideBScheme .eq. ESMF_XGRID_SCHEME_CARTESIAN2D) then
      BisSphere = 0
      BisLatLonDeg = .false.
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, & 
        msg="- invalid sideB scheme", & 
        ESMF_CONTEXT, rcToReturn=rc) 
      return
    endif

    ! compute the necessary mesh merge order accounting for the grid priorities
    allocate(l_sideAPriority(ngrid_a), l_sideBPriority(ngrid_b))
    if(present(sideAPriority)) then
      do i = 1, ngrid_a
        l_sideAPriority(i) = sideAPriority(i)
      enddo
    else
      do i = 1, ngrid_a
        l_sideAPriority(i) = i
      enddo
    endif
    if(present(sideBPriority)) then
      do i = 1, ngrid_b
        l_sideBPriority(i) = sideBPriority(i)
      enddo
    else
      do i = 1, ngrid_b
        l_sideBPriority(i) = i
      enddo
    endif

    !TODO: Create the src/dst Mesh, take care of maskValues
    ! Also assume grids are cartesian and uses native metric
    meshA = ESMF_GridToMesh(sideA(l_sideAPriority(1)), ESMF_STAGGERLOC_CORNER, AisSphere, AisLatLonDeg, &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do i = 2, ngrid_a
      meshAt = ESMF_GridToMesh(sideA(l_sideAPriority(i)), ESMF_STAGGERLOC_CORNER, AisSphere, AisLatLonDeg, &
        regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      ! call into Rendezveus mesh
      !meshA = ESMF_MeshMerge(meshA, meshAt, rc=localrc)
      !if (ESMF_LogFoundError(localrc, &
      !    ESMF_ERR_PASSTHRU, &
      !    ESMF_CONTEXT, rcToReturn=rc)) return
      ! destroy the temporary mesh
      call ESMF_MeshDestroy(meshAt, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    meshB = ESMF_GridToMesh(sideB(l_sideBPriority(1)), ESMF_STAGGERLOC_CORNER, BisSphere, BisLatLonDeg, &
      regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    do i = 2, ngrid_b
      meshBt = ESMF_GridToMesh(sideB(l_sideBPriority(i)), ESMF_STAGGERLOC_CORNER, BisSphere, BisLatLonDeg, &
        regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      ! call into Rendezveus mesh
      !meshB = ESMF_MeshMerge(meshB, meshBt, rc=localrc)
      !if (ESMF_LogFoundError(localrc, &
      !    ESMF_ERR_PASSTHRU, &
      !    ESMF_CONTEXT, rcToReturn=rc)) return
      ! destroy the temporary mesh
      call ESMF_MeshDestroy(meshBt, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo

    ! TODO: compute the interpolation

    allocate(xgtype%sparseMatA2X(ngrid_a), xgtype%sparseMatX2A(ngrid_a), &
             xgtype%sparseMatB2X(ngrid_b), xgtype%sparseMatX2B(ngrid_b), stat=localrc)
    if(localrc /= 0) then
      call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
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
      ESMF_REGRID_METHOD_CONSERVE, &
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

    ! Now we must allocate the F90 pointers and copy weights
    allocate(indicies(2,nentries))
    allocate(weights(nentries))

    call c_ESMC_Copy_TempWeights_xgrid(tweights, indicies(1,1), weights(1))

    !do i = 1, size(indicies,2)
    !   print *, indicies(1,i), '->', indicies(2,i), weights(i), mesharea(i)
    !enddo
    deallocate(indicies, weights)

    ! TODO: loop through sideA and sideB to compute the interpolation
    ! Compute regrid weights in 4 directions? (2 directions have constant wgt matrix)
    ! This time we don't need the middle mesh
    compute_midmesh = 0
    do i = 1, ngrid_a
      meshAt = ESMF_GridToMesh(sideA(l_sideAPriority(i)), ESMF_STAGGERLOC_CORNER, AisSphere, AisLatLonDeg, &
        regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call c_esmc_xgridregrid_create(vm, meshAt, mesh, &
        tmpmesh, compute_midmesh, &
        ESMF_REGRID_METHOD_CONSERVE, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatA2X(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatA2X(i)%factorList(nentries))
      call c_ESMC_Copy_TempWeights_xgrid(tweights, xgtype%sparseMatA2X(i)%factorIndexList(1,1), xgtype%sparseMatA2X(i)%factorList(1))
      ! We can do a bit of checking here because we know the weights must be all 1. in this case
      !print *, 'A2X'
      !do j = 1, size(xgtype%sparseMatA2X(i)%factorIndexList,2)
      !   print *, xgtype%sparseMatA2X(i)%factorIndexList(1,j), '->', xgtype%sparseMatA2X(i)%factorIndexList(2,j), xgtype%sparseMatA2X(i)%factorList(j)
      !enddo
    
      ! Now the reverse direction
      ! an immediate optimization is to use the A side area to simply invert the weight
      !call compute_mesharea(meshAt, sidemesharea, rc=localrc)
      !call compute_mesharea(mesh, mesharea, rc=localrc)
      !deallocate(sidemesharea, mesharea)
      call c_esmc_xgridregrid_create(vm, mesh, meshAt, &
        tmpmesh, compute_midmesh, &
        ESMF_REGRID_METHOD_CONSERVE, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatX2A(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatX2A(i)%factorList(nentries))
      call c_ESMC_Copy_TempWeights_xgrid(tweights, xgtype%sparseMatX2A(i)%factorIndexList(1,1), xgtype%sparseMatX2A(i)%factorList(1))
      !print *, 'X2A'
      !do j = 1, size(xgtype%sparseMatX2A(i)%factorIndexList,2)
      !   print *, xgtype%sparseMatX2A(i)%factorIndexList(1,j), '->', xgtype%sparseMatX2A(i)%factorIndexList(2,j), xgtype%sparseMatX2A(i)%factorList(j)
      !enddo
      call ESMF_MeshDestroy(meshAt, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    ! now do the B side
    do i = 1, ngrid_b
      meshBt = ESMF_GridToMesh(sideB(l_sideBPriority(i)), ESMF_STAGGERLOC_CORNER, BisSphere, BisLatLonDeg, &
        regridConserve=ESMF_REGRID_CONSERVE_ON, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      call c_esmc_xgridregrid_create(vm, meshBt, mesh, &
        tmpmesh, compute_midmesh, &
        ESMF_REGRID_METHOD_CONSERVE, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatB2X(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatB2X(i)%factorList(nentries))
      call c_ESMC_Copy_TempWeights_xgrid(tweights, xgtype%sparseMatB2X(i)%factorIndexList(1,1), xgtype%sparseMatB2X(i)%factorList(1))
      !print *, 'B2X'
      !do j = 1, size(xgtype%sparseMatB2X(i)%factorIndexList,2)
      !   print *, xgtype%sparseMatB2X(i)%factorIndexList(1,j), '->', xgtype%sparseMatB2X(i)%factorIndexList(2,j), xgtype%sparseMatB2X(i)%factorList(j)
      !enddo
      ! TODO:We can do a bit of checking here because we know the weights must be all 1. in this case
    
      ! Now the reverse direction
      call c_esmc_xgridregrid_create(vm, mesh, meshBt, &
        tmpmesh, compute_midmesh, &
        ESMF_REGRID_METHOD_CONSERVE, &
        ESMF_UNMAPPEDACTION_IGNORE, &
        nentries, tweights, &
        localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      allocate(xgtype%sparseMatX2B(i)%factorIndexList(2,nentries))
      allocate(xgtype%sparseMatX2B(i)%factorList(nentries))
      call c_ESMC_Copy_TempWeights_xgrid(tweights, xgtype%sparseMatX2B(i)%factorIndexList(1,1), xgtype%sparseMatX2B(i)%factorList(1))
      !print *, 'X2B'
      !do j = 1, size(xgtype%sparseMatX2B(i)%factorIndexList,2)
      !   print *, xgtype%sparseMatX2B(i)%factorIndexList(1,j), '->', xgtype%sparseMatX2B(i)%factorIndexList(2,j), xgtype%sparseMatX2B(i)%factorList(j)
      !enddo
      call ESMF_MeshDestroy(meshBt, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    enddo
    
    ! call into offline xgrid create with the xgrid specs
    call ESMF_XGridConstruct(xgtype, sideA, sideB, area=mesharea, &
      sparseMatA2X=xgtype%sparseMatA2X, sparseMatX2A=xgtype%sparseMatX2A, &
      sparseMatB2X=xgtype%sparseMatB2X, sparseMatX2B=xgtype%sparseMatX2B, &
      online=.true., &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ! Should we store routehandle instead for online generation of XGrid?
    ! Routehandle can't (shouldn't) be reconciled. Storing SMM parameters
    ! allows XGrid to be redistributed.

    ! store the middle mesh if needed
    ! and clean up temporary memory used
    if(present(storeOverlay)) then
      if(storeOverlay) then
        xgtype%mesh = mesh
      else
        call ESMF_MeshDestroy(mesh, rc=localrc)
        if (ESMF_LogFoundError(localrc, &
            ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    else
      call ESMF_MeshDestroy(mesh, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
          ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    deallocate(l_sideAPriority, l_sideBPriority)

    ! Finalize XGrid Creation
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
#define ESMF_METHOD "ESMF_XGridCreateRaw()"
!BOP
! !IROUTINE:  ESMF_XGridCreate - Create an XGrid from raw input parameters

! !INTERFACE:
! ! Private name; call using ESMF_XGridCreate()

function ESMF_XGridCreateRaw(sideA, sideB, area, centroid, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, &
    name, &
    rc) 

!
! !ARGUMENTS:
type(ESMF_Grid), intent(in)                :: sideA(:), sideB(:)
real(ESMF_KIND_R8), intent(in), optional   :: area(:)
real(ESMF_KIND_R8), intent(in), optional   :: centroid(:,:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatA2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2A(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatB2X(:)
type(ESMF_XGridSpec), intent(in), optional :: sparseMatX2B(:)
character (len=*), intent(in), optional    :: name
integer, intent(out), optional             :: rc 

!
! !RETURN VALUE:
    type(ESMF_XGrid) :: ESMF_XGridCreateRaw

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
!     \item [{[name]}]
!           name of the xgrid object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if the {\tt ESMF\_XGrid} 
!           is created.
!     \end{description}
!
!EOP

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
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
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
    nullify(ESMF_XGridCreateRaw%xgtypep)
    call ESMF_XGridConstructBaseObj(xgtype, name, localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="Constructing xgtype base object ", &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_XGridConstruct(xgtype, sideA, sideB, area, centroid, &
      sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, .false., localrc)
    if (ESMF_LogFoundAllocError(localrc, &
      msg="Constructing xgtype object ", &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Finalize XGrid Creation
    xgtype%status = ESMF_STATUS_READY
    ESMF_XGridCreateRaw%xgtypep => xgtype 
    ESMF_INIT_SET_CREATED(ESMF_XGridCreateRaw)

    call ESMF_XGridValidate(ESMF_XGridCreateRaw, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if(present(rc)) rc = ESMF_SUCCESS

end function ESMF_XGridCreateRaw

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_XGridConstruct()"
!BOPI
! !IROUTINE:  ESMF_XGridConstruct - Construct XGrid from input

! !INTERFACE:
subroutine ESMF_XGridConstruct(xgtype, sideA, sideB, area, centroid, &
    sparseMatA2X, sparseMatX2A, sparseMatB2X, sparseMatX2B, online, rc)
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
logical, intent(in), optional              :: online
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
!     \item [{[online]}]
!           online generation optimization turned on/off (default off)
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} only if successful.
!     \end{description}
!
!EOPI

  integer :: localrc, ngrid_a, ngrid_b
  integer :: ndim, ncells, i
  logical :: l_online

  localrc = ESMF_SUCCESS

  ! Initialize return code   
  if(present(rc)) rc = ESMF_RC_NOT_IMPL
  l_online = .false.
  if(present(online)) l_online = online

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
  if(present(sparseMatA2X) .and. (.not. l_online)) then
      call ESMF_SparseMatca(sparseMatA2X, xgtype%sparseMatA2X, ngrid_a, &
        'sparseMatA2X', rc=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
          msg="- Initializing xgtype%sparseMatX2A ", &
          ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  if(present(sparseMatX2A) .and. (.not. l_online)) then
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

  if(present(sparseMatB2X) .and. (.not. l_online)) then
      call ESMF_SparseMatca(sparseMatB2X, xgtype%sparseMatB2X, ngrid_b, &
        'sparseMatB2X', rc=localrc)
      if (ESMF_LogFoundAllocError(localrc, &
          msg="- Initializing xgtype%sparseMatB2X ", &
          ESMF_CONTEXT, rcToReturn=rc)) return
  endif

  if(present(sparseMatX2B) .and. (.not. l_online)) then
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
  call ESMF_XGridDistGrids(xgtype, rc=localrc)
  if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  if(present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_XGridConstruct

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
    !    call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
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
! !ARGUMENTS:
    type(ESMF_XGridSpec), pointer               :: sparseMat(:)
    integer, intent(in)                         :: dim
    integer, intent(out), optional              :: rc
!
! !RETURN VALUE:
    type(ESMF_DistGrid)                         :: ESMF_XGridDGOverlay

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
              call ESMF_LogSetError(ESMF_RC_ARG_RANK, &
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
        call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
           msg="- number of Grids different from size of sparseMat for "//tag, &
           ESMF_CONTEXT, rcToReturn=rc) 
        return
    endif

    do i = 1, ngrid
        if(.not. associated(sparseMats(i)%factorIndexList) .or. &
           .not. associated(sparseMats(i)%factorList)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
               msg="- sparseMat not initiailzed properly for "//tag, &
               ESMF_CONTEXT, rcToReturn=rc) 
            return
        endif

        if(size(sparseMats(i)%factorIndexList, 2) /= size(sparseMats(i)%factorList, 1)) then
            call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
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
! Copied from FieldRegrid.F90
! Small subroutine to make sure that Grid doesn't
! contain some of the properties that aren't currently
! allowed in regridding
subroutine checkGrid(grid,staggerloc,rc)
    type (ESMF_Grid) :: grid
    type(ESMF_StaggerLoc) :: staggerloc
    integer, intent(out), optional :: rc
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
         call ESMF_LogSetError(ESMF_RC_ARG_BAD, & 
             msg="- can't currently regrid an arbitrarily distributed Grid", & 
             ESMF_CONTEXT, rcToReturn=rc) 
          return
   endif        

   ! Make sure Grid doesn't contain width 1 DEs
   call ESMF_GridGet(grid,localDECount=localDECount, dimCount=dimCount, &
          rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
   
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
             call ESMF_LogSetError(ESMF_RC_ARG_BAD, & 
          msg="- can't currently regrid a grid that contains a DE of width less than 2", & 
             ESMF_CONTEXT, rcToReturn=rc) 
          return
          endif
       enddo
   enddo

   if(present(rc)) rc = ESMF_SUCCESS
end subroutine checkGrid

!------------------------------------------------------------------------------
subroutine compute_mesharea(mesh, area, rc)

type(ESMF_Mesh), intent(inout)             :: mesh
real(ESMF_KIND_R8), intent(inout), pointer :: area(:)
integer, intent(out), optional          :: rc

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
! !IROUTINE: ESMF_XGridDestroy - Free all resources associated with an XGrid
! !INTERFACE:

  subroutine ESMF_XGridDestroy(xgrid, rc)
!
! !ARGUMENTS:
    type(ESMF_XGrid), intent(inout)          :: xgrid       
    integer,          intent(out),  optional :: rc     
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

    call ESMF_XGridValidate(xgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    if (.not.associated(xgrid%xgtypep)) then 
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="Uninitialized or already destroyed XGrid: xgtypep unassociated", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif 

    ! Destruct all xgrid internals and then free xgrid memory.
    call ESMF_BaseGetStatus(xgrid%xgtypep%base, xgridstatus, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    if (xgridstatus .eq. ESMF_STATUS_READY) then

      if((xgrid%xgtypep%is_proxy)) then

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

        !call ESMF_DistGridDestroy(xgrid%xgtypep%distgridM, rc=localrc)
        !if (ESMF_LogFoundError(localrc, &
        !    ESMF_ERR_PASSTHRU, &
        !    ESMF_CONTEXT, rcToReturn=rc)) return

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
    if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_INIT_SET_DELETED(xgrid)

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_XGridDestroy

end module ESMF_XGridCreateMod
