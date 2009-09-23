! $Id: ESMF_Mesh.F90,v 1.20 2009/09/23 23:13:01 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Mesh.F90"
!==============================================================================
!
! ESMF Mesh Module
module ESMF_MeshMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the Mesh class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_MeshMod
!

!   F90 API wrapper of C++ implementation of Mesh
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_VMMod
  use ESMF_DELayoutMod
  use ESMF_DistGridMod
  use ESMF_RHandleMod
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_Mesh
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_Mesh
  sequence
    type(ESMF_Pointer) :: this
    type(ESMF_DistGrid) :: nodal_distgrid
    type(ESMF_DistGrid) :: element_distgrid
    integer :: mesh_freed   ! Has the mesh memory been release?
    integer :: num_nodes
    integer :: num_elements
    ESMF_INIT_DECLARE
  end type

  type ESMF_MeshElement
  sequence
!  private
    integer :: meshelement
  end type

!!!!
!! Use integers instead of type, to be compatible with reading in from VTK and other files
!!  type(ESMF_MeshElement), parameter :: &
!!        ESMF_MESHELEMTYPE_QUAD = ESMF_MeshElement(0), &
!!        ESMF_MESHELEMTYPE_TRI = ESMF_MeshElement(1), &
!!        ESMF_MESHELEMTYPE_HEX = ESMF_MeshElement(2), &
!!        ESMF_MESHELEMTYPE_TET = ESMF_MeshElement(3)
!!!!

  integer, parameter :: &
        ESMF_MESHELEMTYPE_TRI    = 5,  &  ! Triangle
        ESMF_MESHELEMTYPE_QUAD   = 9,  &  ! Quadralateral
        ESMF_MESHELEMTYPE_TETRA  = 10, &  ! Tetrahedron
        ESMF_MESHELEMTYPE_HEX    = 12     ! Hexahedron

  type ESMF_MeshPartitionType
  sequence
!  private
    integer :: meshpartition
  end type

  type(ESMF_MeshPartitionType), parameter :: &
        ESMF_MESH_PARTITON_NODAL = ESMF_MeshPartitionType(0), &
        ESMF_MESH_PARTITION_ELEMENT = ESMF_MeshPartitionType(1)

!------------------------------------------------------------------------------
!     ! ESMF_Mesh
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Mesh               
  public ESMF_MESHELEMTYPE_QUAD, ESMF_MESHELEMTYPE_TRI, &
         ESMF_MESHELEMTYPE_HEX, ESMF_MESHELEMTYPE_TETRA
      
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_MeshCreate           
  public ESMF_MeshWrite
  public ESMF_MeshAddNodes
  public ESMF_MeshAddElements
  public ESMF_MeshDestroy
  public ESMF_MeshFreeMemory
  public ESMF_MeshGetInit
  public ESMF_MeshGet

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Mesh.F90,v 1.20 2009/09/23 23:13:01 oehmke Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

   interface ESMF_MeshCreate
     module procedure ESMF_MeshCreate3Part
     module procedure ESMF_MeshCreate1Part
     module procedure ESMF_MeshCreateFromPointer
   end interface

      contains


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshAddElements()"
!BOP
! !IROUTINE: ESMF_MeshAddElements - Add elements to a Mesh
!
! !INTERFACE:
    subroutine ESMF_MeshAddElements(mesh, elementIds, elementTypes, elementConn, rc)

!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)                :: mesh
    integer, dimension(:), intent(in)             :: elementIds
    integer, dimension(:), intent(in)             :: elementTypes
    integer, dimension(:), intent(in)             :: elementConn
    integer, intent(out), optional                :: rc
!
! !DESCRIPTION:
!   This call is the third and last part of the three part mesh create
!   sequence and should be called after the nodes are added with
!   {\tt ESMF\_MeshAddNodes()}. This call adds the elements to the 
!   mesh and finalizes the create. After this call the Mesh is usable, for
!   example a Field may be built on the created Mesh object and 
!   this Field may be used in a {\tt ESMF\_FieldRegridStore()} call.
!
!   The parameters to this call {\tt elementIds}, {\tt elementTypes}, and
!   {\tt elementConn} describe  the elements to be created. The description 
!   for a particular element lies at the same index location in {\tt elementIds} 
!   and {\tt elementTypes}, but the connection information for that element in 
!   {\tt elementConn} will start at the index given by the sum of the number of nodes 
!   in all of the proceeding elements plus 1. 
!
!   \begin{description}
!   \item [elementIds]
!          An array containing the global ids of the elements to be created on this PET. 
!          This input consists of a 1D array the size of the number of elements on this PET.
!   \item[elementTypes] 
!          An array containing the types of the elements to be created on this PET. The types used
!          must be appropriate for the parametric dimension of the Mesh. Please see
!          Section~\ref{sec:mesh:opt:elemtype} for the list of options. This input consists of 
!          a 1D array the size of the number of elements on this PET.  
!   \item[elementConn] 
!         An array containing the indexes of the sets of nodes to be connected together to form the
!         elements to be created on this PET. The entries in this list are NOT node global ids, 
!         but rather each entry is a local index (1 based) into the list of nodes which were
!         created on this PET by the previous {\tt ESMF\_MeshAddNodes()} call.
!         In other words, an entry of 1 indicates that this element contains the node
!         described by {\tt nodeIds(1)}, {\tt nodeCoords(1)}, etc. passed into the
!         {\tt ESMF\_MeshAddNodes()} call on this PET. It is also
!         important to note that the order of the nodes in an element connectivity list
!         matters. Please see Section~\ref{sec:mesh:opt:elemtype} for diagrams illustrating
!         the correct order of nodes in a element. This input consists of a 1D array with 
!         a total size equal to the sum of the number of nodes contained in each element on
!         this PET. The number of nodes in each element is implied by its element type in 
!         {\tt elementTypes}.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: num_elems

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    num_elems = size(elementIds)
    mesh%num_elements = num_elems
    call C_ESMC_MeshAddElements(mesh%this, num_elems, &
                             elementIds, elementTypes, &
                             elementConn, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! We create two dist grids, one for nodal one for element
    call C_ESMC_MeshCreateDistGrids(mesh%this, mesh%nodal_distgrid, &
                      mesh%element_distgrid, &
                      mesh%num_nodes, mesh%num_elements, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !call ESMF_DistGridPrint(mesh%nodal_distgrid)
    !ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, mesh%nodal_distgrid, rc)

    if (present (rc)) rc = localrc
    
  end subroutine ESMF_MeshAddElements
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshAddNodes()"
!BOP
! !IROUTINE: ESMF_MeshAddNodes - Add nodes to a Mesh
!
! !INTERFACE:
    subroutine ESMF_MeshAddNodes(mesh, nodeIds, nodeCoords, nodeOwners, rc)

!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)                :: mesh
    integer, dimension(:), intent(in)             :: nodeIds
    real(ESMF_KIND_R8), dimension(:), intent(in)  :: nodeCoords
    integer, dimension(:), intent(in)             :: nodeOwners
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   This call is the second part of the three part mesh create
!   sequence and should be called after the mesh's dimensions are set
!   using {\tt ESMF\_MeshCreate()}. This call adds the nodes to the 
!   mesh. The next step is to call {\tt ESMF\_MeshAddElements()}. 
!
!   The parameters to this call {\tt nodeIds}, {\tt nodeCoords}, and 
!   {\tt nodeOwners} describe the nodes to be created on this PET. 
!   The description for a particular node lies at the same index location in 
!   {\tt nodeIds} and {\tt nodeOwners}. Each entry
!   in {\tt nodeCoords} consists of spatial dimension coordinates, so the coordinates
!   for node $n$ in the {\tt nodeIds} array will start at $(n-1)*spatialDim+1$. 
!
!   \begin{description}
!   \item [nodeIds]
!         An array containing the global ids of the nodes to be created on this PET. 
!         This input consists of a 1D array the size of the number of nodes on this PET.
!   \item[nodeCoords] 
!          An array containing the physical coordinates of the nodes to be created on this
!          PET. This input consists of a 1D array the size of the number of nodes on this PET times the Mesh's 
!          spatial dimension ({\tt spatialDim}). The coordinates in this array are ordered
!          so that the coordinates for a node lie in sequence in memory. (e.g. for a 
!          Mesh with spatial dimension 2, the coordinates for node 1 are in nodeCoords(0) and
!          nodeCoords(1), the coordinates for node 2 are in nodeCoords(2) and nodeCoords(3), 
!          etc.). 
!   \item[nodeOwners] 
!         An array containing the PETs that own the nodes to be created on this PET. 
!         If the node is shared with another PET, the value
!         may be a PET other than the current one. Only nodes owned by this PET
!         will have PET local entries in a Field created on the Mesh. This input consists of 
!         a 1D array the size of the number of nodes on this PET.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: num_nodes

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    num_nodes = size(nodeIds)
    mesh%num_nodes = num_nodes
    call C_ESMC_MeshAddNodes(mesh%this, num_nodes, nodeIds, nodeCoords, &
                             nodeOwners, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present (rc)) rc = localrc
    
  end subroutine ESMF_MeshAddNodes
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreate3Part()"
!BOP
! !IROUTINE: ESMF_MeshCreate - Create a Mesh as a 3 step process
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreate3Part(parametricDim, spatialDim, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreate3Part
! !ARGUMENTS:
    integer,                intent(in)            :: parametricDim
    integer,                intent(in)            :: spatialDim
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   This call is the first part of the three part mesh create
!   sequence. This call sets the dimension of the elements in the mesh
!   ({\tt parametricDim}) and the number of coordinate dimensions in the mesh
!   ({\tt spatialDim}). The next step is to call {\tt ESMF\_MeshAddElements()}. 
!
!   \begin{description}
!   \item [parametricDim]
!         Dimension of the topology of the Mesh. (E.g. a mesh constructed of squares would
!         have a parametric dimension of 2, whereas a Mesh constructed of cubes would have one
!         of 3.)
!   \item[spatialDim] 
!         The number of coordinate dimensions needed to describe the locations of the nodes 
!         making up the Mesh. For a manifold, the spatial dimesion can be larger than the 
!         parametric dim (e.g. the 2D surface of a sphere in space), but it can't be smaller. 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_MeshCreate3Part%this = ESMF_NULL_POINTER

    call c_ESMC_meshcreate(ESMF_MeshCreate3Part%this, parametricDim, spatialDim, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_MeshCreate3Part%mesh_freed = 0  ! memory has not been released

    ! Check init status of arguments
    ESMF_INIT_SET_CREATED(ESMF_MeshCreate3Part)

    if (present (rc)) rc = localrc
    
  end function ESMF_MeshCreate3Part
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreate1Part()"
!BOP
! !IROUTINE: ESMF_MeshCreate - Create a Mesh all at once
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreate1Part(parametricDim, spatialDim, &
                         nodeIds, nodeCoords, nodeOwners, &
                         elementIds, elementTypes, elementConn, &
                         rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreate1Part
! !ARGUMENTS:
    integer,                intent(in)            :: parametricDim
    integer,                intent(in)            :: spatialDim
    integer, dimension(:), intent(in)             :: nodeIds
    real(ESMF_KIND_R8), dimension(:), intent(in)  :: nodeCoords
    integer, dimension(:), intent(in)             :: nodeOwners
    integer, dimension(:), intent(in)             :: elementIds
    integer, dimension(:), intent(in)             :: elementTypes
    integer, dimension(:), intent(in)             :: elementConn
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a Mesh object in one step. After this call the Mesh is usable, for
!   example, a Field may be built on the created Mesh object and 
!   this Field may be used in a {\tt ESMF\_FieldRegridStore()} call.
!
!   This call sets the dimension of the elements in the mesh
!   ({\tt parametricDim}) and the number of coordinate dimensions in the mesh
!   ({\tt spatialDim}). It then creates the nodes, and 
!   then creates the elements by connecting together the nodes.
!
!   The parameters to this call {\tt nodeIds}, {\tt nodeCoords}, and 
!   {\tt nodeOwners} describe the nodes to be created on this PET. 
!   The description for a particular node lies at the same index location in 
!   {\tt nodeIds} and {\tt nodeOwners}. Each entry
!   in {\tt nodeCoords} consists of spatial dimension coordinates, so the coordinates
!   for node $n$ in the {\tt nodeIds} array will start at $(n-1)*spatialDim+1$. 
!
!   The parameters to this call {\tt elementIds}, {\tt elementTypes}, and
!   {\tt elementConn} describe  the elements to be created. The description 
!   for a particular element lies at the same index location in {\tt elementIds} 
!   and {\tt elementTypes}, but the connection information for that element in 
!   {\tt elementConn} will start at the index given by the sum of the number of nodes 
!   in all of the proceeding elements plus 1. 
!
!   \begin{description}
!   \item [parametricDim]
!         Dimension of the topology of the Mesh. (E.g. a mesh constructed of squares would
!         have a parametric dimension of 2, whereas a Mesh constructed of cubes would have one
!         of 3.)
!   \item[spatialDim] 
!         The number of coordinate dimensions needed to describe the locations of the nodes 
!         making up the Mesh. For a manifold, the spatial dimesion can be larger than the 
!         parametric dim (e.g. the 2D surface of a sphere in space), but it can't be smaller. 
!   \item [nodeIds]
!         An array containing the global ids of the nodes to be created on this PET. 
!         This input consists of a 1D array the size of the number of nodes on this PET.
!   \item[nodeCoords] 
!          An array containing the physical coordinates of the nodes to be created on this
!          PET. This input consists of a 1D array the size of the number of nodes on this PET times the Mesh's 
!          spatial dimension ({\tt spatialDim}). The coordinates in this array are ordered
!          so that the coordinates for a node lie in sequence in memory. (e.g. for a 
!          Mesh with spatial dimension 2, the coordinates for node 1 are in nodeCoords(0) and
!          nodeCoords(1), the coordinates for node 2 are in nodeCoords(2) and nodeCoords(3), 
!          etc.). 
!   \item[nodeOwners] 
!         An array containing the PETs that own the nodes to be created on this PET. 
!         If the node is shared with another PET, the value
!         may be a PET other than the current one. Only nodes owned by this PET
!         will have PET local entries in a Field created on the Mesh. This input consists of 
!         a 1D array the size of the number of nodes on this PET.
!   \item [elementIds]
!          An array containing the global ids of the elements to be created on this PET. 
!          This input consists of a 1D array the size of the number of elements on this PET.
!   \item[elementTypes] 
!          An array containing the types of the elements to be created on this PET. The types used
!          must be appropriate for the parametric dimension of the Mesh. Please see
!          Section~\ref{sec:mesh:opt:elemtype} for the list of options. This input consists of 
!          a 1D array the size of the number of elements on this PET.  
!   \item[elementConn] 
!         An array containing the indexes of the sets of nodes to be connected together to form the
!         elements to be created on this PET. The entries in this list are NOT node global ids, 
!         but rather each entry is a local index (1 based) into the list of nodes to be 
!         created on this PET by this call.
!         In other words, an entry of 1 indicates that this element contains the node
!         described by {\tt nodeIds(1)}, {\tt nodeCoords(1)}, etc. on this PET. It is also
!         important to note that the order of the nodes in an element connectivity list
!         matters. Please see Section~\ref{sec:mesh:opt:elemtype} for diagrams illustrating
!         the correct order of nodes in a element. This input consists of a 1D array with 
!         a total size equal to the sum of the number of nodes contained in each element on
!         this PET. The number of nodes in each element is implied by its element type in 
!         {\tt elementTypes}.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: num_nodes
    integer                 :: num_elems

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_MeshCreate1Part%this = ESMF_NULL_POINTER

    call c_ESMC_meshcreate(ESMF_MeshCreate1Part%this, parametricDim, spatialDim, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init status of arguments
    ESMF_INIT_SET_CREATED(ESMF_MeshCreate1Part)

    ! Add the nodes
    num_nodes = size(nodeIds)
    call C_ESMC_MeshAddNodes(ESMF_MeshCreate1Part%this, num_nodes, nodeIds, nodeCoords, &
                             nodeOwners, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    num_elems = size(elementTypes)
    call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, num_elems, &
                             elementIds, elementTypes, &
                             elementConn, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! We create two dist grids, one for nodal one for element
    call C_ESMC_MeshCreateDistGrids(ESMF_MeshCreate1Part%this, ESMF_MeshCreate1Part%nodal_distgrid, &
                      ESMF_MeshCreate1Part%element_distgrid, &
                      ESMF_MeshCreate1Part%num_nodes, ESMF_MeshCreate1Part%num_elements, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Setup F90 mesh stucture
    ESMF_MeshCreate1Part%num_nodes = num_nodes
    ESMF_MeshCreate1Part%num_elements = num_elems
    ESMF_MeshCreate1Part%mesh_freed = 0  ! memory has not been released

    !call ESMF_DistGridPrint(ESMF_MeshCreate1Part%nodal_distgrid)
    !ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, ESMF_MeshCreate1Part%nodal_distgrid, rc)

    if (present (rc)) rc = localrc
    
  end function ESMF_MeshCreate1Part
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromPointer()"
!BOPI
! !IROUTINE: ESMF_MeshCreate - Create a Mesh from a C++ pointer
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreateFromPointer(mesh_pointer)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromPointer
! !ARGUMENTS:
    type(ESMF_Pointer),        intent(in)            :: mesh_pointer
!
! !DESCRIPTION:
!   Create an empty mesh.
!
!   \begin{description}
!   \item [parametricDim]
!         Dimension of the topology of the Mesh. (E.g. a mesh constructed of squares would
!         have a parametric dimension of 2, whereas a Mesh constructed of cubes would have one
!         of 3.)
!   \item[spatialDim] 
!         The number of coordinate dimensions needed to describe the locations of the nodes 
!         making up the Mesh. For a manifold, the spatial dimesion can be larger than the 
!         parametric dim (e.g. the 2D surface of a sphere in space), but it can't be smaller. 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented

    ESMF_MeshCreateFromPointer%this = mesh_pointer

    ESMF_MeshCreateFromPointer%mesh_freed = 0  ! memory has not been released

    ! Check init status of arguments
    ESMF_INIT_SET_CREATED(ESMF_MeshCreateFromPointer)

  end function ESMF_MeshCreateFromPointer
!------------------------------------------------------------------------------

! -----------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshDestroy"
!BOP
! !IROUTINE: ESMF_MeshDestroy - Destroy a Mesh
!
! !INTERFACE:
      subroutine ESMF_MeshDestroy(mesh, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)           :: mesh
    integer,        intent(out), optional :: rc
!
! !DESCRIPTION:
!    Destroy the Mesh. This call removes all internal memory associated with {\tt mesh}. 
!  After this call {\tt mesh} will no longer be usable.
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object to be destroyed.
! \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
      integer  :: localrc

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

      if (mesh%mesh_freed .eq. 0) then
        call C_ESMC_MeshDestroy(mesh, localrc)
      endif

      ! TODO: destroy distgrids here

      ESMF_INIT_SET_DELETED(mesh)

      if (present (rc)) rc = localrc

    end subroutine ESMF_MeshDestroy

!-----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshFreeMemory"
!BOP
! !IROUTINE: ESMF_MeshFreeMemory - Remove a Mesh and its memory
!
! !INTERFACE:
      subroutine ESMF_MeshFreeMemory(mesh, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)        :: mesh
    integer,        intent(out), optional :: rc
!
! !DESCRIPTION:
!    This call removes the portions of Mesh which contain connection and coordinate
!    information. After this call Fields build on {\tt mesh} will no longer be usable
!    as part of an {\tt ESMF\_FieldRegridStore()} operation. However, after this call 
!    Fields built on {\tt mesh} can still be used in an {\tt ESMF\_FieldRegridRun()} 
!    operation if the routehandle was generated beforehand. New Fields may also
!    be built on {\tt mesh} after this call.
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object whose memory is to be freed. 
! \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
      integer  :: localrc

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

      ! If already free, fine just return
      if (mesh%mesh_freed .eq. 1) then
        if (present (rc)) rc = ESMF_SUCCESS
        return
      endif

   
      call C_ESMC_MeshFreeMemory(mesh,localrc)

      mesh%mesh_freed = 1

      if (present (rc)) rc = localrc

    end subroutine ESMF_MeshFreeMemory

!------------------------------------------------------------------------------

! -----------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGet"
!BOP
! !IROUTINE: ESMF_MeshGet - Get information from a Mesh
!
! !INTERFACE:
      subroutine ESMF_MeshGet(mesh, nodalDistgrid, elementDistgrid, &
                   numNodes, numElements, isMemFreed, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)             :: mesh
    type(ESMF_DistGrid), intent(out), optional :: nodalDistgrid
    type(ESMF_DistGrid), intent(out), optional :: elementDistgrid
    integer,             intent(out), optional :: numNodes
    integer,             intent(out), optional :: numElements
    logical,             intent(out), optional :: isMemFreed
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Get various information from a mesh.
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object to retrieve information from.
! \item [{[nodalDistgrid]}]
! A distgrid describing the distribution of the nodes across the PETs. Note that
! on each PET the distgrid will only contain entries for nodes owned by that PET.
! \item [{[elementDistgrid]}]
! A distgrid describing the distribution of the elements across the PETs.
! \item [{[numNodes]}]
! The number of nodes on this PET.
! \item [{[numElements]}]
! The number of elements on this PET.
! \item [{[isMemFreed]}]
! Has the coordinate and connection memory been freed from this mesh. If so, it
! can no longer be used as a part of {\tt ESMF\_FieldRegridStore()} calls.
! \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
      integer  :: localrc

      localrc = ESMF_SUCCESS

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

      if (present(nodalDistgrid)) nodalDistgrid = mesh%nodal_distgrid
      if (present(elementDistgrid)) elementDistgrid = mesh%element_distgrid
      if (present(numNodes)) numNodes =mesh%num_nodes
      if (present(numElements)) numElements =mesh%num_elements 
      if (present(isMemFreed)) then
         if (mesh%mesh_freed==1) then
            isMemFreed=.true.
         else 
            isMemFreed=.false.
         endif
      endif

      if (present(rc)) rc = localrc

    end subroutine ESMF_MeshGet

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshWrite()"
!BOPI
! !IROUTINE: ESMF_MeshWrite - Write a Mesh to a VTK file
!
! !INTERFACE:
    subroutine ESMF_MeshWrite(mesh, filename, rc)

!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                   :: mesh
    character (len=*), intent(in)                 :: filename
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Write a mesh to VTK file.
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item[filename] 
!         The name of the output file.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    call C_ESMC_MeshWrite(mesh%this, filename, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = localrc
    
  end subroutine ESMF_MeshWrite
!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetInit"
!BOPI
! !IROUTINE: ESMF_GridGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_MeshGetInit(mesh)
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_MeshGetInit
!
! !ARGUMENTS:
      type(ESMF_Mesh), intent(in), optional :: mesh
!
! !DESCRIPTION:
! Access deep object init code.
!
! The arguments are:
! \begin{description}
! \item [grid]
! Grid object.
! \end{description}
!
!EOPI

    if (present(mesh)) then
      ESMF_MeshGetInit = ESMF_INIT_GET(mesh)
    else
      ESMF_MeshGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_MeshGetInit

!------------------------------------------------------------------------------

end module ESMF_MeshMod
