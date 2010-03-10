! $Id: ESMF_Mesh.F90,v 1.27.2.2 2010/03/10 06:33:08 oehmke Exp $
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
    logical :: isCMeshFreed   ! Has the mesh memory been release?
    integer :: createStage
    logical :: isFullyCreated ! Are the distgrids there and the numOwned X correct
    integer :: numOwnedNodes
    integer :: numOwnedElements
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
  public ESMF_MeshMatch
  public ESMF_MeshSerialize
  public ESMF_MeshDeserialize
  public ESMF_MeshFindPnt

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Mesh.F90,v 1.27.2.2 2010/03/10 06:33:08 oehmke Exp $'

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
! !IROUTINE: ESMF_MeshAddElements - Add elements to a Mesh \label{sec:mesh:api:meshaddelements}
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
!   sequence and should be called after the mesh is created with {\tt ESMF\_MeshCreate()} 
!   (\ref{sec:mesh:api:meshcreate})
!   and after the nodes are added with {\tt ESMF\_MeshAddNodes()} (\ref{sec:mesh:api:meshaddnodes}).
!   This call adds the elements to the 
!   mesh and finalizes the create. After this call the Mesh is usable, for
!   example a Field may be built on the created Mesh object and 
!   this Field may be used in a {\tt ESMF\_FieldRegridStore()} call.
!
!   The parameters to this call {\tt elementIds}, {\tt elementTypes}, and
!   {\tt elementConn} describe the elements to be created. The description 
!   for a particular element lies at the same index location in {\tt elementIds} 
!   and {\tt elementTypes}. Each entry in {\tt elementConn} consists of the list of
!   nodes used to create that element, so the connections for element $e$ in the 
!   {\tt elementIds} array will start at $number\_of\_nodes\_in\_element(1) + number\_of\_nodes\_in\_element(2) +
!   \cdots + number\_of\_nodes\_in\_element(e-1) + 1$ in {\tt elementConn}.
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
!         a total size equal to the sum of the number of nodes in each element on
!         this PET. The number of nodes in each element is implied by its element type in 
!         {\tt elementTypes}. The nodes for each element 
!         are in sequence in this array (e.g. the nodes for element 1 are elementConn(1),
!         elementConn(2), etc.). 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: num_elems, num_elementConn

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    

    ! If we're at the wrong stage then complain
    if (mesh%createStage .ne. 2) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- MeshAddNodes() should be called before this", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    


    num_elems = size(elementIds)
    num_elementConn = size(elementConn)
    call C_ESMC_MeshAddElements(mesh%this, num_elems, &
                             elementIds, elementTypes, &
                             num_elementConn, elementConn, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! We create two dist grids, one for nodal one for element
    call C_ESMC_MeshCreateDistGrids(mesh%this, mesh%nodal_distgrid, &
                      mesh%element_distgrid, &
                      mesh%numOwnedNodes, mesh%numOwnedElements, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    !call ESMF_DistGridPrint(mesh%nodal_distgrid)
    !ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, mesh%nodal_distgrid, rc)

    ! Go to next stage 
    mesh%createStage=3

    ! Set as fully created 
    mesh%isFullyCreated=.true.

    if (present (rc)) rc = localrc
    
  end subroutine ESMF_MeshAddElements
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshAddNodes()"
!BOP
! !IROUTINE: ESMF_MeshAddNodes - Add nodes to a Mesh \label{sec:mesh:api:meshaddnodes}
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
!   using {\tt ESMF\_MeshCreate()} (\ref{sec:mesh:api:meshcreate}).
!   This call adds the nodes to the 
!   mesh. The next step is to call {\tt ESMF\_MeshAddElements()} (\ref{sec:mesh:api:meshaddelements}).
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

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    

    ! If we're at the wrong stage then complain
    if (mesh%createStage .ne. 1) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- MeshAddNodes() should be called before this", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    

    num_nodes = size(nodeIds)
    call C_ESMC_MeshAddNodes(mesh%this, num_nodes, nodeIds, nodeCoords, &
                             nodeOwners, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Go to next stage 
    mesh%createStage=2

    if (present (rc)) rc = localrc
    
  end subroutine ESMF_MeshAddNodes
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreate3Part()"
!BOP
! !IROUTINE: ESMF_MeshCreate - Create a Mesh as a 3 step process \label{sec:mesh:api:meshcreate}
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
!   ({\tt spatialDim}). The next step is to call {\tt ESMF\_MeshAddNodes()} (\ref{sec:mesh:api:meshaddnodes}) 
!   to add the nodes and then {\tt ESMF\_MeshAddElements()} (\ref{sec:mesh:api:meshaddelements}) to add 
!   the elements and finalize the mesh.
!
!   \begin{description}
!   \item [parametricDim]
!         Dimension of the topology of the Mesh. (E.g. a mesh constructed of squares would
!         have a parametric dimension of 2, whereas a Mesh constructed of cubes would have one
!         of 3.)
!   \item[spatialDim] 
!         The number of coordinate dimensions needed to describe the locations of the nodes 
!         making up the Mesh. For a manifold, the spatial dimesion can be larger than the 
!         parametric dim (e.g. the 2D surface of a sphere in 3D space), but it can't be smaller. 
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

    ! The C side has been created
    ESMF_MeshCreate3Part%isCMeshFreed=.false.

    ! Go to next stage 
    ESMF_MeshCreate3Part%createStage=1

    ! Set as not yet created
    ESMF_MeshCreate3Part%isFullyCreated=.false.

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
!   {\tt elementConn} describe the elements to be created. The description 
!   for a particular element lies at the same index location in {\tt elementIds} 
!   and {\tt elementTypes}. Each entry in {\tt elementConn} consists of the list of
!   nodes used to create that element, so the connections for element $e$ in the 
!   {\tt elementIds} array will start at $number\_of\_nodes\_in\_element(1) + number\_of\_nodes\_in\_element(2) +
!   \cdots + number\_of\_nodes\_in\_element(e-1) + 1$ in {\tt elementConn}.
!
!   \begin{description}
!   \item [parametricDim]
!         Dimension of the topology of the Mesh. (E.g. a mesh constructed of squares would
!         have a parametric dimension of 2, whereas a Mesh constructed of cubes would have one
!         of 3.)
!   \item[spatialDim] 
!         The number of coordinate dimensions needed to describe the locations of the nodes 
!         making up the Mesh. For a manifold, the spatial dimesion can be larger than the 
!         parametric dim (e.g. the 2D surface of a sphere in 3D space), but it can't be smaller. 
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
!         {\tt elementTypes}. The nodes for each element 
!         are in sequence in this array (e.g. the nodes for element 1 are elementConn(1),
!         elementConn(2), etc.). 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: num_nodes
    integer                 :: num_elems, num_elementConn

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
    num_elementConn = size(elementConn)
    call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, num_elems, &
                             elementIds, elementTypes, &
                             num_elementConn, elementConn, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! We create two dist grids, one for nodal one for element
    call C_ESMC_MeshCreateDistGrids(ESMF_MeshCreate1Part%this, ESMF_MeshCreate1Part%nodal_distgrid, &
                      ESMF_MeshCreate1Part%element_distgrid, &
                      ESMF_MeshCreate1Part%numOwnedNodes, ESMF_MeshCreate1Part%numOwnedElements, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! The C side has been created
    ESMF_MeshCreate1Part%isCMeshFreed=.false.

    ! Set as fully created 
    ESMF_MeshCreate1Part%isFullyCreated=.true.

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
    function ESMF_MeshCreateFromPointer(mesh_pointer, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromPointer
! !ARGUMENTS:
    type(ESMF_Pointer),        intent(in)            :: mesh_pointer
    integer, intent(out), optional                   :: rc
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
!         parametric dim (e.g. the 2D surface of a sphere in 3D space), but it can't be smaller. 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    if(present(rc)) rc = ESMF_RC_NOT_IMPL
    ! initialize return code; assume routine not implemented

    ! Set pointer
    ESMF_MeshCreateFromPointer%this = mesh_pointer

    ! Check init status of arguments
    ESMF_INIT_SET_CREATED(ESMF_MeshCreateFromPointer)

    ! We create two dist grids, one for nodal one for element
    call C_ESMC_MeshCreateDistGrids(ESMF_MeshCreateFromPointer%this, &
                      ESMF_MeshCreateFromPointer%nodal_distgrid, &
                      ESMF_MeshCreateFromPointer%element_distgrid, &
                      ESMF_MeshCreateFromPointer%numOwnedNodes, &
                      ESMF_MeshCreateFromPointer%numOwnedElements, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! The C side has been created
    ESMF_MeshCreateFromPointer%isCMeshFreed=.false.

    ! Set as fully created 
    ESMF_MeshCreateFromPointer%isFullyCreated=.true.

    if(present(rc)) rc = ESMF_SUCCESS

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

      ! If not already freed then free the c side
      if (.not. mesh%isCMeshFreed) then
        call C_ESMC_MeshDestroy(mesh%this, localrc)

        ! Set this for consistancies sake
         mesh%isCMeshFreed=.true.
      endif


      ! TODO: destroy distgrids here
      if (mesh%isFullyCreated) then
         ! destroy node distgrid          
         call ESMF_DistgridDestroy(mesh%nodal_distgrid, rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

         ! destroy element distgrid          
         call ESMF_DistgridDestroy(mesh%element_distgrid, rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

         ! Set this for consistancies sake
          mesh%isFullyCreated=.false.
      endif

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
!    This call removes the portions of {\tt mesh} which contain connection and coordinate
!    information. After this call, Fields build on {\tt mesh} will no longer be usable
!    as part of an {\tt ESMF\_FieldRegridStore()} operation. However, after this call 
!    Fields built on {\tt mesh} can still be used in an {\tt ESMF\_FieldRegrid()} 
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
      if (mesh%isCMeshFreed) then
        if (present (rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Free internal C++ mesh   
      call C_ESMC_MeshFreeMemory(mesh,localrc)

      ! Set Freed status
      mesh%isCMeshFreed = .true.

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
                   numOwnedNodes, numOwnedElements, isMemFreed, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)             :: mesh
    type(ESMF_DistGrid), intent(out), optional :: nodalDistgrid
    type(ESMF_DistGrid), intent(out), optional :: elementDistgrid
    integer,             intent(out), optional :: numOwnedNodes
    integer,             intent(out), optional :: numOwnedElements
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
! A 1D arbitrary distgrid describing the distribution of the nodes across the PETs. Note that
! on each PET the distgrid will only contain entries for nodes owned by that PET.
! This is the DistGrid that would be used to construct the Array in a Field that is constructed
! on {\tt mesh}.
! \item [{[elementDistgrid]}]
! A 1D arbitrary distgrid describing the distribution of elements across the PETs. Note that
! on each PET the distgrid will only contain entries for elements owned by that PET.
! \item [{[numOwnedNodes]}]
! The number of local nodes which are owned by this PET. This is the number of PET local entries in
! the nodalDistgrid.
! \item [{[numOwnedElements]}]
! The number of local elements which are owned by this PET. Note that every element is owned by 
! the PET it resides on, so unlike for nodes, {\tt numOwnedElements} is identical to the number of elements on
! the PET. It is also the number of PET local entries in the elementDistgrid. 
! \item [{[isMemFreed]}]
! Indicates if the coordinate and connection memory been freed from {\tt mesh}. If so, it
! can no longer be used as part of an {\tt ESMF\_FieldRegridStore()} call.
! \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
      integer  :: localrc

      localrc = ESMF_SUCCESS

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has not been fully created
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    


      if (present(nodalDistgrid)) nodalDistgrid = mesh%nodal_distgrid
      if (present(elementDistgrid)) elementDistgrid = mesh%element_distgrid
      if (present(numOwnedNodes)) numOwnedNodes =mesh%numOwnedNodes
      if (present(numOwnedElements)) numOwnedElements =mesh%numOwnedElements 
      if (present(isMemFreed)) then
            isMemFreed=mesh%isCMeshFreed
      endif

      if (present(rc)) rc = localrc

    end subroutine ESMF_MeshGet

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshMatch()"
!BOPI
! !IROUTINE: ESMF_MeshMatch - Check if two Mesh objects match

! !INTERFACE:
  function ESMF_MeshMatch(mesh1, mesh2, rc)
!
! !RETURN VALUE:
    logical :: ESMF_MeshMatch
      
! !ARGUMENTS:
    type(ESMF_Mesh),  intent(in)              :: mesh1
    type(ESMF_Mesh),  intent(in)              :: mesh2
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Check if {\tt mesh1} and {\tt mesh2} match. Returns
!      .true. if Mesh objects match, .false. otherwise. This
!      method current just checks if mesh1 and mesh2s distgrids match,
!      future work will do a more complex check.
!
!     The arguments are:
!     \begin{description}
!     \item[mesh1] 
!          {\tt ESMF\_Mesh} object.
!     \item[mesh2] 
!          {\tt ESMF\_Mesh} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer      :: localrc      ! local return code
    logical      :: matchResultNode, matchResultElem

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! init to one setting in case of error
    ESMF_MeshMatch = .false.
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh1, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh2, rc)
    
    ! If meshs have not been fully created
    if (.not. mesh1%isFullyCreated) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif        

    if (.not. mesh2%isFullyCreated) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif        

    ! For now just make match to mean that the Mesh's have the same distgrids because that's
    ! all the fields care about
    matchResultNode=ESMF_DistGridMatch(mesh1%nodal_distgrid, mesh2%nodal_distgrid, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    matchResultElem=ESMF_DistGridMatch(mesh1%element_distgrid, mesh2%element_distgrid, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! return successfully
    if (matchResultNode .and. matchResultElem) then
       ESMF_MeshMatch = .true.
    else
       ESMF_MeshMatch = .false.
    endif

    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_MeshMatch
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshSerialize"

!BOPI
! !IROUTINE: ESMF_MeshSerialize - Serialize mesh info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_MeshSerialize(mesh, buffer, length, offset, inquireflag, rc) 
!
! !ARGUMENTS:
      type(ESMF_Mesh), intent(inout) :: mesh
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      type(ESMF_InquireFlag), intent(in), optional :: inquireflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Mesh} object and adds all the information needed
!      to  recreate the object based on this information.  
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [mesh]
!           {\tt ESMF\_Mesh} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item [inquireflag]
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: i,localrc
      type(ESMF_AttReconcileFlag) :: attreconflag
      type(ESMF_InquireFlag) :: linquireflag
      integer :: intMeshFreed,intFullyCreated

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit,mesh,rc)


    ! If mesh has not been fully created
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    


      if (present (inquireflag)) then
        linquireflag = inquireflag
      else
        linquireflag = ESMF_NOINQUIRE
      end if

     ! Serialize Node Distgrid
     call c_ESMC_DistgridSerialize(mesh%nodal_distgrid, buffer, length, offset, &
                                 linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

     ! Serialize Element Distgrid
     call c_ESMC_DistgridSerialize(mesh%element_distgrid, buffer, length, offset, &
                                 linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Convert logicals to ints
      if (mesh%isCMeshFreed) then
        intMeshFreed=1
      else
        intMeshFreed=0
      endif

      ! Serialize other Mesh items
      call c_ESMC_MeshInfoSerialize(intMeshFreed, &
              buffer, length, offset,linquireflag, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! If exists serialize mesh
      if (.not. mesh%isCMeshFreed) then
         call c_ESMC_MeshSerialize(mesh%this, buffer, length, offset, &
                                 linquireflag, localrc)
         if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif

      ! return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_MeshSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshDeserialize"

!BOPI
! !IROUTINE: ESMF_MeshDeserialize - Deserialize a byte stream into a Mesh
!
! !INTERFACE:
      function ESMF_MeshDeserialize(buffer, offset, rc) 
!
! !RETURN VALUE:
      type(ESMF_Mesh) :: ESMF_MeshDeserialize   
!
! !ARGUMENTS:
      character, pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a Mesh object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc
      integer :: i
      type(ESMF_AttReconcileFlag) :: attreconflag
      integer :: intMeshFreed

      ! Initialize
      localrc = ESMF_RC_NOT_IMPL
      if  (present(rc)) rc = ESMF_RC_NOT_IMPL


     ! Deserialize node Distgrid
     call c_ESMC_DistGridDeserialize(ESMF_MeshDeserialize%nodal_distgrid, buffer, offset, localrc)
     if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call ESMF_DistGridSetInitCreated(ESMF_MeshDeserialize%nodal_distgrid, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

     ! Deserialize element Distgrid
     call c_ESMC_DistGridDeserialize(ESMF_MeshDeserialize%element_distgrid, buffer, offset, localrc)
     if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call ESMF_DistGridSetInitCreated(ESMF_MeshDeserialize%element_distgrid, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Deserialize other ESMF_MeshDeserialize items
      call c_ESMC_MeshInfoDeserialize(intMeshFreed, buffer, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! Convert ints to logicals
      if (intMeshFreed .eq. 1) then
         ESMF_MeshDeserialize%isCMeshFreed=.true.
      else
         ESMF_MeshDeserialize%isCMeshFreed=.false.
      endif

      ! Set values who's values are implied by the fact 
      ! that this is a proxy mesh
      ESMF_MeshDeserialize%isFullyCreated=.true.
      ESMF_MeshDeserialize%createStage=3
      ESMF_MeshDeserialize%numOwnedNodes=0
      ESMF_MeshDeserialize%numOwnedElements=0

      ! If exists serialize mesh
      if (.not. ESMF_MeshDeserialize%isCMeshFreed) then
         call c_ESMC_MeshDeserialize(ESMF_MeshDeserialize%this, buffer, &
                                     offset, localrc)
         if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return
      endif
   
     ! Set init status
     ESMF_INIT_SET_CREATED(ESMF_MeshDeserialize)

     if  (present(rc)) rc = ESMF_SUCCESS

     end function ESMF_MeshDeserialize


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

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    

    call C_ESMC_MeshWrite(mesh%this, filename, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = localrc
    
  end subroutine ESMF_MeshWrite
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshFindPnt()"
!BOPI
! !IROUTINE: ESMF_MeshFindPnt - Find points in a mesh
!
! !INTERFACE:
    subroutine ESMF_MeshFindPnt(mesh, unmappedAction, &
                                pntDim, pntCount, pntList, &
                                petList, rc)


!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                     :: mesh
    type(ESMF_UnmappedAction), intent(in), optional :: unmappedAction
    integer, intent(in)                             :: pntDim
    integer, intent(in)                             :: pntCount
    real(ESMF_KIND_R8), pointer                     :: pntList(:)
    integer, pointer                                :: petList(:)
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   Write a mesh to VTK file.
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item [{[unmappedDstAction]}]
!           Specifies what should happen if there are destination points that
!           can't be mapped to a source cell. Options are 
!           {\tt ESMF\_UNMAPPEDACTION\_ERROR} or 
!           {\tt ESMF\_UNMAPPEDACTION\_IGNORE}. If not specified, defaults 
!           to {\tt ESMF\_UNMAPPEDACTION\_ERROR}. 
!   \item [pntDim]
!         The dimension of the points in pntList.
!   \item [pntNum]
!         The number of points in pntList
!   \item [petList]
!         The generated list of pets for each point.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_UnmappedAction) :: localunmappedAction

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogMsgSetError(ESMF_RC_OBJ_WRONG, & 
                 "- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rc) 
       return 
    endif    

   ! Set default vale for unmappedAction
   if (present(unmappedAction)) then
      localunmappedAction=unmappedAction
   else
      localunmappedAction=ESMF_UNMAPPEDACTION_ERROR
   endif

   ! Call into mesh find point subroutine
   ! TODO: ADD GIDS TO THIS INTERFACE AS THEY'RE AVAILABLE FROM C++ METHOD
    call C_ESMC_MeshFindPnt(mesh%this, localunmappedAction, pntDim, pntCount, &
			    pntList, petList, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! return success
     if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_MeshFindPnt
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetInit"
!BOPI
! !IROUTINE: ESMF_MeshGetInit - Internal access routine for init code
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
! \item [mesh]
! Mesh object.
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
