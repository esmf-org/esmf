! $Id: ESMF_Mesh.F90,v 1.17 2009/04/21 03:44:14 w6ws Exp $
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

  type(ESMF_MeshElement), parameter :: &
        ESMF_MESHELEMENT_QUAD = ESMF_MeshElement(0), &
        ESMF_MESHELEMENT_TRI = ESMF_MeshElement(1), &
        ESMF_MESHELEMENT_HEX = ESMF_MeshElement(2), &
        ESMF_MESHELEMENT_TET = ESMF_MeshElement(3)

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
  public ESMF_MeshElement, ESMF_MESHELEMENT_QUAD, ESMF_MESHELEMENT_TRI, &
                           ESMF_MESHELEMENT_HEX, ESMF_MESHELEMENT_TET
      
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
    '$Id: ESMF_Mesh.F90,v 1.17 2009/04/21 03:44:14 w6ws Exp $'

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
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add elements to a mesh.  This call should follow a call to AddNodes.
!   The elements will link together nodes to form topological entitites.
!
!   \begin{description}
!   \item [{[elementIds]}]
!         The global id's of the elements resident on this processor
!   \item[elementTypes] 
!         Topology of the given element (one of ESMF\_MeshElement)
!   \item[elementConn] 
!         Connectivity table.  The table should line up with the elementIds and
!         elementTypes list.  The indices into the node declaration for each
!         element will reside one after the other in this list.  The number
!         of entries should be equal to the number of nodes in the given
!         topology.  The indices should be the local index (1 based) into the array
!         of nodes that was declared with MeshAddNodes
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
    call C_ESMC_MeshAddElements(mesh%this, num_elems, &
                             elementIds(1), elementTypes(1), &
                             elementConn(1), localrc)
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

    rc = localrc
    
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
!   Create an empty mesh.
!
!   \begin{description}
!   \item [nodeIds]
!         The global id's of the nodes resident on this processor
!   \item[nodeCoords] 
!         Physical coordinates of the nodes.  This 1d array will be
!         interpreted by using mesh.spatial\_dim, and is ordered 
!         (ndim, nnodes), fortran ordering
!   \item[nodeOwners] 
!         Processor that owns the node.  If the node is shared, the value
!         will be a processor other than the current one.
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
    call C_ESMC_MeshAddNodes(mesh%this, num_nodes, nodeIds(1), nodeCoords(1), &
                             nodeOwners(1), localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = localrc
    
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
!   Create an empty mesh in three parts - first the mesh, then add
!   nodes and finally add elements.
!
!   \begin{description}
!   \item [parametricDim]
!         Dimension of the topology (quad=2, hex=3,...)
!   \item[spatialDim] 
!         For a manifold, this can be larger than the parametric dim.
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

    rc = localrc
    
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
!   Create an empty mesh.
!
!   \begin{description}
!   \item [parametricDim]
!         Dimension of the topology (quad=2, hex=3,...)
!   \item[spatialDim] 
!         For a manifold, this can be larger than the parametric dim.
!   \item [nodeIds]
!         The global id's of the nodes resident on this processor
!   \item[nodeCoords] 
!         Physical coordinates of the nodes.  This 1d array will be
!         interpreted by using mesh.spatial\_dim, and is ordered 
!         (ndim, nnodes), fortran ordering
!   \item[nodeOwners] 
!         Processor that owns the node.  If the node is shared, the value
!         will be a processor other than the current one.
!   \item [{[elementIds]}]
!         The global id's of the elements resident on this processor
!   \item[elementTypes] 
!         Topology of the given element (one of ESMF\_MeshElement)
!   \item[elementConn] 
!         Connectivity table.  The table should line up with the elementIds and
!         elementTypes list.  The indices into the node declaration for each
!         element will reside one after the other in this list.  The number
!         of entries should be equal to the number of nodes in the given
!         topology.  The indices should be the local index (1 based) into the array
!         of nodes that was declared with MeshAddNodes
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

    ESMF_MeshCreate1Part%mesh_freed = 0  ! memory has not been released

    ! Set init status of arguments
    ESMF_INIT_SET_CREATED(ESMF_MeshCreate1Part)

    ! Add the nodes
    num_nodes = size(nodeIds)
    call C_ESMC_MeshAddNodes(ESMF_MeshCreate1Part%this, num_nodes, nodeIds(1), nodeCoords(1), &
                             nodeOwners(1), localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    num_elems = size(elementTypes)
    call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, num_elems, &
                             elementIds(1), elementTypes(1), &
                             elementConn(1), localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! We create two dist grids, one for nodal one for element
    call C_ESMC_MeshCreateDistGrids(ESMF_MeshCreate1Part%this, ESMF_MeshCreate1Part%nodal_distgrid, &
                      ESMF_MeshCreate1Part%element_distgrid, &
                      ESMF_MeshCreate1Part%num_nodes, ESMF_MeshCreate1Part%num_elements, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !call ESMF_DistGridPrint(ESMF_MeshCreate1Part%nodal_distgrid)
    !ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, ESMF_MeshCreate1Part%nodal_distgrid, rc)

    rc = localrc
    
  end function ESMF_MeshCreate1Part
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromPointer()"
!BOP
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
!         Dimension of the topology (quad=2, hex=3,...)
!   \item[spatialDim] 
!         For a manifold, this can be larger than the parametric dim.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
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
!    Destroy the mesh.
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object.
! \end{description}
!
!EOP
      integer  :: localrc

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

      if (mesh%mesh_freed .eq. 0) then
        call C_ESMC_MeshDestroy(mesh, localrc)
      endif

      ESMF_INIT_SET_DELETED(mesh)

      rc = localrc

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
!    Remove the mesh and its memory,  but retain the Fortran
!    mesh scheme.
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object.
! \end{description}
!
!EOP
      integer  :: localrc

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

      ! If already free, fine just return
      if (mesh%mesh_freed .eq. 1) then
        rc = ESMF_SUCCESS
        return
      endif

   
      call C_ESMC_MeshFreeMemory(mesh,localrc)

      mesh%mesh_freed = 1

      rc = localrc

    end subroutine ESMF_MeshFreeMemory

!------------------------------------------------------------------------------

! -----------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGet"
!BOP
! !IROUTINE: ESMF_MeshGet - Get information from a Mesh
!
! !INTERFACE:
      subroutine ESMF_MeshGet(mesh, nodal_distgrid, element_distgrid, &
                   num_nodes, num_elements, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)             :: mesh
    type(ESMF_DistGrid), intent(out), optional :: nodal_distgrid
    type(ESMF_DistGrid), intent(out), optional :: element_distgrid
    integer,             intent(out), optional :: num_nodes
    integer,             intent(out), optional :: num_elements
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Get information from the mesh.
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object.
! \end{description}
!
!EOP
      integer  :: localrc

      localrc = ESMF_SUCCESS

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

      if (present(nodal_distgrid)) nodal_distgrid = mesh%nodal_distgrid
      if (present(element_distgrid)) element_distgrid = mesh%element_distgrid
      if (present(num_nodes)) num_nodes =mesh%num_nodes
      if (present(num_elements)) num_elements =mesh%num_elements 

      if (present(rc)) rc = localrc

    end subroutine ESMF_MeshGet

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshWrite()"
!BOP
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
!EOP
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
