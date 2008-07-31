! $Id: ESMF_Mesh.F90,v 1.9 2008/07/31 17:09:18 dneckels Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
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

!   F90 API wrapper of C++ implemenation of Mesh
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
  private
    type(ESMF_Pointer) :: this
    type(ESMF_Pointer) :: nodal_distgrid
    type(ESMF_Pointer) :: element_distgrid
    integer :: mesh_freed   ! Has the mesh memory been release?
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

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Mesh.F90,v 1.9 2008/07/31 17:09:18 dneckels Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

   interface ESMF_MeshCreate
     module procedure ESMF_MeshCreate3Part
     module procedure ESMF_MeshCreate1Part
   end interface

      contains

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreate3Part()"
!BOPI
! !IROUTINE: ESMF_MeshCreate3Part - Create a Mesh as a 3 step process.  First
!            the mesh, then add nodes and finally add elements.
!
! !INTERFACE:
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
!EOPI
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
!BOPI
! !IROUTINE: ESMF_MeshCreate1Part - Create a Mesh all at once.
!
! !INTERFACE:
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
!         interpreted by using mesh.spatial_dim, and is ordered 
!         (ndim, nnodes), fortran ordering
!   \item[nodeOwners] 
!         Processor that owns the node.  If the node is shared, the value
!         will be a processor other than the current one.
!   \item [{[elementIds]}]
!         The global id's of the elements resident on this processor
!   \item[elementTypes] 
!         Topology of the given element (one of ESMF_MeshElement)
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
!EOPI
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

    rc = localrc
    
  end function ESMF_MeshCreate1Part
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshWrite()"
!BOPI
! !IROUTINE: ESMF_MeshWrite - Write mesh to a VTK file
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

    rc = localrc
    
  end subroutine ESMF_MeshWrite
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshAddNodes()"
!BOPI
! !IROUTINE: ESMF_MeshAddNodes - Add nodes to a mesh
!
! !INTERFACE:
    subroutine ESMF_MeshAddNodes(mesh, nodeIds, nodeCoords, nodeOwners, rc)

!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                   :: mesh
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
!         interpreted by using mesh.spatial_dim, and is ordered 
!         (ndim, nnodes), fortran ordering
!   \item[nodeOwners] 
!         Processor that owns the node.  If the node is shared, the value
!         will be a processor other than the current one.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: num_nodes

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    num_nodes = size(nodeIds)
    call C_ESMC_MeshAddNodes(mesh%this, num_nodes, nodeIds(1), nodeCoords(1), &
                             nodeOwners(1), localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = localrc
    
  end subroutine ESMF_MeshAddNodes
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshAddElements()"
!BOPI
! !IROUTINE: ESMF_MeshAddElements - Add elements to a mesh
!
! !INTERFACE:
    subroutine ESMF_MeshAddElements(mesh, elementIds, elementTypes, elementConn, rc)

!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                   :: mesh
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
!         Topology of the given element (one of ESMF_MeshElement)
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
!EOPI
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

    call C_ESMC_MeshCreateDistGrids(mesh%this, mesh%nodal_distgrid, &
                      mesh%element_distgrid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    rc = localrc
    
  end subroutine ESMF_MeshAddElements
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

! -----------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshDestroy"
!BOPI
! !IROUTINE: ESMF_MeshDestroy - Destroy the Mesh.
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
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object.
! \end{description}
!
!EOPI
      integer  :: localrc

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

      if (mesh%mesh_freed .eq. 0) then
        call C_ESMC_MeshDestroy(mesh, localrc)
      endif

      ESMF_INIT_SET_DELETED(mesh)

      rc = localrc

    end subroutine ESMF_MeshDestroy

!------------------------------------------------------------------------------

! -----------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshFreeMemory"
!BOPI
! !IROUTINE: ESMF_MeshFreeMemory - Remove the underlying
!    mesh and its memory, but keep the fortran mesh scheme around.
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
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object.
! \end{description}
!
!EOPI
      integer  :: localrc

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

      ! If already free, fine just return
      if (mesh%mesh_freed) then
        rc = ESMF_SUCCESS
        return
      endif

   
      call C_ESMC_MeshFreeMemory(mesh,localrc)

      mesh%mesh_freed = 1

      rc = localrc

    end subroutine ESMF_MeshFreeMemory

!------------------------------------------------------------------------------

end module ESMF_MeshMod
