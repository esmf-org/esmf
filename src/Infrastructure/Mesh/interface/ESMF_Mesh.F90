! $Id: ESMF_Mesh.F90,v 1.76 2011/07/07 19:55:55 rokuingh Exp $
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
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_Mesh.F90,v 1.76 2011/07/07 19:55:55 rokuingh Exp $'
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
  use ESMF_IOUtilMod
  use ESMF_VMMod
  use ESMF_DELayoutMod
  use ESMF_DistGridMod
  use ESMF_RHandleMod
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  use ESMF_IOScripMod
 
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
    integer :: spatialDim
    integer :: parametricDim

    ! Info about Split elements if created from a file
    ! Eventually may allow this even if not created 
    ! from a file
    logical :: hasSplitElem
    integer :: splitElemStart
    integer :: splitElemCount
    integer,pointer :: splitElemMap(:)
    integer :: origElemStart
    integer :: origElemCount

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

  type ESMF_MeshLoc
  sequence
!  private
    integer :: meshloc
  end type

  type(ESMF_MeshLoc), parameter :: &
        ESMF_MESHLOC_NODE = ESMF_MeshLoc(1), &
        ESMF_MESHLOC_ELEMENT = ESMF_MeshLoc(2)

  type ESMF_FileFormat_Flag
  sequence
 ! private
    integer :: fileformat
  end type

  type(ESMF_FileFormat_Flag), parameter :: &
        ESMF_FILEFORMAT_VTK = ESMF_FileFormat_Flag(1), &
        ESMF_FILEFORMAT_SCRIP = ESMF_FileFormat_Flag(2), &
        ESMF_FILEFORMAT_ESMFMESH = ESMF_FileFormat_Flag(3), &
        ESMF_FILEFORMAT_ESMFGRID = ESMF_FileFormat_Flag(4)

!------------------------------------------------------------------------------
!     ! ESMF_Mesh
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Mesh               
  public ESMF_MESHELEMTYPE_QUAD, ESMF_MESHELEMTYPE_TRI, &
         ESMF_MESHELEMTYPE_HEX, ESMF_MESHELEMTYPE_TETRA
  
  public ESMF_FileFormat_Flag, ESMF_FILEFORMAT_VTK, ESMF_FILEFORMAT_SCRIP, &
	 ESMF_FILEFORMAT_ESMFMESH, ESMF_FILEFORMAT_ESMFGRID
    
  public ESMF_MeshLoc
  public ESMF_MESHLOC_NODE, ESMF_MESHLOC_ELEMENT

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public operator(==)
  public operator(/=)

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
  public ESMF_MeshGetElemArea
  public ESMF_MeshGetOrigElemArea
  public ESMF_MeshGetElemFrac
  public ESMF_MeshGetOrigElemFrac
  public ESMF_MeshGetElemSplit
  public ESMF_MeshMergeSplitSrcInd
  public ESMF_MeshMergeSplitDstInd

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_Mesh.F90,v 1.76 2011/07/07 19:55:55 rokuingh Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

   interface ESMF_MeshCreate
     module procedure ESMF_MeshCreate3Part
     module procedure ESMF_MeshCreate1Part
     module procedure ESMF_MeshCreateFromPointer
     module procedure ESMF_MeshCreateFromFile
   end interface

!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_FileFormatEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF GridFileFormatType.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_FileFormatNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF GridFileFormatType.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_MeshLocEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF MeshLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_MeshLocNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF MeshLoc.  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface

!------------------------------------------------------------------------------

!===============================================================================
! MeshOperator() interfaces
!===============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_MeshAssignment(=) - Mesh assignment
!
! !INTERFACE:
!   interface assignment(=)
!   mesh1 = mesh2
!
! !ARGUMENTS:
!   type(ESMF_Mesh) :: mesh1
!   type(ESMF_Mesh) :: mesh2
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Assign mesh1 as an alias to the same ESMF Mesh object in memory
!   as mesh2. If mesh2 is invalid, then mesh1 will be equally invalid after
!   the assignment.
!
!   The arguments are:
!   \begin{description}
!   \item[mesh1]
!     The {\tt ESMF\_Mesh} object on the left hand side of the assignment.
!   \item[mesh2]
!     The {\tt ESMF\_Mesh} object on the right hand side of the assignment.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_MeshOperator(==) - Mesh equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (mesh1 == mesh2) then ... endif
!             OR
!   result = (mesh1 == mesh2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_Mesh), intent(in) :: mesh1
!   type(ESMF_Mesh), intent(in) :: mesh2
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Test whether mesh1 and mesh2 are valid aliases to the same ESMF
!   Mesh object in memory. For a more general comparison of two ESMF Meshes,
!   going beyond the simple alias test, the ESMF\_MeshMatch() function (not yet
!   implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[mesh1]
!     The {\tt ESMF\_Mesh} object on the left hand side of the equality
!     operation.
!   \item[mesh2]
!     The {\tt ESMF\_Mesh} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_MeshEQ

  end interface
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_MeshOperator(/=) - Mesh not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (mesh1 /= mesh2) then ... endif
!             OR
!   result = (mesh1 /= mesh2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_Mesh), intent(in) :: mesh1
!   type(ESMF_Mesh), intent(in) :: mesh2
!
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!   Test whether mesh1 and mesh2 are {\it not} valid aliases to the
!   same ESMF Mesh object in memory. For a more general comparison of two ESMF
!   Meshes, going beyond the simple alias test, the ESMF\_MeshMatch() function
!   (not yet implemented) must be used.
!
!   The arguments are:
!   \begin{description}
!   \item[mesh1]
!     The {\tt ESMF\_Mesh} object on the left hand side of the non-equality
!     operation.
!   \item[mesh2]
!     The {\tt ESMF\_Mesh} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_MeshNE

  end interface
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshEQ()"
!BOPI
! !IROUTINE:  ESMF_MeshEQ - Compare two Meshes for equality
!
! !INTERFACE:
  function ESMF_MeshEQ(mesh1, mesh2)
! 
! !RETURN VALUE:
    logical :: ESMF_MeshEQ

! !ARGUMENTS:
    type(ESMF_Mesh), intent(in) :: mesh1
    type(ESMF_Mesh), intent(in) :: mesh2

! !DESCRIPTION:
!   Test if both {\tt mesh1} and {\tt mesh2} alias the same ESMF Mesh 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE minit1, minit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! TODO: Consider moving this logic to C++: use Base class? status?
    !       Or replicate logic for C interface also.

    ! check inputs
    minit1 = ESMF_MeshGetInit(mesh1)
    minit2 = ESMF_MeshGetInit(mesh2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (minit1 .eq. ESMF_INIT_CREATED .and. &
      minit2 .eq. ESMF_INIT_CREATED) then
      ESMF_MeshEQ = mesh1%this .eq. mesh2%this
    else
      ESMF_MeshEQ = ESMF_FALSE
    endif

  end function ESMF_MeshEQ
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshNE()"
!BOPI
! !IROUTINE:  ESMF_MeshNE - Compare two Meshes for non-equality
!
! !INTERFACE:
  function ESMF_MeshNE(mesh1, mesh2)
! 
! !RETURN VALUE:
    logical :: ESMF_MeshNE

! !ARGUMENTS:
    type(ESMF_Mesh), intent(in) :: mesh1
    type(ESMF_Mesh), intent(in) :: mesh2

! !DESCRIPTION:
!   Test if both {\tt mesh1} and {\tt mesh2} alias the same ESMF Mesh 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE minit1, minit2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).
    
    ESMF_MeshNE = .not.ESMF_MeshEQ(mesh1, mesh2)

  end function ESMF_MeshNE
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshAddElements()"
!BOP
! !IROUTINE: ESMF_MeshAddElements - Add elements to a Mesh \label{sec:mesh:api:meshaddelements}
!
! !INTERFACE:
    subroutine ESMF_MeshAddElements(mesh, elementIds, elementTypes, &
                 elementConn, rc)

!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)         :: mesh
    integer,         intent(in) 	   :: elementIds(:)
    integer,         intent(in) 	   :: elementTypes(:)
    integer,         intent(in) 	   :: elementConn(:)
    integer,         intent(out), optional :: rc
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
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [elementIds]
!          An array containing the global ids of the elements to be created on this PET. 
!          This input consists of a 1D array the size of the number of elements on this PET.
!   \item[elementTypes] 
!          An array containing the types of the elements to be created on this PET. The types used
!          must be appropriate for the parametric dimension of the Mesh. Please see
!          Section~\ref{const:meshelemtype} for the list of options. This input consists of 
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
!         matters. Please see Section~\ref{const:meshelemtype} for diagrams illustrating
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
    type(ESMF_RegridConserve) :: lregridConserve

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! Handle optional conserve argument
! Passing the regridConserve flag into add elements is a fix for source masking
! with patch interpolation.  Right now the Regrid does not support source masking
! on a Mesh.  So this option is defaulting to CONSERVE_ON to make things more simple
! because of the mulitple entry points when creating a Mesh.  When this option is
! added to the pulic API, (when mesh source masking is enabled) this flag will
! need to be defaulted to CONSERVE_OFF, and documentation should be added to tell
! the users that if they are doing conservative with a mesh created in the three
! step process they should make sure to turn CONSERVE_ON in the add element call.

!    if (present(regridConserve)) then
!       lregridConserve=regridConserve
!    else
       lregridConserve=ESMF_REGRID_CONSERVE_ON
!    endif

    ! If we're at the wrong stage then complain
    if (mesh%createStage .ne. 2) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- MeshAddNodes() should be called before this", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    


    num_elems = size(elementIds)
    num_elementConn = size(elementConn)
    call C_ESMC_MeshAddElements(mesh%this, num_elems, &
                             elementIds, elementTypes, &
                             num_elementConn, elementConn, &
                             lregridConserve%regridconserve, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! We create two dist grids, one for nodal one for element
    call C_ESMC_MeshCreateDistGrids(mesh%this, mesh%nodal_distgrid, &
                      mesh%element_distgrid, &
                      mesh%numOwnedNodes, mesh%numOwnedElements, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    !call ESMF_DistGridPrint(mesh%nodal_distgrid)
    !ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, mesh%nodal_distgrid, rc)

    ! Go to next stage 
    mesh%createStage=3

    ! Set as fully created 
    mesh%hasSplitElem=.false.

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
    type(ESMF_Mesh),    intent(inout)         :: mesh
    integer,            intent(in)            :: nodeIds(:)
    real(ESMF_KIND_R8), intent(in)            :: nodeCoords(:)
    integer,            intent(in)            :: nodeOwners(:)
    integer,            intent(out), optional :: rc
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
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If we're at the wrong stage then complain
    if (mesh%createStage .ne. 1) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- MeshAddNodes() should be called before this", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    num_nodes = size(nodeIds)
    call C_ESMC_MeshAddNodes(mesh%this, num_nodes, nodeIds, nodeCoords, &
                             nodeOwners, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
!   This call is {\em collective} across the current VM.
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! The C side has been created
    ESMF_MeshCreate3Part%isCMeshFreed=.false.

    ! Go to next stage 
    ESMF_MeshCreate3Part%createStage=1

    ! Set as not yet created
    ESMF_MeshCreate3Part%isFullyCreated=.false.

    ! Set dimension information
    ESMF_MeshCreate3Part%spatialDim=spatialDim
    ESMF_MeshCreate3Part%parametricDim=parametricDim

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
    type(ESMF_Mesh)                           :: ESMF_MeshCreate1Part
! !ARGUMENTS:
    integer,            intent(in)            :: parametricDim
    integer,            intent(in)            :: spatialDim
    integer,            intent(in)            :: nodeIds(:)
    real(ESMF_KIND_R8), intent(in)            :: nodeCoords(:)
    integer,            intent(in) 	      :: nodeOwners(:)
    integer,            intent(in) 	      :: elementIds(:)
    integer,            intent(in) 	      :: elementTypes(:)
    integer,            intent(in) 	      :: elementConn(:)
    integer,            intent(out), optional :: rc
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
!   This call is {\em collective} across the current VM.
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
!          Section~\ref{const:meshelemtype} for the list of options. This input consists of 
!          a 1D array the size of the number of elements on this PET.  
!   \item[elementConn] 
!         An array containing the indexes of the sets of nodes to be connected together to form the
!         elements to be created on this PET. The entries in this list are NOT node global ids, 
!         but rather each entry is a local index (1 based) into the list of nodes to be 
!         created on this PET by this call.
!         In other words, an entry of 1 indicates that this element contains the node
!         described by {\tt nodeIds(1)}, {\tt nodeCoords(1)}, etc. on this PET. It is also
!         important to note that the order of the nodes in an element connectivity list
!         matters. Please see Section~\ref{const:meshelemtype} for diagrams illustrating
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
    type(ESMF_RegridConserve) :: lregridConserve

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_MeshCreate1Part%this = ESMF_NULL_POINTER

    ! Handle optional conserve argument
! Passing the regridConserve flag into add elements is a fix for source masking
! with patch interpolation.  Right now the Regrid does not support source masking
! on a Mesh.  So this option is defaulting to CONSERVE_ON to make things more simple
! because of the mulitple entry points when creating a Mesh.  When this option is
! added to the pulic API, (when mesh source masking is enabled) this flag will
! need to be defaulted to CONSERVE_OFF, and documentation should be added to tell
! the users that if they are doing conservative with a mesh created in the three
! step process they should make sure to turn CONSERVE_ON in the add element call.

!    if (present(regridConserve)) then
!       lregridConserve=regridConserve
!    else
       lregridConserve=ESMF_REGRID_CONSERVE_ON
!    endif

    call c_ESMC_meshcreate(ESMF_MeshCreate1Part%this, parametricDim, spatialDim, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init status of arguments
    ESMF_INIT_SET_CREATED(ESMF_MeshCreate1Part)

    ! Add the nodes
    num_nodes = size(nodeIds)
    call C_ESMC_MeshAddNodes(ESMF_MeshCreate1Part%this, num_nodes, nodeIds, nodeCoords, &
                             nodeOwners, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    num_elems = size(elementTypes)
    num_elementConn = size(elementConn)
    call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, num_elems, &
                             elementIds, elementTypes, &
                             num_elementConn, elementConn, &
                             lregridConserve%regridconserve, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! We create two dist grids, one for nodal one for element
    call C_ESMC_MeshCreateDistGrids(ESMF_MeshCreate1Part%this, ESMF_MeshCreate1Part%nodal_distgrid, &
                      ESMF_MeshCreate1Part%element_distgrid, &
                      ESMF_MeshCreate1Part%numOwnedNodes, ESMF_MeshCreate1Part%numOwnedElements, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! The C side has been created
    ESMF_MeshCreate1Part%isCMeshFreed=.false.

    ! Can't happen here
    ESMF_MeshCreate1Part%hasSplitElem=.false.

    ! Set as fully created 
    ESMF_MeshCreate1Part%isFullyCreated=.true.

    ! Set dimension information
    ESMF_MeshCreate1Part%spatialDim=spatialDim
    ESMF_MeshCreate1Part%parametricDim=parametricDim

    !call ESMF_DistGridPrint(ESMF_MeshCreate1Part%nodal_distgrid)
    !ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, ESMF_MeshCreate1Part%nodal_distgrid, rc)

    if (present (rc)) rc = localrc
    
  end function ESMF_MeshCreate1Part
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromFile()"
!BOP
! !IROUTINE: ESMF_MeshCreate - Create a Mesh from a file
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreateFromFile(filename, filetypeflag, convert3D, &
                 convertToDual, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromFile
! !ARGUMENTS:
    character(len=*),           intent(in)            :: filename
    type(ESMF_FileFormat_Flag), intent(in)            :: filetypeflag
    logical,                    intent(in),  optional :: convert3D
    logical,                    intent(in),  optional :: convertToDual
    integer,                    intent(out), optional :: rc
! 
! !DESCRIPTION:
!   Create a Mesh from a file. Provides options to convert to 3D and in the case of SCRIP
!   format files, allows the dual of the mesh to be created. 
!
!   This call is {\em collective} across the current VM.
!
!   \begin{description}
!   \item [filename]
!         The name of the grid file
!   \item[filetypeflag] 
!         The file type of the grid file to be read, please see Section~\ref{const:fileformat}
!         for a list of valid options. 
!   \item[convert3D] 
!         if TRUE, the node coordinates will be converted into 3D Cartisian, which
!         is required for a global grid
!   \item[convertToDual] 
!         if TRUE, the mesh will be converted to its dual. If not specified,
!         defaults to true. Converting to dual is not supported with
!         file type {\tt ESMF\_FILEFORMAT\_ESMFMESH}, so when using that file type
!         this parameter has no effect.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    logical::  localConvert3D      ! local flag
    logical::  localConvertToDual      ! local flag
    integer::  localrc

    ! Set Defaults
    if (present(convert3D)) then
	localConvert3D = convert3D
    else
	localConvert3D = .false.
    endif

    if (present(convertToDual)) then
	localConvertToDual = convertToDual
    else
	localConvertToDual = .true.
    endif


    if (filetypeflag .eq. ESMF_FILEFORMAT_SCRIP) then
	ESMF_MeshCreateFromFile = ESMF_MeshCreateFromScrip(filename, localConvert3D, &
          localConvertToDual, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (filetypeflag .eq. ESMF_FILEFORMAT_ESMFMESH) then
	ESMF_MeshCreateFromFile = ESMF_MeshCreateFromUnstruct(filename, localConvert3D, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
    else
       call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
                 msg="- the filetypeflag has to be either ESMF_FILEFORMAT_ESMFMESH or ESMF_FILEFORMAT_SCRIP", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return
    endif

    if (present(rc)) rc=ESMF_SUCCESS
    return
end function ESMF_MeshCreateFromFile
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromUnstruct()"
!BOPI
! !IROUTINE: ESMF_MeshCreate - Create a Mesh from a grid file defined in the ESMF Unstructured
!  Grid format
!  Create a triangle and quad mesh using a global node coordinate table and a distributed
! element connection array
!  arguments:  VertexCoords(3, NodeCnt), where NodeCnt is the total node count for the
!  global mesh, the first dimension stores the x,y,z coordinates of the node
!              CellConnect(4, ElemCnt), where ElemCnt is the total number of elements
!                The first dimension contains the global node IDs at the four corner of the element 
!              StartCell, the first Element ID in this PET
!  in this local PET.  Note the CellConnect is local and VertexCoords is global.
!  In this routine, we have to figure out which nodes are used by the local Elements 
!  and who are the owners of the local nodes, then add the local nodes and elements into
!  the mesh
! !INTERFACE:
! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreateFromUnstruct(filename, convert3D, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromUnstruct
! !ARGUMENTS:
    character(len=*), intent(in)              :: filename
    logical, intent(in)                       :: convert3D
    integer, intent(out), optional            :: rc
!
! !DESCRIPTION:
!   Create a mesh from a grid file defined in the ESMF Unstructured grid format.
!
!   \begin{description}
!   \item [filename]
!         The name of the grid file
!   \item[convert3D] 
!         if TRUE, the node coordinates will be converted into 3D Cartisian, which
!         is required for a global grid
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                             :: localrc      ! local return code
    integer			        :: PetNo, PetCnt 
    real(ESMF_KIND_R8),pointer          :: nodeCoords(:,:)
    integer(ESMF_KIND_I4),pointer       :: elementConn(:,:)
    integer(ESMF_KIND_I4),pointer       :: elmtNum(:)
    integer                             :: startElmt
    integer                             :: NodeNo
    integer                             :: NodeCnt, NodeDim, total
    integer, allocatable                :: NodeId(:)
    integer, allocatable                :: NodeUsed(:)
    real(ESMF_KIND_R8), allocatable     :: NodeCoords1D(:)
    real(ESMF_KIND_R8)                  :: coorX, coorY, deg2rad
    integer, allocatable                :: NodeOwners(:)
    integer, allocatable                :: NodeOwners1(:)

    integer                             :: ElemNo, TotalElements
    integer                             :: ElemCnt,i,j,k,dim
    integer				:: localNodes, myStartElmt
    integer                             :: ConnNo, TotalConnects
    integer, allocatable                :: ElemId(:)
    integer, allocatable                :: ElemType(:)
    integer, allocatable                :: ElemConn(:)
    integer, allocatable                :: LocalElmTable(:)
    integer                             :: sndBuf(1)
    type(ESMF_VM)                       :: vm
    type(ESMF_Mesh)                     :: Mesh
    integer(ESMF_KIND_I4)               :: localSplitElems(1)
    integer(ESMF_KIND_I4)               :: globalSplitElems(1)
    logical                             :: existSplitElems


    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! get global vm information
    !
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Read the mesh definition from the file
    call ESMF_GetMeshFromFile(filename, nodeCoords, elementConn, elmtNum, startElmt, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return

    ! Total number of nodes for the global mesh
    NodeCnt = ubound (nodeCoords, 2)
    NodeDim  = ubound (nodeCoords, 1)
    deg2rad = 3.141592653589793238/180;

    ! create the mesh
    ! The spatialDim=3, we need to convert the 2D coordinate into 3D Cartisian in order
    ! to handle the periodic longitude correctly
    if (convert3D) then
        Mesh = ESMF_MeshCreate3part (2, 3, localrc)
    else
        Mesh = ESMF_MeshCreate3part (2, NodeDim, localrc)
    end if
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! These two arrays are temp arrays
    ! NodeUsed() used for multiple purposes, first, find the owners of the node
    ! later, used to store the local Node ID to be used in the ElmtConn table
    ! NodeOwners1() is the receiving array for ESMF_VMAllReduce(), it will store
    ! the lowest PET number that stores the node.  That PET will become the owner
    ! of the node.
    allocate (NodeUsed(NodeCnt))
    allocate (NodeOwners1(NodeCnt))

    ! Set to a number > PetCnt because it will store the PetNo if this node is used by
    ! the local elements and we will do a global reduce to find the minimal values
    NodeUsed(:)=PetCnt+100

    ! Total number of local elements
    ElemCnt = ubound (elementConn, 2)

    ! Set the coorsponding NodeUsed(:) value to my PetNo if it is used by the local element 
    ! Also calculate the total number of mesh elements based on elmtNum
    ! if elmtNum == 3 or 4, no change, if elmtNum > 4, break it into elmtNum-2 triangles
    totalElements = ElemCnt
    totalConnects = 0
    do ElemNo =1, ElemCnt
        do i=1,elmtNum(ElemNo)	
            NodeUsed(elementConn(i,ElemNo))=PetNo
        enddo
	if (elmtNum(ElemNo) > 4) TotalElements = TotalElements + (elmtNum(ElemNo)-3)
	if (elmtNum(ElemNo) <= 4) then
           TotalConnects = TotalConnects+elmtNum(ElemNo)
        else
	   TotalConnects = TotalConnects+3*(elmtNum(ElemNo)-2)
        end if
    end do

    ! Do a global reduce to find out the lowest PET No that owns each node, the result is in
    ! NodeOwners1(:) 
    call ESMF_VMAllReduce(vm, NodeUsed, NodeOwners1, NodeCnt, ESMF_REDUCE_MIN, rc=localrc)
   if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    

    ! count number of nodes used and convert NodeUsed values into local index
    localNodes = 0
    do NodeNo = 1, NodeCnt
       if (NodeUsed(NodeNo) == PetNo) then
         localNodes = localNodes+1
	 NodeUsed(NodeNo) = localNodes
       else
	 NodeUsed(NodeNo) = 0
       endif
    enddo

    ! allocate nodes arrays for ESMF_MeshAddNodes()
    allocate (NodeId(localNodes))
    if (convert3D) then
       allocate(NodeCoords1D(localNodes*3))
    else
       allocate (NodeCoords1D(localNodes*NodeDim))
    endif
    allocate (NodeOwners(localNodes))

    ! copy vertex information into nodes, NodeUsed(:) now contains either 0 (not for me) or
    ! the local node index.  The owner of the node is stored in NodeOwners1(:)
    ! Also calculate how many nodes are "owned" by me -- total
    i = 1
    total = 0
    do NodeNo = 1, NodeCnt
        if (NodeUsed(NodeNo) > 0) then
	   NodeId     (i) = NodeNo     
	   if (convert3D) then
              coorX = nodeCoords(1,NodeNo)*deg2rad
              coorY = (90.0-nodeCoords(2,NodeNo))*deg2rad
              NodeCoords1D((i-1)*3+1) = COS(coorX)*SIN(coorY)             
              NodeCoords1D((i-1)*3+2) = SIN(coorX)*SIN(coorY)             
              NodeCoords1D((i-1)*3+3) = COS(coorY)
           !   write (*,'(6F8.4)')nodeCoords(:,NodeNo), COS(coorX),SIN(coorX),COS(coorY),SIN(coorY)
           else 
             do dim = 1, NodeDim
               NodeCoords1D ((i-1)*NodeDim+dim) = nodeCoords (dim, NodeNo)
	     end do
           endif
           NodeOwners (i) = NodeOwners1(NodeNo)
	   if (NodeOwners1(NodeNo) == PetNo) total = total+1
           i = i+1
        endif
    end do

    ! Add nodes
    call ESMF_MeshAddNodes (Mesh, NodeId, NodeCoords1D, NodeOwners, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! Need to calculate the total number of ESMF_MESH objects and the start element ID
    ! Do a global gather to get all the local TotalElements

    allocate(localElmTable(PetCnt))
    sndBuf(1)=TotalElements
    call ESMF_VMAllGather(vm, sndBuf, localElmTable, 1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    
    ! Find out the start element ID
    myStartElmt=0
    do i=1,PetNo
      myStartElmt = myStartElmt+localElmTable(i)
    end do
    deallocate(localElmTable)

    ! print *, PetNo, ' My start cell and total local ', myStartElmt, TotalElements, TotalConnects

    ! allocate element arrays for the local elements
    allocate (ElemId(TotalElements))
    allocate (ElemType(TotalElements))
    allocate (ElemConn(TotalConnects))

    ! figure out if there are split elements globally
    !! Fake logical allreduce .or. with MAX
    if (totalElements .gt. ElemCnt) then
       localSplitElems(1)=1
    else 
       localSplitElems(1)=0
    endif
    call ESMF_VMAllReduce(vm, localSplitElems, globalSplitElems, 1, ESMF_REDUCE_MAX, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return    
    if (globalSplitElems(1) .eq. 1) then
       existSplitElems=.true.
    else 
       existSplitElems=.false.
    endif


    ! Set split element info
    if (existSplitElems) then    
       Mesh%hasSplitElem=.true.
       allocate(mesh%splitElemMap(TotalElements))
       Mesh%splitElemStart=myStartElmt+1  ! position of first element
       Mesh%splitElemCount=TotalElements
       Mesh%origElemStart=startElmt  ! position of first element
       Mesh%origElemCount=ElemCnt
    endif

    ! the node number is 0 based, need to change it to 1 based
    ! The ElemId is the global ID.  The myStartElmt is the starting Element ID(-1), and the
    ! element IDs will be from startElmt to startElmt+ElemCnt-1
    ! The ElemConn() contains the four corner node IDs for each element and it is organized
    ! as a 1D array.  The node IDs are "local", which are stored in NodeUsed(:)
    ElemNo = 1
    ConnNo = 0
    do j = 1, ElemCnt
	if (elmtNum(j)==3) then        
           ElemId(ElemNo) = myStartElmt+ElemNo
           ElemType (ElemNo) = ESMF_MESHELEMTYPE_TRI
           do i=1,3
              ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
	   end do
           if (existSplitElems) Mesh%splitElemMap(ElemNo)=j+startElmt-1
           ElemNo=ElemNo+1
           ConnNo=ConnNo+3
	elseif (elmtNum(j)==4) then
           ElemId(ElemNo) = myStartElmt+ElemNo
           ElemType (ElemNo) = ESMF_MESHELEMTYPE_QUAD
           do i=1,4
              ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
	   end do
           if (existSplitElems) Mesh%splitElemMap(ElemNo)=j+startElmt-1
           ElemNo=ElemNo+1
	   ConnNo=ConnNo+4
	else
	! elmtNum(j) > 4, break into elmtNum(j)-2 triangles
	   do k=0,elmtNum(j)-3
             ElemId(ElemNo)=myStartElmt+ElemNo
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_TRI
             ElemConn (ConnNo+1) = NodeUsed(elementConn(1,j))
             ElemConn (ConnNo+2) = NodeUsed(elementConn(2+k,j))
             ElemConn (ConnNo+3) = NodeUsed(elementConn(3+k,j))
             if (existSplitElems) Mesh%splitElemMap(ElemNo)=j+startElmt-1
             ElemNo=ElemNo+1
	     ConnNo=ConnNo+3
	   end do
         end if
    end do
    
    if (ElemNo /= TotalElements+1) then
	write (ESMF_UtilIOStdout,*)  &
            PetNo, ' TotalElements does not match ',ElemNo-1, TotalElements
    end if

    ! Add elements
    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! NEED TO SET THIS HERE, BECAUSE MEshAddElements sets it to false
    if (existSplitElems) then
       Mesh%hasSplitElem=.true.
    else
       Mesh%hasSplitElem=.false.
    endif

    deallocate(NodeUsed, NodeId, NodeCoords1D, NodeOwners, NodeOwners1)
    deallocate(ElemId, ElemType, ElemConn)

    ESMF_MeshCreateFromUnstruct = Mesh

    if (present(rc)) rc=ESMF_SUCCESS
    return
end function ESMF_MeshCreateFromUnstruct
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromScrip()"
!BOPI
! !IROUTINE: ESMF_MeshCreateFrom Scrip - called from ESMF_MeshCreateFromFile
!   Create a mesh from a unstructured grid defined in a SCRIP file
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreateFromScrip(filename, convert3D, convertToDual, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromScrip
! !ARGUMENTS:
    character(len=*), intent(in)              :: filename
    logical, intent(in)                       :: convert3D
    logical, intent(in), optional             :: convertToDual
    integer, intent(out), optional            :: rc
!
! !DESCRIPTION:
!   Create a mesh from a grid file defined in SCRIP format or in ESMF Unstructured grid format.
!
!   \begin{description}
!   \item [filename]
!         The name of the grid file
!   \item[convert3D] 
!         if TRUE, the node coordinates will be converted into 3D Cartisian, which
!         is required for a global grid
!   \item[convertToDual] 
!         if TRUE, the mesh will be converted to it's dual. If not specified,
!         defaults to true. 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    character(len=128)      :: cmd, esmffilename
    integer   	            :: PetNo, PetCnt 
    integer                 :: scrip_file_len, esmf_file_len
    type(ESMF_VM)           :: vm
    integer                 :: dualflag
    integer                 :: unit
    logical                 :: notavail

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Default convert to dual
    if (present(convertToDual)) then
       if (convertToDual) then
          dualflag=1
       else
          dualflag=0
       endif
    else
      dualflag=1
    endif

    ! get global vm information
    !
    call ESMF_VMGetGlobal(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    esmffilename = ".esmf.nc"
    
    if (PetNo == 0) then
        ! this is a serial call into C code for now
        scrip_file_len = len_trim(filename)
        esmf_file_len = len_trim(esmffilename)
        call c_ConvertSCRIP(filename, scrip_file_len, &
          esmffilename, esmf_file_len, dualflag, localrc )
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    call ESMF_VMBarrier(vm)
    ESMF_MeshCreateFromScrip=ESMF_MeshCreateFromUnstruct(esmffilename,convert3D, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    if (PetNo == 0) then
!      system() is not available on some of the compilers, use open/close to
!      delete the file instead
!      write(cmd, '("/bin/rm ",A)') trim(esmffilename)
!      call system(cmd)
!      First find an available unit numer
       call ESMF_UtilIOUnitGet(unit, rc=rc)
       if (rc==ESMF_SUCCESS) then
  	  open(unit, FILE=esmffilename,status='unknown')
          close (unit, STATUS='delete')
       endif
    endif    

    if (present(rc)) rc=ESMF_SUCCESS
    return
end function ESMF_MeshCreateFromScrip
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
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! The C side has been created
    ESMF_MeshCreateFromPointer%isCMeshFreed=.false.

    ! Set as fully created 
    ESMF_MeshCreateFromPointer%hasSplitElem=.false.

    ! Set as fully created 
    ESMF_MeshCreateFromPointer%isFullyCreated=.true.

    if(present(rc)) rc = ESMF_SUCCESS

  end function ESMF_MeshCreateFromPointer
!------------------------------------------------------------------------------

! -----------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshDestroy"
!BOP
! !IROUTINE: ESMF_MeshDestroy - Release resources associated with a Mesh
!
! !INTERFACE:
      subroutine ESMF_MeshDestroy(mesh, keywordenforcer, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)          :: mesh
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,         intent(out),  optional :: rc
!
! !STATUS:
! \apiStatusCompatible
!
! !DESCRIPTION:
!  This call removes internal memory associated with {\tt mesh}. 
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
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

         ! destroy element distgrid          
         call ESMF_DistgridDestroy(mesh%element_distgrid, rc=localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
              ESMF_CONTEXT, rcToReturn=rc)) return

         ! Get rid of split element map
         if (mesh%hasSplitElem) then
            deallocate(mesh%splitElemMap)
         endif

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
      subroutine ESMF_MeshGet(mesh, parametricDim, spatialDim, &
                   nodalDistgrid, elementDistgrid, &
                   numOwnedNodes, ownedNodeCoords, &
                   numOwnedElements, isMemFreed, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
    type(ESMF_Mesh),     intent(inout)         :: mesh
    integer,             intent(out), optional :: parametricDim
    integer,             intent(out), optional :: spatialDim
    type(ESMF_DistGrid), intent(out), optional :: nodalDistgrid
    type(ESMF_DistGrid), intent(out), optional :: elementDistgrid
    integer,             intent(out), optional :: numOwnedNodes
    real(ESMF_KIND_R8),  intent(out), optional :: ownedNodeCoords(:)
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
! \item [{[parametricDim]}]
! Dimension of the topology of the Mesh. (E.g. a mesh constructed of squares would
! have a parametric dimension of 2, whereas a Mesh constructed of cubes would have one
! of 3.)
! \item[{[spatialDim]}] 
! The number of coordinate dimensions needed to describe the locations of the nodes 
! making up the Mesh. For a manifold, the spatial dimesion can be larger than the 
! parametric dim (e.g. the 2D surface of a sphere in 3D space), but it can't be smaller. 
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
! \item [{[ownedNodeCoords]}]
! The coordinates for the local nodes. These coordinates will be in the proper order to correspond
! with the nodes in the {\tt nodalDistgrid} returned by this call, and hence with a Field built on 
! {\tt mesh}. The size of the input array should be the spatial dim of {\tt mesh} times 
! {\tt numOwnedNodes}.
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
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    if (present(ownedNodeCoords)) then
       ! If mesh has been freed then exit
       if (mesh%isCMeshFreed) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                msg="- the mesh internals have been freed", & 
                ESMF_CONTEXT, rcToReturn=rc) 
          return
       endif

       ! Check array size
       if (size(ownedNodeCoords)<mesh%numOwnedNodes*mesh%spatialDim) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
                msg="- owndedNodeCoords too small to hold coordinates", & 
                ESMF_CONTEXT, rcToReturn=rc) 
          return
       endif

       ! Get coords from C
       call C_ESMC_GetLocalCoords(mesh, ownedNodeCoords, localrc) 
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    endif

      if (present(parametricDim)) parametricDim=mesh%parametricDim
      if (present(spatialDim)) spatialDim=mesh%spatialDim
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
    type(ESMF_Mesh),  intent(in)             :: mesh1
    type(ESMF_Mesh),  intent(in)             :: mesh2
    integer,          intent(out),  optional :: rc  
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
    integer                       :: localrc      ! local return code
    type(ESMF_DistGridMatch_Flag) :: matchResultNode, matchResultElem

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! init to one setting in case of error
    ESMF_MeshMatch = .false.
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh1, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh2, rc)
    
    ! If meshes have not been fully created
    if (.not. mesh1%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif        

    if (.not. mesh2%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif        

    ! For now just make match to mean that the Mesh's have the same distgrids because that's
    ! all the fields care about
    matchResultNode=ESMF_DistGridMatch(mesh1%nodal_distgrid, mesh2%nodal_distgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    matchResultElem=ESMF_DistGridMatch(mesh1%element_distgrid, mesh2%element_distgrid, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! return successfully
    if ((matchResultNode >= ESMF_DISTGRIDMATCH_EXACT) .and. &
      (matchResultElem >= ESMF_DISTGRIDMATCH_EXACT)) then
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
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
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
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

     ! Serialize Element Distgrid
     call c_ESMC_DistgridSerialize(mesh%element_distgrid, buffer, length, offset, &
                                 linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! Convert logicals to ints
      if (mesh%isCMeshFreed) then
        intMeshFreed=1
      else
        intMeshFreed=0
      endif

      ! Serialize other Mesh items
      call c_ESMC_MeshInfoSerialize(intMeshFreed, &
              buffer, length, offset,linquireflag, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! If exists serialize mesh
      if (.not. mesh%isCMeshFreed) then
         call c_ESMC_MeshSerialize(mesh%this, buffer, length, offset, &
                                 linquireflag, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
     if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_DistGridSetInitCreated(ESMF_MeshDeserialize%nodal_distgrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

     ! Deserialize element Distgrid
     call c_ESMC_DistGridDeserialize(ESMF_MeshDeserialize%element_distgrid, buffer, offset, localrc)
     if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      call ESMF_DistGridSetInitCreated(ESMF_MeshDeserialize%element_distgrid, rc=localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! Deserialize other ESMF_MeshDeserialize items
      call c_ESMC_MeshInfoDeserialize(intMeshFreed, buffer, offset, localrc)
      if (ESMF_LogFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rcToReturn=rc)) return

      ! Convert ints to logicals
      if (intMeshFreed .eq. 1) then
         ESMF_MeshDeserialize%isCMeshFreed=.true.
      else
         ESMF_MeshDeserialize%isCMeshFreed=.false.
      endif

      ! Set values who's values are implied by the fact 
      ! that this is a proxy mesh
      ESMF_MeshDeserialize%hasSplitElem=.false.
      ESMF_MeshDeserialize%isFullyCreated=.true.
      ESMF_MeshDeserialize%createStage=3
      ESMF_MeshDeserialize%numOwnedNodes=0
      ESMF_MeshDeserialize%numOwnedElements=0

      ! If exists serialize mesh
      if (.not. ESMF_MeshDeserialize%isCMeshFreed) then
         call c_ESMC_MeshDeserialize(ESMF_MeshDeserialize%this, buffer, &
                                     offset, localrc)
         if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    call C_ESMC_MeshWrite(mesh%this, filename, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
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
    subroutine ESMF_MeshFindPnt(mesh, unmappedaction, &
                                pntDim, pntCount, pntList, &
                                petList, rc)


!
! !ARGUMENTS:
    type(ESMF_Mesh),                intent(in)           :: mesh
    type(ESMF_UnmappedAction_Flag), intent(in), optional :: unmappedaction
    integer,                        intent(in)           :: pntDim
    integer,                        intent(in)           :: pntCount
    real(ESMF_KIND_R8), pointer                          :: pntList(:)
    integer,            pointer                          :: petList(:)
    integer,                        intent(out),optional :: rc
!
! !DESCRIPTION:
!   Write a mesh to VTK file.
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item [{[unmappedaction]}]
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
    type(ESMF_UnmappedAction_Flag) :: localunmappedaction

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

   ! Set default vale for unmappedaction
   if (present(unmappedaction)) then
      localunmappedaction=unmappedaction
   else
      localunmappedaction=ESMF_UNMAPPEDACTION_ERROR
   endif

   ! Call into mesh find point subroutine
   ! TODO: ADD GIDS TO THIS INTERFACE AS THEY'RE AVAILABLE FROM C++ METHOD
    call C_ESMC_MeshFindPnt(mesh%this, localunmappedaction, pntDim, pntCount, &
			    pntList, petList, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! return success
     if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_MeshFindPnt
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetElemArea()"
!BOPI
! !IROUTINE: ESMF_MeshGetElemArea - Find area of elements in mesh
!
! !INTERFACE:
    subroutine ESMF_MeshGetElemArea(mesh, areaList, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                     :: mesh
    real(ESMF_KIND_R8), pointer                     :: areaList(:)
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   Write a mesh to VTK file.
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item [areaList]
!         Areas for the mesh elements will be put here
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
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

   ! Call into mesh get areas
    call C_ESMC_MeshGetArea(mesh%this, size(areaList), areaList, localrc);
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! return success
     if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_MeshGetElemArea
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetOrigElemArea()"
!BOPI
! !IROUTINE: ESMF_MeshGetOrigElemArea - Find area of elements in mesh
!
! !INTERFACE:
    subroutine ESMF_MeshGetOrigElemArea(mesh, areaList, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                     :: mesh
    real(ESMF_KIND_R8), pointer                     :: areaList(:)
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   For a Mesh with split elements, get the area of the original 
!   unsplit element. If not split elements then just get Mesh areas.
!  
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item [areaList]
!         Areas for the mesh elements will be put here
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                             :: localrc      ! local return code
    real(ESMF_KIND_R8), pointer         :: splitAreaList(:)
    integer                             :: i,m

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh doesn't have split elements then just get
    ! current areas
    if (.not. mesh%hasSplitElem) then
       call ESMF_MeshGetElemArea(mesh, areaList, rc=localrc)    
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
       if  (present(rc)) rc = ESMF_SUCCESS
       return 
    endif    


    ! check size of areaList
    if (size(areaList) .lt. mesh%origElemCount) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- area list too small to hold element areas", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! Allocate array to hold split areas
    allocate(splitAreaList(mesh%splitElemCount))

    ! Get split areas
    call ESMF_MeshGetElemArea(mesh, splitAreaList, rc=localrc)    
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Put the areas back together
    areaList=0.0_ESMF_KIND_R8
    do i=1,mesh%splitElemCount
       m=mesh%splitElemMap(i)-mesh%origElemStart+1
       areaList(m)=areaList(m)+splitAreaList(i)
    enddo

    
    
    ! Allocate array to hold split areas
    deallocate(splitAreaList)

    ! return success
     if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_MeshGetOrigElemArea
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetElemFrac()"
!BOPI
! !IROUTINE: ESMF_MeshGetElemFrac - Get frac of elements in mesh
!
! !INTERFACE:
    subroutine ESMF_MeshGetElemFrac(mesh, fracList, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                     :: mesh
    real(ESMF_KIND_R8), pointer                     :: fracList(:)
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   Write a mesh to VTK file.
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item [fracList]
!         Fractions for the mesh elements will be put here
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
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

   ! Call into mesh get areas
    call C_ESMC_MeshGetFrac(mesh%this, size(fracList), fracList, localrc);
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! return success
     if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_MeshGetElemFrac
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetOrigElemFrac()"
!BOPI
! !IROUTINE: ESMF_MeshGetOrigElemFrac - Find Frac of elements in mesh
!
! !INTERFACE:
    subroutine ESMF_MeshGetOrigElemFrac(mesh, splitFracList,origfracList, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                     :: mesh
    real(ESMF_KIND_R8), pointer                     :: splitFracList(:)
    real(ESMF_KIND_R8), pointer                     :: origfracList(:)
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   For a Mesh with split elements, get the area of the original 
!   unsplit element. If not split elements then just get Mesh areas.
!  
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item [fracList]
!         Fractions for the mesh elements will be put here
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                             :: localrc      ! local return code
    real(ESMF_KIND_R8), pointer         :: splitAreaList(:)
    real(ESMF_KIND_R8), pointer         :: origAreaList(:)
    integer                             :: i,m

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    


    ! check size of fracList
    if (size(origfracList) .lt. mesh%origElemCount) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
                 msg="- frac list too small to hold element fracs", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! check size of fracList
    if (size(splitfracList) .lt. mesh%splitElemCount) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
                 msg="- frac list too small to hold element fracs", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh doesn't have split elements then just copy
    ! split fractions
    if (.not. mesh%hasSplitElem) then
       if (size(splitFracList) .ne. size(origFracList)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_SIZE, & 
               msg="- no solit elements so frac list sizes should be the same", & 
               ESMF_CONTEXT, rcToReturn=rc) 
          return 
       endif
       origFracList(:)=splitFracList(:)
       return 
    endif    


    ! Allocate array to hold split areas
    allocate(splitAreaList(mesh%splitElemCount))
    allocate(origAreaList(mesh%origElemCount))

    ! Get split areas
    call ESMF_MeshGetElemArea(mesh, splitAreaList, rc=localrc)    
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Put the Fracs back together
    ! TODO: MIGHT MAKE SENSE TO HAVE ONE INTERFACE 
    ! THAT DOES AREA AND FRAC, SO WE DON'T REPEAT THE AREA CALC.
    origFracList=0.0_ESMF_KIND_R8
    origAreaList=0.0_ESMF_KIND_R8
    do i=1,mesh%splitElemCount
       m=mesh%splitElemMap(i)-mesh%origElemStart+1
       origFracList(m)=origFracList(m)+splitAreaList(i)*splitFracList(i)
       origAreaList(m)=origAreaList(m)+splitAreaList(i)
    enddo

    ! Normalize by the area again
    do i=1,mesh%origElemCount
       origFracList(i)=origFracList(i)/origAreaList(i)
    enddo

    ! Allocate array to hold split areas
    deallocate(splitAreaList)
    deallocate(origAreaList)

    ! return success
     if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_MeshGetOrigElemFrac
!------------------------------------------------------------------------------

! -----------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetElemSplit"
!BOPI
! !IROUTINE: ESMF_MeshGetElemSplit - Get element split information from a Mesh
!
! !INTERFACE:
      subroutine ESMF_MeshGetElemSplit(mesh, hasSplitElem, splitElemStart, splitElemCount, &
               splitElemMap, origElemStart,origElemCount, rc)
!
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(inout)            :: mesh
    logical, intent(out), optional            :: hasSplitElem
    integer, intent(out),optional             :: splitElemStart
    integer, intent(out),optional             :: splitElemCount
    integer,  pointer, optional               :: splitElemMap(:)
    integer, intent(out),optional             :: origElemStart
    integer, intent(out),optional             :: origElemCount
    integer, intent(out), optional            :: rc

!
! !DESCRIPTION:
!   Get element split information from a mesh. Will complain if anything is asked
!   for involving splitElements if no split elements are present, except for hasSplitElem.
!   TODO: make this transparent to presence of splitElements
!
! The arguments are:
! \begin{description}
! \item [mesh]
! Mesh object to retrieve information from.
! \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
      integer  :: localrc
      localrc = ESMF_SUCCESS

      ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has not been fully created
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    


    if (present(hasSplitElem))   hasSplitElem=mesh%hasSplitElem

    if (mesh%hasSplitElem) then 
      if (present(splitElemStart)) splitElemStart=mesh%splitElemStart
      if (present(splitElemCount)) splitElemCount=mesh%splitElemCount
      if (present(origElemStart))  origElemStart=mesh%origElemStart
      if (present(origElemCount))  origElemCount=mesh%origElemCount    

      ! copy split Element Map
      if (present(splitElemMap)) then
         if (size(splitElemMap) .lt. size(mesh%splitElemMap)) then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- splitElemMap input parameter wrong size ", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
            return 
         endif            	
  
        splitElemMap(:)=mesh%splitElemMap(:)
      endif
    else
      if (present(splitElemStart) .or. present(splitElemCount) .or. &
          present(origElemStart)  .or. present(origElemCount) .or. &
          present(splitElemMap)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has no split elements", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
      endif 
    endif

    if (present(rc)) rc = localrc

    end subroutine ESMF_MeshGetElemSplit

!------------------------------------------------------------------------------


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshMergeSplitSrcInd()"
!BOPI
! !IROUTINE: ESMF_MeshMergeSplitSrcInd - Merge Split Source Sparse Mat Src Ind
!
! !INTERFACE:
    subroutine ESMF_MeshMergeSplitSrcInd(mesh, factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                     :: mesh
    integer(ESMF_KIND_I4), intent(inout)            :: factorIndexList(:,:)
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   For a Mesh with split elements, merge source factorIndexList
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item [factorIndexList]
!         factorIndexList in which the source will be merged
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer  :: localrc      ! local return code
    integer  :: indCount
    integer  :: i,m
    integer  :: localPet, petCount
    type(ESMF_VM) :: vm
    integer (ESMF_KIND_I4) :: localCount(1)
    integer (ESMF_KIND_I4),pointer :: globalCount(:),globalDispl(:)
    integer (ESMF_KIND_I4),pointer :: globalSplitElemMap(:)
    integer :: totalCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh doesn't have split elements then just leave
    if (.not. mesh%hasSplitElem) then
        if  (present(rc)) rc = ESMF_SUCCESS
       return
    endif    

    ! Get size of list
    indCount=size(factorIndexList,2)

    ! Get VM
    call ESMF_VMGetCurrent(vm,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get VM info
    call ESMF_VMGet(vm, localPet=localPet, &
           petcount=petCount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    !!! Need whole split list so allgather it !!!

    ! Allocate List of counts
    allocate(globalCount(petCount))

    ! Get List of counts
    localCount(1)=size(mesh%splitElemMap)
    call ESMF_VMAllGather(vm,localCount,globalCount,count=1,rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Calculate Displacements
    allocate(globalDispl(petCount))
    globalDispl(1)=0
    do i=2,petCount
        globalDispl(i)=globalDispl(i-1)+globalCount(i-1)
    enddo

    ! Sum size
    totalCount=0
    do i=1,petCount
       totalCount=totalCount+globalCount(i)
    enddo
  
    ! Allocate final area list
    allocate(globalSplitElemMap(totalCount))

    ! Gather all factorIndexList
    call ESMF_VMAllGatherV(vm,sendData=mesh%splitElemMap, sendCount=size(mesh%splitElemMap),&
         recvData=globalSplitElemMap,recvCounts=globalCount,recvOffsets=globalDispl,&
         rc=localrc)


    ! Get rid of helper variables
    deallocate(globalCount)
    deallocate(globalDispl)

    ! Loop processing factorIndexList
    do i=1,indCount
       factorIndexList(1,i)=globalSplitElemMap(factorIndexList(1,i))
    enddo

    ! Get rid of global Map
    deallocate(globalSplitElemMap)
    
    ! return success
     if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_MeshMergeSplitSrcInd
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshMergeSplitDstInd()"
!BOPI
! !IROUTINE: ESMF_MeshMergeSplitDstInd - Merge Split Sparse Mat Dst Ind
!
! !INTERFACE:
    subroutine ESMF_MeshMergeSplitDstInd(mesh, factorList, factorIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                     :: mesh
    real(ESMF_KIND_R8), intent(inout)               :: factorList(:) 
    integer(ESMF_KIND_I4),intent(inout)             :: factorIndexList(:,:)
    integer, intent(out), optional                  :: rc
!
! !DESCRIPTION:
!   For a Mesh with split elements, merge source factorIndexList
!
!   \begin{description}
!   \item [mesh]
!         The mesh.
!   \item [factorList]
!         factorList in which the dst will be merged
!   \item [factorIndexList]
!         factorIndexList in which the dst will be merged
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer  :: localrc      ! local return code
    integer  :: indCount
    integer  :: i,m, split_dst_id, orig_dst_id
    integer  :: split_dst_pos, orig_dst_pos

    real(ESMF_KIND_R8), pointer         :: origAreaList(:)
    real(ESMF_KIND_R8), pointer         :: splitAreaList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has been freed then exit
    if (mesh%isCMeshFreed) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh internals have been freed", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh has been freed then exit
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    ! If mesh doesn't have split elements then just leave
    if (.not. mesh%hasSplitElem) then
        if  (present(rc)) rc = ESMF_SUCCESS
       return
    endif    

    ! Get size of list
    indCount=size(factorIndexList,2)


    ! Allocate array to hold split areas
    allocate(splitAreaList(mesh%numOwnedElements))

    ! Get split areas
    call ESMF_MeshGetElemArea(mesh, splitAreaList, rc=localrc)    
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return   

    ! Allocate array to hold orig areas (before split)
    allocate(origAreaList(mesh%origElemCount))

    ! Get split areas
    call ESMF_MeshGetOrigElemArea(mesh, origAreaList, rc=localrc)    
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return   


    ! Loop changing factorIndexList and weights
    do i=1,indCount
       split_dst_id=factorIndexList(2,i)
       split_dst_pos=split_dst_id-mesh%splitElemStart+1
       orig_dst_id=mesh%splitElemMap(split_dst_pos)
       orig_dst_pos=orig_dst_id-mesh%origElemStart+1

!       write(*,*) i,"b:",factorIndexList(2,i),"a:",orig_dst_id,"  ", &
!                  (splitAreaList(split_dst_pos))/origAreaList(orig_dst_pos)                                       

       ! Set new index
       factorIndexList(2,i)=orig_dst_id

       ! Set new weight
       factorList(i)=(factorList(i)*splitAreaList(split_dst_pos))/ &
                      origAreaList(orig_dst_pos)                                       
    enddo


  ! Get rid of area lists
    deallocate(splitAreaList)
    deallocate(origAreaList)

    ! return success
    if  (present(rc)) rc = ESMF_SUCCESS

end subroutine ESMF_MeshMergeSplitDstInd

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FileFormatEqual"
!BOPI
! !IROUTINE: ESMF_FileFormatEqual - Equality of FileFormats
!
! !INTERFACE:
      function ESMF_FileFormatEqual(FileFormat1, FileFormat2)

! !RETURN VALUE:
      logical :: ESMF_FileFormatEqual

! !ARGUMENTS:

      type (ESMF_FileFormat_Flag), intent(in) :: &
         FileFormat1,      &! Two igrid statuses to compare for
         FileFormat2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF_FileFormat_Flag statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[FileFormat1, FileFormat2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_FileFormatEqual = (FileFormat1%fileformat == &
                              FileFormat2%fileformat)

      end function ESMF_FileFormatEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FileFormatNotEqual"
!BOPI
! !IROUTINE: ESMF_FileFormatNotEqual - Non-equality of FileFormats
!
! !INTERFACE:
      function ESMF_FileFormatNotEqual(FileFormat1, FileFormat2)

! !RETURN VALUE:
      logical :: ESMF_FileFormatNotEqual

! !ARGUMENTS:

      type (ESMF_FileFormat_Flag), intent(in) :: &
         FileFormat1,      &! Two FileFormatType Statuses to compare for
         FileFormat2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF_FileFormat_Flag statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[FileFormat1, FileFormat2]
!          Two statuses of FileFormats to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_FileFormatNotEqual = (FileFormat1%fileformat /= &
                                 FileFormat2%fileformat)

      end function ESMF_FileFormatNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshLocEqual"
!BOPI
! !IROUTINE: ESMF_MeshLocEqual - Equality of MeshLoc statuses
!
! !INTERFACE:
      function ESMF_MeshLocEqual(MeshLoc1, MeshLoc2)

! !RETURN VALUE:
      logical :: ESMF_MeshLocEqual

! !ARGUMENTS:

      type (ESMF_MeshLoc), intent(in) :: &
         MeshLoc1,      &! Two igrid statuses to compare for
         MeshLoc2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF MeshLoc statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[MeshLoc1, MeshLoc2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_MeshLocEqual = (MeshLoc1%meshloc == &
                              MeshLoc2%meshloc)

      end function ESMF_MeshLocEqual
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshLocNotEqual"
!BOPI
! !IROUTINE: ESMF_MeshLocNotEqual - Non-equality of MeshLoc statuses
!
! !INTERFACE:
      function ESMF_MeshLocNotEqual(MeshLoc1, MeshLoc2)

! !RETURN VALUE:
      logical :: ESMF_MeshLocNotEqual

! !ARGUMENTS:

      type (ESMF_MeshLoc), intent(in) :: &
         MeshLoc1,      &! Two MeshLoc Statuses to compare for
         MeshLoc2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF MeshLoc statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[MeshLoc1, MeshLoc2]
!          Two statuses of MeshLocs to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_MeshLocNotEqual = (MeshLoc1%meshloc /= &
                                 MeshLoc2%meshloc)

      end function ESMF_MeshLocNotEqual

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
