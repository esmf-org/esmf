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
!      '$Id$'
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
  use ESMF_IOUGridMod
  use ESMF_ArrayMod

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
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
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

    type(ESMF_CoordSys_Flag) :: coordSys ! Put this here for now. 
                                         ! Eventually may need to also put lower in C++ Mesh???
                                         ! If connect this via MeshCXX then move this there.


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
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
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
        ESMF_MESHELEMTYPE_TRI    = 3,  &  ! Triangle
        ESMF_MESHELEMTYPE_QUAD   = 4,  &  ! Quadralateral
        ESMF_MESHELEMTYPE_TETRA  = 10, &  ! Tetrahedron
        ESMF_MESHELEMTYPE_HEX    = 12     ! Hexahedron



  type ESMF_MeshLoc
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
!  private
    integer :: meshloc
  end type

  type(ESMF_MeshLoc), parameter :: &
        ESMF_MESHLOC_NODE = ESMF_MeshLoc(0), &
        ESMF_MESHLOC_ELEMENT = ESMF_MeshLoc(1), &
 	    ESMF_MESHLOC_NONE = ESMF_MeshLoc(2)

 !------------------------------------------------------------------------------
!     ! ESMF_Mesh
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_Mesh               
  public ESMF_MESHELEMTYPE_QUAD, ESMF_MESHELEMTYPE_TRI, &
          ESMF_MESHELEMTYPE_HEX, ESMF_MESHELEMTYPE_TETRA
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
  public ESMF_MeshIsCreated
  public ESMF_MeshMatch
  public ESMF_MeshSerialize
  public ESMF_MeshDeserialize
  public ESMF_MeshFindPnt
  public ESMF_MeshGetElemArea
  public ESMF_MeshGetOrigElemArea
  public ESMF_MeshGetElemFrac
  public ESMF_MeshGetElemFrac2
  public ESMF_MeshGetOrigElemFrac
  public ESMF_MeshGetElemSplit
  public ESMF_MeshMergeSplitSrcInd
  public ESMF_MeshMergeSplitDstInd
  public ESMF_MeshTurnOnCellMask
  public ESMF_MeshTurnOffCellMask
  public ESMF_MeshTurnOnNodeMask
  public ESMF_MeshTurnOffNodeMask
  public ESMF_MeshCreateDual  ! not a public interface for now
  public ESMF_MeshSetMOAB
  public ESMF_MeshGetIntPtr
  public ESMF_MeshCreateFromIntPtr

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
 ! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

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
     module procedure ESMF_MeshCreateFromDG
     module procedure ESMF_MeshCreateFromMeshes
     module procedure ESMF_MeshCreateRedist
   end interface

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
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
                 elementConn, elementMask, elementArea, elementCoords, rc)

!
! !ARGUMENTS:
    type(ESMF_Mesh),    intent(inout)         :: mesh
    integer,            intent(in)            :: elementIds(:)
    integer,            intent(in)            :: elementTypes(:)
    integer,            intent(in)            :: elementConn(:)
    integer,            intent(in),  optional :: elementMask(:)
    real(ESMF_KIND_R8), intent(in),  optional :: elementArea(:)
    real(ESMF_KIND_R8), intent(in),  optional :: elementCoords(:)
    integer,            intent(out), optional :: rc
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
!          Each element id must be a number equal to or greater than 1. An id should be
!          unique in the sense that different elements must have different ids (the same element
!          that appears on different processors must have the same id). There may be gaps in the sequence
!          of ids, but if these gaps are the same scale as the length of the sequence it can lead to 
!          inefficiencies when the Mesh is used (e.g. in {\tt ESMF\_FieldRegridStore()}).  
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
!   \item [{[elementMask]}]
!          An array containing values which can be used for element masking. Which values indicate
!          masking are chosen via the {\tt srcMaskValues} or {\tt dstMaskValues} arguments to 
!          {\tt ESMF\_FieldRegridStore()} call. This input consists of a 1D array the
!          size of the number of elements on this PET.
!   \item [{[elementArea]}]
!          An array containing element areas. If not specified, the element areas are internally calculated. 
!          This input consists of a 1D array the size of the number of elements on this PET.
!   \item[{[elementCoords]}] 
!          An array containing the physical coordinates of the elements to be created on this
!          PET. This input consists of a 1D array the size of the number of elements on this PET times the Mesh's 
!          spatial dimension ({\tt spatialDim}). The coordinates in this array are ordered
!          so that the coordinates for an element lie in sequence in memory. (e.g. for a 
!          Mesh with spatial dimension 2, the coordinates for element 1 are in elementCoords(1) and
!          elementCoords(2), the coordinates for element 2 are in elementCoords(3) and elementCoords(4), 
!          etc.). 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: num_elems, num_elementConn
    type(ESMF_RegridConserve) :: lregridConserve
    type(ESMF_InterfaceInt) :: elementMaskII
    real(ESMF_KIND_R8) :: tmpArea(2)
    integer :: areaPresent
    real(ESMF_KIND_R8) :: tmpCoords(2)
    integer :: coordsPresent


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

    ! get sizes of lists
    num_elems = size(elementIds)
    num_elementConn = size(elementConn)

    ! If present make sure that elementCoords has the correct size
    if (present(elementCoords)) then
       if (size(elementCoords) .ne. &
            mesh%spatialDim*num_elems) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- elementCoords input array is the wrong size.", &
               ESMF_CONTEXT, rcToReturn=rc)
          return
       endif
    endif    

   ! Create interface int to wrap optional element mask
   elementMaskII = ESMF_InterfaceIntCreate(elementMask, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


    ! set element area if it's present.
    if (present(elementCoords)) then
       if (present(elementArea)) then
          areaPresent=1
          coordsPresent=1
          call C_ESMC_MeshAddElements(mesh%this, num_elems, &
               elementIds, elementTypes, elementMaskII, &
               areaPresent, elementArea, &
               coordsPresent, elementCoords, &
               num_elementConn, elementConn, &
               lregridConserve%regridconserve, &
               mesh%coordSys, mesh%spatialDim, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       else
          areaPresent=0
          coordsPresent=1
          call C_ESMC_MeshAddElements(mesh%this, num_elems, &
               elementIds, elementTypes, elementMaskII, &
               areaPresent, tmpArea, &
               coordsPresent, elementCoords, &
               num_elementConn, elementConn, &
               lregridConserve%regridconserve,&
               mesh%coordSys, mesh%spatialDim, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    else
       if (present(elementArea)) then
          areaPresent=1
          coordsPresent=0
          call C_ESMC_MeshAddElements(mesh%this, num_elems, &
               elementIds, elementTypes, elementMaskII, &
               areaPresent, elementArea, &
               coordsPresent, tmpCoords, &
               num_elementConn, elementConn, &
               lregridConserve%regridconserve, &
               mesh%coordSys, mesh%spatialDim, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       else
          areaPresent=0
          coordsPresent=0
          call C_ESMC_MeshAddElements(mesh%this, num_elems, &
               elementIds, elementTypes, elementMaskII, &
               areaPresent, tmpArea, &
               coordsPresent, tmpCoords, &
               num_elementConn, elementConn, &
               lregridConserve%regridconserve, &
               mesh%coordSys, mesh%spatialDim, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif

    ! Create two distgrids, one for nodes and one for elements
    call C_ESMC_MeshCreateNodeDistGrid(mesh%this, mesh%nodal_distgrid, &
                                       mesh%numOwnedNodes, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    call C_ESMC_MeshCreateElemDistGrid(mesh%this, mesh%element_distgrid, &
                                       mesh%numOwnedElements, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    



    !call ESMF_DistGridPrint(mesh%nodal_distgrid)
    !ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, mesh%nodal_distgrid, rc)

    ! Get rid of interface Int wrapper
    call ESMF_InterfaceIntDestroy(elementMaskII, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


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
    subroutine ESMF_MeshAddNodes(mesh, nodeIds, nodeCoords, nodeOwners, &
                                 nodeMask, rc)

!
! !ARGUMENTS:
    type(ESMF_Mesh),    intent(inout)         :: mesh
    integer,            intent(in)            :: nodeIds(:)
    real(ESMF_KIND_R8), intent(in)            :: nodeCoords(:)
    integer,            intent(in)            :: nodeOwners(:)
    integer,            intent(in),  optional :: nodeMask(:)
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
!          Each node id must be a number equal to or greater than 1. An id should be
!          unique in the sense that different nodes must have different ids (the same node
!          that appears on different processors must have the same id). There may be gaps in the sequence
!          of ids, but if these gaps are the same scale as the length of the sequence it can lead to 
!          inefficiencies when the Mesh is used (e.g. in {\tt ESMF\_FieldRegridStore()}).  
!   \item[nodeCoords] 
!          An array containing the physical coordinates of the nodes to be created on this
!          PET. This input consists of a 1D array the size of the number of nodes on this PET times the Mesh's 
!          spatial dimension ({\tt spatialDim}). The coordinates in this array are ordered
!          so that the coordinates for a node lie in sequence in memory. (e.g. for a 
!          Mesh with spatial dimension 2, the coordinates for node 1 are in nodeCoords(1) and
!          nodeCoords(2), the coordinates for node 2 are in nodeCoords(3) and nodeCoords(4), 
!          etc.). 
!   \item[nodeOwners] 
!         An array containing the PETs that own the nodes to be created on this PET. 
!         If the node is shared with another PET, the value
!         may be a PET other than the current one. Only nodes owned by this PET
!         will have PET local entries in a Field created on the Mesh. This input consists of 
!         a 1D array the size of the number of nodes on this PET.
!   \item [{[nodeMask]}]
!          An array containing values which can be used for node masking. Which values indicate
!          masking are chosen via the {\tt srcMaskValues} or {\tt dstMaskValues} arguments to 
!          {\tt ESMF\_FieldRegridStore()} call. This input consists of a 1D array the
!          size of the number of nodes on this PET.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: num_nodes
    type(ESMF_InterfaceInt) :: nodeMaskII

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


   ! Create interface int to wrap optional element mask
   nodeMaskII = ESMF_InterfaceIntCreate(nodeMask, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


    num_nodes = size(nodeIds)
    call C_ESMC_MeshAddNodes(mesh%this, num_nodes, nodeIds, nodeCoords, &
                         nodeOwners, nodeMaskII, &
                         mesh%coordSys, mesh%spatialDim, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Get rid of interface Int wrapper
    call ESMF_InterfaceIntDestroy(nodeMaskII, rc=localrc)
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
    function ESMF_MeshCreate3Part(parametricDim, spatialDim, &
                                  coordSys, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreate3Part
! !ARGUMENTS:
    integer,                  intent(in)            :: parametricDim
    integer,                  intent(in)            :: spatialDim
    type(ESMF_CoordSys_Flag), intent(in),  optional :: coordSys
    integer,                  intent(out), optional :: rc
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
!         making up the Mesh. For a manifold, the spatial dimension can be larger than the 
!         parametric dim (e.g. the 2D surface of a sphere in 3D space), but it can't be smaller. 
! \item[{[coordSys]}] 
!         The coordinate system of the grid coordinate data. 
!         For a full list of options, please see Section~\ref{const:coordsys}. 
!         If not specified then defaults to ESMF\_COORDSYS\_SPH\_DEG.  
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                  :: localrc      ! local return code
    type(ESMF_CoordSys_Flag) :: coordSysLocal

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Set Default coordSys
    if (present(coordSys)) then
       coordSysLocal=coordSys
    else 
       coordSysLocal=ESMF_COORDSYS_SPH_DEG
    endif

    ! Create C++ Mesh
    ESMF_MeshCreate3Part%this = ESMF_NULL_POINTER

    call c_ESMC_meshcreate(ESMF_MeshCreate3Part%this, parametricDim, spatialDim, &
                           coordSysLocal, localrc)
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

    ! Set CoordSys
    ESMF_MeshCreate3Part%coordSys=coordSysLocal

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
                         nodeIds, nodeCoords, nodeOwners, nodeMask, &
                         elementIds, elementTypes, elementConn, &
                         elementMask, elementArea, elementCoords, &
                         coordSys, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)                           :: ESMF_MeshCreate1Part
! !ARGUMENTS:
    integer,            intent(in)            :: parametricDim
    integer,            intent(in)            :: spatialDim
    integer,            intent(in)            :: nodeIds(:)
    real(ESMF_KIND_R8), intent(in)            :: nodeCoords(:)
    integer,            intent(in)            :: nodeOwners(:)
    integer,            intent(in),  optional :: nodeMask(:)
    integer,            intent(in)            :: elementIds(:)
    integer,            intent(in)            :: elementTypes(:)
    integer,            intent(in)            :: elementConn(:)
    integer,            intent(in),  optional :: elementMask(:)
    real(ESMF_KIND_R8), intent(in),  optional :: elementArea(:)  
    real(ESMF_KIND_R8), intent(in),  optional :: elementCoords(:)
    type(ESMF_CoordSys_Flag), intent(in),  optional :: coordSys
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
!         making up the Mesh. For a manifold, the spatial dimension can be larger than the 
!         parametric dim (e.g. the 2D surface of a sphere in 3D space), but it can't be smaller. 
!   \item [nodeIds]
!         An array containing the global ids of the nodes to be created on this PET. 
!         This input consists of a 1D array the size of the number of nodes on this PET.
!          Each node id must be a number equal to or greater than 1. An id should be
!          unique in the sense that different nodes must have different ids (the same node
!          that appears on different processors must have the same id). There may be gaps in the sequence
!          of ids, but if these gaps are the same scale as the length of the sequence it can lead to 
!          inefficiencies when the Mesh is used (e.g. in {\tt ESMF\_FieldRegridStore()}).  
!   \item[nodeCoords] 
!          An array containing the physical coordinates of the nodes to be created on this
!          PET. This input consists of a 1D array the size of the number of nodes on this PET times the Mesh's 
!          spatial dimension ({\tt spatialDim}). The coordinates in this array are ordered
!          so that the coordinates for a node lie in sequence in memory. (e.g. for a 
!          Mesh with spatial dimension 2, the coordinates for node 1 are in nodeCoords(1) and
!          nodeCoords(2), the coordinates for node 2 are in nodeCoords(3) and nodeCoords(4), 
!          etc.). 
!   \item[nodeOwners] 
!         An array containing the PETs that own the nodes to be created on this PET. 
!         If the node is shared with another PET, the value
!         may be a PET other than the current one. Only nodes owned by this PET
!         will have PET local entries in a Field created on the Mesh. This input consists of 
!         a 1D array the size of the number of nodes on this PET.
!   \item [{[nodeMask]}]
!          An array containing values which can be used for node masking. Which values indicate
!          masking are chosen via the {\tt srcMaskValues} or {\tt dstMaskValues} arguments to 
!          {\tt ESMF\_FieldRegridStore()} call. This input consists of a 1D array the
!          size of the number of nodes on this PET.
!   \item [elementIds]
!          An array containing the global ids of the elements to be created on this PET. 
!          This input consists of a 1D array the size of the number of elements on this PET.
!          Each element id must be a number equal to or greater than 1. An id should be
!          unique in the sense that different elements must have different ids (the same element
!          that appears on different processors must have the same id). There may be gaps in the sequence
!          of ids, but if these gaps are the same scale as the length of the sequence it can lead to 
!          inefficiencies when the Mesh is used (e.g. in {\tt ESMF\_FieldRegridStore()}).  
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
!   \item [{[elementMask]}]
!          An array containing values which can be used for element masking. Which values indicate
!          masking are chosen via the {\tt srcMaskValues} or {\tt dstMaskValues} arguments to 
!          {\tt ESMF\_FieldRegridStore()} call. This input consists of a 1D array the
!          size of the number of elements on this PET.
!   \item [{[elementArea]}]
!          An array containing element areas. If not specified, the element areas are internally calculated. 
!          This input consists of a 1D array the size of the number of elements on this PET.
!   \item[{[elementCoords]}] 
!          An array containing the physical coordinates of the elements to be created on this
!          PET. This input consists of a 1D array the size of the number of elements on this PET times the Mesh's 
!          spatial dimension ({\tt spatialDim}). The coordinates in this array are ordered
!          so that the coordinates for an element lie in sequence in memory. (e.g. for a 
!          Mesh with spatial dimension 2, the coordinates for element 1 are in elementCoords(1) and
!          elementCoords(2), the coordinates for element 2 are in elementCoords(3) and elementCoords(4), 
!          etc.). 
!   \item[{[coordSys]}] 
!         The coordinate system of the grid coordinate data. 
!         For a full list of options, please see Section~\ref{const:coordsys}. 
!         If not specified then defaults to ESMF\_COORDSYS\_SPH\_DEG.  
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
    type(ESMF_InterfaceInt) :: elementMaskII, nodeMaskII
    real(ESMF_KIND_R8) :: tmpArea(2)
    integer :: areaPresent
    real(ESMF_KIND_R8) :: tmpCoords(2)
    integer :: coordsPresent
    type(ESMF_CoordSys_Flag) :: coordSysLocal


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

   
   ! Set Default coordSys
   if (present(coordSys)) then
      coordSysLocal=coordSys
   else 
      coordSysLocal=ESMF_COORDSYS_SPH_DEG
   endif

    ! Create C++ Mesh
    call c_ESMC_meshcreate(ESMF_MeshCreate1Part%this, parametricDim, spatialDim, &
                           coordSyslocal, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init status of arguments
    ESMF_INIT_SET_CREATED(ESMF_MeshCreate1Part)


   ! Create interface int to wrap optional element mask
   nodeMaskII = ESMF_InterfaceIntCreate(nodeMask, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Add the nodes
    num_nodes = size(nodeIds)
    call C_ESMC_MeshAddNodes(ESMF_MeshCreate1Part%this, num_nodes, nodeIds, nodeCoords, &
                             nodeOwners, nodeMaskII, &
                             coordSysLocal, spatialDim, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! Get rid of interface Int wrapper
    call ESMF_InterfaceIntDestroy(nodeMaskII, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return



   ! Create interface int to wrap optional element mask
   elementMaskII = ESMF_InterfaceIntCreate(elementMask, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return


    ! get sizes of lists
    num_elems = size(elementIds)
    num_elementConn = size(elementConn)


    ! If present make sure that elementCoords has the correct size
    if (present(elementCoords)) then
       if (size(elementCoords) .ne. &
            spatialDim*num_elems) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
               msg="- elementCoords input array is the wrong size.", &
               ESMF_CONTEXT, rcToReturn=rc)
          return
       endif
    endif    
    

#if 0
    call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, 
num_elems, &
                             elementIds, elementTypes, elementMaskII, &
                             num_elementConn, elementConn, &
                             lregridConserve%regridconserve, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#endif


    ! set element area if it's present.
    if (present(elementCoords)) then
       if (present(elementArea)) then
          areaPresent=1
          coordsPresent=1
          call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, &
               num_elems, &
               elementIds, elementTypes, elementMaskII, &
               areaPresent, elementArea, &
               coordsPresent, elementCoords, &
               num_elementConn, elementConn, &
               lregridConserve%regridconserve,&
               coordSysLocal, spatialDim, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       else
          areaPresent=0
          coordsPresent=1
          call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, &
               num_elems, &
               elementIds, elementTypes, elementMaskII, &
               areaPresent, tmpArea, &
               coordsPresent, elementCoords, &
               num_elementConn, elementConn, &
               lregridConserve%regridconserve, &
               coordSysLocal, spatialDim, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    else
       if (present(elementArea)) then
          areaPresent=1
          coordsPresent=0
          call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, &
               num_elems, &
               elementIds, elementTypes, elementMaskII, &
               areaPresent, elementArea, &
               coordsPresent, tmpCoords, &
               num_elementConn, elementConn, &
               lregridConserve%regridconserve, &
               coordSysLocal, spatialDim, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       else
          areaPresent=0
          coordsPresent=0
          call C_ESMC_MeshAddElements(ESMF_MeshCreate1Part%this, &
               num_elems, &
               elementIds, elementTypes, elementMaskII, &
               areaPresent, tmpArea, &
               coordsPresent, tmpCoords, &
               num_elementConn, elementConn, &
               lregridConserve%regridconserve, &
               coordSysLocal, spatialDim, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       endif
    endif


    ! Create two distgrids, one for nodes and one for elements
    call C_ESMC_MeshCreateNodeDistGrid(ESMF_MeshCreate1Part%this, &
                                       ESMF_MeshCreate1Part%nodal_distgrid, &
                                       ESMF_MeshCreate1Part%numOwnedNodes, &
                                       localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    call C_ESMC_MeshCreateElemDistGrid(ESMF_MeshCreate1Part%this, &
                                       ESMF_MeshCreate1Part%element_distgrid, &
                                       ESMF_MeshCreate1Part%numOwnedElements, &
                                       localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    



    ! Get rid of interface Int wrapper
    call ESMF_InterfaceIntDestroy(elementMaskII, rc=localrc)
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

    ! Set coord sys information
    ESMF_MeshCreate1Part%coordSys=coordSysLocal

    !call ESMF_DistGridPrint(ESMF_MeshCreate1Part%nodal_distgrid)
    !ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, ESMF_MeshCreate1Part%nodal_distgrid, rc)

    if (present (rc)) rc = localrc
    
  end function ESMF_MeshCreate1Part
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromDG()"
!BOPI
! !IROUTINE: ESMF_MeshCreate - Create a Mesh from an elemental DistGrid
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreateFromDG(distgrid, nodalDistgrid, parametricDim, &
      spatialDim, coordSys, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromDG
! !ARGUMENTS:
    type(ESMF_DistGrid),        intent(in)            :: distgrid
    type(ESMF_DistGrid),        intent(in), optional  :: nodalDistgrid
    integer,                    intent(in), optional  :: parametricDim
    integer,                    intent(in), optional  :: spatialDim
    type(ESMF_CoordSys_Flag),   intent(in), optional  :: coordSys
    integer,                    intent(out), optional :: rc

! 
! !DESCRIPTION:
!   Create a Mesh from an elemental distgrid. Such a mesh will have no coordinate or
!   connectivity information stored.
!
!   \begin{description}
!   \item [distgrid]
!         The elemental distgrid.
!   \item [{[nodalDistgrid]}]
!         The nodal distgrid, if not specified is set to distgrid (i.e. the elemental distgrid).
!   \item [{[parametricDim]}]
!         Dimension of the topology of the Mesh. (E.g. a mesh constructed of squares would
!         have a parametric dimension of 2, whereas a Mesh constructed of cubes would have one
!         of 3.)
!   \item [{[spatialDim]}]
!         The number of coordinate dimensions needed to describe the locations of the nodes 
!         making up the Mesh. For a manifold, the spatial dimension can be larger than the 
!         parametric dim (e.g. the 2D surface of a sphere in 3D space), but it can't be smaller. 
!   \item[{[coordSys]}] 
!         The coordinate system of the grid coordinate data. 
!         For a full list of options, please see Section~\ref{const:coordsys}. 
!         If not specified then defaults to ESMF\_COORDSYS\_SPH\_DEG.  
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer::  localrc, l_pdim, l_sdim
    type(ESMF_CoordSys_Flag) :: coordSysLocal

    l_pdim = 2
    l_sdim = 3
    if(present(parametricDim)) l_pdim = parametricDim
    if(present(spatialDim))    l_sdim = spatialDim

    ! Set Default coordSys
    if (present(coordSys)) then
       coordSysLocal=coordSys
    else 
       coordSysLocal=ESMF_COORDSYS_SPH_DEG
    endif

    ESMF_MeshCreateFromDG = ESMF_MeshCreate3part(l_pdim, l_sdim, &
                           coordSysLocal, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ESMF_MeshCreateFromDG%isFullyCreated=.true.
    ESMF_MeshCreateFromDG%element_distgrid = distgrid
    if (present(nodalDistgrid)) then
      ESMF_MeshCreateFromDG%nodal_distgrid = nodalDistgrid
    else 
      ESMF_MeshCreateFromDG%nodal_distgrid = distgrid
    endif

    ESMF_MeshCreateFromDG%isCMeshFreed = .true. ! helps problems in reconcile

    ESMF_INIT_SET_CREATED(ESMF_MeshCreateFromDG)

    if (present(rc)) rc=ESMF_SUCCESS
    return
end function ESMF_MeshCreateFromDG
!------------------------------------------------------------------------------

!!------------------------------------------------------------------------------
!#undef  ESMF_METHOD
!#define ESMF_METHOD "ESMF_MeshCreateFromMeshesR4()"
!!BOPI
!! !IROUTINE: ESMF_MeshCreate - Create a Mesh from two source Meshes with spatial operations
!!
!! !INTERFACE:
!  ! Private name; call using ESMF_MeshCreate()
!    function ESMF_MeshCreateFromMeshesR4(MeshA, MeshB, MeshOp, areaThreshold, rc)
!!
!!
!! !RETURN VALUE:
!    type(ESMF_Mesh)                                   :: ESMF_MeshCreateFromMeshesR4
!! !ARGUMENTS:
!    type(ESMF_Mesh),            intent(in)            :: MeshA
!    type(ESMF_Mesh),            intent(in)            :: MeshB
!    type(ESMF_MeshOp_Flag),     intent(in)            :: MeshOp
!    real(ESMF_KIND_R4),         intent(in),  optional :: areaThreshold
!    integer,                    intent(out), optional :: rc
!! 
!! !DESCRIPTION:
!!   Create a Mesh from two source Meshes with spatial operations. These spatial operations
!!   treat the points in the two Meshes as point sets. The returned Mesh is either intersection,
!!   union, or difference of the point sets of two source Meshes. 
!!
!!   \begin{description}
!!   \item [MeshA]
!!         The first source Mesh containing the first point set.
!!   \item [MeshB]
!!         The second source Mesh containing the second point set.
!!   \item [MeshOp]
!!         Mesh spatial operation flag. Currently only ESMF_MESHOP_DIFFERENCE is supported.
!!         Please refer to section {\ref const:meshop}
!!   \item [areaThreshold]
!!         Minimum cell area to be accepted to create the resulting Mesh. Cells with area
!!         less than this threshold value are discarded. This is a user tunable parameter
!!         to handle roundoff error when computing with floating point numbers. The default
!!         value is 0.
!!   \item [{[rc]}]
!!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!   \end{description}
!!
!!EOPI
!!------------------------------------------------------------------------------
!    integer                      :: localrc
!    real(ESMF_KIND_R8)           :: l_threshold
!
!    l_threshold = 0.
!    if(present(areaThreshold)) l_threshold = areaThreshold
!
!    call C_ESMC_MeshCreateFromMeshes(meshA, meshB, ESMF_MeshCreateFromMeshesR4, &
!      MeshOp, l_threshold, localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!         ESMF_CONTEXT, rcToReturn=rc)) return
!
!    ! The C side has been created
!    ESMF_MeshCreateFromMeshesR4%isCMeshFreed=.false.
!
!    ! Set as fully created 
!    ESMF_MeshCreateFromMeshesR4%hasSplitElem=.false.
!
!    ESMF_MeshCreateFromMeshesR4%isFullyCreated=.true.
!
!    ESMF_INIT_SET_CREATED(ESMF_MeshCreateFromMeshesR4)
!
!    if (present(rc)) rc=ESMF_SUCCESS
!    return
!
!end function ESMF_MeshCreateFromMeshesR4
!------------------------------------------------------------------------------

!!------------------------------------------------------------------------------
!#undef  ESMF_METHOD
!#define ESMF_METHOD "ESMF_MeshCreateFromMeshesR8()"
!!BOPI
!! !IROUTINE: ESMF_MeshCreate - Create a Mesh from two source Meshes with spatial operations
!!
!! !INTERFACE:
!  ! Private name; call using ESMF_MeshCreate()
!    function ESMF_MeshCreateFromMeshesR8(MeshA, MeshB, MeshOp, areaThreshold, rc)
!!
!!
!! !RETURN VALUE:
!    type(ESMF_Mesh)                                   :: ESMF_MeshCreateFromMeshesR8
!! !ARGUMENTS:
!    type(ESMF_Mesh),            intent(in)            :: MeshA
!    type(ESMF_Mesh),            intent(in)            :: MeshB
!    type(ESMF_MeshOp_Flag),     intent(in)            :: MeshOp
!    real(ESMF_KIND_R8),         intent(in),  optional :: areaThreshold
!    integer,                    intent(out), optional :: rc
!! 
!! !DESCRIPTION:
!!   Create a Mesh from two source Meshes with spatial operations. These spatial operations
!!   treat the points in the two Meshes as point sets. The returned Mesh is either intersection,
!!   union, or difference of the point sets of two source Meshes. 
!!
!!   \begin{description}
!!   \item [MeshA]
!!         The first source Mesh containing the first point set.
!!   \item [MeshB]
!!         The second source Mesh containing the second point set.
!!   \item [MeshOp]
!!         Mesh spatial operation flag. Currently only ESMF_MESHOP_DIFFERENCE is supported.
!!         Please refer to section {\ref const:meshop}
!!   \item [areaThreshold]
!!         Minimum cell area to be accepted to create the resulting Mesh. Cells with area
!!         less than this threshold value are discarded. This is a user tunable parameter
!!         to handle roundoff error when computing with floating point numbers. The default
!!         value is 0.
!!   \item [{[rc]}]
!!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!   \end{description}
!!
!!EOPI
!!------------------------------------------------------------------------------
!    integer                      :: localrc
!    real(ESMF_KIND_R8)           :: l_threshold
!
!    l_threshold = 0.
!    if(present(areaThreshold)) l_threshold = areaThreshold
!
!    call C_ESMC_MeshCreateFromMeshes(meshA, meshB, ESMF_MeshCreateFromMeshesR8, &
!      MeshOp, l_threshold, localrc)
!    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
!         ESMF_CONTEXT, rcToReturn=rc)) return
!
!    ! The C side has been created
!    ESMF_MeshCreateFromMeshesR8%isCMeshFreed=.false.
!
!    ! Set as fully created 
!    ESMF_MeshCreateFromMeshesR8%hasSplitElem=.false.
!
!    ESMF_MeshCreateFromMeshesR8%isFullyCreated=.true.
!
!    ESMF_INIT_SET_CREATED(ESMF_MeshCreateFromMeshesR8)
!
!    if (present(rc)) rc=ESMF_SUCCESS
!    return
!
!end function ESMF_MeshCreateFromMeshesR8
!!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromMeshes()"
!BOPI
! !IROUTINE: ESMF_MeshCreate - Create a Mesh from two source Meshes with spatial operations
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreateFromMeshes(MeshA, MeshB, MeshOp, areaThreshold, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)                                   :: ESMF_MeshCreateFromMeshes
! !ARGUMENTS:
    type(ESMF_Mesh),            intent(in)            :: MeshA
    type(ESMF_Mesh),            intent(in)            :: MeshB
    type(ESMF_MeshOp_Flag),     intent(in)            :: MeshOp
    real(ESMF_KIND_R8),         intent(in),  optional :: areaThreshold
    integer,                    intent(out), optional :: rc
! 
! !DESCRIPTION:
!   Create a Mesh from two source Meshes with spatial operations. These spatial operations
!   treat the points in the two Meshes as point sets. The returned Mesh is either intersection,
!   union, or difference of the point sets of two source Meshes. 
!
!   \begin{description}
!   \item [MeshA]
!         The first source Mesh containing the first point set.
!   \item [MeshB]
!         The second source Mesh containing the second point set.
!   \item [MeshOp]
!         Mesh spatial operation flag. Currently only ESMF_MESHOP_DIFFERENCE is supported.
!         Please refer to section {\ref const:meshop}
!   \item [areaThreshold]
!         Minimum cell area to be accepted to create the resulting Mesh. Cells with area
!         less than this threshold value are discarded. This is a user tunable parameter
!         to handle roundoff error when computing with floating point numbers. The default
!         value is 0.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                      :: localrc
    real(ESMF_KIND_R8)           :: l_threshold

    l_threshold = 0.
    if(present(areaThreshold)) l_threshold = areaThreshold

    call C_ESMC_MeshCreateFromMeshes(meshA, meshB, ESMF_MeshCreateFromMeshes, &
      MeshOp, l_threshold, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! The C side has been created
    ESMF_MeshCreateFromMeshes%isCMeshFreed=.false.

    ! Create two distgrids, one for nodes and one for elements
    call C_ESMC_MeshCreateNodeDistGrid(ESMF_MeshCreateFromMeshes%this, &
         ESMF_MeshCreateFromMeshes%nodal_distgrid, &
         ESMF_MeshCreateFromMeshes%numOwnedNodes, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    call C_ESMC_MeshCreateElemDistGrid(ESMF_MeshCreateFromMeshes%this, &
         ESMF_MeshCreateFromMeshes%element_distgrid, &
         ESMF_MeshCreateFromMeshes%numOwnedElements, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    ! Set as fully created 
    ESMF_MeshCreateFromMeshes%hasSplitElem=.false.

    ESMF_MeshCreateFromMeshes%isFullyCreated=.true.

    ESMF_INIT_SET_CREATED(ESMF_MeshCreateFromMeshes)

    if (present(rc)) rc=ESMF_SUCCESS
    return

end function ESMF_MeshCreateFromMeshes
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromFile()"
!BOP
!\label{API:MeshCreateFromFile}
! !IROUTINE: ESMF_MeshCreate - Create a Mesh from a file
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreateFromFile(filename, fileformat, keywordEnforcer, &
                 convertToDual, addUserArea, meshname, maskFlag, varname, &
		 nodalDistgrid, elementDistgrid, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromFile
! !ARGUMENTS:
    character(len=*),           intent(in)            :: filename
    type(ESMF_FileFormat_Flag), intent(in)            :: fileformat
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                    intent(in),  optional :: convertToDual
    logical,                    intent(in),  optional :: addUserArea
    character(len=*),           intent(in),  optional :: meshname
    type(ESMF_MeshLoc),         intent(in),  optional :: maskFlag
    character(len=*),           intent(in),  optional :: varname
    type(ESMF_DistGrid),        intent(in),  optional :: nodalDistgrid
    type(ESMF_DistGrid),        intent(in),  optional :: elementDistgrid
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
!   \item[fileformat] 
!         The file format of the grid file to be read, please see Section~\ref{const:mesh:fileformat}
!         for a list of valid options. 
!   \item[{[convertToDual]}] 
!         if {\tt .true.}, the mesh will be converted to its dual. If not specified,
!         defaults to {\tt .false.}. 
!   \item[{[addUserArea]}] 
!         if {\tt .true.}, the cell area will be read in from the GRID file.  This feature is
!         only supported when the grid file is in the SCRIP or ESMF format. If not specified, 
!         defaults to {\tt .false.}.
!   \item[{[meshname]}]
!         The dummy variable for the mesh metadata in the UGRID file if the {\tt fileformat}
!         is {\tt ESMF\_FILEFORMAT\_UGRID}.  If not specified, defaults to empty string.
!   \item[{[maskFlag]}]
!         If maskFlag is present, generate the mask using the missing\_value attribute defined in 'varname'
!         This flag is only supported when the grid file is in the UGRID format.
!         The value could be either {\tt ESMF\_MESHLOC\_NODE} or {\tt ESMF\_MESHLOC\_ELEMENT}.  If the value is
!         {\tt ESMF\_MESHLOC\_NODE}, the node mask will be generated and the variable has to be 
!         defined on the "node" (specified by its {\tt location} attribute).  If the value is 
!         {\tt ESMF\_MESHLOC\_ELEMENT}, the element mask will be generated and the variable has to be 
!         defined on the "face" of the mesh.  If the variable is not defined on the right location,
!         no mask will be generated.  If not specified, no mask will be generated.
!   \item[{[varname]}]
!         If maskFlag is present, provide a variable name stored in the UGRID file and
!         the mask will be generated using the missing value of the data value of
!         this variable.  The first two dimensions of the variable has to be the
!         the longitude and the latitude dimension and the mask is derived from the
!         first 2D values of this variable even if this data is 3D, or 4D array. If not 
!         specified, defaults to empty string.
!   \item [{[nodalDistgrid]}]
!         A 1D arbitrary distgrid describing the user-specified distribution of 
!         the nodes across the PETs. 
!   \item [{[elementDistgrid]}]
!         A 1D arbitrary distgrid describing the user-specified distribution of 
!         the elements across the PETs. 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    logical::  localConvertToDual      ! local flag
    logical::  localAddUserArea  
    type(ESMF_Mesh) :: myMesh
    integer::  localrc

    ! Set Defaults
    if (present(convertToDual)) then
	localConvertToDual = convertToDual
    else
	localConvertToDual = .false.
    endif

    if (present(addUserArea)) then
	localAddUserArea = addUserArea
    else
	localAddUserArea = .false.
    endif

    if (present(maskFlag) .and. .not. present(varname)) then
      call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, &
	  msg="- need varname argument to create mask", &
	  ESMF_CONTEXT, rcToReturn=rc)
          return
    endif

    if (fileformat == ESMF_FILEFORMAT_SCRIP) then
	myMesh = ESMF_MeshCreateFromScrip(filename, localConvertToDual, &
          addUserArea=localAddUserArea, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (fileformat == ESMF_FILEFORMAT_ESMFMESH) then
	myMesh = ESMF_MeshCreateFromUnstruct(filename, &
	   addUserArea=localAddUserArea, &
           convertToDual=localConvertToDual, &
	   fileformat=fileformat, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (fileformat == ESMF_FILEFORMAT_UGRID) then
        ! Warning message about add user area
        if (localAddUserArea) then
           call ESMF_LogWrite("ESMF does not currently support " // &
                "user areas in UGRID format, so user areas will " // &
                "not be used for the UGRID file.", &
                ESMF_LOGMSG_WARNING, rc=localrc)
           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rcToReturn=rc)) return
        endif

	if (present(maskFlag)) then
           myMesh = ESMF_MeshCreateFromUnstruct(filename, &
	     fileformat=fileformat, meshname = meshname, &
             convertToDual=localConvertToDual, &
	     maskFlag=maskFlag, varname=varname, &
	     rc=localrc)
	else
           myMesh = ESMF_MeshCreateFromUnstruct(filename, &
	     fileformat=fileformat, meshname = meshname, &
             convertToDual=localConvertToDual, &
	     rc=localrc)
	endif 
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
    else
       call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
                 msg="- unrecognized fileformat", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return
    endif

    if (present(elementDistgrid) .and. present(nodalDistgrid)) then
        ESMF_MeshCreateFromFile = ESMF_MeshCreateRedist(myMesh, &
       			       nodalDistgrid=nodalDistgrid, &
       			       elementDistgrid=elementDistgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
	call ESMF_MeshDestroy(myMesh)
    elseif (present(elementDistgrid)) then
        ESMF_MeshCreateFromFile = ESMF_MeshCreateRedist(myMesh, &
       			       elementDistgrid=elementDistgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
	call ESMF_MeshDestroy(myMesh)
    elseif (present(nodalDistgrid)) then
        ESMF_MeshCreateFromFile = ESMF_MeshCreateRedist(myMesh, &
       			       nodalDistgrid=nodalDistgrid, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return
	call ESMF_MeshDestroy(myMesh)
    else
       ESMF_MeshCreateFromFile = myMesh			       
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
    function ESMF_MeshCreateFromUnstruct(filename, convertToDual, fileformat, meshname, &
			addUserArea, maskFlag, varname, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromUnstruct
! !ARGUMENTS:
    character(len=*), intent(in)              :: filename
    logical, intent(in), optional               :: convertToDual
    type(ESMF_FileFormat_Flag), optional, intent(in) :: fileformat
    character(len=*), optional, intent(in)    :: meshname
    logical, intent(in), optional	      :: addUserArea
    type(ESMF_MeshLoc), intent(in), optional  :: maskFlag
     character(len=*), optional, intent(in)    :: varname
   integer, intent(out), optional            :: rc
!
! !DESCRIPTION:
!   Create a mesh from a grid file defined in the ESMF Unstructured grid format.
!
!   \begin{description}
!   \item [filename]
!         The name of the grid file
!   \item[{[convertToDual]}] 
!         if {\tt .true.}, the mesh will be converted to its dual. If not specified,
!         defaults to {\tt .false.}. 
!   \item[{[addUserArea]}] 
!         if {\tt .true.}, the cell area will be read in from the GRID file.  This feature is
!         only supported when the grid file is in the SCRIP or ESMF format. 
!   \item [{[fileformat]}]
!         The type of grid file
!   \item[{[meshname]}]
!         The dummy variable for the mesh metadata in the UGRID file if the {\tt fileformat}
!         is {\tt ESMF\_FILEFORMAT\_UGRID}
!   \item[{[maskFlag]}]
!      If present, generate the mask using the missing\_value attribute defined in 'varname' on
!      the location defined by the flag, the accepted values are {\tt ESMF\_MESHLOC\_NODE} or 
!      {\tt ESMF\_MESHLOC\_ELEMENT}
!   \item[{[varname]}]
!      If maskFlag is present, provide a variable name stored in the grid file and
!      the mask will be generated using the missing value of the data value of
!      this variable.  The first two dimensions of the variable has to be the
!      the longitude and the latitude dimension and the mask is derived from the
!      first 2D values of this variable even if this data is 3D, or 4D array.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                             :: localrc      ! local return code
    integer			        :: PetNo, PetCnt 
    real(ESMF_KIND_R8),pointer          :: nodeCoords(:,:), faceCoords(:,:)
    integer(ESMF_KIND_I4),pointer       :: elementConn(:,:)
    integer(ESMF_KIND_I4),pointer       :: elmtNum(:)
    integer                             :: startElmt
    integer                             :: NodeNo
    integer                             :: NodeCnt, total
    integer, allocatable                :: NodeId(:)
    integer, allocatable                :: NodeUsed(:)
    real(ESMF_KIND_R8), allocatable     :: NodeCoords1D(:)
    real(ESMF_KIND_R8), allocatable     :: NodeCoordsCart(:)
    real(ESMF_KIND_R8)                  :: coorX, coorY
    integer, allocatable                :: NodeOwners(:)
    integer, allocatable                :: NodeOwners1(:)
    integer, pointer                    :: glbNodeMask(:), NodeMask(:)

    integer                             :: ElemNo, TotalElements, startElemNo
    integer                             :: ElemCnt,i,j,k,dim, nedges
    integer				:: localNodes, myStartElmt
    integer                             :: ConnNo, TotalConnects
    integer, allocatable                :: ElemId(:)
    integer, allocatable                :: ElemType(:)
    integer, allocatable                :: ElemConn(:)
    integer, pointer                    :: elementMask(:), ElemMask(:)
     real(ESMF_KIND_R8), pointer         :: elementArea(:), ElemArea(:)
    integer, allocatable                :: LocalElmTable(:)
    integer                             :: sndBuf(1)
    type(ESMF_VM)                       :: vm
    type(ESMF_Mesh)                     :: Mesh
    integer                             :: numPoly
#if 0
    integer, parameter                  :: maxNumPoly=20
    real(ESMF_KIND_R8)                  :: polyCoords(3*maxNumPoly)
    real(ESMF_KIND_R8)                  :: polyDblBuf(3*maxNumPoly)
    real(ESMF_KIND_R8)                  :: area(maxNumPoly)
    integer                             :: polyIntBuf(maxNumPoly)
    integer                             :: triInd(3*(maxNumPoly-2))
#else 
    integer                             :: maxNumPoly
    real(ESMF_KIND_R8),allocatable      :: polyCoords(:)
    real(ESMF_KIND_R8),allocatable      :: polyDblBuf(:)
    real(ESMF_KIND_R8),allocatable      :: area(:)
    integer,allocatable                 :: polyIntBuf(:)
    integer,allocatable                 :: triInd(:)
#endif
    real(ESMF_KIND_R8)                  :: totalarea
    integer                             :: spatialDim
    integer                             :: parametricDim
    integer                             :: lni,ti,tk
    type(ESMF_FileFormat_Flag)          :: fileformatlocal
    integer                             :: coordDim
    logical                             :: convertToDeg
    logical                             :: haveNodeMask, haveElmtMask
    logical                             :: haveMask
    logical 				:: localAddUserArea
    logical 				:: localConvertToDual
    type(ESMF_MeshLoc)			:: localAddMask
    real(ESMF_KIND_R8), pointer         :: varbuffer(:)
    real(ESMF_KIND_R8)                  :: missingvalue
    type(ESMF_CoordSys_Flag)            :: coordSys
    integer                             :: maxEdges
    logical                             :: hasFaceCoords

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! set faceCoords to null
    faceCoords => NULL()
    hasFaceCoords = .false.

    if (present(addUserArea)) then
	localAddUserArea = addUserArea
    else
	localAddUserArea = .false.
    endif

    if (present(convertToDual)) then
	localConvertToDual = convertToDual
    else
	localConvertToDual = .false.
    endif

    if (present(maskFlag)) then
	localAddMask = maskFlag
     else
	localAddMask = ESMF_MESHLOC_NONE
    endif

    ! Read the mesh definition from the file
    if (present(fileformat)) then
	fileformatlocal = fileformat
    else
	fileformatlocal = ESMF_FILEFORMAT_ESMFMESH
    endif

    if (fileformatlocal == ESMF_FILEFORMAT_UGRID) then
	if (.not. present(meshname)) then
           call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
                             msg="- meshname argument is missing", & 
                             ESMF_CONTEXT, rcToReturn=rc) 
        endif
    endif

    ! get global vm information
    !
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! define default coordinate system
    coordSys = ESMF_COORDSYS_SPH_DEG
 
    if (fileformatlocal == ESMF_FILEFORMAT_ESMFMESH) then
       ! Get coordDim
       call ESMF_EsmfInq(filename,coordDim=coordDim, haveNodeMask=haveNodeMask, &
       	    haveElmtMask=haveElmtMask, maxNodePElement=maxEdges, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

       ! Don't convert if not 2D because that'll be cartesian right now
       if (coordDim .eq. 2) then
          convertToDeg = .true.
       else 
          convertToDeg = .false.
       endif

       ! Get information from file     
       ! Need to return the coordinate system for the nodeCoords 
       if (haveNodeMask) then
           call ESMF_EsmfGetNode(filename, nodeCoords, nodeMask=glbNodeMask,&
	   		        convertToDeg=convertToDeg, coordSys=coordSys, rc=localrc)
       else
           call ESMF_EsmfGetNode(filename, nodeCoords, &
	   		        convertToDeg=convertToDeg, coordSys=coordSys, rc=localrc)
       endif			      			       

       if (haveElmtMask .and. localAddUserArea) then 
            call ESMF_EsmfGetElement(filename, elementConn, elmtNum, &
                                 startElmt, elementMask=elementMask, elementArea=elementArea, &
	 			 centerCoords=faceCoords, &
 				 convertToDeg=convertToDeg, rc=localrc)
       elseif (haveElmtMask) then
            call ESMF_EsmfGetElement(filename, elementConn, elmtNum, &
                                 startElmt, elementMask=elementMask, &
	 			 centerCoords=faceCoords, &
				 convertToDeg=convertToDeg, rc=localrc)
       elseif (localAddUserArea) then
            call ESMF_EsmfGetElement(filename, elementConn, elmtNum, &
                                 startElmt, elementArea=elementArea, &
	 			 centerCoords=faceCoords, &
				 convertToDeg=convertToDeg, rc=localrc)
       else
            call ESMF_EsmfGetElement(filename, elementConn, elmtNum, startElmt, &
                                 centerCoords=faceCoords, &
				 convertToDeg=convertToDeg, rc=localrc)
       endif
       if (associated(faceCoords)) then
            hasFaceCoords = .true.
       endif
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (fileformatlocal == ESMF_FILEFORMAT_UGRID) then
       haveElmtMask = .false.
       haveNodeMask = .false.
       if (localAddMask == ESMF_MESHLOC_ELEMENT) then
          haveElmtMask = .true.
       elseif (localAddMask == ESMF_MESHLOC_NODE) then
          haveNodeMask = .true.
       endif
       ! Get information from file
       call ESMF_GetMeshFromUGridFile(filename, meshname, nodeCoords, elementConn, &
                           elmtNum, startElmt, convertToDeg=.true., &
			   faceCoords=faceCoords, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
       ! Chenk if the grid is 3D or 2D
       coordDim = ubound(nodeCoords,1)
       nodeCnt = ubound(nodeCoords,2)

       ! Check elementConn to find out the max edges
       maxEdges = ubound(elementConn,1)
       if ( associated(faceCoords)) then
       	  hasFaceCoords = .true.
       endif

       if (coordDim == 2 .and. localAddMask == ESMF_MESHLOC_ELEMENT) then
	  !Get the variable and the missing value attribute from file
	  ! Total number of local elements
          ElemCnt = ubound (elementConn, 2)
          allocate(varbuffer(ElemCnt))
	  call ESMF_UGridGetVarByName(filename, varname, varbuffer, startind=startElmt, &
		count=ElemCnt, location="face", &
		missingvalue=missingvalue, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
	  ! Create local mask 
	  allocate(elementMask(ElemCnt))
	  elementMask(:)=1
	  do i=1,ElemCnt
	    if (varbuffer(i) == missingvalue) elementMask(i)=0
          enddo
	  deallocate(varbuffer)
       elseif (coordDim == 2 .and. localAddMask == ESMF_MESHLOC_NODE) then
	  !Get the variable and the missing value attribute from file
	  ! Total number of total nodes
          allocate(varbuffer(nodeCnt))
	  call ESMF_UGridGetVarByName(filename, varname, varbuffer, &
		location="node", &
		missingvalue=missingvalue, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
	  ! Create local mask 
	  allocate(glbNodeMask(nodeCnt))
	  glbNodeMask(:)=1
	  do i=1,nodeCnt
	    if (varbuffer(i) == missingvalue) glbNodeMask(i)=0
          enddo
	  deallocate(varbuffer)
       endif 
    else
       call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
                             msg="- unrecognized fileformat", & 
                             ESMF_CONTEXT, rcToReturn=rc) 
       return
    endif

    nodeCnt = ubound(nodeCoords,2)

   ! Figure out dimensions 
    if (coordDim .eq. 2) then
       parametricDim = 2
       spatialDim = 2
    else if (coordDim .eq. 3) then
       parametricDim = 3
       spatialDim = 3
    else
       call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, & 
            msg="- only coordDim 2 or 3 is supported right now", & 
            ESMF_CONTEXT, rcToReturn=rc) 
       return
    endif

    ! create the mesh
    Mesh = ESMF_MeshCreate3part (parametricDim, spatialDim, &
	       coordSys=coordSys, rc=localrc)
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
    totalElements = ElemCnt
    totalConnects = 0
    maxNumPoly=0
    if (parametricDim .eq. 2) then
       do ElemNo =1, ElemCnt
          do i=1,elmtNum(ElemNo)	
             if (elementConn(i,ElemNo) /= ESMF_MESH_POLYBREAK) then
                NodeUsed(elementConn(i,ElemNo))=PetNo
             endif
          enddo
          TotalConnects = TotalConnects+elmtNum(ElemNo)

          if (elmtNum(ElemNo) > maxNumPoly) then
             maxNumPoly=elmtNum(ElemNo)
          endif
       end do
    else ! If not parametricDim==2, assuming parmetricDim==3
       do ElemNo =1, ElemCnt
          do i=1,elmtNum(ElemNo)
             if (elementConn(i,ElemNo) /= ESMF_MESH_POLYBREAK) then
                NodeUsed(elementConn(i,ElemNo))=PetNo
             endif
          enddo
          TotalConnects = TotalConnects+elmtNum(ElemNo)
       end do       
    endif

   ! write(*,*) "maxNumPoly=",maxNumPoly

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
    allocate (NodeOwners(localNodes))
    
    if (parametricDim .eq. 2) then
       allocate (NodeCoords1D(localNodes*coordDim))
    else ! If not parametricDim==2, assuming parmetricDim==3
       allocate(NodeCoords1D(localNodes*3))
    endif
    
    ! copy vertex information into nodes, NodeUsed(:) now contains either 0 (not for me) or
    ! the local node index.  The owner of the node is stored in NodeOwners1(:)
    ! Also calculate how many nodes are "owned" by me -- total
    i = 1
    total = 0
    if (parametricDim .eq. 2) then
       do NodeNo = 1, NodeCnt
          if (NodeUsed(NodeNo) > 0) then
             NodeId(i) = NodeNo     
             do dim = 1, coordDim
                NodeCoords1D ((i-1)*coordDim+dim) = nodeCoords (dim, NodeNo)
             end do
             NodeOwners (i) = NodeOwners1(NodeNo)
             if (NodeOwners1(NodeNo) == PetNo) total = total+1
             i = i+1
          endif
       end do
    else ! If not parametricDim==2, assuming parmetricDim==3
       do NodeNo = 1, NodeCnt
          if (NodeUsed(NodeNo) > 0) then
             NodeId(i) = NodeNo     
             do dim = 1, 3
                NodeCoords1D ((i-1)*3+dim) = nodeCoords(dim, NodeNo)
	     end do
             NodeOwners (i) = NodeOwners1(NodeNo)
             if (NodeOwners1(NodeNo) == PetNo) total = total+1
             i = i+1
          endif
       end do
    endif


    deallocate(nodeCoords)
    if (.not. haveNodeMask) then
       ! Add nodes
       call ESMF_MeshAddNodes (Mesh, NodeIds=NodeId, &
                            NodeCoords=NodeCoords1D, &
                            NodeOwners=NodeOwners, &
                            rc=localrc)
    else
       allocate(NodeMask(localNodes))
       do i=1,localNodes
         NodeMask(i)=glbNodeMask(NodeId(i))
       enddo
       call ESMF_MeshAddNodes (Mesh, NodeIds=NodeId, &
                            NodeCoords=NodeCoords1D, &
                            NodeOwners=NodeOwners, &
			    NodeMask = NodeMask, &
                            rc=localrc)
       deallocate(NodeMask)
       deallocate(glbNodeMask)		    
    endif 
    
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

    ! allocate element arrays for the local elements
    allocate (ElemId(TotalElements))
    allocate (ElemType(TotalElements))
    allocate (ElemConn(TotalConnects))
    if (localAddUserArea) allocate(ElemArea(TotalElements))



    ! Allocate mask if the user wants one
    haveMask=.false.
    if (haveElmtMask) then 
       allocate (ElemMask(TotalElements))
       haveMask=.true.
    endif


 ! XMRKX
    ! The ElemId is the global ID.  The myStartElmt is the starting Element ID(-1), and the
    ! element IDs will be from startElmt to startElmt+ElemCnt-1
    ! The ElemConn() contains the four corner node IDs for each element and it is organized
    ! as a 1D array.  The node IDs are "local", which are stored in NodeUsed(:)
    ElemNo = 1
    ConnNo = 0
    if (parametricDim .eq. 2) then
       ! Loop through creating Mesh appropriate elements
       do j = 1, ElemCnt
          if (elmtNum(j)==3) then        
             ElemId(ElemNo) = myStartElmt+ElemNo
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_TRI
             do i=1,3
                if (elementConn(i,j) /= ESMF_MESH_POLYBREAK) then
                   ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
                else
                   ElemConn (ConnNo+i) = ESMF_MESH_POLYBREAK
                endif
             end do
	     if (haveElmtMask) ElemMask(ElemNo) = elementMask(j)
	     if (localAddUserArea) ElemArea(ElemNo) = elementArea(j)
             ElemNo=ElemNo+1
             ConnNo=ConnNo+3
          elseif (elmtNum(j)==4) then
             ElemId(ElemNo) = myStartElmt+ElemNo
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_QUAD
             do i=1,4
                if (elementConn(i,j) /= ESMF_MESH_POLYBREAK) then
                   ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
                else
                   ElemConn (ConnNo+i) = ESMF_MESH_POLYBREAK
                endif
             end do
	     if (haveElmtMask) ElemMask(ElemNo) = elementMask(j)
	     if (localAddUserArea) ElemArea(ElemNo) = elementArea(j)
             ElemNo=ElemNo+1
             ConnNo=ConnNo+4
          else
             ElemId(ElemNo) = myStartElmt+ElemNo
             ElemType (ElemNo) = elmtNum(j)
             do i=1,elmtNum(j)
                if (elementConn(i,j) /= ESMF_MESH_POLYBREAK) then
                   ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
                else
                   ElemConn (ConnNo+i) = ESMF_MESH_POLYBREAK
                endif
             end do
	     if (haveElmtMask) ElemMask(ElemNo) = elementMask(j)
	     if (localAddUserArea) ElemArea(ElemNo) = elementArea(j)
             ElemNo=ElemNo+1
             ConnNo=ConnNo+elmtNum(j)
          endif
       enddo
    else ! If not parametricDim==2, assuming parmetricDim==3
       do j = 1, ElemCnt
          if (elmtNum(j)==4) then        
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_TETRA
          elseif (elmtNum(j)==8) then
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_HEX
          else
             call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, & 
                  msg="- in 3D currently only support Tetra. (4 nodes) or Hexa. (8 nodes)", & 
                  ESMF_CONTEXT, rcToReturn=rc) 
             return
          endif

          do i=1,elmtNum(j)
             if (elementConn(i,j) /= ESMF_MESH_POLYBREAK) then
                ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
             else
                ElemConn (ConnNo+i) = ESMF_MESH_POLYBREAK
             endif
          end do
          ElemId(ElemNo) = myStartElmt+ElemNo
          if (haveElmtMask) ElemMask(ElemNo) = elementMask(j)
          if (localAddUserArea) ElemArea(ElemNo) = elementArea(j)
          ElemNo=ElemNo+1
          ConnNo=ConnNo+elmtNum(j)
       end do
    endif

    if (ElemNo /= TotalElements+1) then
	write (ESMF_UtilIOStdout,*)  &
            PetNo, ' TotalElements does not match ',ElemNo-1, TotalElements
    end if
    ! Add elements
    
    if (hasFaceCoords) then
       if (haveMask .and. localAddUserArea) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementMask=ElemMask, elementArea=ElemArea, &
			elementCoords=reshape(faceCoords,(/totalElements*coordDim/)), rc=localrc)
       elseif (haveMask) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementMask=ElemMask, &
			elementCoords=reshape(faceCoords,(/totalElements*coordDim/)), rc=localrc)
       elseif (localAddUserArea) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementArea=ElemArea, &
			elementCoords=reshape(faceCoords,(/totalElements*coordDim/)), rc=localrc)
       else
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementCoords=reshape(faceCoords,(/totalElements*coordDim/)), rc=localrc)
       end if
    else 
       if (haveMask .and. localAddUserArea) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementMask=ElemMask, elementArea=ElemArea, rc=localrc)
       elseif (haveMask) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementMask=ElemMask, rc=localrc)
       elseif (localAddUserArea) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementArea=ElemArea, rc=localrc)
       else
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, rc=localrc)
       end if
    endif 

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(NodeUsed, NodeId, NodeCoords1D, NodeOwners, NodeOwners1)
    deallocate(ElemId, ElemType, ElemConn, elementConn, elmtNum)
    if (haveElmtMask) deallocate(elementMask) 
    if (haveMask) deallocate(ElemMask) 
    if (associated(faceCoords)) deallocate(faceCoords)
    if (localAddUserArea) deallocate(elementArea, ElemArea)

    if (localConvertToDual) then
       	ESMF_MeshCreateFromUnstruct = ESMF_MeshCreateDual(Mesh, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
    else
        ESMF_MeshCreateFromUnstruct = Mesh
    endif
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
    function ESMF_MeshCreateFromScrip(filename, convertToDual, addUserArea, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromScrip
! !ARGUMENTS:
    character(len=*), intent(in)                   :: filename
    logical, intent(in), optional                  :: convertToDual
    logical, intent(in), optional                  :: addUSerArea
    integer, intent(out), optional                 :: rc
!
! !DESCRIPTION:
!   Create a mesh from a grid file defined in SCRIP format or in ESMF Unstructured grid format.
!
!   \begin{description}
!   \item [filename]
!         The name of the grid file
!   \item[convertToDual] 
!         if {\tt .true.}, the mesh will be converted to it's dual. If not specified,
!         defaults to .false. 
!   \item[addUserArea] 
!         if {\tt .true.}, the grid_area defined in the grid file will be added into the mesh.
!         If not specified, defaults to .false. 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc           ! local return code
    character(len=128)      :: cmd, esmffilename
    integer   	            :: PetNo, PetCnt 
    integer                 :: scrip_file_len, esmf_file_len
    type(ESMF_VM)           :: vm
    integer                 :: dualflag
    integer                 :: unit
    logical                 :: notavail
    integer                 :: gridRank
    integer,pointer         :: gridDims(:)
    integer                 :: poleVal, minPoleGid, maxPoleGid

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
      dualflag=0
    endif

    ! If convertToDual is TRUE, cannot use UserArea because the area defined
    ! in the grid file is not for the dual mesh
    if (present(addUserArea)) then
      if (addUserArea .and. dualflag==1) then
         call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="- Cannot use user area when convertToDual flag is set to TRUE", &
              ESMF_CONTEXT, rcToReturn=rc)
         return
      endif
    endif

    ! get global vm information
    !
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    esmffilename = ".esmf.nc"
    
    if (PetNo == 0) then
        ! this is a serial call into C code for now
        call c_ConvertSCRIP(filename, esmffilename,  &
          dualflag, localrc )
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    call ESMF_VMBarrier(vm)
    ESMF_MeshCreateFromScrip=ESMF_MeshCreateFromUnstruct(esmffilename,&
	addUserArea=addUserArea, rc=localrc)
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
          close(unit, STATUS='delete')
       endif
    endif    
   
   ! Add pole information, if created from a 2D grid file
    allocate(gridDims(2))
    call ESMF_ScripInq(filename, grid_rank=gridRank, grid_dims=gridDims, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
         ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    if (gridRank==2) then
       poleVal=4
       minPoleGid=1
       maxPoleGid=gridDims(1)
       call C_ESMC_MeshSetPoles(ESMF_MeshCreateFromScrip, &
            poleVal, minPoleGid, maxPoleGid, localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

       poleVal=5
       minPoleGid=gridDims(1)*gridDims(2)-gridDims(1)+1
       maxPoleGid=gridDims(1)*gridDims(2)
       call C_ESMC_MeshSetPoles(ESMF_MeshCreateFromScrip, &
            poleVal, minPoleGid, maxPoleGid, localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (associated(gridDims)) deallocate(gridDims)

    ! Output success
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
!         making up the Mesh. For a manifold, the spatial dimension can be larger than the 
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

    ! Create two distgrids, one for nodes and one for elements
    call C_ESMC_MeshCreateNodeDistGrid(ESMF_MeshCreateFromPointer%this, &
                                       ESMF_MeshCreateFromPointer%nodal_distgrid, &
                                       ESMF_MeshCreateFromPointer%numOwnedNodes, &
                                       localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    call C_ESMC_MeshCreateElemDistGrid(ESMF_MeshCreateFromPointer%this, &
                                       ESMF_MeshCreateFromPointer%element_distgrid, &
                                       ESMF_MeshCreateFromPointer%numOwnedElements, &
                                       localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    ! The C side has been created
    ESMF_MeshCreateFromPointer%isCMeshFreed=.false.

    ! Set as fully created 
    ESMF_MeshCreateFromPointer%hasSplitElem=.false.

    ! Set as fully created 
    ESMF_MeshCreateFromPointer%isFullyCreated=.true.

    ! Set default coordsys
    ESMF_MeshCreateFromPointer%coordSys=ESMF_COORDSYS_SPH_DEG

    if(present(rc)) rc = ESMF_SUCCESS

  end function ESMF_MeshCreateFromPointer
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromIntPtr()"
!!! BELOW ROUTINE EXPECTED TO GO AWAY WHEN MeshCap AND MeshCXX MERGED !!!
!BOPI
! !IROUTINE: ESMF_MeshCreate - Create a Mesh from a internal C++ pointer
!
! !INTERFACE:
    function ESMF_MeshCreateFromIntPtr(mesh_pointer, rc)

!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromIntPtr
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
!         making up the Mesh. For a manifold, the spatial dimension can be larger than the 
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

    ! Create internal structure and Set pointer
    call C_ESMC_MeshCreateFromIntPtr(ESMF_MeshCreateFromIntPtr%this,  &
                                  mesh_pointer, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return    

    ! Check init status of arguments
    ESMF_INIT_SET_CREATED(ESMF_MeshCreateFromIntPtr)

    ! Create two distgrids, one for nodes and one for elements
    call C_ESMC_MeshCreateNodeDistGrid(ESMF_MeshCreateFromIntPtr%this, &
                                       ESMF_MeshCreateFromIntPtr%nodal_distgrid, &
                                       ESMF_MeshCreateFromIntPtr%numOwnedNodes, &
                                       localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    call C_ESMC_MeshCreateElemDistGrid(ESMF_MeshCreateFromIntPtr%this, &
                                       ESMF_MeshCreateFromIntPtr%element_distgrid, &
                                       ESMF_MeshCreateFromIntPtr%numOwnedElements, &
                                       localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    

    ! The C side has been created
    ESMF_MeshCreateFromIntPtr%isCMeshFreed=.false.

    ! Set as fully created 
    ESMF_MeshCreateFromIntPtr%hasSplitElem=.false.

    ! Set as fully created 
    ESMF_MeshCreateFromIntPtr%isFullyCreated=.true.

    ! Set default coordsys
    ESMF_MeshCreateFromIntPtr%coordSys=ESMF_COORDSYS_SPH_DEG

    if(present(rc)) rc = ESMF_SUCCESS

  end function ESMF_MeshCreateFromIntPtr
!------------------------------------------------------------------------------


    
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshGetIntPtr()"
!BOPI
! !IROUTINE: ESMF_MeshGetIntPtr -- get internal pointer
!
! !INTERFACE:
   subroutine ESMF_MeshGetIntPtr(mesh, internalPtr, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                :: mesh
    type(ESMF_Pointer), intent(out)          :: internalPtr
    integer, intent(out) , optional            :: rc
!
! !DESCRIPTION:
!   Get the internal pointer. 
!
!   \begin{description}
!   \item [mesh]
!         Mesh to get internal pointer from
!   \item [internalPtr]
!         Internal pointer
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc 
    integer :: intMoabOn    

    ! Init localrc
    localrc = ESMF_SUCCESS
    
    call c_esmc_meshgetinternalptr(mesh, internalPtr, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    end subroutine ESMF_MeshGetIntPtr

!!!!!!!!  private Mesh subroutines !!!!!!!!!!
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetNumIds()"
subroutine ESMF_DistGridGetNumIds(distgrid, numIds, rc)
    type(ESMF_DistGrid), intent(in)            :: distgrid
    integer,             intent(out)           :: numIds
    integer,             intent(out), optional :: rc

    type(ESMF_DELayout) :: delayout
    integer :: localDeCount,lDE,numDEIDs
    integer :: localrc

    ! Get delayout from distgrid
    call ESMF_DistGridGet(distgrid, delayout=delayout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return    

    ! Get local number of DEs
    call ESMF_DELayoutGet(delayout, localDECount=localDECount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return    

    ! Iterate summing number of DEs
    numIds=0
    do lDE=0,localDECount-1
       call ESMF_DistGridGet(distgrid,localDe=lDE, &
            elementCount=numDEIds, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

       numIds=numIds+numDEIds
    enddo

end subroutine ESMF_DistGridGetNumIds

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetIds()"
subroutine ESMF_DistGridGetIds(distgrid, Ids, rc)
    type(ESMF_DistGrid), intent(in)            :: distgrid
    integer,             intent(out)           :: Ids(:)
    integer,             intent(out), optional :: rc

    type(ESMF_DELayout) :: delayout
    integer :: localDeCount,lDE,numDEIDs
    integer :: startPos
    integer :: localrc

    ! Get delayout from distgrid
    call ESMF_DistGridGet(distgrid, delayout=delayout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return    

    ! Get local number of DEs
    call ESMF_DELayoutGet(delayout, localDECount=localDECount, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return    

    ! Iterate summing number of DEs
    startPos=1
    do lDE=0,localDECount-1
       call ESMF_DistGridGet(distgrid,localDe=lDE, &
            elementCount=numDEIds, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

       if (numDEIds >0) then
          call ESMF_DistGridGet(distgrid,localDe=lDE, &
               seqIndexList=Ids(startPos:startPos+numDEIds-1), rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return
       endif

       startPos=startPos+numDEIds
    enddo

end subroutine ESMF_DistGridGetIds


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateRedist()"
!BOP

! !IROUTINE: ESMF_MeshCreate - Create a copy of a Mesh with a new distribution
!
! !INTERFACE:
  ! Private name; call using ESMF_MeshCreate()
    function ESMF_MeshCreateRedist(mesh, keywordEnforcer, nodalDistgrid, &
      elementDistgrid, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)                            :: ESMF_MeshCreateRedist

! !ARGUMENTS:
    type(ESMF_Mesh),     intent(in)            :: mesh
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_DistGrid), intent(in),  optional :: nodalDistgrid
    type(ESMF_DistGrid), intent(in),  optional :: elementDistgrid
    integer,             intent(out), optional :: rc
! 
! !DESCRIPTION:
!  Create a copy of an existing Mesh with a new distribution. Information
! in the Mesh such as connections, coordinates, areas, masks, etc. are 
! automatically redistributed to the new Mesh. To redistribute 
! data in Fields built on the original Mesh create a Field on the new Mesh
!  and then use the Field redistribution functionality 
! ({\tt ESMF\_FieldRedistStore()}, etc.). The equivalent methods
! can also be used for data in FieldBundles.  
!
!   \begin{description}
!   \item [mesh]
!         The source Mesh to be redistributed. 
!   \item [{[nodalDistgrid]}]
!         A 1D arbitrary distgrid describing the new distribution of 
!         the nodes across the PETs. 
!   \item [{[elementDistgrid]}]
!         A 1D arbitrary distgrid describing the new distribution of 
!         the elements across the PETs. 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc
    integer :: numNodeIds, numElemIds
    integer, allocatable :: nodeIds(:), elemIds(:)
    type(ESMF_DELayout) :: delayout
    integer             :: localDeCount

    ! Init localrc
    localrc = ESMF_SUCCESS

    ! Check input classes
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, nodalDistgrid, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, elementDistgrid, rc)

    ! If mesh has not been fully created
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    


    ! We don't handle a mesh containing split elements right now, 
    ! so complain and exit. 
    ! TODO: Will need to do this before CESM uses with HOMME grid
    if (mesh%hasSplitElem) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
               msg="- meshes with split elements can not be redisted right now", & 
               ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    !! Fill in information available in input mesh
    ! Same dimensions as input mesh
    ESMF_MeshCreateRedist%spatialDim=mesh%spatialDim
    ESMF_MeshCreateRedist%parametricDim=mesh%parametricDim

    ! Same coordSys
    ESMF_MeshCreateRedist%coordSys=mesh%coordSys

    ! Will have same freed status as input mesh
    ESMF_MeshCreateRedist%isCMeshFreed=mesh%isCMeshFreed

    ! Will have same split status as input mesh
    ESMF_MeshCreateRedist%hasSplitElem=mesh%hasSplitElem

    ! Will have same created status as input mesh
    ESMF_MeshCreateRedist%isFullyCreated=mesh%isFullyCreated


    !!! OPERATE BASED ON PRESENCE OF DISTGRIDS  !!!
    if (present(nodalDistgrid)) then

       !! NODE AND ELEMENT DISTGRID BOTH PRESENT !!
       if (present(elementDistgrid)) then
          ! Get number of node Ids
          call ESMF_DistGridGetNumIds(nodalDistgrid, &
               numNodeIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Get number of element Ids
          call ESMF_DistGridGetNumIds(elementDistgrid, &
               numElemIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! First fill in information not requiring redist

          ! Set Distgrids
          ESMF_MeshCreateRedist%nodal_distgrid=nodalDistgrid
          ESMF_MeshCreateRedist%element_distgrid=elementDistgrid

          ! Set number of owned things
          ESMF_MeshCreateRedist%numOwnedNodes=numNodeIds
          ESMF_MeshCreateRedist%numOwnedElements=numElemIds

          ! If the C side doesn't exist, then don't need to redist, so exit
          if (mesh%isCMeshFreed) then
             ESMF_INIT_SET_CREATED(ESMF_MeshCreateRedist)

             if (present(rc)) rc=ESMF_SUCCESS
             return
          endif

          ! Allocate space for node Ids
          allocate(nodeIds(numNodeIds))

          ! Get node Ids
          call ESMF_DistGridGetIds(nodalDistgrid, &
               nodeIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Allocate space for element Ids
          allocate(elemIds(numElemIds))

          ! Get element Ids
          call ESMF_DistGridGetIds(elementDistgrid,&
               elemIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return


          ! Call into C
          call C_ESMC_MeshCreateRedist(mesh,                  &
               numNodeIds, nodeIds,   &
               numElemIds, elemIds,   & 
               ESMF_MeshCreateRedist, &
               localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Deallocate gid arrays
          deallocate(nodeIds)
          deallocate(elemIds)
       else  !! JUST NODE DISTGRID PRESENT !!
          call ESMF_DistGridGetNumIds(nodalDistgrid, numNodeIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! First fill in elem information not requiring redist
          ESMF_MeshCreateRedist%nodal_distgrid=nodalDistgrid
          ESMF_MeshCreateRedist%numOwnedNodes=numNodeIds


          ! If the C side doesn't exist, then return an error 
          ! because I don't know how to distribute nodes without elem info 
          if (mesh%isCMeshFreed) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
   msg="- method does not work if the input Mesh has no " // &
       "C Mesh attached and just the nodeDistgrid is specified", & 
                  ESMF_CONTEXT, rcToReturn=rc) 
             return
          endif

          ! Allocate space for node Ids
          allocate(nodeIds(numNodeIds))

          ! Get node Ids
          call ESMF_DistGridGetIds(nodalDistgrid, &
               nodeIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return


          ! Call into C
          call C_ESMC_MeshCreateRedistNodes(mesh,                  &
               numNodeIds, nodeIds,   & 
               ESMF_MeshCreateRedist, &
               localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Deallocate gid arrays
          deallocate(nodeIds)

           ! Create elem distgrid and get number of nodes
          call C_ESMC_MeshCreateElemDistGrid( &
                      ESMF_MeshCreateRedist%this, &
                      ESMF_MeshCreateRedist%element_distgrid, &
                      ESMF_MeshCreateRedist%numOwnedElements, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return    
     endif
    else
       !! JUST ELEMENT DISTGRID PRESENT !!
       if (present(elementDistgrid)) then

          call ESMF_DistGridGetNumIds(elementDistgrid, numElemIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! First fill in elem information not requiring redist
          ESMF_MeshCreateRedist%element_distgrid=elementDistgrid
          ESMF_MeshCreateRedist%numOwnedElements=numElemIds


          ! If the C side doesn't exist, then return an error 
          ! because I don't know how to distribute nodes without elem info 
          if (mesh%isCMeshFreed) then
             call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
   msg="- method does not work if the input Mesh has no " // &
       "C Mesh attached and just the elemDistgrid is specified", & 
                  ESMF_CONTEXT, rcToReturn=rc) 
             return
          endif

          ! Allocate space for element Ids
          allocate(elemIds(numElemIds))

          ! Get element Ids
          call ESMF_DistGridGetIds(elementDistgrid, &
                 elemIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return


          ! Call into C
          call C_ESMC_MeshCreateRedistElems(mesh,                  &
               numElemIds, elemIds,   & 
               ESMF_MeshCreateRedist, &
               localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Deallocate gid arrays
          deallocate(elemIds)

           ! Create node distgrid and get number of nodes
          call C_ESMC_MeshCreateNodeDistGrid( &
                      ESMF_MeshCreateRedist%this, &
                      ESMF_MeshCreateRedist%nodal_distgrid, &
                      ESMF_MeshCreateRedist%numOwnedNodes, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return    

       else !! NO DISTGRIDs PRESENT -> make new mesh a copy of the old w/ new distgrids !!
             
          ! Get number of node Ids
          call ESMF_DistGridGetNumIds(mesh%nodal_distgrid, &
               numNodeIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Get number of element Ids
          call ESMF_DistGridGetNumIds(mesh%element_distgrid, &
               numElemIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! First fill in information not requiring redist

          ! Set number of owned things
          ESMF_MeshCreateRedist%numOwnedNodes=numNodeIds
          ESMF_MeshCreateRedist%numOwnedElements=numElemIds

          ! If the C side doesn't exist, then don't need to redist, so exit
          if (mesh%isCMeshFreed) then
             ESMF_INIT_SET_CREATED(ESMF_MeshCreateRedist)

             if (present(rc)) rc=ESMF_SUCCESS
             return
          endif

          ! Allocate space for node Ids
          allocate(nodeIds(numNodeIds))

          ! Get node Ids
          call ESMF_DistGridGetIds(mesh%nodal_distgrid, &
               nodeIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Allocate space for element Ids
          allocate(elemIds(numElemIds))

          ! Get element Ids
          call ESMF_DistGridGetIds(mesh%element_distgrid,&
               elemIds, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return


          ! Call into C
          call C_ESMC_MeshCreateRedist(mesh,                  &
               numNodeIds, nodeIds,   &
               numElemIds, elemIds,   & 
               ESMF_MeshCreateRedist, &
               localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return

          ! Deallocate gid arrays
          deallocate(nodeIds)
          deallocate(elemIds)

           ! Create node distgrid and get number of nodes
          call C_ESMC_MeshCreateNodeDistGrid( &
                      ESMF_MeshCreateRedist%this, &
                      ESMF_MeshCreateRedist%nodal_distgrid, &
                      ESMF_MeshCreateRedist%numOwnedNodes, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return    

           ! Create elem distgrid and get number of elems
          call C_ESMC_MeshCreateElemDistGrid( &
                      ESMF_MeshCreateRedist%this, &
                      ESMF_MeshCreateRedist%element_distgrid, &
                      ESMF_MeshCreateRedist%numOwnedElements, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return    
       endif
    endif


    ! Set as created
    ESMF_INIT_SET_CREATED(ESMF_MeshCreateRedist)

    if (present(rc)) rc=ESMF_SUCCESS
    return

end function ESMF_MeshCreateRedist
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateDual()"
!BOPI

! !IROUTINE: ESMF_MeshCreate - Create a dual of a mesh
!
! !INTERFACE:
    function ESMF_MeshCreateDual(mesh, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)                            :: ESMF_MeshCreateDual

! !ARGUMENTS:
    type(ESMF_Mesh),     intent(in)            :: mesh
    integer,             intent(out), optional :: rc
! 
! !DESCRIPTION:
!  Create the dual of an existing mesh.
!
!   \begin{description}
!   \item [mesh]
!         The source Mesh. 
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc


    ! Init localrc
    localrc = ESMF_SUCCESS

    ! Check input classes
    ESMF_INIT_CHECK_DEEP(ESMF_MeshGetInit, mesh, rc)

    ! If mesh has not been fully created
    if (.not. mesh%isFullyCreated) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                 msg="- the mesh has not been fully created", & 
                 ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    


    ! We don't handle a mesh containing split elements right now, 
    ! so complain and exit. 
    ! TODO: Will need to do this before CESM uses with HOMME grid
    if (mesh%hasSplitElem) then
       call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
               msg="- meshes with split elements can not be redisted right now", & 
               ESMF_CONTEXT, rcToReturn=rc) 
       return 
    endif    

    !! Fill in information available in input mesh
    ! Same dimensions as input mesh
    ESMF_MeshCreateDual%spatialDim=mesh%spatialDim
    ESMF_MeshCreateDual%parametricDim=mesh%parametricDim

    ! Same coordSys
    ESMF_MeshCreateDual%coordSys=mesh%coordSys

    ! Will have same freed status as input mesh
    ESMF_MeshCreateDual%isCMeshFreed=mesh%isCMeshFreed

    ! Will have same split status as input mesh
    ESMF_MeshCreateDual%hasSplitElem=mesh%hasSplitElem

    ! Will have same created status as input mesh
    ESMF_MeshCreateDual%isFullyCreated=mesh%isFullyCreated


    ! Call into C
    call C_ESMC_MeshCreateDual(mesh,     &
         ESMF_MeshCreateDual, &
         localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return


    ! Create node distgrid and get number of nodes
    call C_ESMC_MeshCreateNodeDistGrid( &
                      ESMF_MeshCreateDual%this, &
                      ESMF_MeshCreateDual%nodal_distgrid, &
                      ESMF_MeshCreateDual%numOwnedNodes, localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
               ESMF_CONTEXT, rcToReturn=rc)) return    

    call C_ESMC_MeshCreateElemDistGrid(ESMF_MeshCreateDual%this, &
                                       ESMF_MeshCreateDual%element_distgrid, &
                                       ESMF_MeshCreateDual%numOwnedElements, &
                                       localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return    


    ! Set as created
    ESMF_INIT_SET_CREATED(ESMF_MeshCreateDual)

    if (present(rc)) rc=ESMF_SUCCESS
    return

end function ESMF_MeshCreateDual
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
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
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
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
             ESMF_CONTEXT, rcToReturn=rc)) return

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
! !IROUTINE: ESMF_MeshGet - Get object-wide Mesh information
!
! !INTERFACE:
      subroutine ESMF_MeshGet(mesh, parametricDim, spatialDim, &
                   nodalDistgrid, elementDistgrid, &
                   numOwnedNodes, ownedNodeCoords, &
                   numOwnedElements, ownedElemCoords, isMemFreed, coordSys, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
    type(ESMF_Mesh),          intent(in)            :: mesh
    integer,                  intent(out), optional :: parametricDim
    integer,                  intent(out), optional :: spatialDim
    type(ESMF_DistGrid),      intent(out), optional :: nodalDistgrid
    type(ESMF_DistGrid),      intent(out), optional :: elementDistgrid
    integer,                  intent(out), optional :: numOwnedNodes
    real(ESMF_KIND_R8),       intent(out), optional :: ownedNodeCoords(:)
    integer,                  intent(out), optional :: numOwnedElements
    real(ESMF_KIND_R8),       intent(out), optional :: ownedElemCoords(:)
    logical,                  intent(out), optional :: isMemFreed
    type(ESMF_CoordSys_Flag), intent(out), optional :: coordSys
    integer,                  intent(out), optional :: rc
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
! making up the Mesh. For a manifold, the spatial dimension can be larger than the 
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
! \item [{[ownedElemCoords]}]
! The center coordinates for the local elements. These coordinates will be in the proper order to correspond
! with the elements in the {\tt elementDistgrid} returned by this call, and hence with a Field built on the
! center of {\tt mesh}. The size of the input array should be the spatial dim of {\tt mesh} times
! {\tt numOwnedElements}.
! \item [{[isMemFreed]}]
! Indicates if the coordinate and connection memory been freed from {\tt mesh}. If so, it
! can no longer be used as part of an {\tt ESMF\_FieldRegridStore()} call.
! \item[{[coordSys]}] 
!  The coordinate system of the grid coordinate data. 
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
       call C_ESMC_GetLocalCoords(mesh, ownedNodeCoords, &
                                  mesh%spatialDim,localrc) 
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
           ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(ownedElemCoords)) then
       ! If mesh has been freed then exit
       if (mesh%isCMeshFreed) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_OBJ_WRONG, & 
                msg="- the mesh internals have been freed", & 
                ESMF_CONTEXT, rcToReturn=rc) 
          return
       endif

       ! Check array size
       if (size(ownedElemCoords)<mesh%numOwnedElements*mesh%spatialDim) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_WRONG, & 
                msg="- owndedElemCoords too small to hold coordinates", & 
                ESMF_CONTEXT, rcToReturn=rc) 
          return
       endif

       ! Get coords from C
       call C_ESMC_GetLocalElemCoords(mesh, ownedElemCoords, &
                                  mesh%spatialDim,localrc) 
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
      if (present(coordSys)) coordSys =mesh%coordSys 

      if (present(rc)) rc = localrc

    end subroutine ESMF_MeshGet

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshIsCreated()"
!BOP
! !IROUTINE: ESMF_MeshIsCreated - Check whether a Mesh object has been created

! !INTERFACE:
  function ESMF_MeshIsCreated(mesh, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_MeshIsCreated
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)            :: mesh
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt mesh} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[mesh]
!     {\tt ESMF\_Mesh} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_MeshIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_MeshGetInit(mesh)==ESMF_INIT_CREATED) &
      ESMF_MeshIsCreated = .true.
  end function
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
    integer                       :: i, localrc      ! local return code
    type(ESMF_DistGridMatch_Flag) :: matchResultNode, matchResultElem

    real(ESMF_KIND_R8), pointer   :: area1(:), area2(:)

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
      return
    endif

    ! check area
    allocate(area1(mesh1%numOwnedElements), area2(mesh2%numOwnedElements), stat=localrc)
    if (ESMF_LogFoundAllocError(localrc, &
        msg="- MeshMatch: Allocating area1 and area2 failed ", &
        ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_MeshGetOrigElemArea(mesh1, area1, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_MeshGetOrigElemArea(mesh2, area2, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    do i = 1, mesh1%numOwnedElements
      if(area1(i) /= area2(i)) then
        ESMF_MeshMatch = .false.
        deallocate(area1, area2)
        return
      endif
    enddo

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
#define ESMF_METHOD "ESMF_MeshSetMOAB()"
!BOP
! !IROUTINE: ESMF_MeshSetMOAB -- Toggle using the MOAB library internally. 
!
! !INTERFACE:
   subroutine ESMF_MeshSetMOAB(moabOn, rc)
!
! !ARGUMENTS:
    logical, intent(in)                        :: moabOn
    integer, intent(out) , optional            :: rc
!
! !DESCRIPTION:
!   This method can be employed to turn on or off using the MOAB library 
!   to hold the internal structure of the Mesh. When set to .true. the following
!   Mesh create calls create a Mesh using MOAB internally. When set to .false. the following
!   Mesh create calls use the ESMF native internal mesh respresentation. Note that ESMF Meshes 
!   created on MOAB are only supported in a limited set of operations and should be used
!   with caution as they haven't yet been tested as thoroughly as the native version.  
!
!   \begin{description}
!   \item [moabOn]
!         Variable used to turn MOAB on or off
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer :: localrc 
    integer :: intMoabOn    

    ! Init localrc
    localrc = ESMF_SUCCESS
    
   ! Translate to integer
   intMoabOn=0
   if (moabOn) then
      intMoabOn=1
   endif

    call c_esmc_meshsetMOAB(intMoabOn, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    end subroutine ESMF_MeshSetMOAB

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
#define ESMF_METHOD "ESMF_MeshGetElemFrac2()"
!BOPI
! !IROUTINE: ESMF_MeshGetElemFrac2 - Get frac of elements in mesh
!
! !INTERFACE:
    subroutine ESMF_MeshGetElemFrac2(mesh, fracList, rc)
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
    call C_ESMC_MeshGetFrac2(mesh%this, size(fracList), fracList, localrc);
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return


    ! return success
     if  (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_MeshGetElemFrac2
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
#define ESMF_METHOD "ESMF_MeshTurnOnCellMask()"
!BOPI
! !IROUTINE: ESMF_MeshTurnOnCellMask -- Turn on masking to correspond to maskValues
!
! !INTERFACE:
   subroutine ESMF_MeshTurnOnCellMask(mesh, maskValues, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                :: mesh
    integer(ESMF_KIND_I4), optional            :: maskValues(:)
    integer, intent(out) , optional            :: rc
!
! !DESCRIPTION:
!   Turn on mesh masking
!
!   \begin{description}
!   \item [mesh]
!         The mesh to turn on masking for
!   \item [maskValues]
!         Values to set as masked
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc 
    type(ESMF_InterfaceInt) :: maskValuesArg

    ! Init localrc
    localrc = ESMF_SUCCESS

    ! If not present, then don't need to turn anything on
    if (.not. present(maskValues)) then
       if (present(rc)) rc = ESMF_SUCCESS
       return
    endif

    ! convert mask values 
    maskValuesArg = ESMF_InterfaceIntCreate(maskValues, rc=localrc)
    	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	  ESMF_CONTEXT, rcToReturn=rc)) return
 
    call c_esmc_meshturnoncellmask(mesh, maskValuesArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	    ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterfaceIntDestroy(maskValuesArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	    ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_MeshTurnOnCellMask


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshTurnOffCellMask()"
!BOPI
! !IROUTINE: ESMF_MeshTurnOffCellMask -- Turn off masking
!
! !INTERFACE:
   subroutine ESMF_MeshTurnOffCellMask(mesh, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                :: mesh
    integer, intent(out) , optional            :: rc
!
! !DESCRIPTION:
!   Turn on mesh masking
!
!   \begin{description}
!   \item [mesh]
!         The mesh to turn on masking for
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc 
    
    ! Init localrc
    localrc = ESMF_SUCCESS
    
    call c_esmc_meshturnoffcellmask(mesh, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    end subroutine ESMF_MeshTurnOffCellMask




!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshTurnOnNodeMask()"
!BOPI
! !IROUTINE: ESMF_MeshTurnOnNodeMask -- Turn on masking to correspond to maskValues
!
! !INTERFACE:
   subroutine ESMF_MeshTurnOnNodeMask(mesh, maskValues, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                :: mesh
    integer(ESMF_KIND_I4), optional            :: maskValues(:)
    integer, intent(out) , optional            :: rc
!
! !DESCRIPTION:
!   Turn on mesh masking
!
!   \begin{description}
!   \item [mesh]
!         The mesh to turn on masking for
!   \item [maskValues]
!         Values to set as masked
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc 
    type(ESMF_InterfaceInt) :: maskValuesArg

    ! Init localrc
    localrc = ESMF_SUCCESS

    ! If not present, then don't need to turn anything on
    if (.not. present(maskValues)) then
       if (present(rc)) rc = ESMF_SUCCESS
       return
    endif

    ! convert mask values 
    maskValuesArg = ESMF_InterfaceIntCreate(maskValues, rc=localrc)
    	if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	  ESMF_CONTEXT, rcToReturn=rc)) return
 
    call c_esmc_meshturnonnodemask(mesh, maskValuesArg, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	    ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_InterfaceIntDestroy(maskValuesArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      	    ESMF_CONTEXT, rcToReturn=rc)) return

    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_MeshTurnOnNodeMask


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshTurnOffNodeMask()"
!BOPI
! !IROUTINE: ESMF_MeshTurnOffNodeMask -- Turn off masking
!
! !INTERFACE:
   subroutine ESMF_MeshTurnOffNodeMask(mesh, rc)
!
! !ARGUMENTS:
    type(ESMF_Mesh), intent(in)                :: mesh
    integer, intent(out) , optional            :: rc
!
! !DESCRIPTION:
!   Turn on mesh masking
!
!   \begin{description}
!   \item [mesh]
!         The mesh to turn on masking for
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc 
    
    ! Init localrc
    localrc = ESMF_SUCCESS
    
    call c_esmc_meshturnoffnodemask(mesh, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    
    if (present(rc)) rc = ESMF_SUCCESS
    
    end subroutine ESMF_MeshTurnOffNodeMask



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

#if 0
!------------------------------------------------------------------------------
!---  In this function, the node coordinates of the mesh are read in to multiple PETs in 
!---  parallel and redistributed to its resident nodes based on the cell partition.
!---  The other version of the same function reads the entire node coordinates into every
!---  PET and only store the ones the local cells use.  This version may same some memory
!---  space if the number of nodes in the mesh is huge.  But it creates more communication
!---  and handshaking thus will run slower in most of the cases.
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_MeshCreateFromUnstruct()"
!BOPI
! !IROUTINE: ESMF_MeshCreate - Create a Mesh from a grid file defined in the ESMF Unstructured
!  Grid format -- this is the parallel version and it only works for UGRID.  
!  Keep the code here but commented it out for 6.3.0 release
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
    function ESMF_MeshCreateFromUnstruct(filename, fileformat, meshname, &
			addUserArea, maskFlag, varname, rc)
!
!
! !RETURN VALUE:
    type(ESMF_Mesh)         :: ESMF_MeshCreateFromUnstruct
! !ARGUMENTS:
    character(len=*), intent(in)              :: filename
    type(ESMF_FileFormat_Flag), optional, intent(in) :: fileformat
    character(len=*), optional, intent(in)    :: meshname
    logical, intent(in), optional	      :: addUserArea
    type(ESMF_MeshLoc), intent(in), optional  :: maskFlag
    character(len=*), optional, intent(in)    :: varname
    integer, intent(out), optional            :: rc
!
! !DESCRIPTION:
!   Create a mesh from a grid file defined in the ESMF Unstructured grid format.
!
!   \begin{description}
!   \item [filename]
!         The name of the grid file
!   \item[{[addUserArea]}] 
!         if {\tt .true.}, the cell area will be read in from the GRID file.  This feature is
!         only supported when the grid file is in the SCRIP or ESMF format. 
!   \item [{[fileformat]}]
!         The type of grid file
!   \item[{[meshname]}]
!         The dummy variable for the mesh metadata in the UGRID file if the {\tt fileformat}
!         is {\tt ESMF\_FILEFORMAT\_UGRID}
!   \item[{[maskFlag]}]
!      If present, generate the mask using the missing\_value attribute defined in 'varname' on
!      the location defined by the flag, the accepted values are {\tt ESMF\_MESHLOC\_NODE} or 
!      {\tt ESMF\_MESHLOC\_ELEMENT}
!   \item[{[varname]}]
!      If maskFlag is present, provide a variable name stored in the grid file and
!      the mask will be generated using the missing value of the data value of
!      this variable.  The first two dimensions of the variable has to be the
!      the longitude and the latitude dimension and the mask is derived from the
!      first 2D values of this variable even if this data is 3D, or 4D array.
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
    integer                             :: NodeCnt, total
    integer, allocatable                :: NodeId(:)
    integer, allocatable                :: NodeUsed(:)
    real(ESMF_KIND_R8), allocatable     :: NodeCoords1D(:)
    real(ESMF_KIND_R8), allocatable     :: NodeCoordsCart(:)
    real(ESMF_KIND_R8)                  :: coorX, coorY
    integer, allocatable                :: NodeOwners(:)
    integer, allocatable                :: NodeOwners1(:)
    integer, pointer                    :: glbNodeMask(:), NodeMask(:)

    integer                             :: ElemNo, TotalElements, startElemNo
    integer                             :: ElemCnt,i,j,k,n,dim, nedges
    integer				:: localNodes, myStartElmt
    integer                             :: ConnNo, TotalConnects
    integer, allocatable                :: ElemId(:)
    integer, allocatable                :: ElemType(:)
    integer, allocatable                :: ElemConn(:)
    integer, pointer                    :: elementMask(:), ElemMask(:)
    real(ESMF_KIND_R8), pointer         :: elementArea(:), ElemArea(:)
    integer, allocatable                :: LocalElmTable(:)
    integer                             :: sndBuf(1)
    type(ESMF_VM)                       :: vm
    type(ESMF_Mesh)                     :: Mesh
    integer(ESMF_KIND_I4)               :: localSplitElems(1)
    integer(ESMF_KIND_I4)               :: globalSplitElems(1)
    logical                             :: existSplitElems
    integer                             :: numPoly
#if 0
    integer, parameter                  :: maxNumPoly=20
    real(ESMF_KIND_R8)                  :: polyCoords(3*maxNumPoly)
    real(ESMF_KIND_R8)                  :: polyDblBuf(3*maxNumPoly)
    real(ESMF_KIND_R8)                  :: area(maxNumPoly)
    integer                             :: polyIntBuf(maxNumPoly)
    integer                             :: triInd(3*(maxNumPoly-2))
#else 
    integer                             :: maxNumPoly
    real(ESMF_KIND_R8),allocatable      :: polyCoords(:)
    real(ESMF_KIND_R8),allocatable      :: polyDblBuf(:)
    real(ESMF_KIND_R8),allocatable      :: area(:)
    integer,allocatable                 :: polyIntBuf(:)
    integer,allocatable                 :: triInd(:)
#endif
    real(ESMF_KIND_R8)                  :: totalarea
    integer                             :: spatialDim
    integer                             :: parametricDim
    integer                             :: lni,ti,tk
    type(ESMF_FileFormat_Flag)          :: fileformatlocal
    integer                             :: coordDim
    logical                             :: convertToDeg
    logical                             :: haveNodeMask, haveElmtMask
    logical                             :: haveMask
    logical 				:: localAddUserArea
    type(ESMF_MeshLoc)			:: localAddMask
    real(ESMF_KIND_R8), pointer         :: varbuffer(:)
    real(ESMF_KIND_R8)                  :: missingvalue
    integer                             :: cartSpatialDim    ! sp. dim of grid when converted to 
    type(ESMF_CoordSys_Flag)            :: coordSys

    ! additional variables used to parallelize node IO 
    integer, pointer                    :: segmentTbl(:), pairoffsets(:)
    integer, pointer                    :: nodepairs(:), newpairs(:), mypair(:), allpairs(:)
    integer, pointer                    :: seqIndexList(:)
    integer, pointer                    :: haloIndexList(:)
    integer                             :: totalpairs, startind, endind
    integer                             :: localnodecnt, totalnodecnt, halonodecnt    
    integer                             :: haloNodes, startNode
    type(ESMF_DistGrid)                 :: regDistGrid, nodeDistGrid
    type(ESMF_Array)                    :: regCoordArray, nodeCoordArray
    real(ESMF_KIND_R8), pointer         :: fptr(:,:)
    type(ESMF_RouteHandle)              :: redistHdl, haloHandle

    ! Initialize return code; assume failure until success is certain
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    if (present(addUserArea)) then
	localAddUserArea = addUserArea
    else
	localAddUserArea = .false.
    endif

    if (present(maskFlag)) then
	localAddMask = maskFlag
    else
	localAddMask = ESMF_MESHLOC_NONE
    endif

    ! Read the mesh definition from the file
    if (present(fileformat)) then
	fileformatlocal = fileformat
    else
	fileformatlocal = ESMF_FILEFORMAT_ESMFMESH
    endif

    if (fileformatlocal == ESMF_FILEFORMAT_UGRID) then
	if (.not. present(meshname)) then
           call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
                             msg="- meshname argument is missing", & 
                             ESMF_CONTEXT, rcToReturn=rc) 
        endif
    endif

    ! get global vm information
    !
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up local pet info
    call ESMF_VMGet(vm, localPet=PetNo, petCount=PetCnt, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! Default coordinate system
    coordSys = ESMF_COORDSYS_SPH_DEG
 
    if (fileformatlocal == ESMF_FILEFORMAT_ESMFMESH) then
       ! Get coordDim
       call ESMF_EsmfInq(filename,coordDim=coordDim, haveNodeMask=haveNodeMask, &
       	    haveElmtMask=haveElmtMask, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

       ! Don't convert if not 2D because that'll be cartesian right now
       if (coordDim .eq. 2) then
          convertToDeg = .true.
       else 
          convertToDeg = .false.
       endif

       ! Get information from file     
       if (haveNodeMask) then
           call ESMF_EsmfGetNode(filename, nodeCoords, nodeMask=glbNodeMask,&
	   		      convertToDeg=convertToDeg, coordSys=coordSys, rc=localrc)
       else
           call ESMF_EsmfGetNode(filename, nodeCoords, &
	   		      convertToDeg=convertToDeg, coordSys=coordSys, rc=localrc)
       endif			      			       
       if (haveElmtMask) then
	if (localAddUserArea) then
           call ESMF_EsmfGetElement(filename, elementConn, elmtNum, &
                                 startElmt, elementMask=elementMask, elementArea=elementArea, &
	 			 rc=localrc)
	else
           call ESMF_EsmfGetElement(filename, elementConn, elmtNum, &
                                 startElmt, elementMask=elementMask, &
	 			 rc=localrc)
	endif
       else
	if (localAddUserArea) then
           call ESMF_EsmfGetElement(filename, elementConn, elmtNum, &
                                 startElmt, elementArea=elementArea, &
				 rc=localrc)
	else
           call ESMF_EsmfGetElement(filename, elementConn, elmtNum, &
                                 startElmt, rc=localrc)
	endif
       endif

       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rcToReturn=rc)) return
    elseif (fileformatlocal == ESMF_FILEFORMAT_UGRID) then
       haveElmtMask = .false.
       haveNodeMask = .false.
       if (localAddMask == ESMF_MESHLOC_ELEMENT) then
          haveElmtMask = .true.
       elseif (localAddMask == ESMF_MESHLOC_NODE) then
          haveNodeMask = .true.
       endif
       ! Get information from file
       call ESMF_GetElemFromUGridFile(filename, meshname, elementConn, &
                                   elmtNum, startElmt, rc=localrc)
       if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return

       ! Chenk if the grid is 3D or 2D
       call ESMF_UGridInq(filename, meshname, nodeCount=nodeCnt, &
       	    			    nodeCoordDim=coordDim, rc=localrc)

       if (coordDim == 2 .and. localAddMask == ESMF_MESHLOC_ELEMENT) then
	  !Get the variable and the missing value attribute from file
	  ! Total number of local elements
          ElemCnt = ubound (elementConn, 2)
          allocate(varbuffer(ElemCnt))
	  call ESMF_UGridGetVarByName(filename, varname, varbuffer, startind=startElmt, &
		count=ElemCnt, location="face", &
		missingvalue=missingvalue, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
	  ! Create local mask 
	  allocate(elementMask(ElemCnt))
	  elementMask(:)=1
	  do i=1,ElemCnt
	    if (varbuffer(i) == missingvalue) elementMask(i)=0
          enddo
	  deallocate(varbuffer)
       elseif (coordDim == 2 .and. localAddMask == ESMF_MESHLOC_NODE) then
	  !Get the variable and the missing value attribute from file
	  ! Total number of total nodes
          allocate(varbuffer(nodeCnt))
	  call ESMF_UGridGetVarByName(filename, varname, varbuffer, &
		location="node", &
		missingvalue=missingvalue, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                   ESMF_CONTEXT, rcToReturn=rc)) return
	  ! Create local mask 
	  allocate(glbNodeMask(nodeCnt))
	  glbNodeMask(:)=1
	  do i=1,nodeCnt
	    if (varbuffer(i) == missingvalue) glbNodeMask(i)=0
          enddo
	  deallocate(varbuffer)
       endif 
    else
       call ESMF_LogSetError(ESMF_RC_ARG_WRONG, & 
                             msg="- unrecognized fileformat", & 
                             ESMF_CONTEXT, rcToReturn=rc) 
       return
    endif

   ! Figure out dimensions 
    if (coordDim .eq. 2) then
       parametricDim=2
       spatialDim = 2
       if (coordSys == ESMF_COORDSYS_CART) then
         cartSpatialDim = 2
       else
         cartSpatialDim=3  ! Assuming that this is spherical
       endif
    else if (coordDim .eq. 3) then
       parametricDim=3
       spatialDim=3
       cartSpatialDim=3 
       coordSys = ESMF_COORDSYS_CART 
    else
       call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, & 
            msg="- only coordDim 2 or 3 is supported right now", & 
            ESMF_CONTEXT, rcToReturn=rc) 
       return
    endif

    if (parametricDim == 2) then
        Mesh = ESMF_MeshCreate3part (parametricDim, spatialDim, &
	       coordSys=coordSys,rc=localrc)
    else
        Mesh = ESMF_MeshCreate3part (parametricDim, spatialDim, &
	       coordSys=coordSys,rc=localrc)
    endif    
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! These two arrays are temp arrays
    ! NodeUsed() used for multiple purposes, first, find the owners of the node
    ! later, used to store the local Node ID to be used in the ElmtConn table
    print *, PetNo, 'Before allocate NodeUsed'
    allocate (NodeUsed(NodeCnt))

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
    maxNumPoly=0
    if (parametricDim .eq. 2) then
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
          if (elmtNum(ElemNo) > maxNumPoly) then
             maxNumPoly=elmtNum(ElemNo)
          endif
       end do
    else ! If not parametricDim==2, assuming parmetricDim==3
       do ElemNo =1, ElemCnt
          do i=1,elmtNum(ElemNo)	
             NodeUsed(elementConn(i,ElemNo))=PetNo
          enddo
          TotalConnects = TotalConnects+elmtNum(ElemNo)
       end do       
    endif

   ! write(*,*) "maxNumPoly=",maxNumPoly

    ! create sparse matrix for localNode
    totalpairs = 200
    allocate(mypair(1), nodepairs(totalpairs))
    mypair(1) = 0
    i=1
    j=1
    totalnodecnt=0
    do while (i <= NodeCnt)
      do while (NodeUsed(i)>PetNo)
        i=i+1
      end do
      if (i > NodeCnt) EXIT
      nodepairs(j)=i
      do while (NodeUsed(i)==PetNo)
        i=i+1
        totalnodecnt = totalnodecnt+1
      end do
      nodepairs(j+1)=i-1
      j=j+2
      mypair(1)=mypair(1)+1
      if (j>totalpairs) then
         !print *, 'reallocate nodepairs'
         allocate(newpairs(totalpairs*2))
	 newpairs(1:totalpairs)=nodepairs
         deallocate(nodepairs)
         nodepairs => newpairs
         totalpairs=totalpairs*2
      endif
    enddo

    ! collect the node segment pairs in all the PETs  
    allocate(allpairs(PetCnt))
    call ESMF_VMAllGather(vm, mypair, allpairs, 1, rc=localrc) 
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
    
    allocate(pairoffsets(PetCnt))
    pairoffsets(1)=0
    allpairs = allpairs*2
    totalpairs = allpairs(1)
    do i=2,PetCnt
      pairoffsets(i)=pairoffsets(i-1)+allpairs(i-1)
      totalpairs = totalpairs+allpairs(i)
    enddo
   
    print *, PetNo, " Total node pairs and total nodes: ",  mypair(1), totalpairs, totalnodecnt

    allocate(segmentTbl(totalpairs))
    call ESMF_VMAllGatherV(vm, nodepairs, mypair(1)*2, segmentTbl, allpairs, &
             pairoffsets, rc=localrc)

    !print *, 'pairoffsets ', pairoffsets
    !print *, PetNo, 'segmentTbl ', segmentTbl(pairoffsets(PetNo+1)+1:pairoffsets(PetNo+1)+100)

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Need to decide who owns the local nodes
    ! Assign the node to the first PET that uses it, so only need to 
    ! search the pairs from PET=0:PetNo-1
    halonodecnt = 0
    do j=1, mypair(1)
      startind=nodepairs((j-1)*2+1)
      endind = nodepairs((j-1)*2+2)
      !print *, PetNo, "nodepairs: ", j, startind, endind
      do k=startind, endind
        do i=1, PetNo
          do n = pairoffsets(i)+1, pairoffsets(i+1),2
            if (k < segmentTbl(n)) EXIT
            if (k > segmentTbl(n+1)) CYCLE
            NodeUsed(k)= i-1
	    halonodecnt = halonodecnt+1
	    !print *, PetNo, k, 'fall within ', segmentTbl(n), segmentTbl(n+1)
            goto 100
           enddo
        enddo
        100 continue
      enddo
    enddo

    deallocate(pairoffsets, segmentTbl, allpairs, mypair, nodepairs)
    print *, PetNo, "total local nodes and halo nodes counts:", totalnodecnt, halonodecnt

    ! Create a distgrid based on the local ownership of the node ids
     allocate(seqIndexList(totalnodecnt-halonodecnt))
     allocate(haloIndexList(halonodecnt))
    ! count number of nodes used and convert NodeUsed values into local index
    localNodes = 0
    haloNodes = 0
    do NodeNo = 1, NodeCnt
       if (NodeUsed(NodeNo) == PetNo) then
         localNodes = localNodes+1
         seqIndexList(localNodes)=NodeNo
       elseif (NodeUsed(NodeNo) < PetNo) then
         haloNodes = haloNodes+1
         haloIndexList(haloNodes)=NodeNo
       endif
    enddo
    nodeDistGrid = ESMF_DistGridCreate(arbSeqIndexList=seqIndexList, rc=localrc)
    nodeCoordArray= ESMF_ArrayCreate(nodeDistGrid, ESMF_TYPEKIND_R8, &
    		                distgridToArrayMap=(/2/), &
 		    		haloSeqIndexList=haloIndexList, undistLBound=(/1/), & 
				undistUBound=(/coordDim/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Read in the node coordinates in parallel and redistribute it based on the arb. distgrid
    localnodecnt=NodeCnt/PetCnt
    startNode=localnodecnt*PetNo+1
    if (PetNo == PetCnt-1) then
       localnodecnt = localnodecnt + mod(NodeCnt, PetCnt)
    endif
    call ESMF_GetNodeFromUGridFile(filename, meshname, nodeCoords, &
         nodeCount=localnodecnt, startNode=startNode, &
         convertToDeg = .true., rc=localrc) 
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    print *, PetNo, 'NodeCoords ', nodeCoords(1,1000), nodeCoords(2,1000)
   
    ! Create a distgrid with regular distribution for the NodeCoords
    regDistGrid = ESMF_DistGridCreate((/1/), (/NodeCnt/), &
    		  	   regDecomp=(/PetCnt/), &
    		           decompflag=(/ESMF_DECOMP_RESTLAST/), &
			   indexflag=ESMF_INDEX_GLOBAL,  rc=localrc) 
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Create an array with one undistributed dimension for the coordinates
    regCoordArray= ESMF_ArrayCreate(regDistGrid, ESMF_TYPEKIND_R8, &
    		            distgridToArrayMap=(/2/), &
   		    	    undistLBound=(/1/), undistUBound=(/coordDim/),rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set array values
    call ESMF_ArrayGet(regCoordArray, localDe=0, farrayPtr=fptr, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! NodeCoords(coordDim, localnodecnt), fptr(coordDim, startNode:startNode+localnodecnt)
    ! can we do the following array assignment?
    print *, 'size of NodeCoords: ', ubound(NodeCoords)
    ! print *, 'bounds of fptr: ', lbound(fptr), ubound(fptr)
    fptr(:,:) = NodeCoords(:,:)

    ! print *, 'fptr ', fptr(1,1000), fptr(2,1000)

    deallocate(NodeCoords)

    print *, PetNo, 'Before ArrayRedistStore'
    ! call array redist to redist to the new distribution
    call ESMF_ArrayRedistStore(regCoordArray, nodeCoordArray, routehandle=redistHdl, &
    	 				      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_ArrayRedist(regCoordArray, nodeCoordArray, routehandle=redistHdl, &
    	 				 rc=localrc)               
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    print *, PetNo, 'Before ArrayHaloStore'
    ! Need to get the data for the Halo region
    call ESMF_ArrayHaloStore(nodeCoordArray, haloHandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_ArrayHalo(nodeCoordArray, haloHandle, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    ! Release the route handle
    call ESMF_ArrayHaloRelease(haloHandle, rc=localrc)
    call ESMF_ArrayRedistRelease(redistHdl, rc=localrc)
    call ESMF_ArrayDestroy(regCoordArray)
    call ESMF_DistGridDestroy(regDistGrid)

    ! Now, get the node coordinates fortran pointer, this fptr includes both the
    ! exclusive region and the halo region 
    call ESMF_ArrayGet(nodeCoordArray, farrayPtr=nodeCoords, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
         ESMF_CONTEXT, rcToReturn=rc)) return

    !print *, 'NodeCoords ', nodeCoords(1,1000), nodeCoords(2,1000)
    ! allocate nodes arrays for ESMF_MeshAddNodes()
    allocate (NodeId(totalnodecnt))
    allocate (NodeOwners(totalnodecnt))

    localnodecnt=totalnodecnt-halonodecnt
    NodeId(1:localnodecnt)=seqIndexList
    NodeId(localnodecnt+1:totalnodecnt)=haloIndexList
    NodeOwners(1:localnodecnt)=PetNo
    ! Now change NodeUsed content to store the local node index to be used by elemConn    
    do i = 1, localnodecnt
       NodeUsed(seqIndexList(i))=i
    enddo
    do i = 1, halonodecnt
      NodeOwners(localnodecnt+i) = NodeUsed(haloIndexList(i))
      NodeUsed(haloIndexList(i))=localnodecnt+i
    enddo
    deallocate(seqIndexList, haloIndexList)

    if (parametricDim .eq. 2) then
       allocate (NodeCoords1D(totalnodecnt*CoordDim))
    else ! If not parametricDim==2, assuming parmetricDim==3
       allocate(NodeCoords1D(totalnodecnt*3))
    endif

    i = 1
    total = 0
    if (parametricDim .eq. 2) then
       do i = 1, totalnodecnt
          do dim = 1, CoordDim
            NodeCoords1D ((i-1)*CoordDim+dim) = nodeCoords (dim, i)
          end do
       end do
    else ! If not parametricDim==2, assuming parmetricDim==3
       do NodeNo = 1, totalnodecnt
          do dim = 1, 3
             NodeCoords1D ((i-1)*3+dim) = nodeCoords(dim, NodeNo)
	  end do
       end do
    endif

    ! Add nodes
    if (.not. haveNodeMask) then
       ! Add nodes
       call ESMF_MeshAddNodes (Mesh, NodeIds=NodeId, &
                            NodeCoords=NodeCoords1D, &
                            NodeOwners=NodeOwners, &
                            rc=localrc)
    else
       allocate(NodeMask(localNodes))
       do i=1,localNodes
         NodeMask(i)=glbNodeMask(NodeId(i))
       enddo
       call ESMF_MeshAddNodes (Mesh, NodeIds=NodeId, &
                            NodeCoords=NodeCoords1D, &
                            NodeOwners=NodeOwners, &
			    NodeMask = NodeMask, &
                            rc=localrc)
       deallocate(NodeMask)
       deallocate(glbNodeMask)		    
    endif 

    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_ArrayDestroy(nodeCoordArray)
    call ESMF_DistGridDestroy(nodeDistGrid)

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

    ! allocate element arrays for the local elements
    allocate (ElemId(TotalElements))
    allocate (ElemType(TotalElements))
    allocate (ElemConn(TotalConnects))
    if (localAddUserArea) allocate(ElemArea(TotalElements))

    ! Allocate mask if the user wants one
    haveMask=.false.
    if (haveElmtMask) then 
       allocate (ElemMask(TotalElements))
       haveMask=.true.
    endif

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

    ! Allocate a mask even if the user doesn't want one, if we have split elements 
    if (existSplitElems .and. .not. haveElmtMask) then
       allocate (ElemMask(TotalElements))
       haveMask=.true.
       ElemMask(:)=1  ! default to nothing masked out
    endif    


    ! The ElemId is the global ID.  The myStartElmt is the starting Element ID(-1), and the
    ! element IDs will be from startElmt to startElmt+ElemCnt-1
    ! The ElemConn() contains the four corner node IDs for each element and it is organized
    ! as a 1D array.  The node IDs are "local", which are stored in NodeUsed(:)
    ElemNo = 1
    ConnNo = 0
    if (parametricDim .eq. 2) then
       ! Allocate variables for triangulation
       allocate(polyCoords(3*maxNumPoly))
       allocate(polyDblBuf(3*maxNumPoly))
       allocate(area(maxNumPoly))
       allocate(polyIntBuf(maxNumPoly))
       allocate(triInd(3*(maxNumPoly-2)))

       ! Parametric dim=2 and cartSpatialDim=3 
       ! Means spherical, so calc. cart. coordinates
       if (cartSpatialDim .eq. 3) then
          allocate(NodeCoordsCart(cartSpatialDim*localNodes))

          ti=0
          tk=0
          do i=1,localNodes
             call c_esmc_sphdeg_to_cart(NodeCoords1D(ti+1), &
                                        NodeCoords1D(ti+2),  &
                                        NodeCoordsCart(tk+1), &
                                        NodeCoordsCart(tk+2), &
                                        NodeCoordsCart(tk+3),  &
                                        localrc)
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return    

             ti=ti+2
             tk=tk+3
          enddo
       endif

       ! Loop through creating Mesh appropriate elements
       do j = 1, ElemCnt
          if (elmtNum(j)==3) then        
             ElemId(ElemNo) = myStartElmt+ElemNo
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_TRI
             do i=1,3
                ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
             end do
             if (existSplitElems) Mesh%splitElemMap(ElemNo)=j+startElmt-1
	     if (haveElmtMask) ElemMask(ElemNo) = elementMask(j)
	     if (localAddUserArea) ElemArea(ElemNo) = elementArea(j)
             ElemNo=ElemNo+1
             ConnNo=ConnNo+3
          elseif (elmtNum(j)==4) then
             ElemId(ElemNo) = myStartElmt+ElemNo
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_QUAD
             do i=1,4
                ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
             end do
             if (existSplitElems) Mesh%splitElemMap(ElemNo)=j+startElmt-1
	     if (haveElmtMask) ElemMask(ElemNo) = elementMask(j)
	     if (localAddUserArea) ElemArea(ElemNo) = elementArea(j)
             ElemNo=ElemNo+1
             ConnNo=ConnNo+4
          else
             ! number of points in poly to triangulate
             numPoly=elmtNum(j)
             if (numPoly > maxNumPoly) then
                call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                     msg="- File contains polygons with more sides than triangulation is supported for", &
                     ESMF_CONTEXT, rcToReturn=rc)
                return
             endif

             ! Copy points into input list
             if (cartSpatialDim==2) then
                ti=0
                do k=1,numPoly
                   lni=2*(NodeUsed(elementConn(k,j))-1) ! get the index of the node coords in the local list
                   polyCoords(ti+1)=NodeCoords1D(lni+1)
                   polyCoords(ti+2)=NodeCoords1D(lni+2)
                   ti=ti+2
                enddo
             else if (cartSpatialDim==3) then
                ti=0
                do k=1,numPoly
                   lni=3*(NodeUsed(elementConn(k,j))-1) ! get the index of the node coords in the local list
                   polyCoords(ti+1)=NodeCoordsCart(lni+1)
                   polyCoords(ti+2)=NodeCoordsCart(lni+2)
                   polyCoords(ti+3)=NodeCoordsCart(lni+3)
                   ti=ti+3
                enddo
             endif

             ! Checking for other spatialDims above, not here in a loop

             ! call triangulation routine
             call c_ESMC_triangulate(parametricDim, cartSpatialDim, &
                  numPoly, polyCoords, polyDblBuf, polyIntBuf, &
                  triInd, localrc)   
             if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
                  ESMF_CONTEXT, rcToReturn=rc)) return

             ! translate triangulation out of output list
             ti=0
	     startElemNo = ElemNo
             do k=1,numPoly-2
                ElemId(ElemNo)=myStartElmt+ElemNo
                ElemType (ElemNo) = ESMF_MESHELEMTYPE_TRI
                ElemConn (ConnNo+1) = NodeUsed(elementConn(triInd(ti+1)+1,j))
                ElemConn (ConnNo+2) = NodeUsed(elementConn(triInd(ti+2)+1,j))
                ElemConn (ConnNo+3) = NodeUsed(elementConn(triInd(ti+3)+1,j))
                if (existSplitElems) Mesh%splitElemMap(ElemNo)=j+startElmt-1
 	        if (haveElmtMask) ElemMask(ElemNo) = elementMask(j)
                
                ! Calculate area of sub-triangle
  	        !if (localAddUserArea) then 
                   if (cartSpatialDim==2) then
		     tk=0
		     do i=1,3
                       lni=2*(ElemConn(ConnNo+i)-1) ! get the index of the node coords in the local list
      	               polyCoords(tk+1)=NodeCoords1D(lni+1)
                       polyCoords(tk+2)=NodeCoords1D(lni+2)
                       tk=tk+2
                     enddo
	           else if (cartSpatialDim==3) then
                     tk=0
                     do i=1,3
                       lni=3*(ElemConn(ConnNo+i)-1) ! get the index of the node coords in the local list
                       polyCoords(tk+1)=NodeCoordsCart(lni+1)
                       polyCoords(tk+2)=NodeCoordsCart(lni+2)
                       polyCoords(tk+3)=NodeCoordsCart(lni+3)
                       tk=tk+3
                     enddo
                   endif
                   nEdges = 3
                   call c_ESMC_get_polygon_area(cartSpatialDim, nEdges, polyCoords, area(k), localrc) 
	           if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        	          ESMF_CONTEXT, rcToReturn=rc)) return
                ! endif

                ! If the area of the subtriangle is 0.0 mask it out
                if (area(k)==0.0) then
                   ElemMask(ElemNo)=0
                endif

                ElemNo=ElemNo+1
                ConnNo=ConnNo+3
                ti=ti+3
             enddo
             !!! set the area for each splitted triangle
	     if (localAddUserArea) then
	        totalarea = 0
		do k=1,numPoly-2
	          totalarea = totalarea + area(k)
                enddo
                do k=1, numPoly-2
                  elemArea(startElemNo+k) = elementArea(j)*(area(k)/totalarea)
                enddo
              endif
          end if
       end do

       ! deallocate cart nodes
       if (cartSpatialDim .eq. 3) then
          deallocate(NodeCoordsCart)
       endif

       ! deallocate after triangulation
       deallocate(polyCoords)
       deallocate(polyDblBuf)
       deallocate(area)
       deallocate(polyIntBuf)
       deallocate(triInd)
    else ! If not parametricDim==2, assuming parmetricDim==3
       do j = 1, ElemCnt
          if (elmtNum(j)==4) then        
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_TETRA
          elseif (elmtNum(j)==8) then
             ElemType (ElemNo) = ESMF_MESHELEMTYPE_HEX
          else
             call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, & 
                  msg="- in 3D currently only support Tetra. (4 nodes) or Hexa. (8 nodes)", & 
                  ESMF_CONTEXT, rcToReturn=rc) 
             return
          endif

          do i=1,elmtNum(j)
             ElemConn (ConnNo+i) = NodeUsed(elementConn(i,j))
          end do
          ElemId(ElemNo) = myStartElmt+ElemNo
          if (haveElmtMask) ElemMask(ElemNo) = elementMask(j)
          if (localAddUserArea) ElemArea(ElemNo) = elementArea(j)
          ElemNo=ElemNo+1
          ConnNo=ConnNo+elmtNum(j)
       end do
    endif


    if (ElemNo /= TotalElements+1) then
	write (ESMF_UtilIOStdout,*)  &
            PetNo, ' TotalElements does not match ',ElemNo-1, TotalElements
    end if

    ! Add elements
    if (haveMask .and. localAddUserArea) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementMask=ElemMask, elementArea=ElemArea, rc=localrc)
    elseif (haveMask) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementMask=ElemMask, rc=localrc)
    elseif (localAddUserArea) then
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, &
			elementArea=ElemArea, rc=localrc)
    else
	    call ESMF_MeshAddElements (Mesh, ElemId, ElemType, ElemConn, rc=localrc)
    end if
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return

    ! NEED TO SET THIS HERE, BECAUSE MEshAddElements sets it to false
    if (existSplitElems) then
       Mesh%hasSplitElem=.true.
    else
       Mesh%hasSplitElem=.false.
    endif

    deallocate(NodeUsed, NodeId, NodeCoords1D, NodeOwners)
    deallocate(ElemId, ElemType, ElemConn)
    deallocate(elementConn, elmtNum)
    if (haveElmtMask) deallocate(elementMask) 
    if (haveMask) deallocate(ElemMask) 
     
    if (localAddUserArea) deallocate(elementArea, ElemArea)
    ESMF_MeshCreateFromUnstruct = Mesh

    if (present(rc)) rc=ESMF_SUCCESS
    return
end function ESMF_MeshCreateFromUnstruct
!------------------------------------------------------------------------------
#endif

end module ESMF_MeshMod
