! $Id: ESMF_InternGrid.F90,v 1.1 2007/06/22 23:21:37 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_InternGrid.F90"
!
!     ESMF InternGrid Module
      module ESMF_InternGridMod
!
!==============================================================================
!
! This file contains the InternGrid class definition and all InternGrid class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_InternGridMod - InternGrid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_InternGrid} class.  This class
! provides a unified interface for both {\tt ESMF\_PhysGrid} and 
! {\tt ESMF\_InternDG} information for model InternGrids.  
! Functions for defining and computing {\tt ESMF\_InternGrid}
! information are available through this class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_InternArrayDataMapMod     ! ESMF data map class
      
      use ESMF_VMMod
      use ESMF_InitMacrosMod
      use ESMF_DELayoutMod ! ESMF layout class
      use ESMF_InternArrayMod
      use ESMF_InternArrayGetMod
      use ESMF_InternDGMod    ! ESMF distributed InternGrid class
      use ESMF_PhysCoordMod   ! ESMF physical Coord class
      use ESMF_PhysGridMod    ! ESMF physical InternGrid class
      use ESMF_InternGridTypesMod   ! ESMF basic InternGrid types and primitives
      use ESMF_LogRectInternGridMod ! ESMF logically rectangular InternGrid routines
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    public ESMF_InternGridAddVertHeight
    public ESMF_InternGridCreate
    public ESMF_InternGridDestroy
    public ESMF_InternGridDistribute
    public ESMF_InternGridGet
    public ESMF_InternGridGetAttribute
    public ESMF_InternGridGetAttributeCount
    public ESMF_InternGridGetAttributeInfo
    public ESMF_InternGridGetCoord
    public ESMF_InternGridGetDELocalInfo
    !public ESMF_InternGridGetMask
    !public ESMF_InternGridGetMetric
    public ESMF_InternGridGlobalToDELocalIndex
    public ESMF_InternGridDELocalToGlobalIndex
    public ESMF_InternGridPrint
    public ESMF_InternGridSet
    public ESMF_InternGridSetAttribute
    public ESMF_InternGridValidate
    public ESMF_InternGridBoxIntersectRecv
    public ESMF_InternGridBoxIntersectSend
    public ESMF_InternGridComputeDistance
    public ESMF_InternGridGetAllAxisIndex
    public ESMF_InternGridGetAIsAllDEs
    public ESMF_InternGridGetCellMask
    public ESMF_InternGridGetDELocalAI
    public ESMF_InternGridGlobalToDELocalAI
    public ESMF_InternGridDELocalToGlobalAI
    !public ESMF_InternGridSearch
    public ESMF_InternGridSerialize
    public ESMF_InternGridDeserialize

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_InternGrid.F90,v 1.1 2007/06/22 23:21:37 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_InternGridCreate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_InternGridCreateEmpty
         module procedure ESMF_InternGridCreateRead
         module procedure ESMF_InternGridCreateCopy
         module procedure ESMF_InternGridCreateCutout
         module procedure ESMF_InternGridCreateDiffRes
         module procedure ESMF_InternGridCreateExchange

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_InternGrid} create
!     methods.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_InternGridDistribute - InternGrid Distribute routines
!
! !INTERFACE:
      interface ESMF_InternGridDistribute

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_InternGridDistributeBlock
        module procedure ESMF_InternGridDistributeArbitrary

! !DESCRIPTION:
!     This interface provides a single entry point for methods that distribute
!     (or decompose) an {\tt ESMF\_InternGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_InternGridGet - InternGrid Get routines
!
! !INTERFACE:
      interface ESMF_InternGridGet

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_InternGridGetGeneral
        module procedure ESMF_InternGridGetWithRelloc

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     a variety of information about an {\tt ESMF\_InternGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute  - Get InternGrid attributes
!
! !INTERFACE:
      interface ESMF_InternGridGetAttribute

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_InternGridGetInt4Attr
        module procedure ESMF_InternGridGetInt4ListAttr
        module procedure ESMF_InternGridGetInt8Attr
        module procedure ESMF_InternGridGetInt8ListAttr
        module procedure ESMF_InternGridGetReal4Attr
        module procedure ESMF_InternGridGetReal4ListAttr
        module procedure ESMF_InternGridGetReal8Attr
        module procedure ESMF_InternGridGetReal8ListAttr
        module procedure ESMF_InternGridGetLogicalAttr
        module procedure ESMF_InternGridGetLogicalListAttr
        module procedure ESMF_InternGridGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_InternGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_InternGridGetAttributeInfo - Get type, count from a InternGrid attribute
!     
! !INTERFACE:
      interface ESMF_InternGridGetAttributeInfo

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_InternGridGetAttrInfoByName
        module procedure ESMF_InternGridGetAttrInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_InternGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_InternGridGetCoord - 
!     
! !INTERFACE:
      interface ESMF_InternGridGetCoord

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_InternGridGetCoord
        module procedure ESMF_InternGridGetCoordByDim1D
        module procedure ESMF_InternGridGetCoordByDim2D

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about coordinates from an {\tt ESMF\_InternGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute  - Set InternGrid attributes
!
! !INTERFACE:
      interface ESMF_InternGridSetAttribute

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_InternGridSetInt4Attr
        module procedure ESMF_InternGridSetInt4ListAttr
        module procedure ESMF_InternGridSetInt8Attr
        module procedure ESMF_InternGridSetInt8ListAttr
        module procedure ESMF_InternGridSetReal4Attr
        module procedure ESMF_InternGridSetReal4ListAttr
        module procedure ESMF_InternGridSetReal8Attr
        module procedure ESMF_InternGridSetReal8ListAttr
        module procedure ESMF_InternGridSetLogicalAttr
        module procedure ESMF_InternGridSetLogicalListAttr
        module procedure ESMF_InternGridSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_InternGrid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_InternGridSearch
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_InternGridSearchPoint
!         module procedure ESMF_InternGridSearchList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     search an {\tt ESMF\_InternGrid} for point(s).
!!
!!EOPI
!      end interface
!!
!------------------------------------------------------------------------------

!    < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridAddVertHeight"
!BOP
! !IROUTINE: ESMF_InternGridAddVertHeight - Add a vertical subInternGrid to an existing InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridAddVertHeight(interngrid, delta, coord, vertstagger, &
                                        dimName, dimUnit, name, rc)

!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord
      type(ESMF_InternGridVertStagger), intent(in), optional :: vertstagger
      character(len=*), intent(in), optional :: dimName
      character(len=*), intent(in), optional :: dimUnit
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine adds a vertical subInternGrid (or subInternGrids) to an already
!     allocated InternGrid.  The {\tt ESMF\_InternGridAddVertHeight} interface only
!     creates vertical subInternGrids with coordinate systems where the zero point is
!     defined at the bottom.  An {\tt ESMF\_InternGridAddVert<InternGridVertType>()} can
!     only be called for any {\tt ESMF\_InternGrid} once;  if a vertical subInternGrid
!     already exists for the {\tt ESMF\_InternGrid} that is passed in, an error
!     is returned.  Please note that this subroutine may create more than one
!     subInternGrid because some vertical staggerings infer more than one vertical
!     relative location (for example, {\tt ESMF\_IGRID\_VERT\_STAGGER\_BOTTOM}
!     staggering indicates that some Fields are represented at the vertical cell
!     centers and some at the cell bottom faces).  
!     This routine generates {\tt ESMF\_InternGrid} coordinates from either of two
!     optional sets of arguments:
!     \begin{enumerate}
!       \item given array of coordinate increments or spacings, assuming 0 is 
!          the minimum or starting coordinate (optional argument {\tt delta});
!       \item given array of coordinates (optional argument {\tt coord}).
!     \end{enumerate}
!     If neither of these sets of arguments is present and valid, an error
!     message is issued and an error code returned.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Existing {\tt ESMF\_InternGrid} the vertical subInternGrid(s) is being added to.
!     \item[{[delta]}]
!          Array of physical increments in the vertical direction.
!     \item[{[coord]}]
!          Array of physical coordinates in the vertical direction.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_InternGridVertStagger} specifier denoting vertical subInternGrid
!          stagger.  The default value is ESMF\_IGRID\_VERT\_STAGGER\_CENTER.
!     \item[{[dimName]}]
!          Dimension name.
!     \item[{[dimUnit]}]
!          Dimension unit.
!     \item[{[name]}]
!          Name for the vertical subInternGrid(s).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status
      logical :: dummy
      real(ESMF_KIND_R8) :: minGlobalCoord
      type(ESMF_InternGridVertType) :: vertInternGridType
      type(ESMF_CoordSystem) :: vertCoordSystem
      type(ESMF_InternGridVertStagger) :: vertStaggerUse

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! Set default values
      vertInternGridType    = ESMF_IGRID_VERT_TYPE_HEIGHT
      vertCoordSystem = ESMF_COORD_SYSTEM_HEIGHT
      vertStaggerUse  = ESMF_IGRID_VERT_STAGGER_CENTER
      if (present(vertstagger)) vertstaggerUse = vertStagger
      minGlobalCoord  = 0.0d0

      ! Call InternGridAddVert routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "InternGridStructureUnknown not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridAddVert(interngrid%ptr, minGlobalCoord, delta, coord, &
                                vertInternGridType, vertStaggerUse, &
                                vertCoordSystem, dimName, dimUnit, &
                                name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "InternGridStructureLogRectBlock not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "InternGridStructureUnstruct not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "InternGridStructureUser not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      case default
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                      "Invalid interngrid structure", &
                                      ESMF_CONTEXT, rc)
        return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridAddVertHeight

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridCreateEmpty"
!BOP
! !IROUTINE: ESMF_InternGridCreate - Create a new InternGrid with no contents

! !INTERFACE:
      ! Private name; call using ESMF_InternGridCreate()
      function ESMF_InternGridCreateEmpty(name, rc)
!
! !RETURN VALUE:
      type(ESMF_InternGrid) :: ESMF_InternGridCreateEmpty
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_InternGrid} object and constructs its
!     internal derived types, but does not fill in any contents.  Returns a
!     pointer to the new {\tt ESMF\_InternGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      type(ESMF_InternGridClass), pointer :: interngrid       ! Pointer to new interngrid
      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_InternGridCreateEmpty%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      allocate(interngrid, stat=localrc)
      ! If error write message and return.
      if (ESMF_LogMsgFoundAllocError(localrc, "InternGrid type", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_InternGridConstructNew(interngrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_InternGridCreateEmpty%ptr => interngrid

      ! set the interngrid as valid
      ESMF_INIT_SET_CREATED(ESMF_InternGridCreateEmpty)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_InternGridCreateEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridCreateRead"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_InternGridCreate - Create a new InternGrid by reading in from a file

! !INTERFACE:
      ! Private name; call using ESMF_InternGridCreate()
      function ESMF_InternGridCreateRead(interngridStructure, iospec, name, rc)
!
! !RETURN VALUE:
      type(ESMF_InternGrid) :: ESMF_InternGridCreateRead
!
! !ARGUMENTS:
      integer, intent(in) :: interngridStructure
      type(ESMF_IOSpec), intent(in) :: iospec   ! file specs
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_InternGrid} object, constructs its
!     internals, and reads an {\tt ESMF\_InternGrid} in from a file.  Return a pointer to
!     the new {\tt ESMF\_InternGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngridStructure]
!          InternGrid structure specification.
!     \item[iospec]
!          File I/O specification.
!     \item[{[name]}]
!          {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_InternGridCreateRead%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Call InternGridCreateRead routines based on InternGridStructure

      select case(interngridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_InternGridCreateRead = ESMF_LRInternGridCreateRead(iospec, name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! initialize interngrid as created
      ESMF_INIT_SET_CREATED(ESMF_InternGridCreateRead)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_InternGridCreateRead

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridCreateCopy"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_InternGridCreate - Create a new InternGrid by copying another InternGrid

! !INTERFACE:
      ! Private name; call using ESMF_InternGridCreate()
      function ESMF_InternGridCreateCopy(interngridIn, name, rc)
!
! !RETURN VALUE:
      type(ESMF_InternGrid) :: ESMF_InternGridCreateCopy
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngridIn
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_InternGrid} object, constructs its
!     internals, and copies attributes from another {\tt ESMF\_InternGrid}.  Return a
!     pointer to the new {\tt ESMF\_InternGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngridIn]
!          {\tt ESMF\_InternGrid} to be copied.
!     \item[{[name]}]
!          {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_InternGridCreateCopy%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Call InternGridCreateCopy routines based on InternGridStructure

      select case(interngridIn%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_InternGridCreateCopy = ESMF_LRInternGridCreateCopy(interngridIn, name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! initialize interngrid as created
      ESMF_INIT_SET_CREATED(ESMF_InternGridCreateCopy)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_InternGridCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridCreateCutout"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_InternGridCreate - Create a new InternGrid as a subset of an existing InternGrid

! !INTERFACE:
      ! Private name; call using ESMF_InternGridCreate()
      function ESMF_InternGridCreateCutout(interngridIn, min, max, name, rc)
!
! !RETURN VALUE:
      type(ESMF_InternGrid) :: ESMF_InternGridCreateCutout
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngridIn
      integer, dimension(:), intent(in) :: min
      integer, dimension(:), intent(in) :: max
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_InternGrid} object, constructs its
!     internals, and copies a region from an existing {\tt ESMF\_InternGrid}.
!     Return a pointer to the new {\tt ESMF\_InternGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngridIn]
!          {\tt ESMF\_InternGrid} to be partially copied.
!     \item[min]
!          Minimum global indices for the region of the interngrid to be cutout.
!     \item[max]
!          Maximum global indices for the region of the interngrid to be cutout.
!     \item[{[name]}]
!          {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status


      ! Initialize pointers
      nullify(ESMF_InternGridCreateCutout%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngridIn,rc)

      ! Call InternGridCreateCutout routines based on InternGridStructure

      select case(interngridIn%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_InternGridCreateCutout = ESMF_LRInternGridCreateCutout(interngridIn, min, max, &
                                                        name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! initialize interngrid as created
      ESMF_INIT_SET_CREATED(ESMF_InternGridCreateCutout)


      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_InternGridCreateCutout

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridCreateDiffRes"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_InternGridCreate - Create a new InternGrid by coarsening or refining an existing InternGrid

! !INTERFACE:
      ! Private name; call using ESMF_InternGridCreate()
      function ESMF_InternGridCreateDiffRes(interngridIn, resolution, name, rc)
!
! !RETURN VALUE:
      type(ESMF_InternGrid) :: ESMF_InternGridCreateDiffRes
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngridIn
      integer, dimension(:), intent(in) :: resolution
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_InternGrid} object, constructs its
!     internals, and creates an {\tt ESMF\_InternGrid} by either coarsening or refining an
!     existing {\tt ESMF\_InternGrid}.  Return a pointer to the new {\tt ESMF\_InternGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngridIn]
!          Source {\tt ESMF\_InternGrid} to be coarsened or refined.
!     \item[resolution]
!          Integer resolution factors in each direction.
!          Note:  The above arguments assume refinement by factor if positive
!          and coarsening by absolute value of the factor if negative.  For
!          example, resolution(1)=4 indicates the new {\tt ESMF\_InternGrid} will be
!          four times as resolved in the first direction as the source
!          {\tt ESMF\_InternGrid}, whereas resolution(2)=-3 means the new
!          {\tt ESMF\_InternGrid} will sample every third point in the second 
!          direction.
!     \item[{[name]}]
!          {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_InternGridCreateDiffRes%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngridIn,rc)

      ! Call InternGridCreateDiffRes routines based on InternGridStructure

      select case(interngridIn%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_InternGridCreateDiffRes = &
          ESMF_LRInternGridCreateDiffRes(interngridIn, resolution, name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! initialize interngrid as created
      ESMF_INIT_SET_CREATED(ESMF_InternGridCreateDiffRes)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_InternGridCreateDiffRes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridCreateExchange"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_InternGridCreate - Create a new InternGrid from the intersection of two existing interngrids

! !INTERFACE:
      ! Private name; call using ESMF_InternGridCreate()
      function ESMF_InternGridCreateExchange(interngridIn1, interngridIn2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_InternGrid) :: ESMF_InternGridCreateExchange
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngridIn1
      type(ESMF_InternGrid), intent(in) :: interngridIn2
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_InternGrid} object, constructs its
!     internals, and creates a new {\tt ESMF\_InternGrid} from the intersection of two
!     existing {\tt ESMF\_InternGrids}.  Return a pointer to the new {\tt ESMF\_InternGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngridIn1]
!          First source {\tt ESMF\_InternGrid}.
!     \item[interngridIn2]
!          Second source {\tt ESMF\_InternGrid}.
!     \item[{[name]}]
!          New {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_InternGridCreateExchange%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngridIn1,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngridIn2,rc)

      ! Call InternGridCreateExchange routines based on InternGridStructure

      select case(interngridIn1%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_InternGridCreateExchange = ESMF_LRInternGridCreateExchange(interngridIn1, interngridIn2, &
                                                            name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! initialize interngrid as created
      ESMF_INIT_SET_CREATED(ESMF_InternGridCreateExchange)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_InternGridCreateExchange

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridDestroy"
!BOP
! !IROUTINE: ESMF_InternGridDestroy - Free all resources associated with a InternGrid 

! !INTERFACE:
      subroutine ESMF_InternGridDestroy(interngrid, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys an {\tt ESMF\_InternGrid} object and all related internal structures
!     previously allocated via an {\tt ESMF\_InternGridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be destroyed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! If already destroyed or never created, return ok
      if (.not. associated(interngrid%ptr)) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Uninitialized or destroyed InternGrid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call InternGridDestruct routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        ! allow a user to create an empty interngrid and then delete 
        ! it without being created further.
        localrc = ESMF_SUCCESS

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridDestruct(interngrid%ptr, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      ! If error write message and return.
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! delete the base class
      call ESMF_BaseDestroy(interngrid%ptr%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! free interngrid memory.
      deallocate(interngrid%ptr, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate InternGrid type", &
                                     ESMF_CONTEXT, rc)) return

      ! so we can detect reuse of a deleted interngrid object
      nullify(interngrid%ptr)

      ! initialize interngrid as deleted
      ESMF_INIT_SET_DELETED(interngrid)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridDistributeBlock"
!BOP
! !IROUTINE: ESMF_InternGridDistribute - Distribute a InternGrid with block storage 

! !INTERFACE:
     ! Private name; call using ESMF_InternGridDistribute()
      subroutine ESMF_InternGridDistributeBlock(interngrid, delayout, countsPerDEDim1, &
                                          countsPerDEDim2, decompIds, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in), optional :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      integer, dimension(:), intent(in), optional :: decompIds
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the decomposition of an {\tt ESMF\_InternGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be distributed.
!     \item[delayout]
!         {\tt ESMF\_DELayout} on which {\tt interngrid} is to be decomposed.
!     \item[{[countsPerDEDim1]}]
!          Array denoting the number of interngrid cells per DE in the first
!          decomposition axis.  By default, the number of interngrid cells per DE
!          in a decomposition is calculated internally by an algorithm
!          designed to distribute the cells as evenly as possible.
!          This optional argument is available to allow users to instead
!          specify the decomposition of a InternGrid axis by a related
!          DELayout axis.  The number of elements in this array must be
!          greater than or equal to the number of DE's along the first axis of
!          the attached DELayout.  The sum of this array must equal exactly
!          the number of interngrid cells along a related InternGrid axis, which is the
!          first axis by default but can also be set by the {\tt decompIds}
!          argument in this call.
!     \item[{[countsPerDEDim2]}]
!          Array denoting the number of interngrid cells per DE in the second
!          decomposition axis.  Please see the description of
!          {\tt countsPerDEDim1} above for more details
!     \item[{[decompIds]}]
!          Integer array identifying which InternGrid axes are decomposed.
!          This array describes the relationship between the InternGrid and the
!          DELayout.  The elements of this array contains decompostion
!          information for the corresponding InternGrid axis.  The following is a
!          list of valid values and the meaning of each:
!          \begin{description}
!            \item 0 \  the InternGrid axis is not distributed;
!            \item 1 \  the InternGrid axis is distributed by the first 
!                       decomposition axis in the DELayout;
!            \item 2 \  the InternGrid axis is distributed by the second 
!                       decomposition axis in the DELayout.
!          \end{description}
!          The number of array elements should be greater or equal to the number
!          of InternGrid dimensions.  The default is that the first InternGrid axis is
!          distributed by the first decompostion axis, the second InternGrid axis is
!          distributed by the second decomposition axis, and the third InternGrid axis
!          (if applicable) is not distributed.  The relationship between data
!          axes (from an {\tt ESMF\_Field} or {\tt ESMF\_Array}) and InternGrid
!          axes are defined elsewhere in {\tt ESMF\_FieldDataMap} and
!          {\tt ESMF\_ArrayDataMap} interfaces.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

      ! Call InternGridDistribute routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridDistributeBlock(interngrid%ptr, delayout, countsPerDEDim1, &
                                        countsPerDEDim2, decompIds, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridDistributeBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridDistributeArbitrary"
!BOP
! !IROUTINE: ESMF_InternGridDistribute - Distribute a InternGrid as an arbitrary vector of points

! !INTERFACE:
     ! Private name; call using ESMF_InternGridDistribute()
      subroutine ESMF_InternGridDistributeArbitrary(interngrid, delayout, myCount, &
                                              myIndices, decompIds, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_DELayout), intent(in) :: delayout
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, dimension(:), intent(in), optional :: decompIds
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the decomposition of an {\tt ESMF\_InternGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be distributed.
!     \item[delayout]
!         {\tt ESMF\_DELayout} on which {\tt interngrid} is to be decomposed.
!     \item[myCount]
!          Number of interngrid cells to be distributed to this DE.
!     \item[myIndices]
!          Array of InternGrid indices to be distributed to this DE, as (i,j) pairs.
!          The size of this array must be at least {\tt myCount} in the first
!          dimension and 2 in the second.
!     \item[{[decompIds]}]
!          Integer array identifying which InternGrid axes are decomposed.
!          This array describes the relationship between the InternGrid and the
!          DELayout.  The elements of this array contains decompostion
!          information for the corresponding InternGrid axis.  The following is a
!          list of valid values and the meaning of each:
!          \begin{description}
!            \item 0 \  the InternGrid axis is not distributed;
!            \item 1 \  the InternGrid axis is distributed by the first 
!                       decomposition axis in the DELayout;
!            \item 2 \  the InternGrid axis is distributed by the second 
!                       decomposition axis in the DELayout.
!          \end{description}
!          The number of array elements should be greater or equal to the number
!          of InternGrid dimensions.  The default is that the first InternGrid axis is
!          distributed by the first decomposition axis, the second InternGrid axis is
!          distributed by the second decomposition axis, and the third InternGrid axis
!          (if applicable) is not distributed.  The relationship between data
!          axes (from an {\tt ESMF\_Field} or {\tt ESMF\_Array}) and InternGrid
!          axes are defined elsewhere in {\tt ESMF\_FieldDataMap} and
!          {\tt ESMF\_ArrayDataMap} interfaces.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

      ! Call InternGridDistribute routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridDistributeArbitrary(interngrid%ptr, delayout, myCount, &
                                            myIndices, decompIds, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridDistributeArbitrary

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetGeneral"
!BOP
! !IROUTINE: ESMF_InternGridGet - Get a variety of general information about a InternGrid

! !INTERFACE:
      ! Private name; call using ESMF_InternGridGet()
      subroutine ESMF_InternGridGetGeneral(interngrid, &
                                     horzinterngridtype, vertinterngridtype, &
                                     horzstagger, vertstagger, &
                                     horzcoordsystem, vertcoordsystem, &
                                     coordorder, &
                                     dimCount, distDimCount, interngridstorage, &
                                     minGlobalCoordPerDim, maxGlobalCoordPerDim, &
                                     periodic, delayout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(inout) :: interngrid
      type(ESMF_InternGridType),     intent(out), optional :: horzinterngridtype
      type(ESMF_InternGridVertType), intent(out), optional :: vertinterngridtype
      type(ESMF_InternGridHorzStagger), intent(out), optional :: horzstagger
      type(ESMF_InternGridVertStagger), intent(out), optional :: vertstagger
      type(ESMF_CoordSystem), intent(out), optional :: horzcoordsystem
      type(ESMF_CoordSystem), intent(out), optional :: vertcoordsystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordorder
      integer, intent(out), optional :: dimCount
      integer, intent(out), optional :: distDimCount
      type(ESMF_InternGridStorage), intent(out), optional :: interngridstorage
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minGlobalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxGlobalCoordPerDim
      type(ESMF_Logical), intent(out), dimension(:), optional :: periodic
      type(ESMF_DELayout), intent(out), optional :: delayout
      character(len = *), intent(out), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Gets general information about an {\tt ESMF\_InternGrid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be queried.
!     \item[{[horzinterngridtype]}]
!          {\tt ESMF\_InternGridType} specifier denoting horizontal InternGrid type.
!     \item[{[vertinterngridtype]}]
!          {\tt ESMF\_InternGridVertType} specifier denoting vertical subInternGrid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_InternGridHorzStagger} specifier denoting horizontal InternGrid
!          stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_InternGridHorzStagger} specifier denoting vertical subInternGrid
!          stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal InternGrid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical subInternGrid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the InternGrid and all related Fields (i.e. ZXY).
!     \item[{[dimCount]}]
!          Number of dimensions represented by this InternGrid.
!     \item[{[distDimCount]}]
!          Number of dimensions represented by the distribution of this InternGrid.
!          For InternGrids distributed arbitrarily, this could be different than the
!          rank of the underlying InternGrid.
!     \item[{[interngridstorage]}]
!          {\tt ESMF\_InternGridStorage} specifier denoting InternGrid storage.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical array that returns the periodicity of the coordinate axes.
!     \item[{[delayout]}]
!          {\tt delayout} that this InternGrid was distributed over.
!     \item[{[name]}]
!          {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif
      
      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call InternGridGet routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        ! the only thing that can be retrieved from an empty interngrid is the name
        if (present(name)) then
          call ESMF_GetName(interngrid%ptr%base, name, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif 
        if (present(horzinterngridtype           ) .OR. &
            present(vertinterngridtype           ) .OR. &
            present(horzstagger            ) .OR. &
            present(vertstagger            ) .OR. &
            present(horzcoordsystem        ) .OR. &
            present(vertcoordsystem        ) .OR. &
            present(coordorder             ) .OR. &
            present(dimCount               ) .OR. &
            present(distDimCount           ) .OR. &
            present(interngridstorage            ) .OR. &
            present(minGlobalCoordPerDim   ) .OR. &
            present(maxGlobalCoordPerDim   ) .OR. &
            present(periodic               ) .OR. &
            present(delayout               )) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                    "Unknown interngrid structure", &
                                    ESMF_CONTEXT, rc)) return
        endif

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGet(interngrid, &
                            horzInternGridType=horzinterngridtype, &
                            vertInternGridType=vertinterngridtype, &
                            horzStagger=horzstagger, &
                            vertStagger=vertstagger, &
                            horzCoordSystem=horzcoordsystem, &
                            vertCoordSystem=vertcoordsystem, &
                            coordOrder=coordorder, &
                            dimCount=dimCount, distDimCount=distDimCount, &
                            interngridStorage=interngridstorage, &
                            minGlobalCoordPerDim=minGlobalCoordPerDim, &
                            maxGlobalCoordPerDim=maxGlobalCoordPerDim, &
                            periodic=periodic, delayout=delayout, &
                            name=name, rc=localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetGeneral

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetWithRelloc"
!BOP
! !IROUTINE: ESMF_InternGridGet - Get a variety of relloc-specified information about a InternGrid

! !INTERFACE:
      ! Private name; call using ESMF_InternGridGet()
      subroutine ESMF_InternGridGetWithRelloc(interngrid, horzrelloc, vertrelloc, &
                                        horzinterngridtype, vertinterngridtype, &
                                        horzstagger, vertstagger, &
                                        horzcoordsystem, vertcoordsystem, &
                                        coordorder, &
                                        dimCount, distDimCount, interngridstorage, &
                                        minGlobalCoordPerDim, &
                                        maxGlobalCoordPerDim, &
                                        globalCellCountPerDim, &
                                        globalStartPerDEPerDim, &
                                        maxLocalCellCountPerDim, &
                                        cellCountPerDEPerDim, periodic, &
                                        delayout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(inout) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_InternGridType),     intent(out), optional :: horzinterngridtype
      type(ESMF_InternGridVertType), intent(out), optional :: vertinterngridtype
      type(ESMF_InternGridHorzStagger), intent(out), optional :: horzstagger
      type(ESMF_InternGridVertStagger), intent(out), optional :: vertstagger
      type(ESMF_CoordSystem), intent(out), optional :: horzcoordsystem
      type(ESMF_CoordSystem), intent(out), optional :: vertcoordsystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordorder
      integer, intent(out), optional :: dimCount
      integer, intent(out), optional :: distDimCount
      type(ESMF_InternGridStorage), intent(out), optional :: interngridstorage
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minGlobalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxGlobalCoordPerDim
      integer, intent(out), dimension(:), optional :: globalCellCountPerDim
      integer, intent(out), dimension(:,:), optional :: globalStartPerDEPerDim
      integer, intent(out), dimension(:), optional :: maxLocalCellCountPerDim
      integer, intent(out), dimension(:,:), optional :: cellCountPerDEPerDim
      type(ESMF_Logical), intent(out), dimension(:), optional :: periodic
      type(ESMF_DELayout), intent(out), optional :: delayout
      character(len = *), intent(out), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Gets information about an {\tt ESMF\_InternGrid} or specified subInternGrid, depending
!     on user-supplied relative locations, and a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be queried.
!     \item[horzrelloc]
!          Horizontal relative location of the subInternGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subInternGrid to be queried.
!     \item[{[horzinterngridtype]}]
!          {\tt ESMF\_InternGridType} specifier denoting horizontal InternGrid type.
!     \item[{[vertinterngridtype]}]
!          {\tt ESMF\_InternGridVertType} specifier denoting vertical subInternGrid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_InternGridHorzStagger} specifier denoting horizontal InternGrid
!          stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_InternGridHorzStagger} specifier denoting vertical subInternGrid
!          stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal interngrid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical subInternGrid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the InternGrid and all related Fields (i.e. ZXY).
!     \item[{[dimCount]}]
!          Number of dimensions represented by this InternGrid.
!     \item[{[distDimCount]}]
!          Number of dimensions represented by the distribution of this InternGrid.
!     \item[{[interngridstorage]}]
!          {\tt ESMF\_InternGridStorage} specifier denoting InternGrid storage.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[globalCellCountPerDim]}]
!          Array of numbers of global InternGrid increments in each direction.
!     \item[{[globalStartPerDEPerDim]}]
!          Array of global starting locations for each DE and in each direction.
!     \item[{[maxLocalCellCountPerDim]}]
!          Array of maximum number of InternGrid cells on any DE in each direction.
!     \item[{[cellCountPerDEPerDim]}]
!          2-D array of number of InternGrid cells on each DE and in each direction.
!     \item[{[periodic]}]
!          Logical array that returns the periodicity of the coordinate axes.
!     \item[{[delayout]}]
!          {\tt delayout} that this InternGrid was distributed over.
!     \item[{[name]}]
!          {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call InternGridGet routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        ! the only thing that can be retrieved from an empty interngrid is the name
        if (present(name)) then
          call ESMF_GetName(interngrid%ptr%base, name, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif 
        if (present(horzinterngridtype           ) .OR. &
            present(vertinterngridtype           ) .OR. &
            present(horzstagger            ) .OR. &
            present(vertstagger            ) .OR. &
            present(horzcoordsystem        ) .OR. &
            present(vertcoordsystem        ) .OR. &
            present(coordorder             ) .OR. &
            present(dimCount               ) .OR. &
            present(distDimCount           ) .OR. &
            present(interngridstorage            ) .OR. &
            present(minGlobalCoordPerDim   ) .OR. &
            present(maxGlobalCoordPerDim   ) .OR. &
            present(globalCellCountPerDim  ) .OR. &
            present(globalStartPerDEPerDim ) .OR. &
            present(maxLocalCellCountPerDim) .OR. &
            present(cellCountPerDEPerDim   ) .OR. &
            present(periodic               ) .OR. &
            present(delayout               )) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                    "Unknown interngrid structure", &
                                    ESMF_CONTEXT, rc)) return
        endif

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGet(interngrid, horzrelloc, vertrelloc, &
                            horzinterngridtype, vertinterngridtype, &
                            horzstagger, vertstagger, &
                            horzcoordsystem, vertcoordsystem, &
                            coordorder, dimCount, distDimCount, interngridstorage, &
                            minGlobalCoordPerDim, maxGlobalCoordPerDim, &
                            globalCellCountPerDim, maxLocalCellCountPerDim, &
                            globalStartPerDEPerDim, cellCountPerDEPerDim, &
                            periodic, delayout=delayout, &
                            name=name, rc=localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetWithRelloc

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_InternGridGetAttribute  - Retrieve an attribute
!
! !INTERFACE:
!      subroutine ESMF_InternGridGetAttribute(interngrid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_InternGrid), intent(in) :: interngrid
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns an attribute from the {\tt interngrid}.
!      Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(out) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(out) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(out) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(out) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(out) :: valueList
!     \item type(ESMF\_Logical), intent(out) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(out) :: valueList
!     \item character (len = *), intent(out), value
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetInt4Attr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetInt4Attr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte integer attribute from the InternGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The 4-byte integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetInt4ListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetInt4ListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte integer list attribute from the InternGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The 4-byte integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetInt8Attr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetInt8Attr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte integer attribute from the InternGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The 8-byte integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetIntList8Attr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetInt8ListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte integer list attribute from the InternGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The 8-byte integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetReal4Attr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetReal4Attr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the InternGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The 4-byte real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetReal4ListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetReal4ListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte real list attribute from the InternGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The 4-byte real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetReal8Attr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetReal8Attr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from the InternGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The 8-byte real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetReal8ListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetReal8ListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte real list attribute from the InternGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The real*8 values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetLogicalAttr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetLogicalAttr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a logical attribute from the InternGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetLogicalListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetLogicalListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a logical list attribute from the InternGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the attribute.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetCharAttr"

!BOPI
! !IROUTINE: ESMF_InternGridGetAttribute - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttribute()
      subroutine ESMF_InternGridGetCharAttr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a character attribute from the InternGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetChar(interngrid%ptr%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetAttributeCount"

!BOP
! !IROUTINE: ESMF_InternGridGetAttributeCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_InternGridGetAttributeCount(interngrid, count, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      integer, intent(out) :: count
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns the number of attributes associated with the given InternGrid in
!      the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetCount(interngrid%ptr%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetAttrInfoByName"

!BOP
! !IROUTINE: ESMF_InternGridGetAttributeInfo - Query InternGrid attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttributeInfo()
      subroutine ESMF_InternGridGetAttrInfoByName(interngrid, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns information associated with the named attribute, 
!      including {\tt typekind} and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [count]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      type(ESMF_TypeKind) :: localTk
      integer :: localCount
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetAttrInfoName(interngrid%ptr%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetAttrInfoByNum"

!BOP
! !IROUTINE: ESMF_InternGridGetAttributeInfo - Query InternGrid attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridGetAttributeInfo()
      subroutine ESMF_InternGridGetAttrInfoByNum(interngrid, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute,
!      including {\tt typekind} and item {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [count]
!           Returns the number of items in this attribute.  For character types,
!           this is the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: localrc                          ! local error status
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetAttrInfoNum(interngrid%ptr%base, attributeIndex, &
        localName, localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetAttrInfoByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetCoord"
!BOP
! !IROUTINE: ESMF_InternGridGetCoord - Get the horizontal and/or vertical coordinates of a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridGetCoord(interngrid, horzrelloc, vertrelloc, centerCoord, &
                                   cornerCoord, faceCoord, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(inout) :: interngrid
      type(ESMF_RelLoc), intent(in), optional :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_InternArray), intent(out), dimension(:), optional :: centerCoord
      type(ESMF_InternArray), intent(out), dimension(:), optional :: cornerCoord
      type(ESMF_InternArray), intent(out), dimension(:), optional :: faceCoord
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns coordinate information for the {\tt interngrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be queried.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subInternGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subInternGrid to be queried.
!     \item[{[centerCoord]}]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[{[cornerCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout the InternGrid.
!     \item[{[faceCoord]}]
!          Coordinates of face centers of each cell.  The dimension index should
!          be defined first, followed by the face index.  Faces should
!          be numbered consistently with corners.  For example, face 1 should
!          correspond to the face between corners 1,2.
!     \item[{[reorder]}]
!          If TRUE, reorder any results using a previously set CoordOrder 
!          before returning.  If FALSE, do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available mostly for
!          internal use.
!     \item[{[total]}]
!          If TRUE, return the total coordinates including internally
!          generated boundary cells. If FALSE, return the computational
!          cells (which is what the user will be expecting).  The default
!          value is FALSE.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call InternGridGetCoord routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGetCoord(interngrid, horzrelloc, vertrelloc, centerCoord, &
                                 cornerCoord, faceCoord, reorder, total, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetCoord
      

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetCoordByDim1D"
!BOP
! !IROUTINE: ESMF_InternGridGetCoordByDim1D - Get the horizontal and/or vertical coordinates of a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridGetCoordByDim1D(interngrid, dim, horzrelloc, vertrelloc, &
        centerCoord, cornerCoord, faceCoord, reorder, total, localCounts, &
        docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(inout) :: interngrid
      integer, intent(in) :: dim
      type(ESMF_RelLoc), intent(in), optional :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      real(ESMF_KIND_R8), pointer, dimension(:) :: centerCoord
      real(ESMF_KIND_R8), pointer, dimension(:), optional :: cornerCoord
      real(ESMF_KIND_R8), pointer, dimension(:), optional :: faceCoord
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: localCounts(1)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns coordinate information for the {\tt interngrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be queried.
!     \item[dim]
!          dimension to be queried.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subInternGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subInternGrid to be queried.
!     \item[{[centerCoord]}]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[{[cornerCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout the InternGrid.
!     \item[{[faceCoord]}]
!          Coordinates of face centers of each cell.  The dimension index should
!          be defined first, followed by the face index.  Faces should
!          be numbered consistently with corners.  For example, face 1 should
!          correspond to the face between corners 1,2.
!     \item[{[reorder]}]
!          If TRUE, reorder any results using a previously set CoordOrder 
!          before returning.  If FALSE, do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available mostly for
!          internal use.
!     \item[{[total]}]
!          If TRUE, return the total coordinates including internally
!          generated boundary cells. If FALSE, return the computational
!          cells (which is what the user will be expecting).  The default
!          value is FALSE.
!     \item[{[localCounts]}]
!          Counts per dimension of the local piece of decomposition.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      type(ESMF_InternArray), dimension(:), pointer :: localCenterCoord
      type(ESMF_InternArray), dimension(:), pointer :: localCornerCoord
      type(ESMF_InternArray), dimension(:), pointer :: localFaceCoord

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call InternGridGetCoord routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        allocate(localCenterCoord(2))
        allocate(localCornerCoord(2))
        allocate(localFaceCoord(2))
        call ESMF_LRInternGridGetCoord(interngrid, horzrelloc, vertrelloc, &
          localCenterCoord, localCornerCoord, localFaceCoord, &
          reorder, total, localrc)
        if (present(localCounts)) then
          call ESMF_InternArrayGet(localCenterCoord(1), counts=localCounts, rc=rc)
        endif
!        if (present(centerCoord)) then
          call ESMF_InternArrayGetData(localCenterCoord(dim), centerCoord, &
            docopy, rc=rc)
!        endif
        if (present(cornerCoord)) then
          call ESMF_InternArrayGetData(localCornerCoord(dim), cornerCoord, &
            docopy, rc=rc)
        endif
        if (present(faceCoord)) then
          call ESMF_InternArrayGetData(localFaceCoord(dim), faceCoord,&
            docopy, rc=rc)
        endif
        deallocate(localCenterCoord)
        deallocate(localCornerCoord)
        deallocate(localFaceCoord)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetCoordByDim1D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetCoordByDim2D"
!BOP
! !IROUTINE: ESMF_InternGridGetCoordByDim2D - Get the horizontal and/or vertical coordinates of a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridGetCoordByDim2D(interngrid, dim, horzrelloc, vertrelloc, &
        centerCoord, cornerCoord, faceCoord, reorder, total, localCounts, &
        docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(inout) :: interngrid
      integer, intent(in) :: dim
      type(ESMF_RelLoc), intent(in), optional :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      real(ESMF_KIND_R8), pointer, dimension(:,:), optional :: centerCoord
      real(ESMF_KIND_R8), pointer, dimension(:,:), optional :: cornerCoord
      real(ESMF_KIND_R8), pointer, dimension(:,:), optional :: faceCoord
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: localCounts(2)
      type(ESMF_CopyFlag), intent(in), optional :: docopy
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns coordinate information for the {\tt interngrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be queried.
!     \item[dim]
!          dimension to be queried.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subInternGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subInternGrid to be queried.
!     \item[{[centerCoord]}]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[{[cornerCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout the InternGrid.
!     \item[{[faceCoord]}]
!          Coordinates of face centers of each cell.  The dimension index should
!          be defined first, followed by the face index.  Faces should
!          be numbered consistently with corners.  For example, face 1 should
!          correspond to the face between corners 1,2.
!     \item[{[reorder]}]
!          If TRUE, reorder any results using a previously set CoordOrder 
!          before returning.  If FALSE, do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available mostly for
!          internal use.
!     \item[{[total]}]
!          If TRUE, return the total coordinates including internally
!          generated boundary cells. If FALSE, return the computational
!          cells (which is what the user will be expecting).  The default
!          value is FALSE.
!     \item[{[localCounts]}]
!          Counts per dimension of the local piece of decomposition.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      type(ESMF_InternArray), dimension(:), pointer :: localCenterCoord
      type(ESMF_InternArray), dimension(:), pointer :: localCornerCoord
      type(ESMF_InternArray), dimension(:), pointer :: localFaceCoord

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call InternGridGetCoord routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        allocate(localCenterCoord(2))
        allocate(localCornerCoord(2))
        allocate(localFaceCoord(2))
        call ESMF_LRInternGridGetCoord(interngrid, horzrelloc, vertrelloc, &
          localCenterCoord, localCornerCoord, localFaceCoord, &
          reorder, total, localrc)
        if (present(localCounts)) then
          call ESMF_InternArrayGet(localCenterCoord(1), counts=localCounts, rc=rc)
        endif
        if (present(centerCoord)) then
          call ESMF_InternArrayGetData(localCenterCoord(dim), centerCoord, &
            docopy, rc=rc)
        endif
        if (present(cornerCoord)) then
          call ESMF_InternArrayGetData(localCornerCoord(dim), cornerCoord, &
            docopy, rc=rc)
        endif
        if (present(faceCoord)) then
          call ESMF_InternArrayGetData(localFaceCoord(dim), faceCoord,&
            docopy, rc=rc)
        endif
        deallocate(localCenterCoord)
        deallocate(localCornerCoord)
        deallocate(localFaceCoord)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetCoordByDim2D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetDELocalInfo"
!BOP
! !IROUTINE: ESMF_InternGridGetDELocalInfo - Get DE-local information for a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridGetDELocalInfo(interngrid, horzrelloc, vertrelloc, &
                                myDE, localCellCount, localCellCountPerDim, &
                                minLocalCoordPerDim, maxLocalCoordPerDim, &
                                globalStartPerDim, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      integer, intent(out), optional :: myDE
      integer, intent(out), optional :: localCellCount
      integer, dimension(:), intent(out), optional :: localCellCountPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minLocalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxLocalCoordPerDim
      integer, dimension(:), intent(out), optional :: globalStartPerDim
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Gets InternGrid or subInternGrid information for a particular Decomposition
!     Element (DE) assigned to this PET.  This routine cannot retrieve
!     information about a DE on an different PET.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be queried.
!     \item[horzrelloc]
!          Horizontal relative location of the subInternGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subInternGrid to be queried.
!     \item[{[myDE]}]
!          Identifier for this {\tt ESMF\_DE}, zero-based.  Note that this is 
!          a returned value, not an input one.
!     \item[{[localCellCount]}]
!          Local (on this {\tt ESMF\_DE}) number of cells.
!     \item[{[localCellCountPerDim]}]
!          Local (on this {\tt ESMF\_DE}) number of cells per dimension.
!     \item[{[minLocalCoordPerDim]}]
!          Array of minimum local coordinate values on this DE in each dimension.
!          The number of array elements should be greater or equal to the number
!          of InternGrid dimensions.
!     \item[{[maxLocalCoordPerDim]}]
!          Array of maximum local coordinate values on this DE in each dimension.
!          The number of array elements should be greater or equal to the number
!          of InternGrid dimensions.
!     \item[{[globalStartPerDim]}]
!          Global index of starting counts for each dimension.
!          The number of array elements should be greater or equal to the number
!          of InternGrid dimensions.
!     \item[{[reorder]}]
!          If TRUE, reorder any results using a previously set CoordOrder
!          before returning.  If FALSE, do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available primarily for
!          internal use.
!     \item[{[total]}]
!          If TRUE, return queries based on the total coordinates including
!          internally generated boundary cells. If FALSE, return queries based
!          on the computational cells (which is what the user will be expecting).
!          The default value is FALSE.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call InternGridGetDELocalInfo routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGetDELocalInfo(interngrid, horzrelloc, vertrelloc, &
                              myDE, localCellCount, localCellCountPerDim, &
                              minLocalCoordPerDim, maxLocalCoordPerDim, &
                              globalStartPerDim, reorder=reorder, &
                              total=total, rc=localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetDELocalInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGlobalToDELocalIndex"
!BOP
! !IROUTINE: ESMF_InternGridGlobalToDELocalIndex - Translate global indexing to DE-local

! !INTERFACE:
      subroutine ESMF_InternGridGlobalToDELocalIndex(interngrid, horzrelloc, vertrelloc, &
                                               global1D, local1D, &
                                               global2D, local2D, &
                                               dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) :: global1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: local1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) :: global2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: local2D
      integer, dimension(:), optional, intent(in) :: dimOrder
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Translates an array of integer cell identifiers from global indexing 
!     to DE-local indexing.  This routine is intended to identify equivalent
!     positions of interngrid elements in distributed (DE-local) arrays and gathered
!     (global) arrays, either by memory location or index pairs.
!     WARNING:  This routine is meant for very limited user access.  It works
!               with InternGrid indices and will give erroneous results if applied to
!               Field or Array indices.  In the future, this should be a Field
!               method, but in the meantime it will be left available here.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be used.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subInternGrid to be used for the
!          translation.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subInternGrid to be used for the
!          translation.
!     \item[{[global1D]}]
!          One-dimensional array of global identifiers to be translated.
!          Usage of this optional argument infers translating between positions
!          in memory from a global array to a DE-local (or distributed) one.
!          This array is dimensioned (N), where N is the number of memory
!          locations to be translated.
!     \item[{[local1D]}]
!          One-dimensional array of DE-local identifiers for the return of the
!          translation.  This array must be the same size as {\tt global1D},
!          and must be present if {\tt global1D} is present.  If either of
!          these conditions is not met, an error is issued.
!     \item[{[global2D]}]
!          Two-dimensional array of global identifiers to be translated.
!          Usage of this optional argument infers translating between indices
!          in IJ space.  This array is assumed to be dimensioned (N,2), where
!          N is the number of index locations to be translated and the second
!          dimension corresponds to the two InternGrid indices that are distributed 
!          (currently any two dimensions of a three-dimensional InternGrid can be
!          distributed).  So to translate three sets of global indices to
!          DE-local indexing,
!          \begin{description}
!            \item {\tt global2D(1,1)} = index1(1)
!            \item {\tt global2D(1,2)} = index1(2)
!            \item {\tt global2D(2,1)} = index2(1)
!            \item {\tt global2D(2,2)} = index2(2)
!            \item {\tt global2D(3,1)} = index3(1)
!            \item {\tt global2D(3,2)} = index3(2)
!          \end{description}
!     \item[{[local2D]}]
!          Two-dimensional array of DE-local identifiers for the return of the
!          translation.  This array must be the same size as {\tt global2D},
!          and must be present if {\tt global2D} is present.  If either of
!          these conditions is not met, an error is issued.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call InternGridGlobalToDELocalIndex routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGlobalToDELocalIndex(interngrid, horzrelloc, vertrelloc, &
                                             global1D, local1D, &
                                             global2D, local2D, &
                                             dimOrder, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGlobalToDELocalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridDELocalToGlobalIndex"
!BOP
! !IROUTINE: ESMF_InternGridDELocalToGlobalIndex - Translate DE-local indexing to global

! !INTERFACE:
      subroutine ESMF_InternGridDELocalToGlobalIndex(interngrid, horzrelloc, vertrelloc, &
                                               local1D, global1D, &
                                               local2D, global2D, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) ::  local1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: global1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) ::  local2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: global2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Translates an array of integer cell identifiers from DE-local indexing 
!     to global indexing.  This routine is intended to identify equivalent
!     positions of interngrid elements in distributed (DE-local) arrays and gathered
!     (global) arrays, either by memory location or index pairs.
!     WARNING:  This routine is meant for very limited user access.  It works
!               with InternGrid indices and will give erroneous results if applied to
!               Field or Array indices.  In the future, this should be a Field
!               method, but in the meantime it will be left available here.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be used.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subInternGrid to be used for the
!          translation.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subInternGrid to be used for the
!          translation.
!     \item[{[local1D]}]
!          One-dimensional array of DE-local identifiers to be translated.
!          Usage of this optional argument infers translating between positions
!          in memory from a DE-local (or distributed) InternGrid array to a global one.
!          This array is dimensioned (N), where N is the number of memory
!          locations to be translated.
!     \item[{[global1D]}]
!          One-dimensional array of global identifiers for the return of the
!          translation.  This array must be the same size as {\tt local1D},
!          and must be present if {\tt local1D} is present.  If either of
!          these conditions is not met, an error is issued.
!     \item[{[local2D]}]
!          Two-dimensional array of DE-local identifiers to be translated.
!          Usage of this optional argument infers translating between indices
!          in IJ space.  This array is assumed to be dimensioned (N,2), where
!          N is the number of index locations to be translated and the second
!          dimension corresponds to the two InternGrid indices that are distributed 
!          (currently any two dimensions of a three-dimensional InternGrid can be
!          distributed).  So to translate three sets of DE-local indices to
!          global indexing,
!          \begin{description}
!            \item {\tt local2D(1,1)} = index1(1)
!            \item {\tt local2D(1,2)} = index1(2)
!            \item {\tt local2D(2,1)} = index2(1)
!            \item {\tt local2D(2,2)} = index2(2)
!            \item {\tt local2D(3,1)} = index3(1)
!            \item {\tt local2D(3,2)} = index3(2)
!          \end{description}
!     \item[{[global2D]}]
!          Two-dimensional array of global identifiers for the return of the
!          translation.  This array must be the same size as {\tt local2D},
!          and must be present if {\tt local2D} is present.  If either of
!          these conditions is not met, an error is issued.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! check if interngrid%ptr is associated
      if (.not. associated(interngrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized InternGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check interngrid status
      if (interngrid%ptr%interngridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized interngrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call InternGridDELocalToGlobalIndex routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridDELocalToGlobalIndex(interngrid, horzrelloc, vertrelloc, &
                                             local1D, global1D, &
                                             local2D, global2D, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridDELocalToGlobalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridPrint"
!BOP
! !IROUTINE: ESMF_InternGridPrint - Print the contents of a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridPrint(interngrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid),   intent(in) :: interngrid
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Prints information about the {\tt interngrid} to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to print.
!     \item[{[options]}]
!          Print options are not yet supported.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      !character(len=ESMF_MAXSTR) :: name, str
      type(ESMF_InternGridClass), pointer :: gp
      integer :: i
      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      print *, "********Begin InternGrid Print:"
      if (.not. associated(interngrid%ptr)) then
        print *, "Empty or Uninitialized InternGrid"
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      !TODO: complete prints

      gp => interngrid%ptr
  !    call ESMF_StatusString(gp%interngridStatus, str, rc)
  !    print *, "InternGrid status = ", trim(str)

      if ((gp%interngridStatus.ne.ESMF_IGRID_STATUS_READY) .AND. &
          (gp%interngridStatus.ne.ESMF_IGRID_STATUS_INIT)) then
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif

      call ESMF_BasePrint(gp%base, "", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! if the interngrid has been distributed, then print the associated
      ! physgrids and interndgs
      if (gp%interngridStatus.eq.ESMF_IGRID_STATUS_READY) then

        ! Print the Associated physgrids
        print *, 'PhysGrids associated with this interngrid:'
        do i=1, gp%numPhysGrids
          call ESMF_PhysGridPrint(gp%physgrids(i), 'no-opt')
        enddo

        ! Print the InternDG
        print *, 'InternDGs associated with this InternGrid:'
        do i=1, gp%numInternDGs
          call ESMF_InternDGPrint(gp%interndgs(i), 'no-opt')
        enddo
      endif

      print *, "*********End InternGrid Print"

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSet"
!BOP
! !IROUTINE: ESMF_InternGridSet - Set a variety of information about a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridSet(interngrid, horzinterngridtype, vertinterngridtype, &
                              horzstagger, vertstagger, &
                              horzcoordsystem, vertcoordsystem, &
                              coordorder, minGlobalCoordPerDim, &
                              maxGlobalCoordPerDim, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_InternGridType),     intent(in), optional :: horzinterngridtype
      type(ESMF_InternGridVertType), intent(in), optional :: vertinterngridtype
      type(ESMF_InternGridHorzStagger), intent(in), optional :: horzstagger
      type(ESMF_InternGridVertStagger), intent(in), optional :: vertstagger
      type(ESMF_CoordSystem), intent(in), optional :: horzcoordsystem
      type(ESMF_CoordSystem), intent(in), optional :: vertcoordsystem
      type(ESMF_CoordOrder), intent(in), optional :: coordorder
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: maxGlobalCoordPerDim
      type(ESMF_Logical), intent(in), optional :: periodic(:)
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Sets information for the InternGrid that may not have been included at
!     InternGrid creation.
!     WARNING:  This routine does not automatically regenerate the InternGrid
!               when used to reset its values, some of which may significantly
!               alter the existing InternGrid.  Therefore this routine may only
!               be used prior to the {\tt ESMF\_InternGridDistribute()} call.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be modified.
!     \item[{[horzinterngridType]}]
!          {\tt ESMF\_InternGridType} specifier denoting horizontal InternGrid type.
!     \item[{[vertinterngridType]}]
!          {\tt ESMF\_InternGridVertType} specifier denoting vertical subInternGrid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_InternGridHorzStagger} specifier denoting horizontal InternGrid
!          stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_InternGridVertStagger} specifier denoting vertical subInternGrid
!          stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal InternGrid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical subInternGrid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the InternGrid and all related Fields (i.e. ZXY).
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical array that returns the periodicity of the coordinate axes.
!     \item[{[name]}]
!          Character string name of {\tt ESMF\_InternGrid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: i                                ! loop index
      type(ESMF_InternGridClass), pointer :: interngridp      ! Pointer to new interngrid

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! Initialize other variables
      interngridp => interngrid%ptr

      ! if present, set information filling in interngrid derived type
      if (present(horzinterngridtype   )) interngridp%horzInternGridType    = horzinterngridtype
      if (present(vertinterngridtype   )) interngridp%vertInternGridType    = vertinterngridtype
      if (present(horzstagger    )) interngridp%horzStagger     = horzstagger
      if (present(vertstagger    )) interngridp%vertStagger     = vertstagger
      if (present(horzcoordsystem)) interngridp%horzCoordSystem = horzcoordsystem
      if (present(vertcoordsystem)) interngridp%vertCoordSystem = vertcoordsystem
      if (present(coordorder     )) interngridp%coordOrder      = coordorder
      if (present(periodic)) then
        do i=1,ESMF_MAXIGRIDDIM
          if (i > size(periodic)) exit
          interngridp%periodic(i) = periodic(i)
        enddo
      endif

      if (present(minGlobalCoordPerDim)) then
   !     if (size(minGlobalCoordPerDim) .gt. ESMF_MAXIGRIDDIM) exit  ! TODO
        do i=1,size(minGlobalCoordPerDim)
          interngridp%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
        enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
   !     if (size(maxGlobalCoordPerDim) .gt. ESMF_MAXIGRIDDIM) exit  ! TODO
        do i=1,size(maxGlobalCoordPerDim)
          interngridp%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
        enddo
      endif

      if (present(name)) then
          call ESMF_SetName(interngridp%base, name, "InternGrid", localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_InternGridSetAttribute - Set an attribute
!
! !INTERFACE:
!      subroutine ESMF_InternGridSetAttribute(interngrid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_InternGrid), intent(inout) :: interngrid
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values    
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt interngrid}.
!     The attribute has a {\tt name} and either a {\tt value} or a
!     {\tt valueList}.
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(in) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(in) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(in) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(in) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(in) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(in) :: valueList
!     \item type(ESMF\_Logical), intent(in) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(in) :: valueList
!     \item character (len = *), intent(in), value
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetInt4Attr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetInt4Attr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(inout) :: interngrid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the InternGrid.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The 4-byte integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetInt4ListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetInt4ListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 4-byte integer list attribute to the InternGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The 4-byte integer values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
  
      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetInt8Attr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetInt8Attr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(inout) :: interngrid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches an 8-byte integer attribute to the InternGrid.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The 8-byte integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetInt8ListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetInt8ListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 8-byte integer list attribute to the InternGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The 8-byte integer values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
  
      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetReal4Attr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetReal4Attr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the InternGrid.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The 4-byte real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetReal4ListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetReal4ListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 4-byte real list attribute to the InternGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The 4-byte real values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetReal8Attr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetReal8Attr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches an 8-byte real attribute to the InternGrid.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The 8-byte real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetReal8ListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetReal8ListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches an 8-byte real list attribute to the InternGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The 8-byte real values of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetLogicalAttr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetLogicalAttr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a logical attribute to the InternGrid.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetLogicalListAttr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetLogicalListAttr(interngrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a logical list attribute to the InternGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [value]
!           The logical true/false values of the attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(interngrid%ptr%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetCharAttr"

!BOPI
! !IROUTINE: ESMF_InternGridSetAttribute - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_InternGridSetAttribute()
      subroutine ESMF_InternGridSetCharAttr(interngrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a character attribute to the InternGrid.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           An {\tt ESMF\_InternGrid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      call c_ESMC_AttributeSetChar(interngrid%ptr%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridValidate"
!BOP
! !IROUTINE: ESMF_InternGridValidate - Check validity of a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridValidate(interngrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in) :: interngrid
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that an {\tt ESMF\_InternGrid} is internally consistent.  Currently
!     checks to ensure:
!     \begin{enumerate}
!        \item the pointer to the InternGrid is associated; and
!        \item the InternGrid status indicates the InternGrid is ready to use.
!     \end{enumerate}
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be validated.
!     \item[{[options]}]
!          Validation options are not yet supported.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      if (.not. associated(interngrid%ptr)) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized InternGrid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call validate routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown interngrid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridValidate(interngrid, options, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid InternGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridBoxIntersectRecv"
!BOPI
! !IROUTINE: ESMF_InternGridBoxIntersectRecv - Determine a DomainList covering a box

! !INTERFACE:
      subroutine ESMF_InternGridBoxIntersectRecv(srcInternGrid, dstInternGrid, parentVM, &
                                           domainList, hasSrcData, hasDstData, &
                                           total, layer, &
                                           srcrelloc, dstrelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: srcInternGrid
      type(ESMF_InternGrid) :: dstInternGrid
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_DomainList), intent(out) :: domainList ! BOB changed this to just out
      logical, intent(in) :: hasSrcData
      logical, intent(in) :: hasDstData
      logical, intent(in) :: total
      logical, intent(in) :: layer
      type(ESMF_RelLoc), intent(in), optional :: srcrelloc
      type(ESMF_RelLoc), intent(in), optional :: dstrelloc
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     This routine computes the DomainList that must be received in order to
!     cover the de-local "boxes" of a destination InternGrid.  This routine is for the
!     case of a DE that is part of a destination InternGrid determining which DEs it
!     will receive data from.  All PETs that are part of either the source or
!     destination DELayouts must call this routine, due to some necessary
!     global communication calls.
!
!     The arguments are:
!     \begin{description}
!     \item[srcInternGrid]
!          Source {\tt ESMF\_InternGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[dstInternGrid]
!          Destination {\tt ESMF\_InternGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[parentVM]
!          {\tt ESMF\_VM} covering the union of the source and destination
!          InternGrids.
!     \item[domainList]
!          Resulting {\tt ESMF\_DomainList} containing the set of
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[hasSrcData]
!          Logical flag to indicate whether or not the local PET has data
!          on the source InternGrid.
!     \item[hasDstData]
!          Logical flag to indicate whether or not the local PET has data
!          on the destination InternGrid.
!     \item[total]
!          If TRUE, return DomainLists based on the total coordinates including
!          internally generated boundary cells. If FALSE, return DomainLists
!          based on the computational cells (which is what the user will be
!          expecting).
!     \item[layer]
!          Logical flag to indicate the domainList should add an extra layer
!          of cells, which in necessary for some regridding algorithms in
!          some situations.
!     \item[{[srcrelloc]}]
!          Optional argument to set the relative location of the source
!          subInternGrid for all searches.  The default is ESMF_CELL_CENTER.
!     \item[{[dstrelloc]}]
!          Optional argument to set the relative location of the destination
!          subInternGrid for all searches.  The default is ESMF_CELL_CENTER.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,srcinterngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,dstinterngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVM,rc)


      if ((.not. associated(srcInternGrid%ptr)) .OR. &
          (.not. associated(dstInternGrid%ptr))) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized InternGrid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call intersect routines based on InternGridStructure

      select case(srcInternGrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown interngrid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridBoxIntersectRecv(srcInternGrid, dstInternGrid, parentVM, &
                                         domainList, hasSrcData, hasDstData, &
                                         total, layer, &
                                         srcrelloc, dstrelloc, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid InternGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridBoxIntersectRecv

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridBoxIntersectSend"
!BOPI
! !IROUTINE: ESMF_InternGridBoxIntersectSend - Determine a DomainList covering a box

! !INTERFACE:
      subroutine ESMF_InternGridBoxIntersectSend(srcInternGrid, dstInternGrid, domainList, &
                                           total, layer, &
                                           srcrelloc, dstrelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: srcInternGrid
      type(ESMF_InternGrid) :: dstInternGrid
      type(ESMF_DomainList), intent(out) :: domainList !BOB changed this to just out
      logical, intent(in) :: total
      logical, intent(in) :: layer
      type(ESMF_RelLoc), intent(in), optional :: srcrelloc
      type(ESMF_RelLoc), intent(in), optional :: dstrelloc
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     This routine computes the DomainList that must be sent in order to cover
!     the de-local "boxes" of a destination InternGrid.  This routine is for the case
!     of a DE that is part of a source InternGrid determining which DEs it will send
!     its data to.  This routine should not be called if this PET does not
!     have any source data.
!
!     The arguments are:
!     \begin{description}
!     \item[srcInternGrid]
!          Source {\tt ESMF\_InternGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[dstInternGrid]
!          Destination {\tt ESMF\_InternGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[domainList]
!          Resulting {\tt ESMF\_DomainList} containing the set of
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[total]
!          If TRUE, return DomainLists based on the total coordinates including
!          internally generated boundary cells. If FALSE, return DomainLists
!          based on the computational cells (which is what the user will be
!          expecting).
!     \item[layer]
!          Logical flag to indicate the domainList should add an extra layer
!          of cells, which in necessary for some regridding algorithms in
!          some situations.
!     \item[{[srcrelloc]}]
!          Optional argument to set the relative location of the source
!          subInternGrid for all searches.  The default is ESMF_CELL_CENTER.
!     \item[{[dstrelloc]}]
!          Optional argument to set the relative location of the destination
!          subInternGrid for all searches.  The default is ESMF_CELL_CENTER.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,srcInternGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,dstInternGrid,rc)

      if ((.not. associated(dstInternGrid%ptr)) .or. &
          (.not. associated(srcInternGrid%ptr))) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized InternGrid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call intersect routines based on InternGridStructure

      select case(srcInternGrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown interngrid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridBoxIntersectSend(srcInternGrid, dstInternGrid, domainList, &
                                         total, layer, srcrelloc, dstrelloc, &
                                         localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid InternGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridBoxIntersectSend

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridComputeDistance"
!BOPI
! !IROUTINE: ESMF_InternGridComputeDistance - Compute distance between points
!
! !INTERFACE:
      function ESMF_InternGridComputeDistance(x1, y1, x2, y2, coordSystem, rc)

! !RETURN VALUE:
      real(ESMF_KIND_R8) :: ESMF_InternGridComputeDistance

! !ARGUMENTS:

      real(ESMF_KIND_R8), intent(in) :: x1      ! x,y coordinates of two points
      real(ESMF_KIND_R8), intent(in) :: y1      ! between which the distance is
      real(ESMF_KIND_R8), intent(in) :: x2      ! to be computed
      real(ESMF_KIND_R8), intent(in) :: y2
      type(ESMF_CoordSystem) :: coordSystem    ! coordinate system in which the
                                                ! points are given
      integer, optional :: rc                   ! return code

! !DESCRIPTION:
!     This routine computes the distance between two points given the
!     coordinates of the two points.
!
!     The arguments are:
!     \begin{description}
!     \item[x1,y1,x2,y2]
!          Coordinates of two points between which to compute distance.
!     \item[coordSystem]
!          Coordinate system in which the points are given
!          (e.g. spherical, Cartesian)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! branch to appropriate PhysGrid routine to compute distance
      if (coordSystem .eq. ESMF_COORD_SYSTEM_SPHERICAL) then
        ESMF_InternGridComputeDistance = &
          ESMF_PhysGridCompDistSpherical(x1, y1, x2, y2, rc=localrc)
      elseif (coordSystem .eq. ESMF_COORD_SYSTEM_CARTESIAN) then
        ESMF_InternGridComputeDistance = &
          ESMF_PhysGridCompDistCartesian(x1, y1, x2, y2, rc=localrc)
      else
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "Distance in coordinate system not yet supported", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set return code and exit
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_InternGridComputeDistance

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetAllAxisIndex"
!BOPI
! !IROUTINE: ESMF_InternGridGetAllAxisIndex - Get all axis indices for a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridGetAllAxisIndex(interngrid, globalAI, horzrelloc, &
                                          vertrelloc, AICountPerDE, total, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: globalAI
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      integer, dimension(:), pointer, optional :: AICountPerDE
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_InternDG} attribute with the given value.
!
!   The arguments are:
!   \begin{description}
!   \item[interngrid]
!        InternGrid to be queried.
!   \item[globalAI]
!        2D Array of AxisIndex types, must be (number of DEs, interngrid rank) long,
!        intent(out) for this routine.
!   \item[horzrelloc]
!        Required for a 2D interngrid; controls which of the InternDGs will be
!        used to answer the query.  (e.g. Cell-centered data will return
!        different counts than vertex-based data.)
!   \item[{[vertrelloc]}]
!        Not required by the fortran interface, but required if the InternGrid
!        is 3D.
!   \item[{[AICountPerDE]}]
!        Required if the InternGrid has an ARBITRARY distribution; ignored if it
!        does not.
!   \item[{[total]}]
!        If TRUE, return queries based on the total coordinates including
!        internally generated boundary cells. If FALSE, return queries based
!        on the computational cells (which is what the user will be expecting).
!        The default value is FALSE.
!   \item[{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! Call InternGridGetAllAxisIndex routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown interngrid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGetAllAxisIndex(interngrid, globalAI, horzrelloc, vertrelloc, &
                                        AICountPerDE, total, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid InternGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetAllAxisIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetAIsAllDEs"
!BOPI
! !IROUTINE: ESMF_InternGridGetAIsAllDEs - Get a InternGrid's AIs for all DEs

! !INTERFACE:
      subroutine ESMF_InternGridGetAIsAllDEs(interngrid, localGlobalFlag, &
                                       AIListPerDEPerRank, &
                                       horzRelLoc, vertRelLoc, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_LocalGlobalFlag), intent(in) :: localGlobalFlag
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AIListPerDEPerRank
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_InternDG} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[localGlobalFlag]
!          {\tt ESMF\_LocalGlobalFlag] identifier indicating whether the returned
!          array of {\tt ESMF\_AxisIndex} types should be in local or global
!          index space.
!     \item[AIListPerDEPerRank]
!          2D array of {\tt ESMF\_AxisIndex} types containing results.  If
!          allocated, it must be of size (nDEs,interngridrank).
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! Call InternGridGetAllAxisIndex routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                   "Unknown interngrid structure", &
                                   ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGetAIsAllDEs(interngrid, localGlobalFlag, AIListPerDEPerRank, &
                                     horzRelLoc, vertRelLoc, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                   "InternGrid structure Log Rect Block", &
                                   ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                   "InternGrid structure Unstructured", &
                                   ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                   "InternGrid structure User", &
                                   ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                   "Invalid InternGrid structure", &
                                   ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetAIsAllDEs

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetCellMask"
!BOPI
! !IROUTINE: ESMF_InternGridGetCellMask - Retrieves cell identifier mask for a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridGetCellMask(interngrid, maskArray, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_InternArray), intent(out) :: maskArray !BOB switched from inout to out
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of get retrieves an {\tt ESMF\_Array} of cell types for an
!     {\tt ESMF\_InternGrid} from a corresponding {\tt ESMF\_PhysGrid}.
!     This mask is intended for internal use to indicate which cells are in
!     the computational regime (cellType=0), a ghost region (cellType=1), or a
!     halo region (cellType=2).
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_InternGrid} to be queried.
!     \item[maskArray]
!          {\tt ESMF\_Array} to contain the internally-used cell array denoting
!          whether cells are in the computational regime, a ghost region, or a
!          halo region.
!     \item[relloc]
!          Relative location of the {\tt ESMF\_PhysGrid} to be queried.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! Call InternGridGetCellMask routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown interngrid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGetCellMask(interngrid, maskArray, relloc, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "InternGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid InternGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetCellMask

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetDELocalAI"
!BOPI
! !IROUTINE: ESMF_InternGridGetDELocalAI - Get local aixs index DE information for a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridGetDELocalAI(interngrid, AIPerDim, horzrelloc, &
                                       vertrelloc, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_AxisIndex), dimension(:), intent(out) :: AIPerDim
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_InternDG} attribute with the given value.  Since a single
!     {\tt ESMF\_InternGrid} can have many {\tt ESMF\_InternDGs}, the correct
!     {\tt ESMF\_InternDG} must be identified by this calling routine.  For a 3D
!     {\tt ESMF\_InternGrid}, the user must supply identifiers for both the horizontal
!     and vertical interngrids if querying for an array of values, like 
!     localCellCountPerDim.  The {\tt ESMF\_InternDG(s)} are identified
!     using the set of input variables:  horzrelloc and/or vertrelloc.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item[AIPerDim]
!          Global axis indices for each dimension.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
!     \item[{[reorder]}]
!          If TRUE, reorder any results using a previously set CoordOrder
!          before returning.  If FALSE, do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available primarily for
!          internal use.
!     \item[{[total]}]
!          If TRUE, return queries based on the total coordinates including
!          internally generated boundary cells. If FALSE, return queries based
!          on the computational cells (which is what the user will be expecting).
!          The default value is FALSE.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: i,j

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! Call InternGridGetDELocalInfo routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGetDELocalInfo(interngrid, horzrelloc, vertrelloc, &
                                       globalAIPerDim=AIPerDim, &
                                       reorder=reorder, total=total, rc=localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetDELocalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGlobalToDELocalAI"
!BOPI
! !IROUTINE: ESMF_InternGridGlobalToDELocalAI - Translate global axis index to DE local

! !INTERFACE:
      subroutine ESMF_InternGridGlobalToDELocalAI(interngrid, horzrelloc, vertrelloc, &
                                            globalAI1D, localAI1D, &
                                            globalAI2D, localAI2D, &
                                            dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_AxisIndex), dimension(:), optional, intent(inout) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(inout) :: globalAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) ::  localAI2D
      integer, dimension(:), optional, intent(in) :: dimOrder
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_InternDG} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be used.
!     \item[{[globalAI1D]}]
!          One-dimensional array of global AxisIndices to be translated.
!     \item[{[localAI1D]}]
!          One-dimensional array of local AxisIndices corresponding to global AIs.
!     \item[{[globalAI2D]}]
!          Two-dimensional array of global AxisIndices to be translated.
!     \item[{[localAI2D]}]
!          Two-dimensional array of local AxisIndices corresponding to global AIs.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: i,j

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      if (present(globalAI1D)) then
          do i=1,size(globalAI1D)
              ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit, ESMF_AxisIndexInit,globalAI1D(i))
          enddo
      endif

      if (present(globalAI2D)) then
          do j=1,size(globalAI2D,2)
          do i=1,size(globalAI2D,1)
              ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit, ESMF_AxisIndexInit,globalAI2D(i,j))
          enddo
          enddo
      endif


      ! Call InternGridGlobalToDELocalAI routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridGlobalToDELocalAI(interngrid, horzrelloc, vertrelloc, &
                                          globalAI1D, localAI1D, &
                                          globalAI2D, localAI2D, &
                                          dimOrder, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGlobalToDELocalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridDELocalToGlobalAI"
!BOPI
! !IROUTINE: ESMF_InternGridDELocalToGlobalAI - Translate DE local axis index to global

! !INTERFACE:
      subroutine ESMF_InternGridDELocalToGlobalAI(interngrid, horzrelloc, vertrelloc, &
                                            localAI1D, globalAI1D, &
                                            localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_AxisIndex), dimension(:), optional, intent(inout) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(inout) ::  localAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) :: globalAI2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_InternDG} routine that translates an array of
!     axis indices from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be used.
!     \item[{[localAI1D]}]
!          One-dimensional array of local AxisIndices to be translated.
!     \item[{[globalAI1D]}]
!          One-dimensional array of global AxisIndices corresponding to local AIs.
!     \item[{[localAI2D]}]
!          Two-dimensional array of local AxisIndices to be translated.
!     \item[{[globalAI2D]}]
!          Two-dimensional array of global AxisIndices corresponding to local AIs.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: i,j

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      if (present(localAI1D)) then
          do i=1,size(localAI1D)
              ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit, ESMF_AxisIndexInit,localAI1D(i))
          enddo
      endif

      if (present(localAI2D)) then
          do j=1,size(localAI2D,2)
          do i=1,size(localAI2D,1)
              ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit, ESMF_AxisIndexInit,localAI2D(i,j))
          enddo
          enddo
      endif

      ! Call InternGridDELocalToGlobalAI routines based on InternGridStructure

      select case(interngrid%ptr%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridDELocalToGlobalAI(interngrid, horzrelloc, vertrelloc, &
                                          localAI1D, globalAI1D, &
                                          localAI2D, globalAI2D, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridDELocalToGlobalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSearchPoint"
!!BOPI
!! !IROUTINE: ESMF_InternGridSearchPoint - Search the interngrid for a cell containing point
!
! !INTERFACE:
!      subroutine ESMF_InternGridSearchPoint(dstAdd, x, y, DEID, searchInternGrid, &
!                                      physInternGridID, rc)
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) :: dstAdd       ! location in interngrid of interngrid cell
!                                            ! containing search point
!      real (kind=?), intent(in) :: x        ! x coordinates of search point 
!      real (kind=?), intent(in) :: y        ! y coordinates of search point 
!      integer, intent(in) :: DEID           ! DE which owns the search point
!      type(ESMF_InternGrid), intent(in) :: searchInternGrid
!                                            ! interngrid to search for location of point
!      integer, intent(in), optional :: physInternGridID
!                                            ! id of the subinterngrid to search
!                                            ! (if more than one subinterngrid)
!      integer, intent(out), optional :: rc  ! return code
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the interngrid of a interngrid cell 
!!     containing the point given by the input x,y coordinates.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dstAdd]
!!          Address of interngrid cell containing the search point.
!!     \item[x]
!!          X coordinates of search point.
!!     \item[y]
!!          Y coordinates of search point.
!!     \item[DEID]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[searchInternGrid]
!!          ESMF {\tt ESMF\_InternGrid} to search for location.
!!     \item[{[physInternGridID]}]
!!          If more than one {\tt ESMF\_PhysGrid} is contained in 
!!          {\tt ESMF\_InternGrid}, choose which interngrid to search (default is 1st
!!          {\tt ESMF\_PhysGrid}?).
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOPI
!
!      integer :: localrc                          ! local error status
!      logical :: dummy
!
!      ! Initialize return code; assume routine not implemented
!      if (present(rc)) rc = ESMF_RC_NOT_IMPL
!
!!     Call Search routines based on InternGridStructure
!
!      select case(interngrid%ptr%interngridStructure%interngridStructure)
!
!      !-------------
!      ! ESMF_IGRID_STRUCTURE_UNKNOWN
!      case(0)
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
!                                      "Unknown interngrid structure", &
!                                      ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_IGRID_STRUCTURE_LOGRECT
!      case(1)
!        call ESMF_LRInternGridSearchPoint(dstAdd, x, y, DEID, searchInternGrid, &
!                                    physInternGridID, localrc)
!
!      !-------------
!      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
!      case(2)
!        dummy = (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                       "InternGrid structure Log Rect Block", &
!                                       ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
!      case(3)
!        dummy =  ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                       "InternGrid structure Unstructured", &
!                                       ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_IGRID_STRUCTURE_USER
!      case(4)
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                      "InternGrid structure User", &
!                                      ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      case default
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
!                                      "Invalid InternGrid structure", &
!                                      ESMF_CONTEXT, rc))
!        return
!      end select
!
!      if (ESMF_LogMsgFoundError(localrc, &
!                                ESMF_ERR_PASSTHRU, &
!                                ESMF_CONTEXT, rc)) return
!
!      if (present(rc)) rc = ESMF_SUCCESS
!
!      end subroutine ESMF_InternGridSearchPoint
!
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSerialize"

!BOPI
! !IROUTINE: ESMF_InternGridSerialize - Serialize interngrid info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_InternGridSerialize(interngrid, buffer, length, offset, rc) 
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(inout) :: interngrid 
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_InternGrid} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_InternGridWrite()} and {\tt ESMF\_InternGridRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [interngrid]
!           {\tt ESMF\_InternGrid} object to be serialized.
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
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                     ! Error status
      integer :: i
      type(ESMF_DELayout) :: delayout
      type(ESMF_InternGridClass), pointer :: gp    ! interngrid class

      ! shortcut to internals
      gp => interngrid%ptr


      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)


      call c_ESMC_BaseSerialize(gp%base, buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! serialize the interngrid derived type 
      call c_ESMC_InternGridSerialize(gp%dimCount,        gp%interngridStructure, &
                                gp%horzInternGridType,    gp%vertInternGridType, &
                                gp%horzStagger,     gp%vertStagger, &
                                gp%interngridStorage, &
                                gp%horzCoordSystem, gp%vertCoordSystem, &
                                gp%coordOrder,      gp%coordIndex, &
                                gp%periodic(1), &
                                gp%minGlobalCoordPerDim(1), &
                                gp%maxGlobalCoordPerDim(1), &
                                buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      do i = 1,gp%dimCount
        call c_ESMC_StringSerialize(gp%dimNames(i), buffer(1), length, offset, localrc)
        call c_ESMC_StringSerialize(gp%dimUnits(i), buffer(1), length, offset, localrc)
      enddo

      ! serialize the interngrid specific information
      select case(gp%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridSerialize(interngrid%ptr, buffer, length, offset, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select
      ! check local error code from the above case statement
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! serialize the delayout
      call ESMF_InternGridGet(interngrid, delayout=delayout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_DELayoutSerialize(delayout, buffer, length, offset, &
                                  localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! serialize the decomposition information
      call ESMF_InternDGSerialize(gp%interndgs(1), buffer, length, offset, &
                                  localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridDeserialize"

!BOPI
! !IROUTINE: ESMF_InternGridDeserialize - Deserialize a byte stream into a InternGrid
!
! !INTERFACE:
      function ESMF_InternGridDeserialize(vm, buffer, offset, rc) 
!
! !RETURN VALUE:
      type(ESMF_InternGrid) :: ESMF_InternGridDeserialize   
!
! !ARGUMENTS:
      type(ESMF_VM) :: vm
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a InternGrid object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_InternGridWrite()} and {\tt ESMF\_InternGridRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [vm]
!           Current VM into which this object should be deserialized.
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

      integer :: localrc, status             ! Error status, allocation status
      integer :: i, j, n
      integer :: oldDimCount, npets, npets2
      integer :: newDEId, oldDEId, petId
      integer :: petMatchCount, petMatchList(1)
      integer, dimension(:), allocatable :: decompIDs, newNDEs, oldNDEs
      integer, dimension(:), allocatable :: newCountPerDE1, newCountPerDE2
      integer, dimension(:), allocatable :: oldCountPerDE1, oldCountPerDE2
      integer, dimension(:), allocatable :: petList, petListHelper, petTrack
      type(ESMF_DELayout) :: newDELayout, oldDELayout
      type(ESMF_InternGridClass), pointer :: gp


      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

      ! in case of error, make sure this is invalid.
      nullify(ESMF_InternGridDeserialize%ptr)

      ! shortcut to internals
      allocate(gp, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, &
                                     "space for new InternGrid object", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_InternGridConstructNew(gp, "dummy", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call ESMF_BaseCreate(gp%base, "InternGrid", "dummy", 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! this overwrites the name and adds attributes to the base obj.
      call c_ESMC_BaseDeserialize(gp%base, buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_InternGridDeserialize(gp%dimCount,        gp%interngridStructure, &
                                  gp%horzInternGridType,    gp%vertInternGridType, &
                                  gp%horzStagger,     gp%vertStagger, &
                                  gp%interngridStorage, &
                                  gp%horzCoordSystem, gp%vertCoordSystem, &
                                  gp%coordOrder,      gp%coordIndex, &
                                  gp%periodic(1), &
                                  gp%minGlobalCoordPerDim(1), &
                                  gp%maxGlobalCoordPerDim(1), &
                                  buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      do i = 1,gp%dimCount
        call c_ESMC_StringDeserialize(gp%dimNames(i), buffer(1), offset, localrc)
        call c_ESMC_StringDeserialize(gp%dimUnits(i), buffer(1), offset, localrc)
      enddo

      select case(gp%interngridStructure%interngridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown interngrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRInternGridDeserialize(gp, buffer, offset, localrc)

      !-------------
      ! ESMF_IGRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "InternGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid InternGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      ! TODO: call the appropriate interngrid create function?  is this necessary?
      ! set the interngrid status
      gp%interngridStatus = ESMF_IGRID_STATUS_INIT
      ESMF_InternGridDeserialize%ptr => gp

      ! turn on created flag
      ESMF_INIT_SET_CREATED(ESMF_InternGridDeserialize)

      ! deserialize the old delayout
      oldDELayout = ESMF_DELayoutDeserialize(buffer, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Query the old delayout and allocate arrays based on its information
      ! For now, allocate everything as 2 regardless of the rank of the layout.
      ! If the layout is rank 1, it does not change anything if the second
      ! dimension is length 1 by default.
      call ESMF_DELayoutGetDeprecated(oldDELayout, dimCount=oldDimCount, rc=localrc)
      allocate(decompIDs(2), &
                 newNDEs(2), &
                 oldNDEs(2), stat=localrc)
      oldNDEs = 1
      call ESMF_DELayoutGetDeprecated(oldDELayout, deCountPerDim=oldNDEs(1:oldDimCount), &
                            rc=localrc)
      allocate(oldCountPerDE1(oldNDEs(1)))
      allocate(oldCountPerDE2(oldNDEs(2)))

      ! deserialize the decomposition information
      call ESMF_InternDGDeserialize(buffer, offset, decompIDs, oldCountPerDE1, &
                                    oldCountPerDE2, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! create a new DELayout from the attached VM, using information from the 
      ! old one
      ! first, figure out the size of the layout.  the new layout has to be the same
      ! size in the second direction as the old one in order to correctly spawn the
      ! deserialized interngrids
      call ESMF_VMGet(vm, petCount=npets, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      newNDEs(2) = oldNDEs(2)
      newNDEs(1) = (npets+newNDEs(2)-1)/newNDEs(2)   
                                      ! TODO: this only works for 1-1
                                      !       should figure out the number
                                      !       of DEs for the new layout based
                                      !       in part on the number of DEs per
                                      !       pet from the old one
      npets2 = newNDEs(1)*newNDEs(2)  ! in case the division is not even

      ! now allocate a petlist and more for the new delayout
      allocate(       petlist(0:npets2-1), &
                      petListHelper(1:npets2), &
                      petTrack(0:npets-1), &
               newCountPerDE1(newNDEs(1)), &
               newCountPerDE2(newNDEs(2)), stat=localrc)
      petlist        = 0
      petTrack       = 1

      ! set default petlist 
      do j   = 1,newNDEs(2)
        do i = 1,newNDEs(1)
          newDEId = (j-1)*newNDEs(1) + i - 1
          petlist(newDEId) = newDEId
        enddo
      enddo

      ! figure out where the DEs from the old delayout need to be in the new one
      ! loop over the DEs from the old one
      do j   = 1,oldNDEs(2)
        do i = 1,oldNDEs(1)
          oldDEId = (j-1)*oldNDEs(1) + i - 1
          newDEId = (j-1)*newNDEs(1) + i - 1
          call ESMF_DELayoutGetDEMatchPET(oldDELayout, oldDEId, vm, &
                                          petMatchCount, petMatchList, rc)
          petId            = petMatchList(1)
          petlist(newDEId) = petId
          petTrack(petId)  = 0
        enddo
      enddo

      ! fill in the petlist with the remaining petIds
      do j   = 1,newNDEs(2)
        do i = oldNDEs(1)+1,newNDEs(1)
          newDEId = (j-1)*newNDEs(1) + i - 1
          do n = 0,npets-1
            if (petTrack(n).ne.0) then
              petId       = n
              petTrack(n) = 0
              exit
            endif
          enddo
          petlist(newDEId) = petId
        enddo
      enddo
      
      do i = 1, npets2
        petListHelper(i) = petlist(i-1)
      enddo

      newCountPerDE1 = 0
      newCountPerDE2 = 0
      do i = 1,oldNDEs(1)
        newCountPerDE1(i) = oldCountPerDE1(i)
      enddo
      do j = 1,oldNDEs(2)
        newCountPerDE2(j) = oldCountPerDE2(j)
      enddo

      ! check for errors -- did all the pets get used?
  !    if (petTrack.ne.0) then
  !    endif

      ! create new delayout from the vm and petlist
      ! the use of petListHelper is a work-around for PGI 5.x issue with
      ! assumed-shape arrays *gjt*
      newDELayout = ESMF_DELayoutCreate(vm, deCountList=newNDEs, &
        petList=petListHelper, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! call distribute function
      call ESMF_InternGridDistribute(ESMF_InternGridDeserialize, newDELayout, &
                               countsPerDEDim1=newCountPerDE1, &
                               countsPerDEDim2=newCountPerDE2, &
                               decompIds=decompIds, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! clean up
      deallocate(     decompIDs, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(       newNDEs, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(       oldNDEs, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(oldCountPerDE1, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(oldCountPerDE2, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(       petlist, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(       petListHelper, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(      petTrack, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(newCountPerDE1, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(newCountPerDE2, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return

      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_InternGridDeserialize
!------------------------------------------------------------------------------


      end module ESMF_InternGridMod

