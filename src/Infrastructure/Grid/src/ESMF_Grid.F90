! $Id: ESMF_Grid.F90,v 1.190 2004/08/17 23:18:30 jwolfe Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_Grid.F90"
!
!     ESMF Grid Module
      module ESMF_GridMod
!
!==============================================================================
!
! This file contains the Grid class definition and all Grid class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_GridMod - Grid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_Grid} class.  This class
! provides a unified interface for both {\tt ESMF\_PhysGrid} and 
! {\tt ESMF\_DistGrid} information for model grids.  
! Functions for defining and computing {\tt ESMF\_Grid}
! information are available through this class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseTypesMod    ! ESMF base class
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_ArrayDataMapMod     ! ESMF data map class
      use ESMF_DELayoutMod ! ESMF layout class
      use ESMF_ArrayMod
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_PhysCoordMod   ! ESMF physical coord class
      use ESMF_PhysGridMod    ! ESMF physical grid class
      use ESMF_GridTypesMod   ! ESMF basic grid types and primitives
      use ESMF_LogRectGridMod ! ESMF logically rectangular grid routines
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

    public ESMF_GridAddVertHeight
    public ESMF_GridCreate
    public ESMF_GridDestroy
    public ESMF_GridDistribute
    public ESMF_GridGet
    public ESMF_GridGetAttribute
    public ESMF_GridGetAttributeCount
    public ESMF_GridGetAttributeInfo
    public ESMF_GridGetCoord
    public ESMF_GridGetDELocalInfo
    !public ESMF_GridGetMask
    !public ESMF_GridGetMetric
    public ESMF_GridGlobalToDELocalIndex
    public ESMF_GridDELocalToGlobalIndex
    public ESMF_GridPrint
    public ESMF_GridSet
    public ESMF_GridSetAttribute
    public ESMF_GridSetCoord
    public ESMF_GridSetMask
    public ESMF_GridSetMetric
    public ESMF_GridValidate
    public ESMF_GridBoxIntersectRecv
    public ESMF_GridBoxIntersectSend
    public ESMF_GridComputeDistance
    public ESMF_GridGetAllAxisIndex
    public ESMF_GridGetCellMask
    public ESMF_GridGetDELocalAI
    public ESMF_GridGlobalToDELocalAI
    public ESMF_GridDELocalToGlobalAI
    !public ESMF_GridSearch

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.190 2004/08/17 23:18:30 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_GridCreate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridCreateEmpty
         module procedure ESMF_GridCreateRead
         module procedure ESMF_GridCreateCopy
         module procedure ESMF_GridCreateCutout
         module procedure ESMF_GridCreateDiffRes
         module procedure ESMF_GridCreateExchange

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Grid} create
!     methods.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGet - Grid Get routines
!
! !INTERFACE:
      interface ESMF_GridGet

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_GridGetGeneral
        module procedure ESMF_GridGetWithRelloc

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     a variety of information about an {\tt ESMF\_Grid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetAttribute  - Get Grid attributes
!
! !INTERFACE:
      interface ESMF_GridGetAttribute

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_GridGetInt4Attr
        module procedure ESMF_GridGetInt4ListAttr
        module procedure ESMF_GridGetInt8Attr
        module procedure ESMF_GridGetInt8ListAttr
        module procedure ESMF_GridGetReal4Attr
        module procedure ESMF_GridGetReal4ListAttr
        module procedure ESMF_GridGetReal8Attr
        module procedure ESMF_GridGetReal8ListAttr
        module procedure ESMF_GridGetLogicalAttr
        module procedure ESMF_GridGetLogicalListAttr
        module procedure ESMF_GridGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_Grid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetAttributeInfo - Get type, count from a Grid attribute
!     
! !INTERFACE:
      interface ESMF_GridGetAttributeInfo

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_GridGetAttrInfoByName
        module procedure ESMF_GridGetAttrInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_Grid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridSetAttribute  - Set Grid attributes
!
! !INTERFACE:
      interface ESMF_GridSetAttribute

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_GridSetInt4Attr
        module procedure ESMF_GridSetInt4ListAttr
        module procedure ESMF_GridSetInt8Attr
        module procedure ESMF_GridSetInt8ListAttr
        module procedure ESMF_GridSetReal4Attr
        module procedure ESMF_GridSetReal4ListAttr
        module procedure ESMF_GridSetReal8Attr
        module procedure ESMF_GridSetReal8ListAttr
        module procedure ESMF_GridSetLogicalAttr
        module procedure ESMF_GridSetLogicalListAttr
        module procedure ESMF_GridSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Grid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------

!BOPI
! !INTERFACE:
      interface ESMF_GridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetCoordFromArray
         module procedure ESMF_GridSetCoordFromBuffer
         module procedure ESMF_GridSetCoordCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     coordinates as part of an {\tt ESMF\_Grid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_GridSetMask

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetMaskFromArray
         module procedure ESMF_GridSetMaskFromBuffer
         module procedure ESMF_GridSetMaskCopy
         module procedure ESMF_GridSetMaskFromMask

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     logical masks as part of an {\tt ESMF\_Grid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_GridSetMetric

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetMetricFromArray
         module procedure ESMF_GridSetMetricFromBuffer
         module procedure ESMF_GridSetMetricCompute
         module procedure ESMF_GridSetMetricCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     metrics as part of an {\tt Grid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_GridSearch
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_GridSearchPoint
!         module procedure ESMF_GridSearchList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     search an {\tt ESMF\_Grid} for point(s).
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
#define ESMF_METHOD "ESMF_GridAddVertHeight"
!BOP
! !IROUTINE: ESMF_GridAddVertHeight - Add a vertical dimension to an existing Grid

! !INTERFACE:
      subroutine ESMF_GridAddVertHeight(grid, delta, coord, vertstagger, &
                                        dimName, dimUnit, name, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord
      type(ESMF_GridVertStagger), intent(in), optional :: vertstagger
      character(len=*), intent(in), optional :: dimName
      character(len=*), intent(in), optional :: dimUnit
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine adds a vertical subGrid to an already 
!     allocated {\tt grid}.
!     This explicit interface only creates vertical subGrids with coordinate
!     systems where the zero point is defined at the bottom.
!     Only one vertical subGrid is allowed for any {\tt ESMF\_Grid}, 
!     so if a vertical subGrid
!     already exists for the Grid that is passed in, an error is returned.
!     This routine generates {\tt ESMF\_Grid} coordinates from either of two
!     optional sets of arguments:
!     \begin{enumerate}
!     \item given array of deltas (variable delta), assuming 0 is 
!        the minimum or starting coordinate
!     \item given array of coordinates (variable coords)
!     \end{enumerate}
!     If neither of these sets of arguments is present and valid, an error
!     message is issued and an error code returned.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to add vertical grid to.
!     \item[{[delta]}]
!          Array of physical increments in the vertical direction.
!     \item[{[coord]}]
!          Array of physical coordinates in the vertical direction.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_GridVertStagger} specifier to denote vertical grid stagger.
!     \item[{[dimName]}]
!          Dimension name.
!     \item[{[dimUnit]}]
!          Dimension unit.
!     \item[{[name]}]
!          Name for the vertical grid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status
      logical :: dummy
      real(ESMF_KIND_R8) :: minGlobalCoord
      type(ESMF_GridVertType) :: vertGridType
      type(ESMF_CoordSystem) :: vertCoordSystem

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Set default values
      vertGridType    = ESMF_GRID_VERT_TYPE_HEIGHT
      vertCoordSystem = ESMF_COORD_SYSTEM_HEIGHT
      minGlobalCoord  = 0.0d0

      ! Call GridAddVert routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      !  ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "GridStructureUnknown not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridAddVert(grid%ptr, minGlobalCoord, delta, coord, &
                                vertGridType, vertstagger, &
                                vertCoordSystem, dimName, dimUnit, &
                                name, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "GridStructureLogRectBlock not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "GridStructureUnstruct not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "GridStructureUser not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      case default
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                      "Invalid grid structure", &
                                      ESMF_CONTEXT, rc)
        return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddVertHeight

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateEmpty"
!BOP
! !IROUTINE: ESMF_GridCreate - Create a new Grid with no contents

! !INTERFACE:
      ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateEmpty(name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateEmpty
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object and constructs its
!     internals, but does not fill in any contents.  Returns a pointer to
!     the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      type(ESMF_GridClass), pointer :: grid       ! Pointer to new grid
      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateEmpty%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      allocate(grid, stat=localrc)
      ! If error write message and return.
      if (ESMF_LogMsgFoundAllocError(localrc, "Grid type", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_GridCreateEmpty%ptr => grid
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GridCreateEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateRead"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a new Grid by reading in from a file

! !INTERFACE:
      ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateRead(gridStructure, iospec, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateRead
!
! !ARGUMENTS:
      integer, intent(in) :: gridStructure
      type(ESMF_IOSpec), intent(in) :: iospec   ! file specs
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and reads an {\tt ESMF\_Grid} in from a file.  Return a pointer to
!     the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridStructure]
!          Grid structure specification.
!     \item[iospec]
!          File I/O specification.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_GridCreateRead%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridCreateRead routines based on GridStructure

      select case(gridStructure)

      !-------------
      !  ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_GridCreateRead = ESMF_LRGridCreateRead(iospec, name, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GridCreateRead

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateCopy"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a new Grid by copying another Grid

! !INTERFACE:
      ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateCopy(gridIn, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateCopy
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: gridIn
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and copies attributes from another {\tt ESMF\_Grid}.  Return a
!     pointer to the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridIn]
!          {\tt ESMF\_Grid} to be copied.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_GridCreateCopy%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridCreateCopy routines based on GridStructure

      select case(gridIn%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_GridCreateCopy = ESMF_LRGridCreateCopy(gridIn, name, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GridCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateCutout"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a new Grid as a subset of an existing Grid

! !INTERFACE:
      ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateCutout(gridIn, min, max, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateCutout
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: gridIn
      integer, dimension(:), intent(in) :: min
      integer, dimension(:), intent(in) :: max
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and copies a region from an existing {\tt ESMF\_Grid}.
!     Return a pointer to the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridIn]
!          {\tt ESMF\_Grid} to be partially copied.
!     \item[min]
!          Minimum global indices for the region of the grid to be cutout.
!     \item[max]
!          Maximum global indices for the region of the grid to be cutout.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_GridCreateCutout%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridCreateCutout routines based on GridStructure

      select case(gridIn%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_GridCreateCutout = ESMF_LRGridCreateCutout(gridIn, min, max, &
                                                        name, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GridCreateCutout

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateDiffRes"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a new Grid by coarsening or refining an existing Grid

! !INTERFACE:
      ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateDiffRes(gridIn, resolution, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateDiffRes
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: gridIn
      integer, dimension(:), intent(in) :: resolution
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and creates an {\tt ESMF\_Grid} by either coarsening or refining an
!     existing {\tt ESMF\_Grid}.  Return a pointer to the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridIn]
!          Source {\tt ESMF\_Grid} to be coarsened or refined.
!     \item[resolution]
!          Integer resolution factors in each direction.
!          Note:  The above arguments assume refinement by factor if positive
!          and coarsening by absolute value of the factor if negative.  For
!          example, resolution(1)=4 indicates the new {\tt ESMF\_Grid} will be
!          four times as resolved in the first direction as the source
!          {\tt ESMF\_Grid}, whereas resolution(2)=-3 means the new
!          {\tt ESMF\_Grid} will sample every third point in the second 
!          direction.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_GridCreateDiffRes%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridCreateDiffRes routines based on GridStructure

      select case(gridIn%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_GridCreateDiffRes = &
          ESMF_LRGridCreateDiffRes(gridIn, resolution, name, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GridCreateDiffRes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridCreateExchange"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridCreate - Create a new Grid from the intersection of two existing grids

! !INTERFACE:
      ! Private name; call using ESMF_GridCreate()
      function ESMF_GridCreateExchange(gridIn1, gridIn2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateExchange
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: gridIn1
      type(ESMF_Grid), intent(in) :: gridIn2
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and creates a new {\tt ESMF\_Grid} from the intersection of two
!     existing {\tt ESMF\_Grids}.  Return a pointer to the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridIn1]
!          First source {\tt ESMF\_Grid}.
!     \item[gridIn2]
!          Second source {\tt ESMF\_Grid}.
!     \item[{[name]}]
!          New {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_GridCreateExchange%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridCreateExchange routines based on GridStructure

      select case(gridIn1%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        ESMF_GridCreateExchange = ESMF_LRGridCreateExchange(gridIn1, gridIn2, &
                                                            name, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_GridCreateExchange

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDestroy"
!BOP
! !IROUTINE: ESMF_GridDestroy - Free all resources associated with a Grid 

! !INTERFACE:
      subroutine ESMF_GridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys an {\tt ESMF\_Grid} object previously allocated
!     via an {\tt ESMF\_GridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be destroyed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If already destroyed or never created, return ok
      if (.not. associated(grid%ptr)) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Uninitialized or destroyed Grid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call GridDestruct routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        ! TODO: decide if it's ok to create an empty grid and then delete 
        !   it without being created further. (allow it for now)
        !localrc = ESMF_FAILURE
        localrc = ESMF_SUCCESS
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "GridStructureUnknown not supported", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridDestruct(grid%ptr, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      ! If error write message and return.
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! delete the base class
      call ESMF_BaseDestroy(grid%ptr%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! free grid memory.
      deallocate(grid%ptr, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate Grid type", &
                                     ESMF_CONTEXT, rc)) return

      ! so we can detect reuse of a deleted grid object
      nullify(grid%ptr)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDistribute"
!BOP
! !IROUTINE: ESMF_GridDistribute - Distribute a Grid that has already been initialized

! !INTERFACE:
      subroutine ESMF_GridDistribute(grid, delayout, countsPerDEDim1, &
                                     countsPerDEDim2, decompIds, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in), optional :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      integer, dimension(:), intent(in), optional :: decompIds
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the decomposition of the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be distributed.
!     \item[delayout]
!         {\tt ESMF\_DELayout} on which the {\tt grid} is to be decomposed.
!     \item[{[countsPerDEDim1]}]
!          Array denoting the number of grid cells per DE in the first
!          decomposition axis.  By default, the number of grid cells per DE
!          in a decomposition is calculated internally by an algorithm
!          designed to distribute the cells as evenly as possible.
!          This optional argument is available to allow users to instead
!          specify the decompostion of a grid axis by a related delayout
!          axis.  The number of elements in this array must be greater than
!          or equal to the number of DE's along the first axis of the
!          attached {\tt delayout}.  The sum of this array must equal exactly
!          the number of grid cells along related grid axis, which is the
!          first axis by default but can also be set by the [{[decompIds]}]
!          argument in this call.
!     \item[{[countsPerDEDim2]}]
!          Array denoting the number of grid cells per DE in the second
!          decomposition axis.  Please see the description of
!          [{[countsPerDEDim1]}] above for more deatils
!     \item[{[decompIds]}]
!          Integer array of identifiers for which {\tt grid} axes are decomposed.
!          This array describes the relationship between the {\tt grid} and the
!          {\tt delayout}.  The elements of this array contains decompostion
!          information for the corresponding grid axis.  If this element is:
!                  0   the grid axis is not distributed;
!                  1   the grid axis is distributed by the first decomposition
!                      axis in the {\tt delayout];
!                  2   the grid axis is distributed by the second decomposition
!                      axis in the {\tt delayout];
!          The number of array elements should be greater or equal to the number
!          of grid dimensions.  The default is that the first grid axis is
!          distributed by the first decompostion axis, the second grid axis is
!          distributed by the second decomposition axis, and the third grid axis
!          (if applicable) is not distributed.  The relationship between data
!          axes (from an {\tt ESMF\_Field} or {\tt ESMF\_Array}) and {\tt grid}
!          axes are defined elsewhere in {\tt ESMF\_FieldDataMap} and
!          {\tt ESMF\_ArrayDataMap} interfaces.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridDistribute routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridDistribute(grid%ptr, delayout, countsPerDEDim1, &
                                   countsPerDEDim2, decompIds, name, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDistribute

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetGeneral"
!BOP
! !IROUTINE: ESMF_GridGet - Get a variety of general information about a Grid

! !INTERFACE:
      ! Private name; call using ESMF_GridGet()
      subroutine ESMF_GridGetGeneral(grid, &
                                     horzgridtype, vertgridtype, &
                                     horzstagger, vertstagger, &
                                     horzcoordsystem, vertcoordsystem, &
                                     coordorder, dimCount, &
                                     minGlobalCoordPerDim, maxGlobalCoordPerDim, &
                                     periodic, delayout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_GridType),     intent(out), optional :: horzgridtype
      type(ESMF_GridVertType), intent(out), optional :: vertgridtype
      type(ESMF_GridHorzStagger), intent(out), optional :: horzstagger
      type(ESMF_GridVertStagger), intent(out), optional :: vertstagger
      type(ESMF_CoordSystem), intent(out), optional :: horzcoordsystem
      type(ESMF_CoordSystem), intent(out), optional :: vertcoordsystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordorder
      integer, intent(out), optional :: dimCount
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
!     Gets general information about an {\tt ESMF\_Grid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be queried.
!     \item[{[horzgridtype]}]
!          {\tt ESMF\_GridType} specifier to denote horizontal grid type.
!     \item[{[vertgridtype]}]
!          {\tt ESMF\_GridVertType} specifier to denote vertical grid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_GridHorzStagger} specifier to denote horizontal grid stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_GridHorzStagger} specifier to denote vertical grid stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote the default coordinate
!          ordering for the Grid and all related Fields (i.e. KIJ).
!     \item[{[dimCount]}]
!          Number of dimensions represented by this {\tt grid}.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Returns the periodicity along the coordinate axes - logical array.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call GridGet routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      !  ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        ! the only thing that can be retrieved from an empty grid is the name
        if (present(name)) then
          call ESMF_GetName(grid%ptr%base, name, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif 
        if (present(horzgridtype           ) .OR. &
            present(vertgridtype           ) .OR. &
            present(horzstagger            ) .OR. &
            present(vertstagger            ) .OR. &
            present(horzcoordsystem        ) .OR. &
            present(vertcoordsystem        ) .OR. &
            present(coordorder             ) .OR. &
            present(dimCount               ) .OR. &
            present(minGlobalCoordPerDim   ) .OR. &
            present(maxGlobalCoordPerDim   ) .OR. &
            present(periodic               ) .OR. &
            present(delayout               )) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                    "Unknown grid structure", &
                                    ESMF_CONTEXT, rc)) return
        endif

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGet(grid, &
                            horzGridType=horzgridtype, &
                            vertGridType=vertgridtype, &
                            horzStagger=horzstagger, &
                            vertStagger=vertstagger, &
                            horzCoordSystem=horzcoordsystem, &
                            vertCoordSystem=vertcoordsystem, &
                            coordOrder=coordorder, dimCount=dimCount, &
                            minGlobalCoordPerDim=minGlobalCoordPerDim, &
                            maxGlobalCoordPerDim=maxGlobalCoordPerDim, &
                            periodic=periodic, delayout=delayout, &
                            name=name, rc=localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid Grid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetGeneral

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetWithRelloc"
!BOP
! !IROUTINE: ESMF_GridGet - Get a variety of relloc-specified information about a Grid

! !INTERFACE:
      ! Private name; call using ESMF_GridGet()
      subroutine ESMF_GridGetWithRelloc(grid, horzrelloc, vertrelloc, &
                                        horzgridtype, vertgridtype, &
                                        horzstagger, vertstagger, &
                                        horzcoordsystem, vertcoordsystem, &
                                        coordorder, dimCount, &
                                        minGlobalCoordPerDim, &
                                        maxGlobalCoordPerDim, &
                                        globalCellCountPerDim, &
                                        globalStartPerDEPerDim, &
                                        maxLocalCellCountPerDim, &
                                        cellCountPerDEPerDim, periodic, &
                                        delayout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_GridType),     intent(out), optional :: horzgridtype
      type(ESMF_GridVertType), intent(out), optional :: vertgridtype
      type(ESMF_GridHorzStagger), intent(out), optional :: horzstagger
      type(ESMF_GridVertStagger), intent(out), optional :: vertstagger
      type(ESMF_CoordSystem), intent(out), optional :: horzcoordsystem
      type(ESMF_CoordSystem), intent(out), optional :: vertcoordsystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordorder
      integer, intent(out), optional :: dimCount
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
!     Gets information about an {\tt ESMF\_Grid}, depending on user-supplied
!     relative locations, and a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be queried.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
!     \item[{[horzgridtype]}]
!          {\tt ESMF\_GridType} specifier to denote horizontal grid type.
!     \item[{[vertgridtype]}]
!          {\tt ESMF\_GridVertType} specifier to denote vertical grid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_GridHorzStagger} specifier to denote horizontal grid stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_GridHorzStagger} specifier to denote vertical grid stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote the default coordinate
!          ordering for the Grid and all related Fields (i.e. KIJ).
!     \item[{[dimCount]}]
!          Number of dimensions represented by this {\tt grid}.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[globalCellCountPerDim]}]
!          Array of numbers of global grid increments in each direction.
!     \item[{[globalStartPerDEPerDim]}]
!          Array of global starting locations for each DE and in each direction.
!     \item[{[maxLocalCellCountPerDim]}]
!          Array of maximum grid counts on any DE in each direction.
!     \item[{[cellCountPerDEPerDim]}]
!          2-D array of grid counts on each DE and in each direction.
!     \item[{[periodic]}]
!          Returns the periodicity along the coordinate axes - logical array.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call GridGet routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      !  ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        ! the only thing that can be retrieved from an empty grid is the name
        if (present(name)) then
          call ESMF_GetName(grid%ptr%base, name, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif 
        if (present(horzgridtype           ) .OR. &
            present(vertgridtype           ) .OR. &
            present(horzstagger            ) .OR. &
            present(vertstagger            ) .OR. &
            present(horzcoordsystem        ) .OR. &
            present(vertcoordsystem        ) .OR. &
            present(coordorder             ) .OR. &
            present(dimCount               ) .OR. &
            present(minGlobalCoordPerDim   ) .OR. &
            present(maxGlobalCoordPerDim   ) .OR. &
            present(globalCellCountPerDim  ) .OR. &
            present(globalStartPerDEPerDim ) .OR. &
            present(maxLocalCellCountPerDim) .OR. &
            present(cellCountPerDEPerDim   ) .OR. &
            present(periodic               ) .OR. &
            present(delayout               )) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                    "Unknown grid structure", &
                                    ESMF_CONTEXT, rc)) return
        endif

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGet(grid, horzrelloc, vertrelloc, &
                            horzgridtype, vertgridtype, &
                            horzstagger, vertstagger, &
                            horzcoordsystem, vertcoordsystem, &
                            coordorder, dimCount, minGlobalCoordPerDim, &
                            maxGlobalCoordPerDim, globalCellCountPerDim, &
                            globalStartPerDEPerDim, maxLocalCellCountPerDim, &
                            cellCountPerDEPerDim, periodic, delayout, &
                            name, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid Grid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetWithRelloc

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetInt4Attr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetInt4Attr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte integer attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The 4-byte integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetInt4ListAttr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetInt4ListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte integer list attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetInt8Attr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetInt8Attr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte integer attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The 8-byte integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetIntList8Attr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetInt8ListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte integer list attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetReal4Attr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetReal4Attr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The 4-byte real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetReal4ListAttr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetReal4ListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte real list attribute from an {\tt ESMF\_Grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetReal8Attr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetReal8Attr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The 8-byte real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetReal8ListAttr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetReal8ListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte real list attribute from an {\tt ESMF\_Grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLogicalAttr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetLogicalAttr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a logical attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetLogicalListAttr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetLogicalListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a logical list attribute from the {\tt grid}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(grid%ptr%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCharAttr"

!BOP
! !IROUTINE: ESMF_GridGetAttribute - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttribute()
      subroutine ESMF_GridGetCharAttr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a character attribute from the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetChar(grid%ptr%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetAttributeCount"

!BOP
! !IROUTINE: ESMF_GridGetAttributeCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_GridGetAttributeCount(grid, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out) :: count
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns the number of attributes associated with the given {\tt grid} in
!      the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetCount(grid%ptr%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetAttrInfoByName"

!BOP
! !IROUTINE: ESMF_GridGetAttributeInfo - Query Grid attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttributeInfo()
      subroutine ESMF_GridGetAttrInfoByName(grid, name, datatype, &
                                            datakind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character(len=*), intent(in) :: name
      type(ESMF_DataType), intent(out), optional :: datatype
      type(ESMF_DataKind), intent(out), optional :: datakind
      integer, intent(out), optional :: count
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns information associated with the named attribute, 
!      including {\tt datatype}, {\tt datakind} (if applicable),
!      and {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[datatype]}]
!           The data type of the attribute. One of the values
!           {\tt ESMF\_DATA\_INTEGER}, {\tt ESMF\_DATA\_REAL},
!           {\tt ESMF\_DATA\_LOGICAL}, or {\tt ESMF\_DATA\_CHARACTER}.
!     \item [{[datakind]}]
!           The datakind of the attribute, if attribute is type
!           {\tt ESMF\_DATA\_INTEGER} or {\tt ESMF\_DATA\_REAL}.
!           One of the values {\tt ESMF\_I4}, {\tt ESMF\_I8}, {\tt ESMF\_R4},
!           or {\tt ESMF\_R8}.
!           For all other types the value {\tt ESMF\_NOKIND} is returned.
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
      type(ESMF_DataType) :: localDt
      type(ESMF_DataKind) :: localDk
      integer :: localCount
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetAttrInfoName(grid%ptr%base, name, &
                                           localDt, localDk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(datatype)) datatype = localDt
      if (present(datakind)) datakind = localDk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetAttrInfoByNum"

!BOP
! !IROUTINE: ESMF_GridGetAttributeInfo - Query Grid attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_GridGetAttributeInfo()
      subroutine ESMF_GridGetAttrInfoByNum(grid, attributeIndex, name, &
                                           datatype, datakind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_DataType), intent(out), optional :: datatype
      type(ESMF_DataKind), intent(out), optional :: datakind
      integer, intent(out), optional :: count
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute,
!      including {\tt datatype}, {\tt datakind} (if applicable),
!      and item {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[datatype]}]
!           The data type of the attribute. One of the values
!           {\tt ESMF\_DATA\_INTEGER}, {\tt ESMF\_DATA\_REAL},
!           {\tt ESMF\_DATA\_LOGICAL}, or {\tt ESMF\_DATA\_CHARACTER}.
!     \item [{[datakind]}]
!           The datakind of the attribute, if attribute is type
!           {\tt ESMF\_DATA\_INTEGER} or {\tt ESMF\_DATA\_REAL}.
!           One of the values {\tt ESMF\_I4}, {\tt ESMF\_I8}, {\tt ESMF\_R4},
!           or {\tt ESMF\_R8}.
!           For all other types the value {\tt ESMF\_NOKIND} is returned.
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
      type(ESMF_DataType) :: localDt
      type(ESMF_DataKind) :: localDk
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetAttrInfoNum(grid%ptr%base, attributeIndex, &
                                          localName, localDt, localDk, &
                                          localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(datatype)) datatype = localDt
      if (present(datakind)) datakind = localDk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetAttrInfoByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoord"
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get the coordinates of a Grid

! !INTERFACE:
      subroutine ESMF_GridGetCoord(grid, horzrelloc, vertrelloc, centerCoord, &
                                   cornerCoord, faceCoord, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_RelLoc), intent(in), optional :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_Array), intent(out), dimension(:), optional :: centerCoord
      type(ESMF_Array), intent(out), dimension(:), optional :: cornerCoord
      type(ESMF_Array), intent(out), optional :: faceCoord
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns coordinate information for the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be queried.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the {\tt grid} to be
!          queried.
!     \item[{[vertrelloc]}]
!          Vertical relative location of the {\tt grid} to be
!          queried.
!     \item[{[centerCoord]}]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[{[cornerCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout grid.
!     \item[{[faceCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the face index.  Faces should
!          be numbered consistently with corners.  For example, face 1 should
!          correspond to the face between corners 1,2.
!     \item[{[reorder]}]
!          Logical flag.  If TRUE, reorder any results using a previously set
!          CoordOrder before returning.  If FALSE do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available mostly for
!          internal use.
!     \item[{[total]}]
!          Logical. If TRUE, return the total coordinates including internally
!          generated boundary cells. If FALSE return the
!          computational cells (which is what the user will be expecting.)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call GridGetCoord routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGetCoord(grid, horzrelloc, vertrelloc, centerCoord, &
                                 cornerCoord, faceCoord, reorder, total, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCoord

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetDELocalInfo"
!BOP
! !IROUTINE: ESMF_GridGetDELocalInfo - Get local DE information for a Grid

! !INTERFACE:
      subroutine ESMF_GridGetDELocalInfo(grid, horzrelloc, vertrelloc, &
                                myDE, localCellCount, localCellCountPerDim, &
                                minLocalCoordPerDim, maxLocalCoordPerDim, &
                                globalStartPerDim, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      integer, intent(inout), optional :: myDE
      integer, intent(inout), optional :: localCellCount
      integer, dimension(:), intent(inout), optional :: localCellCountPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minLocalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxLocalCoordPerDim
      integer, dimension(:), intent(inout), optional :: globalStartPerDim
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Gets {\tt grid} information for a particular Decomposition Element (DE).
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be queried.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
!     \item[{[myDE]}]
!          Identifier for this {\tt ESMF\_DE}, zero-based.
!     \item[{[localCellCount]}]
!          Local (on this {\tt ESMF\_DE}) number of cells.
!     \item[{[localCellCountPerDim]}]
!          Local (on this {\tt ESMF\_DE}) number of cells per dimension.
!     \item[{[minLocalCoordPerDim]}]
!          Array of minimum local physical coordinates in each dimension.
!     \item[{[maxLocalCoordPerDim]}]
!          Array of maximum local physical coordinates in each dimension.
!     \item[{[globalStartPerDim]}]
!          Global index of starting counts for each dimension.
!     \item[{[reorder]}]
!          Logical flag.  If TRUE, reorder any results using a previously set 
!          CoordOrder before returning.  If FALSE do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available primarily for
!          internal use.
!     \item[{[total]}]
!          Logical flag to indicate getting DistGrid information for total cells.
!          The default is the computational regime.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call GridGetDELocalInfo routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGetDELocalInfo(grid, horzrelloc, vertrelloc, &
                              myDE, localCellCount, localCellCountPerDim, &
                              minLocalCoordPerDim, maxLocalCoordPerDim, &
                              globalStartPerDim, reorder=reorder, &
                              total=total, rc=localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDELocalInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGlobalToDELocalIndex"
!BOP
! !IROUTINE: ESMF_GridGlobalToDELocalIndex - Translate global indexing to DE local

! !INTERFACE:
      subroutine ESMF_GridGlobalToDELocalIndex(grid, horzrelloc, vertrelloc, &
                                               global1D, local1D, &
                                               global2D, local2D, &
                                               dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
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
!     positions of grid elements in distributed (local) arrays and gathered
!     (global) arrays, either by memory location or index pairs.
!     WARNING:  This routine is meant for very limited user access.  It works
!               with Grid indices and will give erroneous results if applied to
!               Field or Array indices.  In the future, this should be a Field
!               method, but in the meantime it will be left available here.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be used.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
!     \item[{[global1D]}]
!          One-dimensional array of global identifiers to be translated.
!          Usage of this optional argument infers translating between positions
!          in memory from a global array to a local (or distributed) one.
!          This array is dimensioned (N), where N is the number of memory
!          locations to be translated.
!     \item[{[local1D]}]
!          One-dimensional array of local identifiers for the return of the
!          translation.  This array must be the same size as [{[global1D]}],
!          and must be present if [{[global1D]}] is present.  If either of
!          these conditions is not met, an error is issued.
!     \item[{[global2D]}]
!          Two-dimensional array of global identifiers to be translated.
!          Usage of this optional argument infers translating between indices
!          in ij space.  This array is assumed to be dimensioned (N,2), where
!          N is the number of index locations to be translated and the second
!          dimension corresponds to the two grid indices that are distributed 
!          (currently any two dimensions of a three-dimensional grid can be
!          distributed).  So to translate three sets of global indices to
!          local indexing,
!                [{[global2D(1,1)]}] = index1(1)
!                [{[global2D(1,2)]}] = index1(2)
!                [{[global2D(2,1)]}] = index2(1)
!                [{[global2D(2,2)]}] = index2(2)
!                [{[global2D(3,1)]}] = index3(1)
!                [{[global2D(3,2)]}] = index3(2)
!     \item[{[local2D]}]
!          Two-dimensional array of local identifiers for the return of the
!          translation.  This array must be the same size as [{[global2D]}],
!          and must be present if [{[global2D]}] is present.  If either of
!          these conditions is not met, an error is issued.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call GridGlobalToDELocalIndex routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown grid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGlobalToDELocalIndex(grid, horzrelloc, vertrelloc, &
                                             global1D, local1D, &
                                             global2D, local2D, &
                                             dimOrder, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid Grid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGlobalToDELocalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDELocalToGlobalIndex"
!BOP
! !IROUTINE: ESMF_GridDELocalToGlobalIndex - Translate DE local indexing to global

! !INTERFACE:
      subroutine ESMF_GridDELocalToGlobalIndex(grid, horzrelloc, vertrelloc, &
                                               local1D, global1D, &
                                               local2D, global2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
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
!     positions of grid elements in distributed (local) arrays and gathered
!     (global) arrays, either by memory location or index pairs.
!     WARNING:  This routine is meant for very limited user access.  It works
!               with Grid indices and will give erroneous results if applied to
!               Field or Array indices.  In the future, this should be a Field
!               method, but in the meantime it will be left available here.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be used.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
!     \item[{[local1D]}]
!          One-dimensional array of global identifiers to be translated.
!          Usage of this optional argument infers translating between positions
!          in memory from a local (or distributed) grid array to a global one.
!          This array is dimensioned (N), where N is the number of memory
!          locations to be translated.
!     \item[{[global1D]}]
!          One-dimensional array of global identifiers for the return of the
!          translation.  This array must be the same size as [{[local1D]}],
!          and must be present if [{[local1D]}] is present.  If either of
!          these conditions is not met, an error is issued.
!     \item[{[local2D]}]
!          Two-dimensional array of local identifiers to be translated.
!          Usage of this optional argument infers translating between indices
!          in ij space.  This array is assumed to be dimensioned (N,2), where
!          N is the number of index locations to be translated and the second
!          dimension corresponds to the two grid indices that are distributed 
!          (currently any two dimensions of a three-dimensional grid can be
!          distributed).  So to translate three sets of local indices to
!          global indexing,
!                [{[local2D(1,1)]}] = index1(1)
!                [{[local2D(1,2)]}] = index1(2)
!                [{[local2D(2,1)]}] = index2(1)
!                [{[local2D(2,2)]}] = index2(2)
!                [{[local2D(3,1)]}] = index3(1)
!                [{[local2D(3,2)]}] = index3(2)
!     \item[{[global2D]}]
!          Two-dimensional array of global identifiers for the return of the
!          translation.  This array must be the same size as [{[local2D]}],
!          and must be present if [{[local2D]}] is present.  If either of
!          these conditions is not met, an error is issued.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check grid status
      if (grid%ptr%gridStatus.eq.ESMF_GRID_STATUS_UNINIT) then
        if (ESMF_LogWrite("trying to query an uninitialized grid", &
                          ESMF_LOG_WARNING, ESMF_CONTEXT)) continue
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call GridDELocalToGlobalIndex routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown grid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridDELocalToGlobalIndex(grid, horzrelloc, vertrelloc, &
                                             local1D, global1D, &
                                             local2D, global2D, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid Grid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDELocalToGlobalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridPrint"
!BOP
! !IROUTINE: ESMF_GridPrint - Print the contents of a Grid

! !INTERFACE:
      subroutine ESMF_GridPrint(grid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid),   intent(in) :: grid
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Prints information about the {\tt grid} to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to print.
!     \item[{[options]}]
!          Print options are not yet supported.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      !character(len=ESMF_MAXSTR) :: name, str
      type(ESMF_GridClass), pointer :: gp
      integer :: i
      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      print *, "********Begin Grid Print:"
      if (.not. associated(grid%ptr)) then
        print *, "Empty or Uninitialized Grid"
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      !TODO: complete prints

      gp => grid%ptr
  !    call ESMF_StatusString(gp%gridStatus, str, rc)
  !    print *, "Grid status = ", trim(str)

      if (gp%gridStatus /= ESMF_GRID_STATUS_READY) then
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      call ESMF_BasePrint(gp%base, "", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! TODO: add calls to //physgrid-Done\\ and distgrid prints

      ! Print the Associated physgrids
      print *, 'PhysGrids associated with this grid:'
      do i=1, gp%numPhysGrids
        call ESMF_PhysGridPrint(gp%physgrids(i), 'no-opt')
      enddo

      ! Print the DistGrid
      print *, 'DistGrids associated with this Grid:'
      do i=1, gp%numDistGrids
        call ESMF_DistGridPrint(gp%distgrids(i), 'no-opt')
      enddo

      print *, "*********End Grid Print"

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSet"
!BOP
! !IROUTINE: ESMF_GridSet - Set a variety of information about a Grid

! !INTERFACE:
      subroutine ESMF_GridSet(grid, horzgridtype, vertgridtype, &
                              horzstagger, vertstagger, &
                              horzcoordsystem, vertcoordsystem, &
                              coordorder, minGlobalCoordPerDim, &
                              maxGlobalCoordPerDim, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_GridType),     intent(in), optional :: horzgridtype
      type(ESMF_GridVertType), intent(in), optional :: vertgridtype
      type(ESMF_GridHorzStagger), intent(in), optional :: horzstagger
      type(ESMF_GridVertStagger), intent(in), optional :: vertstagger
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
!     Sets information for the {\tt grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[{[horzgridType]}]
!          {\tt ESMF\_GridType} specifier to denote horizontal grid type.
!     \item[{[vertgridType]}]
!          {\tt ESMF\_GridVertType} specifier to denote vertical grid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_GridHorzStagger} specifier to denote horizontal grid stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_GridVertStagger} specifier to denote vertical grid stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote the default coordinate
!          ordering for the Grid and all related Fields (i.e. KIJ).
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[name]}]
!          Character string name of {\tt ESMF\_Grid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status
      integer :: i                                ! loop index
      type(ESMF_GridClass), pointer :: gridp      ! Pointer to new grid

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Initialize other variables
      gridp => grid%ptr

      ! if present, set information filling in grid derived type
      if (present(horzgridtype   )) gridp%horzGridType    = horzgridtype
      if (present(vertgridtype   )) gridp%vertGridType    = vertgridtype
      if (present(horzstagger    )) gridp%horzStagger     = horzstagger
      if (present(vertstagger    )) gridp%vertStagger     = vertstagger
      if (present(horzcoordsystem)) gridp%horzCoordSystem = horzcoordsystem
      if (present(vertcoordsystem)) gridp%vertCoordSystem = vertcoordsystem
      if (present(coordorder     )) gridp%coordOrder      = coordorder
      if (present(periodic)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(periodic)) exit
          gridp%periodic(i) = periodic(i)
        enddo
      endif

      if (present(minGlobalCoordPerDim)) then
   !     if (size(minGlobalCoordPerDim) .gt. ESMF_MAXGRIDDIM) exit  ! TODO
        do i=1,size(minGlobalCoordPerDim)
          gridp%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
        enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
   !     if (size(maxGlobalCoordPerDim) .gt. ESMF_MAXGRIDDIM) exit  ! TODO
        do i=1,size(maxGlobalCoordPerDim)
          gridp%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
        enddo
      endif

      if (present(name)) then
          call ESMF_SetName(gridp%base, name, "Grid", localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetInt4Attr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetInt4Attr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the {\tt grid}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The 4-byte integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetInt4ListAttr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetInt4ListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 4-byte integer list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
  
      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I4, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetInt8Attr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetInt8Attr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(inout) :: grid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches an 8-byte integer attribute to the {\tt grid}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The 8-byte integer value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetInt8ListAttr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetInt8ListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 8-byte integer list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
  
      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_INTEGER, ESMF_I8, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetReal4Attr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetReal4Attr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the {\tt grid}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The 4-byte real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetReal4ListAttr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetReal4ListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 4-byte real list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_REAL, ESMF_R4, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetReal8Attr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetReal8Attr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches an 8-byte real attribute to the {\tt grid}.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The 8-byte real value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetReal8ListAttr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetReal8ListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches an 8-byte real list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_REAL, ESMF_R8, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetLogicalAttr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetLogicalAttr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a logical attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The logical true/false value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, 1, &
                                    value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetLogicalListAttr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetLogicalListAttr(grid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a logical list attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: limit
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(grid%ptr%base, name, &
                                    ESMF_DATA_LOGICAL, ESMF_NOKIND, count, &
                                    valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCharAttr"

!BOP
! !IROUTINE: ESMF_GridSetAttribute - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_GridSetAttribute()
      subroutine ESMF_GridSetCharAttr(grid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a character attribute to the {\tt grid}.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [grid]
!           An {\tt ESMF\_Grid} object.
!     \item [name]
!           The name of the attribute to add.
!     \item [value]
!           The character value of the attribute to add.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call c_ESMC_AttributeSetChar(grid%ptr%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCoordCopy"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetCoord - Copy coordinates from one Grid to another

! !INTERFACE:
      ! Private name; call using ESMF_GridSetCoord()
      subroutine ESMF_GridSetCoordCopy(grid, gridIn, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: gridIn
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies the coordinates of an {\tt ESMF\_Grid} from another Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[gridIn]
!          {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item[id]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCoordFromArray"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetCoord - Set the coordinates of a Grid from an existing ESMF array

! !INTERFACE:
      ! Private name; call using ESMF_GridSetCoord()
      subroutine ESMF_GridSetCoordFromArray(grid, array, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the coordinates exist already and are being
!     passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item[{[id]}]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordFromArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetCoordFromBuffer"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetCoord - Set the coordinates of a Grid from an existing data buffer

! !INTERFACE:
      ! Private name; call using ESMF_SetCoord()
      subroutine ESMF_GridSetCoordFromBuffer(Grid, buffer, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real(ESMF_KIND_R8), dimension (:), pointer :: buffer
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the coordinates exist already and are being
!     passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item[id]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordFromBuffer

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetMaskFromArray"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetMaskFromArray - Set a mask in a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetMaskFromArray(grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the logical mask data exists already and is
!     being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item [{[name]}]
!           {\tt LMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetMaskFromArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetMaskFromBuffer"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetMaskFromBuffer - Set a mask in a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetMaskFromBuffer(grid, buffer, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real, dimension (:), pointer :: buffer
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the logical mask data exists already and is
!     being passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt ESMF\_Mask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetMaskFromBuffer

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetMaskCopy"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetMaskCopy - Copies a mask from one grid to another.

! !INTERFACE:
      subroutine ESMF_GridSetMaskCopy(grid, gridIn, name, nameIn, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: gridIn
      character (len=*), intent(in), optional :: name
      character (len=*), intent(in), optional :: nameIn
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a logical mask for an {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[gridIn]
!          {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [{[name]}]
!           {\tt ESMF\_Mask} name to be set.
!     \item [{[nameIn]}]
!           {\tt ESMF\_Mask} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetMaskCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetMaskFromMask"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetMaskFromMask - Set a mask in a Grid from an existing mask

! !INTERFACE:
      subroutine ESMF_GridSetMaskFromMask(grid, mask, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: mask
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes mask data will be created from an existing
!     mask, where the masks are potentially different kinds (i.e. logical or
!     multiplicative.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[mask]
!          Mask identifier to be set from.
!     \item [{[name]}]
!           {\tt ESMF\_Mask} name to be copied into.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetMaskFromMask

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetMetricFromArray"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetMetricFromArray - Set a metric for a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetMetricFromArray(grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the metric data exists already and is being
!     passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item [{[name]}]
!           {\tt ESMF\_Metric} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricFromArray

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetMetricFromBuffer"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetMetricFromBuffer - Set a metric for a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetMetricFromBuffer(grid, buffer, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real, dimension (:), pointer :: buffer
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the metric data exists already and is being
!     passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt ESMF\_Metric} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricFromBuffer

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetMetricCompute"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetMetricCompute - Compute a metric for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetMetricCompute(grid, name, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: id
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes a metric for an {\tt ESMF\_Grid} via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item[id]
!          Identifier for predescribed metrics.  TODO: make list
!     \item [{[name]}]
!           {\tt ESMF\_Metric} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricCompute

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSetMetricCopy"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_GridSetMetricCopy - Copies a metric from one grid to another

! !INTERFACE:
      subroutine ESMF_GridSetMetricCopy(grid, name, gridIn, nameIn, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      type(ESMF_Grid), intent(in) :: gridIn
      character (len=*), intent(in) :: nameIn  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a metric for an {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be modified.
!     \item [name]
!           {\tt ESMF\_Metric} name to be set.
!     \item[gridIn]
!          {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [nameIn]
!           {\tt ESMF\_Metric} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridValidate"
!BOP
! !IROUTINE: ESMF_GridValidate - Check validity of a Grid

! !INTERFACE:
      subroutine ESMF_GridValidate(grid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that an {\tt ESMF\_Grid} is internally consistent.  Currently
!     checks to make sure:
!          the pointer to the grid is associated;
!          the grid status indicates the grid is ready to use.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be validated.
!     \item[{[options]}]
!          Validation options are not yet supported.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      if (.not. associated(grid%ptr)) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized Grid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call validate routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown grid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridValidate(grid, options, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridBoxIntersectRecv"
!BOPI
! !IROUTINE: ESMF_GridBoxIntersectRecv - Determine a DomainList covering a box

! !INTERFACE:
      subroutine ESMF_GridBoxIntersectRecv(grid, &
                                           localMinPerDim, localMaxPerDim, &
                                           domainList, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), dimension(:), intent(in) :: localMinPerDim
                                                         ! array of local mins
      real(ESMF_KIND_R8), dimension(:), intent(in) :: localMaxPerDim
                                                         ! array of local maxs
      type(ESMF_DomainList), intent(inout) :: domainList ! domain list
      logical, intent(in), optional :: total             ! flag to indicate
                                                         ! total cells in the
                                                         ! domainList
      integer, intent(out), optional :: rc               ! return code

! !DESCRIPTION:
!     This routine computes the DomainList necessary to cover a given "box"
!     described by an array of min/max's.  This routine is for the case of
!     a DE that is part of a destination Grid determining which DEs it will
!     receive data from.

!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Source {\tt ESMF\_Grid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[localMinPerDim]
!          Array of local minimum coordinates, one per rank of the array,
!          defining the "box."
!     \item[localMaxPerDim]
!          Array of local maximum coordinates, one per rank of the array,
!          defining the "box."
!     \item[domainList]
!          Resulting {\tt ESMF\_DomainList} containing the set of
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[{[total]}]
!          Logical flag to indicate the domainList should use total cells
!          instead of computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      if (.not. associated(grid%ptr)) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized Grid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call intersect routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown grid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridBoxIntersectRecv(grid, localMinPerDim, localMaxPerDim, &
                                         domainList, total, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridBoxIntersectRecv

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridBoxIntersectSend"
!BOPI
! !IROUTINE: ESMF_GridBoxIntersectSend - Determine a DomainList covering a box

! !INTERFACE:
      subroutine ESMF_GridBoxIntersectSend(dstGrid, srcGrid, &
                                           localMinPerDim, localMaxPerDim, &
                                           myAI, domainList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: dstGrid
      type(ESMF_Grid) :: srcGrid
      real(ESMF_KIND_R8), dimension(:), intent(in) :: localMinPerDim
                                                         ! array of local mins
      real(ESMF_KIND_R8), dimension(:), intent(in) :: localMaxPerDim
                                                         ! array of local maxs
      type(ESMF_AxisIndex), dimension(:), intent(in) :: myAI
      type(ESMF_DomainList), intent(inout) :: domainList ! domain list
      integer, intent(out), optional :: rc               ! return code

! !DESCRIPTION:
!     This routine computes the DomainList necessary to cover a given "box"
!     described by an array of min/max's.  This routine is for the case of
!     a DE that is part of a source Grid determining which DEs it will send
!     its data to.
!
!     The arguments are:
!     \begin{description}
!     \item[dstGrid]
!          Destination {\tt ESMF\_Grid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[srcGrid]
!          Source {\tt ESMF\_Grid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[localMinPerDim]
!          Array of local minimum coordinates, one per rank of the array,
!          defining the "box."
!     \item[localMaxPerDim]
!          Array of local maximum coordinates, one per rank of the array,
!          defining the "box."
!     \item[myAI]
!          {\tt ESMF\_AxisIndex} for this DE on the sending (source)
!          {\tt ESMF\_Grid}, assumed to be in global indexing.
!     \item[domainList]
!          Resulting {\tt ESMF\_DomainList} containing the set of
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      if ((.not. associated(dstGrid%ptr)) .or. &
          (.not. associated(srcGrid%ptr))) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized Grid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call intersect routines based on GridStructure

      select case(srcGrid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown grid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridBoxIntersectSend(dstGrid, srcGrid, &
                                         localMinPerDim, localMaxPerDim, &
                                         myAI, domainList, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridBoxIntersectSend

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridComputeDistance"
!BOPI
! !IROUTINE: ESMF_GridComputeDistance - Compute distance between points
!
! !INTERFACE:
      function ESMF_GridComputeDistance(x1, y1, x2, y2, coordSystem, rc)

! !RETURN VALUE:
      real(ESMF_KIND_R8) :: ESMF_GridComputeDistance

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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!EOPI

      integer :: localrc                          ! local error status
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! branch to appropriate PhysGrid routine to compute distance
      if (coordSystem .eq. ESMF_COORD_SYSTEM_SPHERICAL) then
        ESMF_GridComputeDistance = &
          ESMF_PhysGridCompDistSpherical(x1, y1, x2, y2, rc=localrc)
      elseif (coordSystem .eq. ESMF_COORD_SYSTEM_CARTESIAN) then
        ESMF_GridComputeDistance = &
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

      end function ESMF_GridComputeDistance

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetAllAxisIndex"
!BOPI
! !IROUTINE: ESMF_GridGetAllAxisIndex - Get all axis indices for a Grid

! !INTERFACE:
      subroutine ESMF_GridGetAllAxisIndex(grid, globalAI, horzrelloc, &
                                          vertrelloc, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: globalAI
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[globalAI]
!          Global axis indices on all DE's.
!     \item[{[total]}]
!          Logical flag for whether the axis indices should be for total
!          cells or not.  Default is false, which infers computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridGetAllAxisIndex routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGetAllAxisIndex(grid, globalAI, horzrelloc, vertrelloc, &
                                        total, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetAllAxisIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCellMask"
!BOPI
! !IROUTINE: ESMF_GridGetCellMask - Retrieves cell identifier mask for a Grid

! !INTERFACE:
      subroutine ESMF_GridGetCellMask(grid, maskArray, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_Array), intent(inout) :: maskArray
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of get retrieves an {\tt ESMF\_Array} of cell types for an
!     {\tt ESMF\_Grid} from a corresponding {\tt ESMF\_PhysGrid}.
!     This mask is intended for internal use to indicate which cells are in
!     the computational regime (cellType=0), a ghost region (cellType=1), or a
!     halo region (cellType=2).
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt ESMF\_Grid} to be queried.
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
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridGetCellMask routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      !  ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown grid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGetCellMask(grid, maskArray, relloc, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "Grid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid Grid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCellMask

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetDELocalAI"
!BOPI
! !IROUTINE: ESMF_GridGetDELocalAI - Get local aixs index DE information for a Grid

! !INTERFACE:
      subroutine ESMF_GridGetDELocalAI(grid, AIPerDim, horzrelloc, &
                                       vertrelloc, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AIPerDim
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_DistGrid} attribute with the given value.  Since a single
!     {\tt ESMF\_Grid} can have many {\tt ESMF\_DistGrids}, the correct
!     {\tt ESMF\_DistGrid} must be identified by this calling routine.  For a 3D
!     {\tt ESMF\_Grid}, the user must supply identifiers for both the horizontal
!     and vertical grids if querying for an array of values, like 
!     localCellCountPerDim.  The {\tt ESMF\_DistGrid(s)} are identified
!     using the set of input variables:  horzrelloc and/or vertrelloc.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[AIPerDim]
!          Global axis indices for each dimension.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
!     \item[{[reorder]}]
!          Logical flag.  If TRUE, reorder any results using a previously set
!          CoordOrder before returning.  If FALSE do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available primarily for
!          internal use.
!     \item[{[total]}]
!          Logical flag to indicate getting DistGrid information for total cells.
!          The default is the computational regime.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridGetDELocalInfo routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown grid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGetDELocalInfo(grid, horzrelloc, vertrelloc, &
                                       globalAIPerDim=AIPerDim, &
                                       reorder=reorder, total=total, rc=localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid Grid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDELocalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGlobalToDELocalAI"
!BOPI
! !IROUTINE: ESMF_GridGlobalToDELocalAI - Translate global axis index to DE local

! !INTERFACE:
      subroutine ESMF_GridGlobalToDELocalAI(grid, horzrelloc, vertrelloc, &
                                            globalAI1D, localAI1D, &
                                            globalAI2D, localAI2D, &
                                            dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) :: globalAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) ::  localAI2D
      integer, dimension(:), optional, intent(in) :: dimOrder
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_DistGrid} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
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
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridGlobalToDELocalAI routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown grid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridGlobalToDELocalAI(grid, horzrelloc, vertrelloc, &
                                          globalAI1D, localAI1D, &
                                          globalAI2D, localAI2D, &
                                          dimOrder, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid Grid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGlobalToDELocalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDELocalToGlobalAI"
!BOPI
! !IROUTINE: ESMF_GridDELocalToGlobalAI - Translate DE local axis index to global

! !INTERFACE:
      subroutine ESMF_GridDELocalToGlobalAI(grid, horzrelloc, vertrelloc, &
                                            localAI1D, globalAI1D, &
                                            localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) ::  localAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) :: globalAI2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_DistGrid} routine that translates an array of
!     axis indices from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
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
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridDELocalToGlobalAI routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown grid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT
      case(1)
        call ESMF_LRGridDELocalToGlobalAI(grid, horzrelloc, vertrelloc, &
                                          localAI1D, globalAI1D, &
                                          localAI2D, globalAI2D, localrc)

      !-------------
      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_GRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "Grid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid Grid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDELocalToGlobalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSearchPoint"
!!BOPI
!! !IROUTINE: ESMF_GridSearchPoint - Search the grid for a cell containing point
!
! !INTERFACE:
!      subroutine ESMF_GridSearchPoint(dstAdd, x, y, DEID, searchGrid, &
!                                      physGridID, rc)
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) :: dstAdd       ! location in grid of grid cell
!                                            ! containing search point
!      real (kind=?), intent(in) :: x        ! x coordinates of search point 
!      real (kind=?), intent(in) :: y        ! y coordinates of search point 
!      integer, intent(in) :: DEID           ! DE which owns the search point
!      type(ESMF_Grid), intent(in) :: searchGrid
!                                            ! grid to search for location of point
!      integer, intent(in), optional :: physGridID
!                                            ! id of the subgrid to search
!                                            ! (if more than one subgrid)
!      integer, intent(out), optional :: rc  ! return code
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the grid of a grid cell 
!!     containing the point given by the input x,y coordinates.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dstAdd]
!!          Address of grid cell containing the search point.
!!     \item[x]
!!          X coordinates of search point.
!!     \item[y]
!!          Y coordinates of search point.
!!     \item[DEID]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[searchGrid]
!!          ESMF {\tt ESMF\_Grid} to search for location.
!!     \item[{[physGridID]}]
!!          If more than one {\tt ESMF\_PhysGrid} is contained in 
!!          {\tt ESMF\_Grid}, choose which grid to search (default is 1st
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
!      ! Initialize return code; assume failure until success is certain
!      if (present(rc)) rc = ESMF_FAILURE
!
!!     Call Search routines based on GridStructure
!
!      select case(grid%ptr%gridStructure%gridStructure)
!
!      !-------------
!      ! ESMF_GRID_STRUCTURE_UNKNOWN
!      case(0)
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
!                                      "Unknown grid structure", &
!                                      ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_GRID_STRUCTURE_LOGRECT
!      case(1)
!        call ESMF_LRGridSearchPoint(dstAdd, x, y, DEID, searchGrid, &
!                                    physGridID, localrc)
!
!      !-------------
!      ! ESMF_GRID_STRUCTURE_LOGRECT_BLK
!      case(2)
!        dummy = (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                       "Grid structure Log Rect Block", &
!                                       ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_GRID_STRUCTURE_UNSTRUCT
!      case(3)
!        dummy =  ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                       "Grid structure Unstructured", &
!                                       ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_GRID_STRUCTURE_USER
!      case(4)
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                      "Grid structure User", &
!                                      ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      case default
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
!                                      "Invalid Grid structure", &
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
!      end subroutine ESMF_GridSearchPoint
!
!------------------------------------------------------------------------------

      end module ESMF_GridMod

