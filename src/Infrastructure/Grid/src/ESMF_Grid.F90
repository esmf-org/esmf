! $Id: ESMF_Grid.F90,v 1.172 2004/06/13 23:53:12 cdeluca Exp $
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
    public ESMF_GridGetCoord
    public ESMF_GridGetDE
    !public ESMF_GridGetMask
    !public ESMF_GridGetMetric
    public ESMF_GridGlobalToLocalIndex
    public ESMF_GridLocalToGlobalIndex
    public ESMF_GridPrint
    public ESMF_GridSet
    public ESMF_GridSetCoord
    public ESMF_GridSetMask
    public ESMF_GridSetMetric
    public ESMF_GridValidate
    public ESMF_GridBoxIntersectRecv
    public ESMF_GridBoxIntersectSend
    public ESMF_GridComputeDistance
    public ESMF_GridGetAllAxisIndex
    public ESMF_GridGetCellMask
    !public ESMF_GridSearch

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.172 2004/06/13 23:53:12 cdeluca Exp $'

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
! !INTERFACE:
      interface ESMF_GridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetCoordFromArray
         module procedure ESMF_GridSetCoordFromBuffer
         module procedure ESMF_GridSetCoordCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     coordinates as part of a {\tt ESMF\_Grid}.

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
!     logical masks as part of a {\tt ESMF\_Grid}.

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
!     metrics as part of a {\tt Grid}.

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
!!     search a {\tt ESMF\_Grid} for point(s).
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
      subroutine ESMF_GridAddVertHeight(grid, delta, coord, vertStagger, &
                                        dimName, dimUnit, name, rc)

!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord
      type(ESMF_GridVertStagger), intent(in), optional :: vertStagger
      character(len=*), intent(in), optional :: dimName
      character(len=*), intent(in), optional :: dimUnit
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine adds a vertical subGrid to an already 
!     allocated {\tt ESMF\_Grid}.
!     This explicit interface only creates vertical subGrids with coordinate
!     systems where the zero point is defined at the bottom.
!     Only one vertical subGrid is allowed for any Grid, 
!     so if a vertical subGrid
!     already exists for the Grid that is passed in, an error is returned.
!     This routine generates {\tt ESMF\_Grid} coordinates from either of two
!     optional sets of arguments:
!     \begin{enumerate}
!     \item given array of deltas (variable delta) and assumes 0 is 
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
!     \item[{[vertStagger]}]
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
                                vertGridType, vertStagger, &
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
!     internals, but does not fill in any contents.  Return a pointer to
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

      type(ESMF_GridClass), pointer :: grid        ! Pointer to new grid
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
!     internals, and reads a {\tt ESMF\_Grid} in from a file.  Return a pointer to
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
!     internals, and creates a {\tt ESMF\_Grid} by either coarsening or refining an
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
!     Destroys a {\tt ESMF\_Grid} object previously allocated
!     via an {\tt ESMF\_GridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          The class to be destroyed.
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

      ! free field memory.
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
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be distributed.
!     \item[delayout]
!         {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[countsPerDEDim1]}]
!          Array of number of grid increments per DE in the x-direction.
!     \item[{[countsPerDEDim2]}]
!          Array of number of grid increments per DE in the y-direction.
!     \item[{[decompIds]}]
!          Identifier for which Grid axes are decomposed.
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
#define ESMF_METHOD "ESMF_GridGet"
!BOP
! !IROUTINE: ESMF_GridGet - Get a variety of information about a Grid

! !INTERFACE:
      subroutine ESMF_GridGet(grid, horzRelLoc, vertRelLoc, &
                              horzGridType, vertGridType, &
                              horzStagger, vertStagger, &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder, dimCount, minGlobalCoordPerDim, &
                              maxGlobalCoordPerDim, globalCellCountPerDim, &
                              globalStartPerDEPerDim, maxLocalCellCountPerDim, &
                              cellCountPerDEPerDim, periodic, delayout, &
                              name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      type(ESMF_GridType),     intent(out), optional :: horzGridType
      type(ESMF_GridVertType), intent(out), optional :: vertGridType
      type(ESMF_GridHorzStagger), intent(out), optional :: horzStagger
      type(ESMF_GridVertStagger), intent(out), optional :: vertStagger
      type(ESMF_CoordSystem), intent(out), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(out), optional :: vertCoordSystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordOrder
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
!     This version sets a variety of information about a {\tt ESMF\_Grid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[{[horzRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
!     \item[{[horzGridType]}]
!          Integer specifier to denote horizontal grid type.
!     \item[{[vertGridType]}]
!          Integer specifier to denote vertical grid type.
!     \item[{[horzStagger]}]
!          Integer specifier to denote horizontal grid stagger.
!     \item[{[vertStagger]}]
!          Integer specifier to denote vertical grid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[coordOrder]}]
!          Integer specifier to denote coordinate ordering.
!     \item[{[dimCount]}]
!          Number of dimensions represented by this Grid.
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

      ! Call GridGet routines based on GridStructure

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
        call ESMF_LRGridGet(grid, horzRelLoc, vertRelLoc, &
                            horzGridType, vertGridType, &
                            horzStagger, vertStagger, &
                            horzCoordSystem, vertCoordSystem, &
                            coordOrder, dimCount, minGlobalCoordPerDim, &
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

      end subroutine ESMF_GridGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetCoord"
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get the coordinates of a Grid

! !INTERFACE:
      subroutine ESMF_GridGetCoord(grid, horzRelLoc, vertRelLoc, centerCoord, &
                                   cornerCoord, faceCoord, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      type(ESMF_Array), intent(out), dimension(:), optional :: centerCoord
      type(ESMF_Array), intent(out), dimension(:), optional :: cornerCoord
      type(ESMF_Array), intent(out), optional :: faceCoord
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Determines the appropriate physGrid to query from either a physGridId or
!     relloc and returns the requested information.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be queried.
!     \item[{[horzRelLoc]}]
!          Horizontal relative location of the {\tt ESMF\_PhysGrid} to be
!          queried.
!     \item[{[vertRelLoc]}]
!          Vertical relative location of the {\tt ESMF\_PhysGrid} to be
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
!          Logical.  If TRUE, reorder any results using the GridOrder before
!          returning.  If FALSE do not reorder.  The default value is TRUE
!          and users should not need to reset this for most applications.
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
        call ESMF_LRGridGetCoord(grid, horzRelLoc, vertRelLoc, centerCoord, &
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
#define ESMF_METHOD "ESMF_GridGetDE"
!BOP
! !IROUTINE: ESMF_GridGetDE - Get local DE information for a Grid

! !INTERFACE:
      subroutine ESMF_GridGetDE(grid, horzRelLoc, vertRelLoc, &
                                myDE, localCellCount, localCellCountPerDim, &
                                minLocalCoordPerDim, maxLocalCoordPerDim, &
                                globalStartPerDim, globalAIPerDim, reorder, &
                                total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer, intent(inout), optional :: myDE
      integer, intent(inout), optional :: localCellCount
      integer, dimension(:), intent(inout), optional :: localCellCountPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minLocalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxLocalCoordPerDim
      integer, dimension(:), intent(inout), optional :: globalStartPerDim
      type(ESMF_AxisIndex), dimension(:), intent(inout), &
                        optional :: globalAIPerDim
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.  Since a single
!     {\tt ESMF\_Grid} can have many {\tt ESMF\_DistGrids}, the correct
!     {\tt ESMF\_DistGrid} must be identified by this calling routine.  For a 3D
!     {\tt ESMF\_Grid}, the user must supply identifiers for both the horizontal
!     and vertical grids if querying for an array of values, like 
!     localCellCountPerDim.  The {\tt ESMF\_DistGrid(s)} are identified
!     using the set of input variables:  horzRelLoc and/or vertRelLoc.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[horzRelLoc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertRelLoc]}]
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
!     \item[{[globalAIPerDim]}]
!          Global axis indices for each dimension.
!     \item[{[reorder]}]
!          Logical.  If TRUE, reorder any results using the GridOrder before
!          returning.  If FALSE do not reorder.  The default value is TRUE
!          and users should not need to reset this for most applications.
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

      ! Call GridGetDE routines based on GridStructure

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
        call ESMF_LRGridGetDE(grid, horzRelLoc, vertRelLoc, &
                              myDE, localCellCount, localCellCountPerDim, &
                              minLocalCoordPerDim, maxLocalCoordPerDim, &
                              globalStartPerDim, globalAIPerDim, reorder, &
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

      end subroutine ESMF_GridGetDE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGlobalToLocalIndex"
!BOP
! !IROUTINE: ESMF_GridGlobalToLocalIndex - Translate global indexing to DE local

! !INTERFACE:
      subroutine ESMF_GridGlobalToLocalIndex(grid, horzRelLoc, vertRelLoc, &
                                             global1D, local1D, &
                                             global2D, local2D, &
                                             globalAI1D, localAI1D, &
                                             globalAI2D, localAI2D, &
                                             dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) :: global1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: local1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) :: global2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: local2D
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
!     \item[{[global1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of global identifiers to be
!          translated.  Infers translating between positions in memory.
!     \item[{[local1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of local identifiers
!          corresponding to global identifiers.
!     \item[{[global2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of global identifiers to be
!          translated.  Infers translating between indices in ij space.
!     \item[{[local2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of local identifiers
!          corresponding to global identifiers.
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
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridGlobalToLocalIndex routines based on GridStructure

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
        call ESMF_LRGridGlobalToLocalIndex(grid, horzRelLoc, vertRelLoc, &
                                           global1D, local1D, &
                                           global2D, local2D, &
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

      end subroutine ESMF_GridGlobalToLocalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridLocalToGlobalIndex"
!BOP
! !IROUTINE: ESMF_GridLocalToGlobalIndex - Translate DE local indexing to global

! !INTERFACE:
      subroutine ESMF_GridLocalToGlobalIndex(grid, horzRelLoc, vertRelLoc, &
                                             local1D, global1D, &
                                             local2D, global2D, &
                                             localAI1D, globalAI1D, &
                                             localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) ::  local1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: global1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) ::  local2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: global2D
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) ::  localAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) :: globalAI2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_DistGrid} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be used.
!     \item[{[local1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of local identifiers to be
!          translated.  Infers translating between positions in memory.
!     \item[{[global1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of global identifiers
!          corresponding to local identifiers.
!     \item[{[local2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of local identifiers to be
!          translated.  Infers translating between indices in ij space.
!     \item[{[global2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of global identifiers
!          corresponding to local identifiers.
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
!EOP

      integer :: localrc                          ! local error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Call GridLocalToGlobalIndex routines based on GridStructure

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
        call ESMF_LRGridLocalToGlobalIndex(grid, horzRelLoc, vertRelLoc, &
                                           local1D, global1D, &
                                           local2D, global2D, &
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

      end subroutine ESMF_GridLocalToGlobalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridPrint"
!BOP
! !IROUTINE: ESMF_GridPrint - Print the contents of a Grid

! !INTERFACE:
      subroutine ESMF_GridPrint(grid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Print information about a {\t ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[opt]
!          Print options that control the type of information and level of
!          detail.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      character(len=ESMF_MAXSTR) :: name, str
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
      subroutine ESMF_GridSet(grid, horzGridType, vertGridType, &
                              horzStagger, vertStagger, &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder, minGlobalCoordPerDim, &
                              maxGlobalCoordPerDim, periodic, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
      type(ESMF_GridType),     intent(in), optional :: horzGridType
      type(ESMF_GridVertType), intent(in), optional :: vertGridType
      type(ESMF_GridHorzStagger), intent(in), optional :: horzStagger
      type(ESMF_GridVertStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: maxGlobalCoordPerDim
      type(ESMF_Logical), intent(in), optional :: periodic(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version sets a variety of information about a {\tt ESMF\_Grid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[{[horzGridType]}]
!          Integer specifier to denote horizontal grid type.
!     \item[{[vertGridType]}]
!          Integer specifier to denote vertical grid type.
!     \item[{[horzStagger]}]
!          Integer specifier to denote horizontal grid stagger.
!     \item[{[vertStagger]}]
!          Integer specifier to denote vertical grid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[coordOrder]}]
!          Integer specifier to denote coordinate ordering.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! local error status
      integer :: i                                ! loop index

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! if present, set information filling in grid derived type
      if (present(horzGridType)) grid%horzGridType = horzGridType
      if (present(vertGridType)) grid%vertGridType = vertGridType
      if (present(horzStagger)) grid%horzStagger = horzStagger
      if (present(vertStagger)) grid%vertStagger = vertStagger
      if (present(horzCoordSystem)) grid%horzCoordSystem = horzCoordSystem
      if (present(vertCoordSystem)) grid%vertCoordSystem = vertCoordSystem
      if (present(coordOrder)) grid%coordOrder = coordOrder
      if (present(periodic)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(periodic)) exit
          grid%periodic(i) = periodic(i)
        enddo
      endif

      if (present(minGlobalCoordPerDim)) then
   !     if (size(minGlobalCoordPerDim) .gt. ESMF_MAXGRIDDIM) exit  ! TODO
        do i=1,size(minGlobalCoordPerDim)
          grid%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
        enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
   !     if (size(maxGlobalCoordPerDim) .gt. ESMF_MAXGRIDDIM) exit  ! TODO
        do i=1,size(maxGlobalCoordPerDim)
          grid%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
        enddo
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSet

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
!     This version of set copies the coordinates of a {\tt ESMF\_Grid} from another Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[gridIn]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
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
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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
!     This version of set copies a logical mask for a {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[gridIn]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
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
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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
!     This version of set internally computes a metric for a {\tt ESMF\_Grid} via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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
!     This version of set copies a metric for a {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item [name]
!           {\tt ESMF\_Metric} name to be set.
!     \item[gridIn]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
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
      subroutine ESMF_GridValidate(grid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that a {\tt ESMF\_Grid} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[opt]}]
!          Validation options.
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
        call ESMF_LRGridValidate(grid, opt, localrc)

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
      subroutine ESMF_GridGetAllAxisIndex(grid, globalAI, horzRelLoc, &
                                          vertRelLoc, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: globalAI
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
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
        call ESMF_LRGridGetAllAxisIndex(grid, globalAI, horzRelLoc, vertRelLoc, &
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
!          Pointer to a {\tt ESMF\_Grid} to be modified.
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

















