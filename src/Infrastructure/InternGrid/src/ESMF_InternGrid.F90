! $Id: ESMF_InternGrid.F90,v 1.2 2007/06/23 04:25:45 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_IGrid.F90"
!
!     ESMF IGrid Module
      module ESMF_IGridMod
!
!==============================================================================
!
! This file contains the IGrid class definition and all IGrid class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_IGridMod - IGrid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_IGrid} class.  This class
! provides a unified interface for both {\tt ESMF\_PhysGrid} and 
! {\tt ESMF\_InternDG} information for model IGrids.  
! Functions for defining and computing {\tt ESMF\_IGrid}
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
      use ESMF_InternDGMod    ! ESMF distributed IGrid class
      use ESMF_PhysCoordMod   ! ESMF physical Coord class
      use ESMF_PhysGridMod    ! ESMF physical IGrid class
      use ESMF_IGridTypesMod   ! ESMF basic IGrid types and primitives
      use ESMF_LogRectIGridMod ! ESMF logically rectangular IGrid routines
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

    public ESMF_IGridAddVertHeight
    public ESMF_IGridCreate
    public ESMF_IGridDestroy
    public ESMF_IGridDistribute
    public ESMF_IGridGet
    public ESMF_IGridGetAttribute
    public ESMF_IGridGetAttributeCount
    public ESMF_IGridGetAttributeInfo
    public ESMF_IGridGetCoord
    public ESMF_IGridGetDELocalInfo
    !public ESMF_IGridGetMask
    !public ESMF_IGridGetMetric
    public ESMF_IGridGlobalToDELocalIndex
    public ESMF_IGridDELocalToGlobalIndex
    public ESMF_IGridPrint
    public ESMF_IGridSet
    public ESMF_IGridSetAttribute
    public ESMF_IGridValidate
    public ESMF_IGridBoxIntersectRecv
    public ESMF_IGridBoxIntersectSend
    public ESMF_IGridComputeDistance
    public ESMF_IGridGetAllAxisIndex
    public ESMF_IGridGetAIsAllDEs
    public ESMF_IGridGetCellMask
    public ESMF_IGridGetDELocalAI
    public ESMF_IGridGlobalToDELocalAI
    public ESMF_IGridDELocalToGlobalAI
    !public ESMF_IGridSearch
    public ESMF_IGridSerialize
    public ESMF_IGridDeserialize

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_InternGrid.F90,v 1.2 2007/06/23 04:25:45 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_IGridCreate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_IGridCreateEmpty
         module procedure ESMF_IGridCreateRead
         module procedure ESMF_IGridCreateCopy
         module procedure ESMF_IGridCreateCutout
         module procedure ESMF_IGridCreateDiffRes
         module procedure ESMF_IGridCreateExchange

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_IGrid} create
!     methods.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IGridDistribute - IGrid Distribute routines
!
! !INTERFACE:
      interface ESMF_IGridDistribute

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_IGridDistributeBlock
        module procedure ESMF_IGridDistributeArbitrary

! !DESCRIPTION:
!     This interface provides a single entry point for methods that distribute
!     (or decompose) an {\tt ESMF\_IGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IGridGet - IGrid Get routines
!
! !INTERFACE:
      interface ESMF_IGridGet

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_IGridGetGeneral
        module procedure ESMF_IGridGetWithRelloc

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     a variety of information about an {\tt ESMF\_IGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IGridGetAttribute  - Get IGrid attributes
!
! !INTERFACE:
      interface ESMF_IGridGetAttribute

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_IGridGetInt4Attr
        module procedure ESMF_IGridGetInt4ListAttr
        module procedure ESMF_IGridGetInt8Attr
        module procedure ESMF_IGridGetInt8ListAttr
        module procedure ESMF_IGridGetReal4Attr
        module procedure ESMF_IGridGetReal4ListAttr
        module procedure ESMF_IGridGetReal8Attr
        module procedure ESMF_IGridGetReal8ListAttr
        module procedure ESMF_IGridGetLogicalAttr
        module procedure ESMF_IGridGetLogicalListAttr
        module procedure ESMF_IGridGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_IGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IGridGetAttributeInfo - Get type, count from a IGrid attribute
!     
! !INTERFACE:
      interface ESMF_IGridGetAttributeInfo

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_IGridGetAttrInfoByName
        module procedure ESMF_IGridGetAttrInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_IGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IGridGetCoord - 
!     
! !INTERFACE:
      interface ESMF_IGridGetCoord

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_IGridGetCoord
        module procedure ESMF_IGridGetCoordByDim1D
        module procedure ESMF_IGridGetCoordByDim2D

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about coordinates from an {\tt ESMF\_IGrid}.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IGridSetAttribute  - Set IGrid attributes
!
! !INTERFACE:
      interface ESMF_IGridSetAttribute

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_IGridSetInt4Attr
        module procedure ESMF_IGridSetInt4ListAttr
        module procedure ESMF_IGridSetInt8Attr
        module procedure ESMF_IGridSetInt8ListAttr
        module procedure ESMF_IGridSetReal4Attr
        module procedure ESMF_IGridSetReal4ListAttr
        module procedure ESMF_IGridSetReal8Attr
        module procedure ESMF_IGridSetReal8ListAttr
        module procedure ESMF_IGridSetLogicalAttr
        module procedure ESMF_IGridSetLogicalListAttr
        module procedure ESMF_IGridSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_IGrid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_IGridSearch
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_IGridSearchPoint
!         module procedure ESMF_IGridSearchList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     search an {\tt ESMF\_IGrid} for point(s).
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
#define ESMF_METHOD "ESMF_IGridAddVertHeight"
!BOP
! !IROUTINE: ESMF_IGridAddVertHeight - Add a vertical subIGrid to an existing IGrid

! !INTERFACE:
      subroutine ESMF_IGridAddVertHeight(igrid, delta, coord, vertstagger, &
                                        dimName, dimUnit, name, rc)

!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord
      type(ESMF_IGridVertStagger), intent(in), optional :: vertstagger
      character(len=*), intent(in), optional :: dimName
      character(len=*), intent(in), optional :: dimUnit
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine adds a vertical subIGrid (or subIGrids) to an already
!     allocated IGrid.  The {\tt ESMF\_IGridAddVertHeight} interface only
!     creates vertical subIGrids with coordinate systems where the zero point is
!     defined at the bottom.  An {\tt ESMF\_IGridAddVert<IGridVertType>()} can
!     only be called for any {\tt ESMF\_IGrid} once;  if a vertical subIGrid
!     already exists for the {\tt ESMF\_IGrid} that is passed in, an error
!     is returned.  Please note that this subroutine may create more than one
!     subIGrid because some vertical staggerings infer more than one vertical
!     relative location (for example, {\tt ESMF\_IGRID\_VERT\_STAGGER\_BOTTOM}
!     staggering indicates that some Fields are represented at the vertical cell
!     centers and some at the cell bottom faces).  
!     This routine generates {\tt ESMF\_IGrid} coordinates from either of two
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
!     \item[igrid]
!          Existing {\tt ESMF\_IGrid} the vertical subIGrid(s) is being added to.
!     \item[{[delta]}]
!          Array of physical increments in the vertical direction.
!     \item[{[coord]}]
!          Array of physical coordinates in the vertical direction.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_IGridVertStagger} specifier denoting vertical subIGrid
!          stagger.  The default value is ESMF\_IGRID\_VERT\_STAGGER\_CENTER.
!     \item[{[dimName]}]
!          Dimension name.
!     \item[{[dimUnit]}]
!          Dimension unit.
!     \item[{[name]}]
!          Name for the vertical subIGrid(s).
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status
      logical :: dummy
      real(ESMF_KIND_R8) :: minGlobalCoord
      type(ESMF_IGridVertType) :: vertIGridType
      type(ESMF_CoordSystem) :: vertCoordSystem
      type(ESMF_IGridVertStagger) :: vertStaggerUse

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! Set default values
      vertIGridType    = ESMF_IGRID_VERT_TYPE_HEIGHT
      vertCoordSystem = ESMF_COORD_SYSTEM_HEIGHT
      vertStaggerUse  = ESMF_IGRID_VERT_STAGGER_CENTER
      if (present(vertstagger)) vertstaggerUse = vertStagger
      minGlobalCoord  = 0.0d0

      ! Call IGridAddVert routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "IGridStructureUnknown not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridAddVert(igrid%ptr, minGlobalCoord, delta, coord, &
                                vertIGridType, vertStaggerUse, &
                                vertCoordSystem, dimName, dimUnit, &
                                name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "IGridStructureLogRectBlock not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "IGridStructureUnstruct not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "IGridStructureUser not supported", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      case default
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                      "Invalid igrid structure", &
                                      ESMF_CONTEXT, rc)
        return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridAddVertHeight

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateEmpty"
!BOP
! !IROUTINE: ESMF_IGridCreate - Create a new IGrid with no contents

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreate()
      function ESMF_IGridCreateEmpty(name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateEmpty
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object and constructs its
!     internal derived types, but does not fill in any contents.  Returns a
!     pointer to the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      type(ESMF_IGridClass), pointer :: igrid       ! Pointer to new igrid
      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(igrid)
      nullify(ESMF_IGridCreateEmpty%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      allocate(igrid, stat=localrc)
      ! If error write message and return.
      if (ESMF_LogMsgFoundAllocError(localrc, "IGrid type", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize igrid internals.
      call ESMF_IGridConstructNew(igrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_IGridCreateEmpty%ptr => igrid

      ! set the igrid as valid
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateEmpty)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateRead"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_IGridCreate - Create a new IGrid by reading in from a file

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreate()
      function ESMF_IGridCreateRead(igridStructure, iospec, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateRead
!
! !ARGUMENTS:
      integer, intent(in) :: igridStructure
      type(ESMF_IOSpec), intent(in) :: iospec   ! file specs
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internals, and reads an {\tt ESMF\_IGrid} in from a file.  Return a pointer to
!     the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igridStructure]
!          IGrid structure specification.
!     \item[iospec]
!          File I/O specification.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_IGridCreateRead%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Call IGridCreateRead routines based on IGridStructure

      select case(igridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        ESMF_IGridCreateRead = ESMF_LRIGridCreateRead(iospec, name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! initialize igrid as created
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateRead)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateRead

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateCopy"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_IGridCreate - Create a new IGrid by copying another IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreate()
      function ESMF_IGridCreateCopy(igridIn, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateCopy
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igridIn
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internals, and copies attributes from another {\tt ESMF\_IGrid}.  Return a
!     pointer to the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igridIn]
!          {\tt ESMF\_IGrid} to be copied.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_IGridCreateCopy%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Call IGridCreateCopy routines based on IGridStructure

      select case(igridIn%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        ESMF_IGridCreateCopy = ESMF_LRIGridCreateCopy(igridIn, name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! initialize igrid as created
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateCopy)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateCutout"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_IGridCreate - Create a new IGrid as a subset of an existing IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreate()
      function ESMF_IGridCreateCutout(igridIn, min, max, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateCutout
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igridIn
      integer, dimension(:), intent(in) :: min
      integer, dimension(:), intent(in) :: max
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internals, and copies a region from an existing {\tt ESMF\_IGrid}.
!     Return a pointer to the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igridIn]
!          {\tt ESMF\_IGrid} to be partially copied.
!     \item[min]
!          Minimum global indices for the region of the igrid to be cutout.
!     \item[max]
!          Maximum global indices for the region of the igrid to be cutout.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status


      ! Initialize pointers
      nullify(ESMF_IGridCreateCutout%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igridIn,rc)

      ! Call IGridCreateCutout routines based on IGridStructure

      select case(igridIn%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        ESMF_IGridCreateCutout = ESMF_LRIGridCreateCutout(igridIn, min, max, &
                                                        name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! initialize igrid as created
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateCutout)


      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateCutout

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateDiffRes"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_IGridCreate - Create a new IGrid by coarsening or refining an existing IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreate()
      function ESMF_IGridCreateDiffRes(igridIn, resolution, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateDiffRes
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igridIn
      integer, dimension(:), intent(in) :: resolution
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internals, and creates an {\tt ESMF\_IGrid} by either coarsening or refining an
!     existing {\tt ESMF\_IGrid}.  Return a pointer to the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igridIn]
!          Source {\tt ESMF\_IGrid} to be coarsened or refined.
!     \item[resolution]
!          Integer resolution factors in each direction.
!          Note:  The above arguments assume refinement by factor if positive
!          and coarsening by absolute value of the factor if negative.  For
!          example, resolution(1)=4 indicates the new {\tt ESMF\_IGrid} will be
!          four times as resolved in the first direction as the source
!          {\tt ESMF\_IGrid}, whereas resolution(2)=-3 means the new
!          {\tt ESMF\_IGrid} will sample every third point in the second 
!          direction.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_IGridCreateDiffRes%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igridIn,rc)

      ! Call IGridCreateDiffRes routines based on IGridStructure

      select case(igridIn%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        ESMF_IGridCreateDiffRes = &
          ESMF_LRIGridCreateDiffRes(igridIn, resolution, name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! initialize igrid as created
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateDiffRes)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateDiffRes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateExchange"
! TODO: make BOP when filled
!BOPI
! !IROUTINE: ESMF_IGridCreate - Create a new IGrid from the intersection of two existing igrids

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreate()
      function ESMF_IGridCreateExchange(igridIn1, igridIn2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateExchange
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igridIn1
      type(ESMF_IGrid), intent(in) :: igridIn2
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internals, and creates a new {\tt ESMF\_IGrid} from the intersection of two
!     existing {\tt ESMF\_IGrids}.  Return a pointer to the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igridIn1]
!          First source {\tt ESMF\_IGrid}.
!     \item[igridIn2]
!          Second source {\tt ESMF\_IGrid}.
!     \item[{[name]}]
!          New {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! local error status

      ! Initialize pointers
      nullify(ESMF_IGridCreateExchange%ptr)

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igridIn1,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igridIn2,rc)

      ! Call IGridCreateExchange routines based on IGridStructure

      select case(igridIn1%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        ESMF_IGridCreateExchange = ESMF_LRIGridCreateExchange(igridIn1, igridIn2, &
                                                            name, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! initialize igrid as created
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateExchange)

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateExchange

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridDestroy"
!BOP
! !IROUTINE: ESMF_IGridDestroy - Free all resources associated with a IGrid 

! !INTERFACE:
      subroutine ESMF_IGridDestroy(igrid, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys an {\tt ESMF\_IGrid} object and all related internal structures
!     previously allocated via an {\tt ESMF\_IGridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be destroyed.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! If already destroyed or never created, return ok
      if (.not. associated(igrid%ptr)) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Uninitialized or destroyed IGrid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call IGridDestruct routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        ! allow a user to create an empty igrid and then delete 
        ! it without being created further.
        localrc = ESMF_SUCCESS

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridDestruct(igrid%ptr, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      ! If error write message and return.
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! delete the base class
      call ESMF_BaseDestroy(igrid%ptr%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! free igrid memory.
      deallocate(igrid%ptr, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate IGrid type", &
                                     ESMF_CONTEXT, rc)) return

      ! so we can detect reuse of a deleted igrid object
      nullify(igrid%ptr)

      ! initialize igrid as deleted
      ESMF_INIT_SET_DELETED(igrid)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridDistributeBlock"
!BOP
! !IROUTINE: ESMF_IGridDistribute - Distribute a IGrid with block storage 

! !INTERFACE:
     ! Private name; call using ESMF_IGridDistribute()
      subroutine ESMF_IGridDistributeBlock(igrid, delayout, countsPerDEDim1, &
                                          countsPerDEDim2, decompIds, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in), optional :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      integer, dimension(:), intent(in), optional :: decompIds
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the decomposition of an {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be distributed.
!     \item[delayout]
!         {\tt ESMF\_DELayout} on which {\tt igrid} is to be decomposed.
!     \item[{[countsPerDEDim1]}]
!          Array denoting the number of igrid cells per DE in the first
!          decomposition axis.  By default, the number of igrid cells per DE
!          in a decomposition is calculated internally by an algorithm
!          designed to distribute the cells as evenly as possible.
!          This optional argument is available to allow users to instead
!          specify the decomposition of a IGrid axis by a related
!          DELayout axis.  The number of elements in this array must be
!          greater than or equal to the number of DE's along the first axis of
!          the attached DELayout.  The sum of this array must equal exactly
!          the number of igrid cells along a related IGrid axis, which is the
!          first axis by default but can also be set by the {\tt decompIds}
!          argument in this call.
!     \item[{[countsPerDEDim2]}]
!          Array denoting the number of igrid cells per DE in the second
!          decomposition axis.  Please see the description of
!          {\tt countsPerDEDim1} above for more details
!     \item[{[decompIds]}]
!          Integer array identifying which IGrid axes are decomposed.
!          This array describes the relationship between the IGrid and the
!          DELayout.  The elements of this array contains decompostion
!          information for the corresponding IGrid axis.  The following is a
!          list of valid values and the meaning of each:
!          \begin{description}
!            \item 0 \  the IGrid axis is not distributed;
!            \item 1 \  the IGrid axis is distributed by the first 
!                       decomposition axis in the DELayout;
!            \item 2 \  the IGrid axis is distributed by the second 
!                       decomposition axis in the DELayout.
!          \end{description}
!          The number of array elements should be greater or equal to the number
!          of IGrid dimensions.  The default is that the first IGrid axis is
!          distributed by the first decompostion axis, the second IGrid axis is
!          distributed by the second decomposition axis, and the third IGrid axis
!          (if applicable) is not distributed.  The relationship between data
!          axes (from an {\tt ESMF\_Field} or {\tt ESMF\_Array}) and IGrid
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

      ! Call IGridDistribute routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridDistributeBlock(igrid%ptr, delayout, countsPerDEDim1, &
                                        countsPerDEDim2, decompIds, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridDistributeBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridDistributeArbitrary"
!BOP
! !IROUTINE: ESMF_IGridDistribute - Distribute a IGrid as an arbitrary vector of points

! !INTERFACE:
     ! Private name; call using ESMF_IGridDistribute()
      subroutine ESMF_IGridDistributeArbitrary(igrid, delayout, myCount, &
                                              myIndices, decompIds, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
      type(ESMF_DELayout), intent(in) :: delayout
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, dimension(:), intent(in), optional :: decompIds
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Sets the decomposition of an {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be distributed.
!     \item[delayout]
!         {\tt ESMF\_DELayout} on which {\tt igrid} is to be decomposed.
!     \item[myCount]
!          Number of igrid cells to be distributed to this DE.
!     \item[myIndices]
!          Array of IGrid indices to be distributed to this DE, as (i,j) pairs.
!          The size of this array must be at least {\tt myCount} in the first
!          dimension and 2 in the second.
!     \item[{[decompIds]}]
!          Integer array identifying which IGrid axes are decomposed.
!          This array describes the relationship between the IGrid and the
!          DELayout.  The elements of this array contains decompostion
!          information for the corresponding IGrid axis.  The following is a
!          list of valid values and the meaning of each:
!          \begin{description}
!            \item 0 \  the IGrid axis is not distributed;
!            \item 1 \  the IGrid axis is distributed by the first 
!                       decomposition axis in the DELayout;
!            \item 2 \  the IGrid axis is distributed by the second 
!                       decomposition axis in the DELayout.
!          \end{description}
!          The number of array elements should be greater or equal to the number
!          of IGrid dimensions.  The default is that the first IGrid axis is
!          distributed by the first decomposition axis, the second IGrid axis is
!          distributed by the second decomposition axis, and the third IGrid axis
!          (if applicable) is not distributed.  The relationship between data
!          axes (from an {\tt ESMF\_Field} or {\tt ESMF\_Array}) and IGrid
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

      ! Call IGridDistribute routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridDistributeArbitrary(igrid%ptr, delayout, myCount, &
                                            myIndices, decompIds, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridDistributeArbitrary

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetGeneral"
!BOP
! !IROUTINE: ESMF_IGridGet - Get a variety of general information about a IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridGet()
      subroutine ESMF_IGridGetGeneral(igrid, &
                                     horzigridtype, vertigridtype, &
                                     horzstagger, vertstagger, &
                                     horzcoordsystem, vertcoordsystem, &
                                     coordorder, &
                                     dimCount, distDimCount, igridstorage, &
                                     minGlobalCoordPerDim, maxGlobalCoordPerDim, &
                                     periodic, delayout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid
      type(ESMF_IGridType),     intent(out), optional :: horzigridtype
      type(ESMF_IGridVertType), intent(out), optional :: vertigridtype
      type(ESMF_IGridHorzStagger), intent(out), optional :: horzstagger
      type(ESMF_IGridVertStagger), intent(out), optional :: vertstagger
      type(ESMF_CoordSystem), intent(out), optional :: horzcoordsystem
      type(ESMF_CoordSystem), intent(out), optional :: vertcoordsystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordorder
      integer, intent(out), optional :: dimCount
      integer, intent(out), optional :: distDimCount
      type(ESMF_IGridStorage), intent(out), optional :: igridstorage
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
!     Gets general information about an {\tt ESMF\_IGrid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be queried.
!     \item[{[horzigridtype]}]
!          {\tt ESMF\_IGridType} specifier denoting horizontal IGrid type.
!     \item[{[vertigridtype]}]
!          {\tt ESMF\_IGridVertType} specifier denoting vertical subIGrid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting vertical subIGrid
!          stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal IGrid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical subIGrid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).
!     \item[{[dimCount]}]
!          Number of dimensions represented by this IGrid.
!     \item[{[distDimCount]}]
!          Number of dimensions represented by the distribution of this IGrid.
!          For IGrids distributed arbitrarily, this could be different than the
!          rank of the underlying IGrid.
!     \item[{[igridstorage]}]
!          {\tt ESMF\_IGridStorage} specifier denoting IGrid storage.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical array that returns the periodicity of the coordinate axes.
!     \item[{[delayout]}]
!          {\tt delayout} that this IGrid was distributed over.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif
      
      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call IGridGet routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        ! the only thing that can be retrieved from an empty igrid is the name
        if (present(name)) then
          call ESMF_GetName(igrid%ptr%base, name, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif 
        if (present(horzigridtype           ) .OR. &
            present(vertigridtype           ) .OR. &
            present(horzstagger            ) .OR. &
            present(vertstagger            ) .OR. &
            present(horzcoordsystem        ) .OR. &
            present(vertcoordsystem        ) .OR. &
            present(coordorder             ) .OR. &
            present(dimCount               ) .OR. &
            present(distDimCount           ) .OR. &
            present(igridstorage            ) .OR. &
            present(minGlobalCoordPerDim   ) .OR. &
            present(maxGlobalCoordPerDim   ) .OR. &
            present(periodic               ) .OR. &
            present(delayout               )) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                    "Unknown igrid structure", &
                                    ESMF_CONTEXT, rc)) return
        endif

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGet(igrid, &
                            horzIGridType=horzigridtype, &
                            vertIGridType=vertigridtype, &
                            horzStagger=horzstagger, &
                            vertStagger=vertstagger, &
                            horzCoordSystem=horzcoordsystem, &
                            vertCoordSystem=vertcoordsystem, &
                            coordOrder=coordorder, &
                            dimCount=dimCount, distDimCount=distDimCount, &
                            igridStorage=igridstorage, &
                            minGlobalCoordPerDim=minGlobalCoordPerDim, &
                            maxGlobalCoordPerDim=maxGlobalCoordPerDim, &
                            periodic=periodic, delayout=delayout, &
                            name=name, rc=localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetGeneral

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetWithRelloc"
!BOP
! !IROUTINE: ESMF_IGridGet - Get a variety of relloc-specified information about a IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridGet()
      subroutine ESMF_IGridGetWithRelloc(igrid, horzrelloc, vertrelloc, &
                                        horzigridtype, vertigridtype, &
                                        horzstagger, vertstagger, &
                                        horzcoordsystem, vertcoordsystem, &
                                        coordorder, &
                                        dimCount, distDimCount, igridstorage, &
                                        minGlobalCoordPerDim, &
                                        maxGlobalCoordPerDim, &
                                        globalCellCountPerDim, &
                                        globalStartPerDEPerDim, &
                                        maxLocalCellCountPerDim, &
                                        cellCountPerDEPerDim, periodic, &
                                        delayout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_IGridType),     intent(out), optional :: horzigridtype
      type(ESMF_IGridVertType), intent(out), optional :: vertigridtype
      type(ESMF_IGridHorzStagger), intent(out), optional :: horzstagger
      type(ESMF_IGridVertStagger), intent(out), optional :: vertstagger
      type(ESMF_CoordSystem), intent(out), optional :: horzcoordsystem
      type(ESMF_CoordSystem), intent(out), optional :: vertcoordsystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordorder
      integer, intent(out), optional :: dimCount
      integer, intent(out), optional :: distDimCount
      type(ESMF_IGridStorage), intent(out), optional :: igridstorage
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
!     Gets information about an {\tt ESMF\_IGrid} or specified subIGrid, depending
!     on user-supplied relative locations, and a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be queried.
!     \item[horzrelloc]
!          Horizontal relative location of the subIGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subIGrid to be queried.
!     \item[{[horzigridtype]}]
!          {\tt ESMF\_IGridType} specifier denoting horizontal IGrid type.
!     \item[{[vertigridtype]}]
!          {\tt ESMF\_IGridVertType} specifier denoting vertical subIGrid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting vertical subIGrid
!          stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal igrid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical subIGrid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).
!     \item[{[dimCount]}]
!          Number of dimensions represented by this IGrid.
!     \item[{[distDimCount]}]
!          Number of dimensions represented by the distribution of this IGrid.
!     \item[{[igridstorage]}]
!          {\tt ESMF\_IGridStorage} specifier denoting IGrid storage.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[globalCellCountPerDim]}]
!          Array of numbers of global IGrid increments in each direction.
!     \item[{[globalStartPerDEPerDim]}]
!          Array of global starting locations for each DE and in each direction.
!     \item[{[maxLocalCellCountPerDim]}]
!          Array of maximum number of IGrid cells on any DE in each direction.
!     \item[{[cellCountPerDEPerDim]}]
!          2-D array of number of IGrid cells on each DE and in each direction.
!     \item[{[periodic]}]
!          Logical array that returns the periodicity of the coordinate axes.
!     \item[{[delayout]}]
!          {\tt delayout} that this IGrid was distributed over.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call IGridGet routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        ! the only thing that can be retrieved from an empty igrid is the name
        if (present(name)) then
          call ESMF_GetName(igrid%ptr%base, name, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif 
        if (present(horzigridtype           ) .OR. &
            present(vertigridtype           ) .OR. &
            present(horzstagger            ) .OR. &
            present(vertstagger            ) .OR. &
            present(horzcoordsystem        ) .OR. &
            present(vertcoordsystem        ) .OR. &
            present(coordorder             ) .OR. &
            present(dimCount               ) .OR. &
            present(distDimCount           ) .OR. &
            present(igridstorage            ) .OR. &
            present(minGlobalCoordPerDim   ) .OR. &
            present(maxGlobalCoordPerDim   ) .OR. &
            present(globalCellCountPerDim  ) .OR. &
            present(globalStartPerDEPerDim ) .OR. &
            present(maxLocalCellCountPerDim) .OR. &
            present(cellCountPerDEPerDim   ) .OR. &
            present(periodic               ) .OR. &
            present(delayout               )) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                    "Unknown igrid structure", &
                                    ESMF_CONTEXT, rc)) return
        endif

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGet(igrid, horzrelloc, vertrelloc, &
                            horzigridtype, vertigridtype, &
                            horzstagger, vertstagger, &
                            horzcoordsystem, vertcoordsystem, &
                            coordorder, dimCount, distDimCount, igridstorage, &
                            minGlobalCoordPerDim, maxGlobalCoordPerDim, &
                            globalCellCountPerDim, maxLocalCellCountPerDim, &
                            globalStartPerDEPerDim, cellCountPerDEPerDim, &
                            periodic, delayout=delayout, &
                            name=name, rc=localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetWithRelloc

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_IGridGetAttribute  - Retrieve an attribute
!
! !INTERFACE:
!      subroutine ESMF_IGridGetAttribute(igrid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_IGrid), intent(in) :: igrid
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns an attribute from the {\tt igrid}.
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
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
#define ESMF_METHOD "ESMF_IGridGetInt4Attr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute  - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetInt4Attr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte integer attribute from the IGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetInt4ListAttr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetInt4ListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte integer list attribute from the IGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
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

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetInt8Attr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute  - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetInt8Attr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte integer attribute from the IGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetIntList8Attr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetInt8ListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte integer list attribute from the IGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
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

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetReal4Attr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetReal4Attr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the IGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetReal4ListAttr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetReal4ListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a 4-byte real list attribute from the IGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
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

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetReal8Attr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetReal8Attr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from the IGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetReal8ListAttr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetReal8ListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns an 8-byte real list attribute from the IGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
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

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetLogicalAttr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetLogicalAttr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a logical attribute from the IGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetLogicalListAttr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetLogicalListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a logical list attribute from the IGrid.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
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

      call c_ESMC_AttributeGetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetCharAttr"

!BOPI
! !IROUTINE: ESMF_IGridGetAttribute - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttribute()
      subroutine ESMF_IGridGetCharAttr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a character attribute from the IGrid.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetChar(igrid%ptr%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetAttributeCount"

!BOP
! !IROUTINE: ESMF_IGridGetAttributeCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_IGridGetAttributeCount(igrid, count, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      integer, intent(out) :: count
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns the number of attributes associated with the given IGrid in
!      the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetCount(igrid%ptr%base, count, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetAttrInfoByName"

!BOP
! !IROUTINE: ESMF_IGridGetAttributeInfo - Query IGrid attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttributeInfo()
      subroutine ESMF_IGridGetAttrInfoByName(igrid, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
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
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetAttrInfoName(igrid%ptr%base, name, &
        localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetAttrInfoByNum"

!BOP
! !IROUTINE: ESMF_IGridGetAttributeInfo - Query IGrid attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridGetAttributeInfo()
      subroutine ESMF_IGridGetAttrInfoByNum(igrid, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
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
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      call c_ESMC_AttributeGetAttrInfoNum(igrid%ptr%base, attributeIndex, &
        localName, localTk, localCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetAttrInfoByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetCoord"
!BOP
! !IROUTINE: ESMF_IGridGetCoord - Get the horizontal and/or vertical coordinates of a IGrid

! !INTERFACE:
      subroutine ESMF_IGridGetCoord(igrid, horzrelloc, vertrelloc, centerCoord, &
                                   cornerCoord, faceCoord, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid
      type(ESMF_RelLoc), intent(in), optional :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      type(ESMF_InternArray), intent(out), dimension(:), optional :: centerCoord
      type(ESMF_InternArray), intent(out), dimension(:), optional :: cornerCoord
      type(ESMF_InternArray), intent(out), dimension(:), optional :: faceCoord
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns coordinate information for the {\tt igrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be queried.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subIGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subIGrid to be queried.
!     \item[{[centerCoord]}]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[{[cornerCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout the IGrid.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call IGridGetCoord routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGetCoord(igrid, horzrelloc, vertrelloc, centerCoord, &
                                 cornerCoord, faceCoord, reorder, total, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetCoord
      

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetCoordByDim1D"
!BOP
! !IROUTINE: ESMF_IGridGetCoordByDim1D - Get the horizontal and/or vertical coordinates of a IGrid

! !INTERFACE:
      subroutine ESMF_IGridGetCoordByDim1D(igrid, dim, horzrelloc, vertrelloc, &
        centerCoord, cornerCoord, faceCoord, reorder, total, localCounts, &
        docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid
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
!     Returns coordinate information for the {\tt igrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be queried.
!     \item[dim]
!          dimension to be queried.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subIGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subIGrid to be queried.
!     \item[{[centerCoord]}]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[{[cornerCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout the IGrid.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call IGridGetCoord routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        allocate(localCenterCoord(2))
        allocate(localCornerCoord(2))
        allocate(localFaceCoord(2))
        call ESMF_LRIGridGetCoord(igrid, horzrelloc, vertrelloc, &
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
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetCoordByDim1D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetCoordByDim2D"
!BOP
! !IROUTINE: ESMF_IGridGetCoordByDim2D - Get the horizontal and/or vertical coordinates of a IGrid

! !INTERFACE:
      subroutine ESMF_IGridGetCoordByDim2D(igrid, dim, horzrelloc, vertrelloc, &
        centerCoord, cornerCoord, faceCoord, reorder, total, localCounts, &
        docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid
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
!     Returns coordinate information for the {\tt igrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be queried.
!     \item[dim]
!          dimension to be queried.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subIGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subIGrid to be queried.
!     \item[{[centerCoord]}]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[{[cornerCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout the IGrid.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call IGridGetCoord routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        allocate(localCenterCoord(2))
        allocate(localCornerCoord(2))
        allocate(localFaceCoord(2))
        call ESMF_LRIGridGetCoord(igrid, horzrelloc, vertrelloc, &
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
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetCoordByDim2D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetDELocalInfo"
!BOP
! !IROUTINE: ESMF_IGridGetDELocalInfo - Get DE-local information for a IGrid

! !INTERFACE:
      subroutine ESMF_IGridGetDELocalInfo(igrid, horzrelloc, vertrelloc, &
                                myDE, localCellCount, localCellCountPerDim, &
                                minLocalCoordPerDim, maxLocalCoordPerDim, &
                                globalStartPerDim, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
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
!     Gets IGrid or subIGrid information for a particular Decomposition
!     Element (DE) assigned to this PET.  This routine cannot retrieve
!     information about a DE on an different PET.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be queried.
!     \item[horzrelloc]
!          Horizontal relative location of the subIGrid to be queried.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subIGrid to be queried.
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
!          of IGrid dimensions.
!     \item[{[maxLocalCoordPerDim]}]
!          Array of maximum local coordinate values on this DE in each dimension.
!          The number of array elements should be greater or equal to the number
!          of IGrid dimensions.
!     \item[{[globalStartPerDim]}]
!          Global index of starting counts for each dimension.
!          The number of array elements should be greater or equal to the number
!          of IGrid dimensions.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call IGridGetDELocalInfo routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGetDELocalInfo(igrid, horzrelloc, vertrelloc, &
                              myDE, localCellCount, localCellCountPerDim, &
                              minLocalCoordPerDim, maxLocalCoordPerDim, &
                              globalStartPerDim, reorder=reorder, &
                              total=total, rc=localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetDELocalInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGlobalToDELocalIndex"
!BOP
! !IROUTINE: ESMF_IGridGlobalToDELocalIndex - Translate global indexing to DE-local

! !INTERFACE:
      subroutine ESMF_IGridGlobalToDELocalIndex(igrid, horzrelloc, vertrelloc, &
                                               global1D, local1D, &
                                               global2D, local2D, &
                                               dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
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
!     positions of igrid elements in distributed (DE-local) arrays and gathered
!     (global) arrays, either by memory location or index pairs.
!     WARNING:  This routine is meant for very limited user access.  It works
!               with IGrid indices and will give erroneous results if applied to
!               Field or Array indices.  In the future, this should be a Field
!               method, but in the meantime it will be left available here.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be used.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subIGrid to be used for the
!          translation.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subIGrid to be used for the
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
!          dimension corresponds to the two IGrid indices that are distributed 
!          (currently any two dimensions of a three-dimensional IGrid can be
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call IGridGlobalToDELocalIndex routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGlobalToDELocalIndex(igrid, horzrelloc, vertrelloc, &
                                             global1D, local1D, &
                                             global2D, local2D, &
                                             dimOrder, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGlobalToDELocalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridDELocalToGlobalIndex"
!BOP
! !IROUTINE: ESMF_IGridDELocalToGlobalIndex - Translate DE-local indexing to global

! !INTERFACE:
      subroutine ESMF_IGridDELocalToGlobalIndex(igrid, horzrelloc, vertrelloc, &
                                               local1D, global1D, &
                                               local2D, global2D, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
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
!     positions of igrid elements in distributed (DE-local) arrays and gathered
!     (global) arrays, either by memory location or index pairs.
!     WARNING:  This routine is meant for very limited user access.  It works
!               with IGrid indices and will give erroneous results if applied to
!               Field or Array indices.  In the future, this should be a Field
!               method, but in the meantime it will be left available here.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be used.
!     \item[{[horzrelloc]}]
!          Horizontal relative location of the subIGrid to be used for the
!          translation.
!     \item[[{vertrelloc]}]
!          Vertical relative location of the subIGrid to be used for the
!          translation.
!     \item[{[local1D]}]
!          One-dimensional array of DE-local identifiers to be translated.
!          Usage of this optional argument infers translating between positions
!          in memory from a DE-local (or distributed) IGrid array to a global one.
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
!          dimension corresponds to the two IGrid indices that are distributed 
!          (currently any two dimensions of a three-dimensional IGrid can be
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check if igrid%ptr is associated
      if (.not. associated(igrid%ptr)) then
        call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, &
          "Uninitialized IGrid argument", &
          ESMF_CONTEXT, rc)
        return
      endif

      ! check igrid status
      if (igrid%ptr%igridStatus.eq.ESMF_IGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("trying to query an uninitialized igrid", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      ! Call IGridDELocalToGlobalIndex routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridDELocalToGlobalIndex(igrid, horzrelloc, vertrelloc, &
                                             local1D, global1D, &
                                             local2D, global2D, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridDELocalToGlobalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridPrint"
!BOP
! !IROUTINE: ESMF_IGridPrint - Print the contents of a IGrid

! !INTERFACE:
      subroutine ESMF_IGridPrint(igrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid),   intent(in) :: igrid
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Prints information about the {\tt igrid} to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to print.
!     \item[{[options]}]
!          Print options are not yet supported.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      !character(len=ESMF_MAXSTR) :: name, str
      type(ESMF_IGridClass), pointer :: gp
      integer :: i
      integer :: localrc                          ! local error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      print *, "********Begin IGrid Print:"
      if (.not. associated(igrid%ptr)) then
        print *, "Empty or Uninitialized IGrid"
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      !TODO: complete prints

      gp => igrid%ptr
  !    call ESMF_StatusString(gp%igridStatus, str, rc)
  !    print *, "IGrid status = ", trim(str)

      if ((gp%igridStatus.ne.ESMF_IGRID_STATUS_READY) .AND. &
          (gp%igridStatus.ne.ESMF_IGRID_STATUS_INIT)) then
        if (present(rc)) rc = ESMF_RC_NOT_IMPL
        return
      endif

      call ESMF_BasePrint(gp%base, "", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! if the igrid has been distributed, then print the associated
      ! physgrids and interndgs
      if (gp%igridStatus.eq.ESMF_IGRID_STATUS_READY) then

        ! Print the Associated physgrids
        print *, 'PhysGrids associated with this igrid:'
        do i=1, gp%numPhysGrids
          call ESMF_PhysGridPrint(gp%physgrids(i), 'no-opt')
        enddo

        ! Print the InternDG
        print *, 'InternDGs associated with this IGrid:'
        do i=1, gp%numInternDGs
          call ESMF_InternDGPrint(gp%interndgs(i), 'no-opt')
        enddo
      endif

      print *, "*********End IGrid Print"

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSet"
!BOP
! !IROUTINE: ESMF_IGridSet - Set a variety of information about a IGrid

! !INTERFACE:
      subroutine ESMF_IGridSet(igrid, horzigridtype, vertigridtype, &
                              horzstagger, vertstagger, &
                              horzcoordsystem, vertcoordsystem, &
                              coordorder, minGlobalCoordPerDim, &
                              maxGlobalCoordPerDim, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
      type(ESMF_IGridType),     intent(in), optional :: horzigridtype
      type(ESMF_IGridVertType), intent(in), optional :: vertigridtype
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzstagger
      type(ESMF_IGridVertStagger), intent(in), optional :: vertstagger
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
!     Sets information for the IGrid that may not have been included at
!     IGrid creation.
!     WARNING:  This routine does not automatically regenerate the IGrid
!               when used to reset its values, some of which may significantly
!               alter the existing IGrid.  Therefore this routine may only
!               be used prior to the {\tt ESMF\_IGridDistribute()} call.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be modified.
!     \item[{[horzigridType]}]
!          {\tt ESMF\_IGridType} specifier denoting horizontal IGrid type.
!     \item[{[vertigridType]}]
!          {\tt ESMF\_IGridVertType} specifier denoting vertical subIGrid type.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_IGridVertStagger} specifier denoting vertical subIGrid
!          stagger.
!     \item[{[horzcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal IGrid.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical subIGrid.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical array that returns the periodicity of the coordinate axes.
!     \item[{[name]}]
!          Character string name of {\tt ESMF\_IGrid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc                          ! local error status
      integer :: i                                ! loop index
      type(ESMF_IGridClass), pointer :: igridp      ! Pointer to new igrid

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! Initialize other variables
      igridp => igrid%ptr

      ! if present, set information filling in igrid derived type
      if (present(horzigridtype   )) igridp%horzIGridType    = horzigridtype
      if (present(vertigridtype   )) igridp%vertIGridType    = vertigridtype
      if (present(horzstagger    )) igridp%horzStagger     = horzstagger
      if (present(vertstagger    )) igridp%vertStagger     = vertstagger
      if (present(horzcoordsystem)) igridp%horzCoordSystem = horzcoordsystem
      if (present(vertcoordsystem)) igridp%vertCoordSystem = vertcoordsystem
      if (present(coordorder     )) igridp%coordOrder      = coordorder
      if (present(periodic)) then
        do i=1,ESMF_MAXIGRIDDIM
          if (i > size(periodic)) exit
          igridp%periodic(i) = periodic(i)
        enddo
      endif

      if (present(minGlobalCoordPerDim)) then
   !     if (size(minGlobalCoordPerDim) .gt. ESMF_MAXIGRIDDIM) exit  ! TODO
        do i=1,size(minGlobalCoordPerDim)
          igridp%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
        enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
   !     if (size(maxGlobalCoordPerDim) .gt. ESMF_MAXIGRIDDIM) exit  ! TODO
        do i=1,size(maxGlobalCoordPerDim)
          igridp%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
        enddo
      endif

      if (present(name)) then
          call ESMF_SetName(igridp%base, name, "IGrid", localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_IGridSetAttribute - Set an attribute
!
! !INTERFACE:
!      subroutine ESMF_IGridSetAttribute(igrid, name, <value argument>, rc)
!
! !ARGUMENTS:
!      type(ESMF_IGrid), intent(inout) :: igrid
!      character (len = *), intent(in) :: name
!      <value argument>, see below for supported values    
!      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt igrid}.
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
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
#define ESMF_METHOD "ESMF_IGridSetInt4Attr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetInt4Attr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the IGrid.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_I4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetInt4ListAttr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetInt4ListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 4-byte integer list attribute to the IGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetInt8Attr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetInt8Attr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches an 8-byte integer attribute to the IGrid.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_I8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetInt8ListAttr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetInt8ListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 8-byte integer list attribute to the IGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetReal4Attr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetReal4Attr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the IGrid.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_R4, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetReal4ListAttr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetReal4ListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a 4-byte real list attribute to the IGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetReal8Attr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetReal8Attr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches an 8-byte real attribute to the IGrid.
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_R8, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetReal8ListAttr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetReal8ListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches an 8-byte real list attribute to the IGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is
!     given by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetLogicalAttr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetLogicalAttr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a logical attribute to the IGrid.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetLogicalListAttr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetLogicalListAttr(igrid, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      integer, intent(in) :: count
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Attaches a logical list attribute to the IGrid.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt valueList} is
!     given by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      limit = size(valueList)
      if (count > limit) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "count longer than valueList", &
                                  ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(igrid%ptr%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetCharAttr"

!BOPI
! !IROUTINE: ESMF_IGridSetAttribute - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_IGridSetAttribute()
      subroutine ESMF_IGridSetCharAttr(igrid, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Attaches a character attribute to the IGrid.
!     The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           An {\tt ESMF\_IGrid} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      call c_ESMC_AttributeSetChar(igrid%ptr%base, name, value, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridValidate"
!BOP
! !IROUTINE: ESMF_IGridValidate - Check validity of a IGrid

! !INTERFACE:
      subroutine ESMF_IGridValidate(igrid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: igrid
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that an {\tt ESMF\_IGrid} is internally consistent.  Currently
!     checks to ensure:
!     \begin{enumerate}
!        \item the pointer to the IGrid is associated; and
!        \item the IGrid status indicates the IGrid is ready to use.
!     \end{enumerate}
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be validated.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      if (.not. associated(igrid%ptr)) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized IGrid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call validate routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown igrid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridValidate(igrid, options, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid IGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridBoxIntersectRecv"
!BOPI
! !IROUTINE: ESMF_IGridBoxIntersectRecv - Determine a DomainList covering a box

! !INTERFACE:
      subroutine ESMF_IGridBoxIntersectRecv(srcIGrid, dstIGrid, parentVM, &
                                           domainList, hasSrcData, hasDstData, &
                                           total, layer, &
                                           srcrelloc, dstrelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: srcIGrid
      type(ESMF_IGrid) :: dstIGrid
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
!     cover the de-local "boxes" of a destination IGrid.  This routine is for the
!     case of a DE that is part of a destination IGrid determining which DEs it
!     will receive data from.  All PETs that are part of either the source or
!     destination DELayouts must call this routine, due to some necessary
!     global communication calls.
!
!     The arguments are:
!     \begin{description}
!     \item[srcIGrid]
!          Source {\tt ESMF\_IGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[dstIGrid]
!          Destination {\tt ESMF\_IGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[parentVM]
!          {\tt ESMF\_VM} covering the union of the source and destination
!          IGrids.
!     \item[domainList]
!          Resulting {\tt ESMF\_DomainList} containing the set of
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[hasSrcData]
!          Logical flag to indicate whether or not the local PET has data
!          on the source IGrid.
!     \item[hasDstData]
!          Logical flag to indicate whether or not the local PET has data
!          on the destination IGrid.
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
!          subIGrid for all searches.  The default is ESMF_CELL_CENTER.
!     \item[{[dstrelloc]}]
!          Optional argument to set the relative location of the destination
!          subIGrid for all searches.  The default is ESMF_CELL_CENTER.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcigrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstigrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVM,rc)


      if ((.not. associated(srcIGrid%ptr)) .OR. &
          (.not. associated(dstIGrid%ptr))) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized IGrid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call intersect routines based on IGridStructure

      select case(srcIGrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown igrid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridBoxIntersectRecv(srcIGrid, dstIGrid, parentVM, &
                                         domainList, hasSrcData, hasDstData, &
                                         total, layer, &
                                         srcrelloc, dstrelloc, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid IGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridBoxIntersectRecv

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridBoxIntersectSend"
!BOPI
! !IROUTINE: ESMF_IGridBoxIntersectSend - Determine a DomainList covering a box

! !INTERFACE:
      subroutine ESMF_IGridBoxIntersectSend(srcIGrid, dstIGrid, domainList, &
                                           total, layer, &
                                           srcrelloc, dstrelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: srcIGrid
      type(ESMF_IGrid) :: dstIGrid
      type(ESMF_DomainList), intent(out) :: domainList !BOB changed this to just out
      logical, intent(in) :: total
      logical, intent(in) :: layer
      type(ESMF_RelLoc), intent(in), optional :: srcrelloc
      type(ESMF_RelLoc), intent(in), optional :: dstrelloc
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     This routine computes the DomainList that must be sent in order to cover
!     the de-local "boxes" of a destination IGrid.  This routine is for the case
!     of a DE that is part of a source IGrid determining which DEs it will send
!     its data to.  This routine should not be called if this PET does not
!     have any source data.
!
!     The arguments are:
!     \begin{description}
!     \item[srcIGrid]
!          Source {\tt ESMF\_IGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[dstIGrid]
!          Destination {\tt ESMF\_IGrid} to use to calculate the resulting
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
!          subIGrid for all searches.  The default is ESMF_CELL_CENTER.
!     \item[{[dstrelloc]}]
!          Optional argument to set the relative location of the destination
!          subIGrid for all searches.  The default is ESMF_CELL_CENTER.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)

      if ((.not. associated(dstIGrid%ptr)) .or. &
          (.not. associated(srcIGrid%ptr))) then
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Empty or Uninitialized IGrid", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Call intersect routines based on IGridStructure

      select case(srcIGrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                      "Unknown igrid structure", &
                                      ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridBoxIntersectSend(srcIGrid, dstIGrid, domainList, &
                                         total, layer, srcrelloc, dstrelloc, &
                                         localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid IGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridBoxIntersectSend

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridComputeDistance"
!BOPI
! !IROUTINE: ESMF_IGridComputeDistance - Compute distance between points
!
! !INTERFACE:
      function ESMF_IGridComputeDistance(x1, y1, x2, y2, coordSystem, rc)

! !RETURN VALUE:
      real(ESMF_KIND_R8) :: ESMF_IGridComputeDistance

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
        ESMF_IGridComputeDistance = &
          ESMF_PhysGridCompDistSpherical(x1, y1, x2, y2, rc=localrc)
      elseif (coordSystem .eq. ESMF_COORD_SYSTEM_CARTESIAN) then
        ESMF_IGridComputeDistance = &
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

      end function ESMF_IGridComputeDistance

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetAllAxisIndex"
!BOPI
! !IROUTINE: ESMF_IGridGetAllAxisIndex - Get all axis indices for a IGrid

! !INTERFACE:
      subroutine ESMF_IGridGetAllAxisIndex(igrid, globalAI, horzrelloc, &
                                          vertrelloc, AICountPerDE, total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
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
!   \item[igrid]
!        IGrid to be queried.
!   \item[globalAI]
!        2D Array of AxisIndex types, must be (number of DEs, igrid rank) long,
!        intent(out) for this routine.
!   \item[horzrelloc]
!        Required for a 2D igrid; controls which of the InternDGs will be
!        used to answer the query.  (e.g. Cell-centered data will return
!        different counts than vertex-based data.)
!   \item[{[vertrelloc]}]
!        Not required by the fortran interface, but required if the IGrid
!        is 3D.
!   \item[{[AICountPerDE]}]
!        Required if the IGrid has an ARBITRARY distribution; ignored if it
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! Call IGridGetAllAxisIndex routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown igrid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGetAllAxisIndex(igrid, globalAI, horzrelloc, vertrelloc, &
                                        AICountPerDE, total, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid IGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetAllAxisIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetAIsAllDEs"
!BOPI
! !IROUTINE: ESMF_IGridGetAIsAllDEs - Get a IGrid's AIs for all DEs

! !INTERFACE:
      subroutine ESMF_IGridGetAIsAllDEs(igrid, localGlobalFlag, &
                                       AIListPerDEPerRank, &
                                       horzRelLoc, vertRelLoc, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
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
!     \item[igrid]
!          Class to be queried.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          igrid.
!     \item[localGlobalFlag]
!          {\tt ESMF\_LocalGlobalFlag] identifier indicating whether the returned
!          array of {\tt ESMF\_AxisIndex} types should be in local or global
!          index space.
!     \item[AIListPerDEPerRank]
!          2D array of {\tt ESMF\_AxisIndex} types containing results.  If
!          allocated, it must be of size (nDEs,igridrank).
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          igrid.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! Call IGridGetAllAxisIndex routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                   "Unknown igrid structure", &
                                   ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGetAIsAllDEs(igrid, localGlobalFlag, AIListPerDEPerRank, &
                                     horzRelLoc, vertRelLoc, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                   "IGrid structure Log Rect Block", &
                                   ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                   "IGrid structure Unstructured", &
                                   ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                   "IGrid structure User", &
                                   ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                   "Invalid IGrid structure", &
                                   ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetAIsAllDEs

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetCellMask"
!BOPI
! !IROUTINE: ESMF_IGridGetCellMask - Retrieves cell identifier mask for a IGrid

! !INTERFACE:
      subroutine ESMF_IGridGetCellMask(igrid, maskArray, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
      type(ESMF_InternArray), intent(out) :: maskArray !BOB switched from inout to out
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of get retrieves an {\tt ESMF\_Array} of cell types for an
!     {\tt ESMF\_IGrid} from a corresponding {\tt ESMF\_PhysGrid}.
!     This mask is intended for internal use to indicate which cells are in
!     the computational regime (cellType=0), a ghost region (cellType=1), or a
!     halo region (cellType=2).
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          {\tt ESMF\_IGrid} to be queried.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! Call IGridGetCellMask routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      !  ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unknown igrid structure", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGetCellMask(igrid, maskArray, relloc, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Log Rect Block", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure Unstructured", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
         if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid structure User", &
                                 ESMF_CONTEXT, rc)) return

      !-------------
      case default
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Invalid IGrid structure", &
                                 ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetCellMask

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetDELocalAI"
!BOPI
! !IROUTINE: ESMF_IGridGetDELocalAI - Get local aixs index DE information for a IGrid

! !INTERFACE:
      subroutine ESMF_IGridGetDELocalAI(igrid, AIPerDim, horzrelloc, &
                                       vertrelloc, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
      type(ESMF_AxisIndex), dimension(:), intent(out) :: AIPerDim
      type(ESMF_RelLoc), intent(in) :: horzrelloc
      type(ESMF_RelLoc), intent(in), optional :: vertrelloc
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt ESMF\_InternDG} attribute with the given value.  Since a single
!     {\tt ESMF\_IGrid} can have many {\tt ESMF\_InternDGs}, the correct
!     {\tt ESMF\_InternDG} must be identified by this calling routine.  For a 3D
!     {\tt ESMF\_IGrid}, the user must supply identifiers for both the horizontal
!     and vertical igrids if querying for an array of values, like 
!     localCellCountPerDim.  The {\tt ESMF\_InternDG(s)} are identified
!     using the set of input variables:  horzrelloc and/or vertrelloc.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          Class to be queried.
!     \item[AIPerDim]
!          Global axis indices for each dimension.
!     \item[horzrelloc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          igrid.
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          igrid.
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! Call IGridGetDELocalInfo routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGetDELocalInfo(igrid, horzrelloc, vertrelloc, &
                                       globalAIPerDim=AIPerDim, &
                                       reorder=reorder, total=total, rc=localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetDELocalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGlobalToDELocalAI"
!BOPI
! !IROUTINE: ESMF_IGridGlobalToDELocalAI - Translate global axis index to DE local

! !INTERFACE:
      subroutine ESMF_IGridGlobalToDELocalAI(igrid, horzrelloc, vertrelloc, &
                                            globalAI1D, localAI1D, &
                                            globalAI2D, localAI2D, &
                                            dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
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
!     \item[igrid]
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

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


      ! Call IGridGlobalToDELocalAI routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridGlobalToDELocalAI(igrid, horzrelloc, vertrelloc, &
                                          globalAI1D, localAI1D, &
                                          globalAI2D, localAI2D, &
                                          dimOrder, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGlobalToDELocalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridDELocalToGlobalAI"
!BOPI
! !IROUTINE: ESMF_IGridDELocalToGlobalAI - Translate DE local axis index to global

! !INTERFACE:
      subroutine ESMF_IGridDELocalToGlobalAI(igrid, horzrelloc, vertrelloc, &
                                            localAI1D, globalAI1D, &
                                            localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
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
!     \item[igrid]
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
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

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

      ! Call IGridDELocalToGlobalAI routines based on IGridStructure

      select case(igrid%ptr%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridDELocalToGlobalAI(igrid, horzrelloc, vertrelloc, &
                                          localAI1D, globalAI1D, &
                                          localAI2D, globalAI2D, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridDELocalToGlobalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSearchPoint"
!!BOPI
!! !IROUTINE: ESMF_IGridSearchPoint - Search the igrid for a cell containing point
!
! !INTERFACE:
!      subroutine ESMF_IGridSearchPoint(dstAdd, x, y, DEID, searchIGrid, &
!                                      physIGridID, rc)
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) :: dstAdd       ! location in igrid of igrid cell
!                                            ! containing search point
!      real (kind=?), intent(in) :: x        ! x coordinates of search point 
!      real (kind=?), intent(in) :: y        ! y coordinates of search point 
!      integer, intent(in) :: DEID           ! DE which owns the search point
!      type(ESMF_IGrid), intent(in) :: searchIGrid
!                                            ! igrid to search for location of point
!      integer, intent(in), optional :: physIGridID
!                                            ! id of the subigrid to search
!                                            ! (if more than one subigrid)
!      integer, intent(out), optional :: rc  ! return code
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the igrid of a igrid cell 
!!     containing the point given by the input x,y coordinates.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dstAdd]
!!          Address of igrid cell containing the search point.
!!     \item[x]
!!          X coordinates of search point.
!!     \item[y]
!!          Y coordinates of search point.
!!     \item[DEID]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[searchIGrid]
!!          ESMF {\tt ESMF\_IGrid} to search for location.
!!     \item[{[physIGridID]}]
!!          If more than one {\tt ESMF\_PhysGrid} is contained in 
!!          {\tt ESMF\_IGrid}, choose which igrid to search (default is 1st
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
!!     Call Search routines based on IGridStructure
!
!      select case(igrid%ptr%igridStructure%igridStructure)
!
!      !-------------
!      ! ESMF_IGRID_STRUCTURE_UNKNOWN
!      case(0)
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
!                                      "Unknown igrid structure", &
!                                      ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_IGRID_STRUCT_LOGRECT
!      case(1)
!        call ESMF_LRIGridSearchPoint(dstAdd, x, y, DEID, searchIGrid, &
!                                    physIGridID, localrc)
!
!      !-------------
!      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
!      case(2)
!        dummy = (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                       "IGrid structure Log Rect Block", &
!                                       ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
!      case(3)
!        dummy =  ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                       "IGrid structure Unstructured", &
!                                       ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      ! ESMF_IGRID_STRUCTURE_USER
!      case(4)
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
!                                      "IGrid structure User", &
!                                      ESMF_CONTEXT, rc))
!        return
!
!      !-------------
!      case default
!        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
!                                      "Invalid IGrid structure", &
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
!      end subroutine ESMF_IGridSearchPoint
!
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSerialize"

!BOPI
! !IROUTINE: ESMF_IGridSerialize - Serialize igrid info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_IGridSerialize(igrid, buffer, length, offset, rc) 
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid 
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_IGrid} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_IGridWrite()} and {\tt ESMF\_IGridRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [igrid]
!           {\tt ESMF\_IGrid} object to be serialized.
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
      type(ESMF_IGridClass), pointer :: gp    ! igrid class

      ! shortcut to internals
      gp => igrid%ptr


      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)


      call c_ESMC_BaseSerialize(gp%base, buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! serialize the igrid derived type 
      call c_ESMC_IGridSerialize(gp%dimCount,        gp%igridStructure, &
                                gp%horzIGridType,    gp%vertIGridType, &
                                gp%horzStagger,     gp%vertStagger, &
                                gp%igridStorage, &
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

      ! serialize the igrid specific information
      select case(gp%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridSerialize(igrid%ptr, buffer, length, offset, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select
      ! check local error code from the above case statement
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! serialize the delayout
      call ESMF_IGridGet(igrid, delayout=delayout, rc=localrc)
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

      end subroutine ESMF_IGridSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridDeserialize"

!BOPI
! !IROUTINE: ESMF_IGridDeserialize - Deserialize a byte stream into a IGrid
!
! !INTERFACE:
      function ESMF_IGridDeserialize(vm, buffer, offset, rc) 
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridDeserialize   
!
! !ARGUMENTS:
      type(ESMF_VM) :: vm
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a IGrid object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_IGridWrite()} and {\tt ESMF\_IGridRead()}.
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
      type(ESMF_IGridClass), pointer :: gp


      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,vm,rc)

      ! in case of error, make sure this is invalid.
      nullify(ESMF_IGridDeserialize%ptr)

      ! shortcut to internals
      allocate(gp, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, &
                                     "space for new IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_IGridConstructNew(gp, "dummy", localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call ESMF_BaseCreate(gp%base, "IGrid", "dummy", 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! this overwrites the name and adds attributes to the base obj.
      call c_ESMC_BaseDeserialize(gp%base, buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_IGridDeserialize(gp%dimCount,        gp%igridStructure, &
                                  gp%horzIGridType,    gp%vertIGridType, &
                                  gp%horzStagger,     gp%vertStagger, &
                                  gp%igridStorage, &
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

      select case(gp%igridStructure%igridStructure)

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNKNOWN
      case(0)
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                  "Unknown igrid structure", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT
      case(1)
        call ESMF_LRIGridDeserialize(gp, buffer, offset, localrc)

      !-------------
      ! ESMF_IGRID_STRUCT_LOGRECT_BLK
      case(2)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Log Rect Block", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_UNSTRUCT
      case(3)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure Unstructured", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      ! ESMF_IGRID_STRUCTURE_USER
      case(4)
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                  "IGrid structure User", &
                                  ESMF_CONTEXT, rc)) return

      !-------------
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                  "Invalid IGrid structure", &
                                  ESMF_CONTEXT, rc)) return
      end select

      ! TODO: call the appropriate igrid create function?  is this necessary?
      ! set the igrid status
      gp%igridStatus = ESMF_IGRID_STATUS_INIT
      ESMF_IGridDeserialize%ptr => gp

      ! turn on created flag
      ESMF_INIT_SET_CREATED(ESMF_IGridDeserialize)

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
      ! deserialized igrids
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
      call ESMF_IGridDistribute(ESMF_IGridDeserialize, newDELayout, &
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

      end function ESMF_IGridDeserialize
!------------------------------------------------------------------------------


      end module ESMF_IGridMod

