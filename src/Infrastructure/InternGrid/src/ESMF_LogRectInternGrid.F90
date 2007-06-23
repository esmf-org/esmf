! $Id: ESMF_LogRectInternGrid.F90,v 1.2 2007/06/23 04:25:46 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_LogRectIGrid.F90"
!
!     ESMF LogRectIGrid Module
      module ESMF_LogRectIGridMod
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
! !MODULE: ESMF_LogRectIGridMod - LogRectIGrid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_LogRectIGrid} class.  
! This class provides a unified interface for both {\tt ESMF\_PhysGrid} and 
! {\tt ESMF\_InternDG} information for model interngrids.  
! Functions for defining and computing {\tt ESMF\_IGrid}
! information are available through this class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod   ! ESMF base class
      use ESMF_UtilMod        ! ESMF base class
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_LogErrMod
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_InternArrayDataMapMod     ! ESMF data map class
      use ESMF_DELayoutMod    ! ESMF layout class
      use ESMF_InternArrayMod
      use ESMF_InternArrayCreateMod
      use ESMF_InternArrayGetMod
      use ESMF_InternDGMod    ! ESMF distributed interngrid class
      use ESMF_PhysCoordMod   ! ESMF physical coord class
      use ESMF_PhysGridMod    ! ESMF physical interngrid class
      use ESMF_IGridTypesMod   ! ESMF basic interngrid types and primitives
      use ESMF_InitMacrosMod
      use ESMF_VMMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

      ! TODO: temporary fix - the new Reconcile code is not compatible (yet)
      !  with the fast domainOption.  so make it public and have Reconcile
      !  turn it off the first time it is called.
      integer :: domainOption = 1
      public domainOption

      real(ESMF_KIND_R8), parameter :: fake = -999999.99d0

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    public ESMF_IGridCreateHorzLatLon
    public ESMF_IGridCreateHorzLatLonUni
    public ESMF_IGridCreateHorzXY
    public ESMF_IGridCreateHorzXYUni
    public ESMF_LRIGridAddVert
    public ESMF_LRIGridDistributeBlock
    public ESMF_LRIGridDistributeArbitrary
    public ESMF_LRIGridCreateRead
    public ESMF_LRIGridCreateCopy
    public ESMF_LRIGridCreateCutout
    public ESMF_LRIGridCreateDiffRes
    public ESMF_LRIGridCreateExchange
    public ESMF_LRIGridAddInternDGBlock
    public ESMF_LRIGridAddInternDGArb
    public ESMF_LRIGridAddPhysGridBlock
    public ESMF_LRIGridAddPhysGridArb
    public ESMF_LRIGridAddVertPhysGrid
    public ESMF_LRIGridGetCoord
    public ESMF_LRIGridGetDELocalInfo   ! access InternDG from above
    public ESMF_LRIGridGetAllAxisIndex  ! access InternDG from above
    public ESMF_LRIGridGetAIsAllDEs     ! access InternDG from above
    public ESMF_LRIGridGlobalToDELocalIndex
    public ESMF_LRIGridDELocalToGlobalIndex
    public ESMF_LRIGridGlobalToDELocalAI
    public ESMF_LRIGridDELocalToGlobalAI
    public ESMF_LRIGridGet
    public ESMF_LRIGridSet
    public ESMF_LRIGridGetCellMask
    public ESMF_LRIGridSetCellMask
    public ESMF_LRIGridSetBoundBoxesBlock
    public ESMF_LRIGridSetBoundBoxesArb
    public ESMF_LRIGridSetCoord
    public ESMF_LRIGridValidate
    public ESMF_LRIGridBoxIntersectRecv
    public ESMF_LRIGridBoxIntersectSend
    public ESMF_LRIGridDestruct
    public ESMF_LRIGridSerialize
    public ESMF_LRIGridDeserialize
    !public ESMF_LRIGridSearch

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_LogRectInternGrid.F90,v 1.2 2007/06/23 04:25:46 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_LRIGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_LRIGridConstructSpecd
         module procedure ESMF_LRIGridConstructUniform

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct a
!     complete {\tt ESMF\_IGrid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_IGridCreateHorzLatLon

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_IGridCreateHorzLatLonCoord
         module procedure ESMF_IGridCreateHorzLatLonDelta

! !DESCRIPTION:
!     This interface provides a single entry point for methods that create a
!     complete horizontal lat/lon {\tt ESMF\_IGrid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_IGridCreateHorzXY

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_IGridCreateHorzXYCoord
         module procedure ESMF_IGridCreateHorzXYDelta

! !DESCRIPTION:
!     This interface provides a single entry point for methods that create a
!     complete horizontal XY {\tt ESMF\_IGrid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_LRIGridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_LRIGridSetCoordComputeBlock
         module procedure ESMF_LRIGridSetCoordComputeArb

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     coordinates as part of a {\tt ESMF\_IGrid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_LRIGridSetCellMask

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_LRIGridSetCellMaskBlock
         module procedure ESMF_LRIGridSetCellMaskArb

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     cell masks as part of a {\tt ESMF\_IGrid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_LRIGridSearch
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_LRIGridSearchPoint
!         module procedure ESMF_LRIGridSearchList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     search a {\tt ESMF\_IGrid} for point(s).
!!
!!EOPI
!      end interface
!!
!------------------------------------------------------------------------------

!    < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! This section includes the IGrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateHorzLatLonCoord"
!BOP
! !IROUTINE: ESMF_IGridCreateHorzLatLon - Create a new horizontal LatLon IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreateHorzlatLon()
      function ESMF_IGridCreateHorzLatLonCoord(coord1, coord2, &
                                              horzstagger, dimNames, dimUnits, &
                                              coordorder, periodic, name, rc)

!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateHorzLatLonCoord
!
! !ARGUMENTS:
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzstagger
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordorder
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internal derived types, and internally generates the {\tt ESMF\_IGrid}.
!     Returns a pointer to the new {\tt ESMF\_IGrid}.  This routine creates an
!     {\tt ESMF\_IGrid} with the following parameters:
!     \begin{description}
!       \item logically rectangular;
!       \item user-specified spacing;
!       \item horizontal spherical coordinate system.
!     \end{description}
!     This routine generates {\tt ESMF\_IGrid} coordinates from the following
!     set of arguments:
!     \begin{description}
!       \item given arrays of coordinates (arguments {\tt coord1} and {\tt coord2}).
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[coord1]
!          Array of physical vertex coordinates in the first direction.
!          Note that there must be 1 more vertex coordinate in each dimension
!          than the number of cells.
!     \item[coord2]
!          Array of physical vertex coordinates in the second direction.
!          Note that there must be 1 more vertex coordinate in each dimension
!          than the number of cells.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.  If none is specified, the default is 
!          {\tt ESMF\_INTERNGRID\_HORZ\_STAGGER\_A}.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).  If none
!          is specified, the default is {\tt ESMF\_COORD\_ORDER\_XYZ}.
!     \item[{[periodic]}]
!          Logical array denoting the periodicity of the coordinate axes.
!          The default is FALSE for all axes.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! Error status
      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      type(ESMF_IGridType)           :: horzIGridType
      type(ESMF_CoordSystem)        :: horzCoordSystem

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_IGridCreateHorzLatLonCoord%ptr)

      ! set applicable default values
      horzIGridType    = ESMF_INTERNGRID_TYPE_XY
      horzCoordSystem = ESMF_COORD_SYSTEM_SPHERICAL

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_LRIGridConstructSpecd(interngrid, 2, coord1, coord2, &
                                     horzIGridType=horzIGridType, &
                                     horzStagger=horzstagger, &
                                     horzCoordSystem=horzCoordSystem, &
                                     dimNames=dimNames, dimunits=dimUnits, &
                                     coordOrder=coordorder, &
                                     periodic=periodic, name=name, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_IGridCreateHorzLatLonCoord%ptr => interngrid
      if (present(rc)) rc = ESMF_SUCCESS

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateHorzLatLonCoord)

      end function ESMF_IGridCreateHorzLatLonCoord

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateHorzLatLonDelta"
!BOP
! !IROUTINE: ESMF_IGridCreateHorzLatLon - Create a new horizontal LatLon IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreateHorzlatLon()
      function ESMF_IGridCreateHorzLatLonDelta(minGlobalCoordPerDim, &
                                              delta1, delta2, horzstagger, &
                                              dimNames, dimUnits, &
                                              coordorder, periodic, name, rc)

!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateHorzLatLonDelta
!
! !ARGUMENTS:
      real(ESMF_KIND_R8), dimension(:), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta2
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzstagger
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordorder
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internal derived types, and internally generates the {\tt ESMF\_IGrid}.
!     Returns a pointer to the new {\tt ESMF\_IGrid}.  This routine creates an
!     {\tt ESMF\_IGrid} with the following parameters:
!     \begin{description}
!       \item logically rectangular;
!       \item user-specified spacing;
!       \item horizontal spherical coordinate system.
!     \end{description}
!     This specific routine generates {\tt ESMF\_IGrid} coordinates from the
!     following set of arguments:
!     \begin{description}
!       \item given array of minimum coordinates and arrays of deltas 
!             (arguments {\tt minGlobalCoordPerDim}, {\tt delta1} and
!             {\tt delta2}).
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[minGlobalCoordsPerDim]
!          Array of minimum physical coordinate in each direction.
!          Note this is the vertex coordinate and not the cell center.
!     \item[delta1]
!          Array of physical increments between nodes in the first direction.
!          These are cell widths, and there should be as many as there are
!          cells in the interngrid.
!     \item[delta2]
!          Array of physical increments between nodes in the second direction.
!          These are cell widths, and there should be as many as there are
!          cells in the interngrid.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.  If none is specified, the default is 
!          {\tt ESMF\_INTERNGRID\_HORZ\_STAGGER\_A}.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).  If none
!          is specified, the default is {\tt ESMF\_COORD\_ORDER\_XYZ}.
!     \item[{[periodic]}]
!          Logical array denoting the periodicity of the coordinate axes.
!          The default is FALSE for all axes.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! Error status
      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      type(ESMF_IGridType)           :: horzIGridType
      type(ESMF_CoordSystem)        :: horzCoordSystem

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_IGridCreateHorzLatLonDelta%ptr)

      ! set applicable default values
      horzIGridType    = ESMF_INTERNGRID_TYPE_XY
      horzCoordSystem = ESMF_COORD_SYSTEM_SPHERICAL

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_LRIGridConstructSpecd(interngrid, 2, &
                                     minGlobalCoordPerDim=minGlobalCoordPerDim, &
                                     delta1=delta1, delta2=delta2, &
                                     horzIGridType=horzIGridType, &
                                     horzStagger=horzstagger, &
                                     horzCoordSystem=horzCoordSystem, &
                                     dimNames=dimNames, dimunits=dimUnits, &
                                     coordOrder=coordorder, &
                                     periodic=periodic, name=name, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_IGridCreateHorzLatLonDelta%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateHorzLatLonDelta)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateHorzLatLonDelta

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateHorzLatLonUni"
!BOP
! !IROUTINE: ESMF_IGridCreateHorzLatLonUni - Create a new uniform horizontal LatLon IGrid

! !INTERFACE:
      function ESMF_IGridCreateHorzLatLonUni(counts, minGlobalCoordPerDim, &
                                            maxGlobalCoordPerDim, &
                                            deltaPerDim, horzstagger, &
                                            dimNames, dimUnits, &
                                            coordorder, periodic, name, rc)
                                             
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateHorzLatLonUni
!
! !ARGUMENTS:
      integer, dimension(:), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(:), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                             maxGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                             deltaPerDim
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzstagger
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordorder
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internal derived types, and internally generates the {\tt ESMF\_IGrid}.
!     Returns a pointer to the new {\tt ESMF\_IGrid}.  This routine creates an
!     {\tt ESMF\_IGrid} with the following parameters:
!     \begin{description}
!       \item logically rectangular;
!       \item uniformly spaced coordinates (the distance between any 
!                   two consecutive interngrid points is equal);
!       \item horizontal spherical coordinate system.
!     \end{description}
!     This routine generates {\tt ESMF\_IGrid} coordinates from either of two
!     optional sets of arguments:
!     \begin{enumerate}
!       \item given min, max, and count (arguments {\tt minGlobalCoordPerDim}, 
!             {\tt maxGlobalCoordPerDim}, and {\tt counts});
!       \item given min, delta, and count (arguments {\tt minGlobalCoordPerDim}, 
!             {\tt deltaPerDim}, and {\tt counts}).
!     \end{enumerate}
!     If neither of these sets of arguments is present and valid, an error
!     message is issued and the program is terminated.
!
!     The arguments are:
!     \begin{description}
!     \item[counts]
!          Array of number of interngrid increments in each dimension.  This array
!          must have at least a length of two and have valid values in the
!          first two array locations or a fatal error occurs.
!     \item[minGlobalCoordPerDim]
!          Array of minimum physical coordinates in each dimension.
!          Note these are the vertex coordinates and not the cell centers.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum physical coordinates in each direction.
!          Note these are the vertex coordinates and not the cell centers.
!     \item[{[deltaPerDim]}]
!          Array of constant physical increments in each direction.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.  If none is specified, the default is 
!          {\tt ESMF\_INTERNGRID\_HORZ\_STAGGER\_A}.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).  If none
!          is specified, the default is {\tt ESMF\_COORD\_ORDER\_XYZ}.
!     \item[{[periodic]}]
!          Logical array denoting the periodicity of the coordinate axes.
!          The default is FALSE for all axes.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! Error status
      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      type(ESMF_IGridType)           :: horzIGridType
      type(ESMF_CoordSystem)        :: horzCoordSystem

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_IGridCreateHorzLatLonUni%ptr)

      ! set applicable default values
      horzIGridType    = ESMF_INTERNGRID_TYPE_LATLON_UNI
      horzCoordSystem = ESMF_COORD_SYSTEM_SPHERICAL

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_LRIGridConstructUniform(interngrid, 2, counts(1:2), &
                                       minGlobalCoordPerDim, &
                                       maxGlobalCoordPerDim, deltaPerDim, &
                                       horzIGridType, horzstagger, &
                                       horzCoordSystem, &
                                       dimNames, dimUnits, &
                                       coordorder, periodic, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_IGridCreateHorzLatLonUni%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateHorzLatLonUni)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateHorzLatLonUni

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateHorzXYCoord"
!BOP
! !IROUTINE: ESMF_IGridCreateHorzXY - Create a new horizontal XY IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreateHorzXY()
      function ESMF_IGridCreateHorzXYCoord(coord1, coord2, &
                                          horzstagger, dimNames, dimUnits, &
                                          coordorder, periodic, name, rc)

!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateHorzXYCoord
!
! !ARGUMENTS:
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzstagger
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordorder
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internal dervied types, and internally generates the {\tt ESMF\_IGrid}.
!     Returns a pointer to the new {\tt ESMF\_IGrid}.  This routine creates an
!     {\tt ESMF\_IGrid} with the following parameters:
!     \begin{description}
!       \item logically rectangular;
!       \item user-specified spacing;
!       \item horizontal cartesian coordinate system.
!     \end{description}
!     This routine generates {\tt ESMF\_IGrid} coordinates from the following
!     set of arguments:
!     \begin{description}
!       \item given arrays of coordinates (arguments {\tt coord1} and
!             {\tt coord2}).
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[coord1]
!          Array of physical vertex coordinates in the first direction.
!          Note that there must be 1 more vertex coordinate in each dimension
!          than the number of cells.
!     \item[coord2]
!          Array of physical vertex coordinates in the second direction.
!          Note that there must be 1 more vertex coordinate in each dimension
!          than the number of cells.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.  If none is specified, the default is 
!          {\tt ESMF\_INTERNGRID\_HORZ\_STAGGER\_A}.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).  If none
!          is specified, the default is {\tt ESMF\_COORD\_ORDER\_XYZ}.
!     \item[{[periodic]}]
!          Logical array denoting the periodicity of the coordinate axes.
!          The default is FALSE for all axes.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      integer                       :: localrc    ! Error status
      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      type(ESMF_IGridType)           :: horzIGridType
      type(ESMF_CoordSystem)        :: horzCoordSystem

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_IGridCreateHorzXYCoord%ptr)

      ! set applicable default values
      horzIGridType    = ESMF_INTERNGRID_TYPE_XY
      horzCoordSystem = ESMF_COORD_SYSTEM_CARTESIAN

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_LRIGridConstructSpecd(interngrid, 2, coord1, coord2, &
                                     horzIGridType=horzIGridType, &
                                     horzStagger=horzStagger, &
                                     horzCoordSystem=horzCoordSystem, &
                                     dimNames=dimNames, dimunits=dimUnits, &
                                     coordOrder=coordOrder, &
                                     periodic=periodic, name=name, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_IGridCreateHorzXYCoord%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateHorzXYCoord)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateHorzXYCoord

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateHorzXYDelta"
!BOP
! !IROUTINE: ESMF_IGridCreateHorzXY - Create a new horizontal XY IGrid

! !INTERFACE:
      ! Private name; call using ESMF_IGridCreateHorzXY()
      function ESMF_IGridCreateHorzXYDelta(minGlobalCoordPerDim, &
                                          delta1, delta2, &
                                          horzstagger, dimNames, dimUnits, &
                                          coordorder, periodic, name, rc)

!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateHorzXYDelta
!
! !ARGUMENTS:
      real(ESMF_KIND_R8), dimension(:), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta2
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzstagger
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordorder
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internal derived types, and internally generates the {\tt ESMF\_IGrid}.
!     Returns a pointer to the new {\tt ESMF\_IGrid}.  This routine creates an
!     {\tt ESMF\_IGrid} with the following parameters:
!     \begin{description}
!       \item logically rectangular;
!       \item user-specified spacing;
!       \item horizontal cartesian coordinate system.
!     \end{description}
!     This routine generates {\tt ESMF\_IGrid} coordinates from the following
!     set of arguments:
!     \begin{description}
!       \item given array of minimum coordinates and arrays of deltas
!             (arguments {\tt minGlobalCoordPerDim}, {\tt delta1} and
!             {\tt delta2}).
!     \end{description}
!
!     The arguments are:
!     \begin{description}
!     \item[minGlobalCoordsPerDim]
!          Array of minimum physical coordinates in each direction.
!          Note this is the vertex coordinate and not the cell center.
!     \item[delta1]
!          Array of physical increments between nodes in the first direction.
!          These are cell widths, and there should be as many as there are
!          cells in the interngrid.
!     \item[delta2]
!          Array of physical increments between nodes in the second direction.
!          These are cell widths, and there should be as many as there are
!          cells in the interngrid.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.  If none is specified, the default is 
!          {\tt ESMF\_INTERNGRID\_HORZ\_STAGGER\_A}.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).  If none
!          is specified, the default is {\tt ESMF\_COORD\_ORDER\_XYZ}.
!     \item[{[periodic]}]
!          Logical array denoting the periodicity of the coordinate axes.
!          The default is FALSE for all axes.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      integer                       :: localrc    ! Error status
      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      type(ESMF_IGridType)           :: horzIGridType
      type(ESMF_CoordSystem)        :: horzCoordSystem

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_IGridCreateHorzXYDelta%ptr)

      ! set applicable default values
      horzIGridType    = ESMF_INTERNGRID_TYPE_XY
      horzCoordSystem = ESMF_COORD_SYSTEM_CARTESIAN

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_LRIGridConstructSpecd(interngrid, 2, &
                                     minGlobalCoordPerDim=minGlobalCoordPerDim, &
                                     delta1=delta1, delta2=delta2, &
                                     horzIGridType=horzIGridType, &
                                     horzStagger=horzStagger, &
                                     horzCoordSystem=horzCoordSystem, &
                                     dimNames=dimNames, dimunits=dimUnits, &
                                     coordOrder=coordOrder, &
                                     periodic=periodic, name=name, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_IGridCreateHorzXYDelta%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateHorzXYDelta)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateHorzXYDelta

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridCreateHorzXYUni"
!BOP
! !IROUTINE: ESMF_IGridCreateHorzXYUni - Create a new uniform horizontal XY IGrid

! !INTERFACE:
      function ESMF_IGridCreateHorzXYUni(counts, minGlobalCoordPerDim, &
                                        maxGlobalCoordPerDim, &
                                        deltaPerDim, horzstagger, &
                                        dimNames, dimUnits, &
                                        coordorder, periodic, name, rc)
                                             
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_IGridCreateHorzXYUni
!
! !ARGUMENTS:
      integer, dimension(:), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(:), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                             maxGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                             deltaPerDim
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzstagger
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordorder
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internal derived types, and internally generates the {\tt ESMF\_IGrid}.
!     Returns a pointer to the new {\tt ESMF\_IGrid}.  This routine creates an
!     {\tt ESMF\_IGrid} with the following parameters:
!     \begin{description}
!       \item logically rectangular;
!       \item uniformly spaced coordinates (the distance between any 
!                   two consecutive interngrid points is equal);
!       \item horizontal cartesian coordinate system.
!     \end{description}
!     This routine generates {\tt ESMF\_IGrid} coordinates from either of two
!     optional sets of arguments:
!     \begin{enumerate}
!       \item given min, max, and count (arguments {\tt minGlobalCoordPerDim}, 
!             {\tt maxGlobalCoordPerDim}, and {\tt counts});
!       \item given min, delta, and count (arguments {\tt minGlobalCoordPerDim}, 
!             {\tt deltaPerDim}, and {\tt counts}).
!     \end{enumerate}
!     If neither of these sets of arguments is present and valid, an error
!     message is issued and the program is terminated.
!
!     The arguments are:
!     \begin{description}
!     \item[counts]
!          Array of number of interngrid increments in each dimension.  This array
!          must have at least a length of two and have valid values in the
!          first two array locations or a fatal error occurs.
!     \item[minGlobalCoordPerDim]
!          Array of minimum physical coordinates in each dimension.
!          Note this is the vertex coordinate and not the cell center.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum physical coordinates in each direction.
!          Note this is the vertex coordinate and not the cell center.
!     \item[{[deltaPerDim]}]
!          Array of constant physical increments in each direction.
!     \item[{[horzstagger]}]
!          {\tt ESMF\_IGridHorzStagger} specifier denoting horizontal IGrid
!          stagger.  If none is specified, the default is 
!          {\tt ESMF\_INTERNGRID\_HORZ\_STAGGER\_A}.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[coordorder]}]
!          {\tt ESMF\_CoordOrder} specifier denoting the default coordinate
!          ordering for the IGrid and all related Fields (i.e. ZXY).  If none
!          is specified, the default is {\tt ESMF\_COORD\_ORDER\_XYZ}.
!     \item[{[periodic]}]
!          Logical array denoting the periodicity of the coordinate axes.
!          The default is FALSE for all axes.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      integer                       :: localrc    ! Error status
      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      type(ESMF_IGridType)           :: horzIGridType
      type(ESMF_CoordSystem)        :: horzCoordSystem

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_IGridCreateHorzXYUni%ptr)

      ! set applicable default values
      horzIGridType    = ESMF_INTERNGRID_TYPE_XY_UNI
      horzCoordSystem = ESMF_COORD_SYSTEM_CARTESIAN

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_LRIGridConstructUniform(interngrid, 2, counts(1:2), &
                                       minGlobalCoordPerDim, &
                                       maxGlobalCoordPerDim, deltaPerDim, &
                                       horzIGridType, horzStagger, &
                                       horzCoordSystem, &
                                       dimNames, dimUnits, &
                                       coordOrder, periodic, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_IGridCreateHorzXYUni%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_IGridCreateHorzXYUni)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_IGridCreateHorzXYUni

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridAddVert"
!BOPI
! !IROUTINE: ESMF_LRIGridAddVert - Add a vertical subIGrid to an existing IGrid

! !INTERFACE:
      subroutine ESMF_LRIGridAddVert(interngrid, minGlobalCoord, delta, coord, &
                                    vertinterngridtype, vertstagger, &
                                    vertcoordsystem, dimName, dimUnit, &
                                    name, rc)
                                             
!
! !ARGUMENTS:
      type(ESMF_IGridClass), pointer :: interngrid
      real(ESMF_KIND_R8), intent(in), optional :: minGlobalCoord
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord
      type(ESMF_IGridVertType), intent(in), optional :: vertinterngridtype
      type(ESMF_IGridVertStagger), intent(in), optional :: vertstagger
      type(ESMF_CoordSystem), intent(in), optional :: vertcoordsystem
      character(len=*), intent(in), optional :: dimName
      character(len=*), intent(in), optional :: dimUnit
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine adds a vertical subIGrid to an already allocated {\tt ESMF_IGrid}.
!     Only one vertical subIGrid is allowed for any IGrid, so if a vertical subIGrid
!     already exists for the IGrid that is passed in, an error is returned.
!     This routine generates {\tt ESMF\_IGrid} coordinates from either of two
!     optional sets of arguments:                 
!     \begin{enumerate}
!       \item given min and array of deltas (optional arguments
!             {\tt minGlobalCoord} and {\tt delta});
!       \item given array of coordinates (optional argument {\tt coord}).
!     \end{enumerate}
!     If neither of these sets of arguments is present and valid, an error
!     message is issued and the program is terminated.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_IGrid} to add vertical interngrid to.
!     \item[{[minGlobalCoord]}]
!          Minimum physical coordinate in the vertical direction.
!     \item[{[delta]}]
!          Array of physical increments in the vertical direction.
!     \item[{[coord]}]
!          Array of physical coordinates in the vertical direction.
!     \item[{[vertinterngridtype]}]
!          {\tt ESMF\_IGridVertType} specifier denoting vertical interngrid type.
!     \item[{[vertstagger]}]
!          {\tt ESMF\_IGridVertStagger} specifier denoting vertical interngrid stagger.
!     \item[{[vertcoordsystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical interngrid.
!     \item[{[dimName]}]
!          Dimension name.
!     \item[{[dimUnit]}]
!          Dimension unit.
!     \item[{[name]}]
!          Name for the vertical interngrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc                          ! Error status
      integer :: count, i
      real(ESMF_KIND_R8) :: minGlobalCoordUse, maxGlobalCoordUse
      real(ESMF_KIND_R8), dimension(:), pointer :: coordsUse
      type(ESMF_LogRectIGrid), pointer :: lrinterngrid
      type(ESMF_LocalArray), dimension(:), pointer :: coords

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! sanity check for bad values  TODO: more checks
      if (present(delta)) count = size(delta)
      if (present(coord)) count = size(coord)-1
      if (count .le. 1) then
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "bad value for count", &
                                 ESMF_CONTEXT, rc)) return
        ! TODO: "bad value for count, ", count
      endif

      allocate(coords(3), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coords", &
                                     ESMF_CONTEXT, rc)) return
      if (associated(interngrid%interngridSpecific%logRectIGrid%coords)) then
        coords(1) = interngrid%interngridSpecific%logRectIGrid%coords(1)
        coords(2) = interngrid%interngridSpecific%logRectIGrid%coords(2)
      endif

      interngrid%interngridSpecific%logRectIGrid%coords => coords

      ! Two ways to make a subinterngrid here: by coordinates or by minimum and delta
      ! by coordinates:
      allocate(coordsUse(count+1), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating coordsUse", &
                                     ESMF_CONTEXT, rc)) return

      if (present(coord)) then
        do i = 1,count+1
          coordsUse(i) = coord(i)
        enddo
        count  = size(coord) - 1   !TODO: coords indicate corner points
                                   !      or center points?

      ! by deltas (and minCoord):     TODO: make it a starting value instead of min
      elseif (present(delta)) then
        coordsUse(1) = minGlobalCoord    ! TODO: make sure it's here
        do i = 1,count
          coordsUse(i+1) = coordsUse(i) + delta(i)
        enddo
      endif

      coords(3) = ESMF_LocalArrayCreate(coordsUse, ESMF_DATA_COPY, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      minGlobalCoordUse = minval(coordsUse)
      maxGlobalCoordUse = maxval(coordsUse)

      ! Fill in logRectIGrid derived type with subroutine arguments
      lrinterngrid => interngrid%interngridSpecific%logRectIGrid
      lrinterngrid%countPerDim(3) = count

      ! Fill in interngrid derived type with subroutine arguments
      interngrid%dimCount      = 3
      if (present(vertIGridType   )) interngrid%vertIGridType    = vertIGridType
      if (present(vertStagger    )) interngrid%vertStagger     = vertStagger
      if (present(vertCoordSystem)) interngrid%vertCoordSystem = vertCoordSystem

      ! Set dimension name and unit
      if (present(dimName)) then
        interngrid%dimNames(3) = dimName
      endif
      if (present(dimUnit)) then
        interngrid%dimUnits(3) = dimUnit
      endif

      ! Set global domain limit
      interngrid%minGlobalCoordPerDim(3) = minGlobalCoordUse
      interngrid%maxGlobalCoordPerDim(3) = maxGlobalCoordUse

      ! Clean up
      deallocate(coordsUse, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coordsUse", &
                                     ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridAddVert

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridCreateRead"
!BOPI
! !IROUTINE: ESMF_LRIGridCreateRead - Create a new IGrid read in from a file

! !INTERFACE:
      function ESMF_LRIGridCreateRead(iospec, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_LRIGridCreateRead
!
! !ARGUMENTS:
      type(ESMF_IOSpec), intent(in) :: iospec   ! file specs
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internal derived types, and reads a {\tt ESMF\_IGrid} in from a file.
!     Return a pointer to the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[iospec]
!          File I/O specification.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_LRIGridCreateRead%ptr)

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_IGridConstructNew(interngrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_LRIGridCreateRead%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_LRIGridCreateRead)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LRIGridCreateRead

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridCreateCopy"
!BOPI
! !IROUTINE: ESMF_LRIGridCreateCopy - Create a new IGrid by copying another IGrid

! !INTERFACE:
      function ESMF_LRIGridCreateCopy(interngridIn, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_LRIGridCreateCopy
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: interngridIn
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internal derived types, and copies attributes from another
!     {\tt ESMF\_IGrid}.  Return a pointer to the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngridIn]
!          {\tt ESMF\_IGrid} to be copied.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngridIn,rc)

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_LRIGridCreateCopy%ptr)

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_IGridConstructNew(interngrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                               ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_LRIGridCreateCopy%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_LRIGridCreateCopy)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LRIGridCreateCopy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridCreateCutout"
!BOPI
! !IROUTINE: ESMF_LRIGridCreateCutout - Create a new IGrid as a subset of an existing IGrid

! !INTERFACE:
      function ESMF_LRIGridCreateCutout(interngridIn, min, max, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_LRIGridCreateCutout
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: interngridIn
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
!     \item[interngridIn]
!          {\tt ESMF\_IGrid} to be partially copied.
!     \item[min]
!          Minimum global indices for the region of the interngrid to be cutout.
!     \item[max]
!          Maximum global indices for the region of the interngrid to be cutout.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngridIn,rc)

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_LRIGridCreateCutout%ptr)

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_IGridConstructNew(interngrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_LRIGridCreateCutout%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_LRIGridCreateCutout)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LRIGridCreateCutout

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridCreateDiffRes"
!BOPI
! !IROUTINE: ESMF_LRIGridCreateDiffRes - Create a new IGrid by coarsening or refining an existing IGrid

! !INTERFACE:
      function ESMF_LRIGridCreateDiffRes(interngridIn, resolution, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_LRIGridCreateDiffRes
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: interngridIn
      integer, dimension(:), intent(in) :: resolution
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_IGrid} object, constructs its
!     internals, and creates a {\tt ESMF\_IGrid} by either coarsening or refining an
!     existing {\tt ESMF\_IGrid}.  Return a pointer to the new {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngridIn]
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
! !REQUIREMENTS:  TODO
!EOPI

      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngridIn,rc)

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_LRIGridCreateDiffRes%ptr)

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_IGridConstructNew(interngrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_LRIGridCreateDiffRes%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_LRIGridCreateDiffRes)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LRIGridCreateDiffRes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridCreateExchange"
!BOPI
! !IROUTINE: ESMF_LRIGridCreateExchange - Create a new IGrid from the intersection of two existing interngrids

! !INTERFACE:
      function ESMF_LRIGridCreateExchange(interngridIn1, interngridIn2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_IGrid) :: ESMF_LRIGridCreateExchange
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: interngridIn1
      type(ESMF_IGrid), intent(in) :: interngridIn2
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
!     \item[interngridIn1]
!          First source {\tt ESMF\_IGrid}.
!     \item[interngridIn2]
!          Second source {\tt ESMF\_IGrid}.
!     \item[{[name]}]
!          New {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      type(ESMF_IGridClass), pointer :: interngrid       ! Pointer to new interngrid
      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngridIn1,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngridIn2,rc)

      ! Initialize pointers
      nullify(interngrid)
      nullify(ESMF_LRIGridCreateExchange%ptr)

      allocate(interngrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid object", &
                                     ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize interngrid internals.
      call ESMF_IGridConstructNew(interngrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      ESMF_LRIGridCreateExchange%ptr => interngrid

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(ESMF_LRIGridCreateExchange)


      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_LRIGridCreateExchange

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridConstructSpecificNew"
!BOPI
! !IROUTINE: ESMF_LRIGridConstructSpecificNew - Construct a new empty logRectIGrid specific type

! !INTERFACE:
      subroutine ESMF_LRIGridConstructSpecificNew(lrinterngrid, rc)
!
! !ARGUMENTS:
      type(ESMF_LogRectIGrid) :: lrinterngrid
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already allocated
!     {\tt ESMF\_LogRectIGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_LogRectIGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only.
!
!     The arguments are:
!     \begin{description}
!     \item[lrinterngrid]
!          The {\tt ESMF\_LogRectIGrid} object to be constructed.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: i

      ! Initialize return code; assume routine not implemented
      rc     = ESMF_RC_NOT_IMPL

      ! check and set input variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_LogRectIGridGetInit, ESMF_LogRectIGridInit,lrinterngrid)

      ! Initialize lrinterngrid contents to default values
      do i = 1,ESMF_MAXINTERNGRIDDIM
        lrinterngrid%countPerDim(i) = 0
        lrinterngrid%deltaPerDim(i) = 0.0
      enddo
      nullify(lrinterngrid%coords)

      ! Set return values.
      rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridConstructSpecificNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridConstructUniform"
!BOPI
! !IROUTINE: ESMF_LRIGridConstructUniform - Construct a uniform IGrid

! !INTERFACE:
      subroutine ESMF_LRIGridConstructUniform(interngrid, dimCount, counts, &
                                             minGlobalCoordPerDim, &
                                             maxGlobalCoordPerDim, &
                                             deltaPerDim, &
                                             horzIGridType, horzStagger, &
                                             horzCoordSystem, &
                                             dimNames, dimUnits, &
                                             coordOrder, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: interngrid
      integer, intent(in) :: dimCount
      integer, dimension(:), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(:), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                            maxGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                            deltaPerDim
      type(ESMF_IGridType), intent(in), optional :: horzIGridType
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_IGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_IGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_IGridCreate}, which calls
!     {\tt ESMF\_IGridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid}
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[counts]
!          Array of number of interngrid increments in each dimension.
!     \item[minGlobalCoordPerDim]
!          Array of minimum physical coordinates in each dimension.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum physical coordinates in each direction.
!     \item[{[horzIGridType]}]
!          Integer specifier to denote horizontal interngrid type.
!     \item[{[horzStagger]}]
!          Integer specifier to denote horizontal interngrid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal interngrid.
!     \item[{[coordOrder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote the default coordinate
!          ordering for the IGrid and all related Fields (i.e. KIJ).
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: localrc                          ! Error status
      integer :: i
      type(ESMF_LogRectIGrid), pointer :: lrinterngrid
      real(ESMF_KIND_R8) :: recheck
      real(ESMF_KIND_R8), dimension(dimCount) :: useMaxes, useDeltas
      character(len=ESMF_MAXSTR) :: msgbuf

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize the derived type contents, including setting name
      call ESMF_IGridConstructNew(interngrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! sanity check for bad values
      do i=1,dimCount
        if (counts(i) .le. 0) then
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "bad value for count", &
                                 ESMF_CONTEXT, rc)) return
           !! "bad value for count, ", counts(i), "for dimension ", i
           return
        endif
      enddo

      ! Fill in default values for optional arguments which weren't specified
      ! and check for an over-specified system which isn't consistent.  (in which
      ! case, deltas are overwritten and a warning printed.)
      interngrid%interngridStructure   = ESMF_INTERNGRID_STRUCTURE_LOGRECT
      interngrid%horzStagger     = ESMF_INTERNGRID_HORZ_STAGGER_A
      interngrid%coordOrder      = ESMF_COORD_ORDER_XYZ
      if (present(deltaPerDim)) then
         do i=1,dimCount
           useDeltas(i) = deltaPerDim(i)
         enddo
      else
         useDeltas(:) = 1.0   ! default values
      endif

      if (present(maxGlobalCoordPerDim)) then
         do i=1,dimCount
           useMaxes(i) = maxGlobalCoordPerDim(i)
         enddo
      else
         do i=1,dimCount
           useMaxes(i) = minGlobalCoordPerDim(i) + (counts(i) * useDeltas(i))
         enddo
      endif

      if (present(deltaPerDim) .and. present(maxGlobalCoordPerDim)) then
         do i=1,dimCount
            recheck = (useMaxes(i) - minGlobalCoordPerDim(i)) / float(counts(i))
            if (recheck-useDeltas(i) .gt. 0.00001) then
                write(msgbuf, *) "Inconsistent set of min, max, deltas, and ", &
                                 "counts specified"
                call ESMF_LogWrite(msgbuf, ESMF_LOG_WARNING, ESMF_CONTEXT)
                write(msgbuf, *) "delta for dimension", i, "reset from", &
                                 useDeltas(i), "to ", recheck
                call ESMF_LogWrite(msgbuf, ESMF_LOG_WARNING, ESMF_CONTEXT)
              useDeltas(i) = recheck
            endif
         enddo
      endif

      ! Fill in logRectIGrid derived type with subroutine arguments
      allocate(interngrid%interngridSpecific%logRectIGrid, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "logRectIGrid", &
                                     ESMF_CONTEXT, rc)) return

      lrinterngrid => interngrid%interngridSpecific%logRectIGrid
      call ESMF_LRIGridConstructSpecificNew(lrinterngrid, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      do i = 1,dimCount
        lrinterngrid%countPerDim(i) = counts(i)
        lrinterngrid%deltaPerDim(i) = useDeltas(i) 
      enddo

      ! Fill in interngrid derived type with subroutine arguments
      interngrid%dimCount      = dimCount
      if (present(horzIGridType   )) interngrid%horzIGridType    = horzIGridType
      if (present(horzStagger    )) interngrid%horzStagger     = horzStagger
      if (present(horzCoordSystem)) interngrid%horzCoordSystem = horzCoordSystem
      if (present(coordOrder     )) interngrid%coordOrder      = coordOrder

      ! Set dimension names and units for each dimension
      if (present(dimNames)) then
        do i = 1,dimCount
          if (i > size(dimNames)) exit
          interngrid%dimNames(i) = dimNames(i)
        enddo
      endif
      if (present(dimUnits)) then
        do i = 1,dimCount
          if (i > size(dimUnits)) exit
          interngrid%dimUnits(i) = dimUnits(i)
        enddo
      endif

      ! Set periodic flags for each dimension
      if (present(periodic)) then
        do i = 1,dimCount
          if (i > size(periodic)) exit
          interngrid%periodic(i) = periodic(i)
        enddo
      endif

      ! Set global domain limits
      if (size(minGlobalCoordPerDim) > ESMF_MAXINTERNGRIDDIM) then
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "minGlobalCoordPerDim too big", &
                                 ESMF_CONTEXT, rc)) return
      endif
      do i = 1,dimCount
        interngrid%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
      enddo

      if (present(maxGlobalCoordPerDim)) then
        if (size(maxGlobalCoordPerDim) > ESMF_MAXINTERNGRIDDIM) then
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "maxGlobalCoordPerDim too big", &
                                 ESMF_CONTEXT, rc)) return
        endif
        do i = 1,dimCount
          interngrid%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
        enddo
      else
        ! default values computed above
        do i = 1,dimCount
          interngrid%maxGlobalCoordPerDim(i) = useMaxes(i)
        enddo
      endif

      interngrid%interngridStatus = ESMF_INTERNGRID_STATUS_INIT

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(interngrid)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridConstructUniform

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridConstructSpecd"
!BOPI
! !IROUTINE: ESMF_LRIGridConstructSpecd - Construct a specified IGrid

! !INTERFACE:
      subroutine ESMF_LRIGridConstructSpecd(interngrid, dimCount, &
                                           coord1, coord2, coord3, &
                                           minGlobalCoordPerDim, &
                                           delta1, delta2, delta3, &
                                           horzIGridType, vertIGridType, &
                                           horzStagger, vertStagger, &
                                           horzCoordSystem, vertCoordSystem, &
                                           dimNames, dimUnits, &
                                           coordOrder, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: interngrid
      integer, intent(in) :: dimCount
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord3
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                         minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta3
      type(ESMF_IGridType), intent(in), optional :: horzIGridType
      type(ESMF_IGridVertType), intent(in), optional :: vertIGridType
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzStagger
      type(ESMF_IGridVertStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      character (len=*), dimension(:), intent(in), optional :: dimNames
      character (len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_IGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_IGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_IGridCreate}, which calls
!     {\tt ESMF\_IGridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid}
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[{[coord1]}]
!          Array of physical coordinates in the first direction.
!     \item[{[coord2]}]
!          Array of physical coordinates in the second direction.
!     \item[{[coord3]}]
!          Array of physical coordinates in the third direction.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum physical coordinate in each direction.
!     \item[{[delta1]}]
!          Array of physical increments between nodes in the first direction.
!     \item[{[delta2]}]
!          Array of physical increments between nodes in the second direction.
!     \item[{[delta3]}]
!          Array of physical increments between nodes in the third direction.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[horzIGridType]}]
!          Integer specifier to denote horizontal interngrid type.
!     \item[{[vertIGridType]}]
!          Integer specifier to denote vertical interngrid type.
!     \item[{[horzStagger]}]
!          Integer specifier to denote horizontal interngrid stagger.
!     \item[{[vertStagger]}]
!          Integer specifier to denote vertical interngrid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal interngrid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical interngrid.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: localrc                          ! Error status
      integer :: i
      integer, dimension(dimCount) :: counts
      logical :: dummy
      real(ESMF_KIND_R8), dimension(dimCount) :: minGlobalCoordPerDimUse, &
                                                 maxGlobalCoordPerDimUse
      real(ESMF_KIND_R8), dimension(:), pointer :: coordsUse1, coordsUse2, &
                                                   coordsUse3
      type(ESMF_LocalArray), dimension(:), pointer :: coords
      type(ESMF_LogRectIGrid), pointer :: lrinterngrid

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Initialize the derived type contents, including setting name
      call ESMF_IGridConstructNew(interngrid, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Fill in logRectIGrid derived type with subroutine arguments
      ! TODO: check stat return code against 0 (not ESMF_FAILURE)
      interngrid%interngridStructure   = ESMF_INTERNGRID_STRUCTURE_LOGRECT
      interngrid%horzStagger     = ESMF_INTERNGRID_HORZ_STAGGER_A
      interngrid%coordOrder      = ESMF_COORD_ORDER_XYZ
      allocate(interngrid%interngridSpecific%logRectIGrid, stat=localrc) 
      if (ESMF_LogMsgFoundAllocError(localrc, "logRectIGrid", &
                                     ESMF_CONTEXT, rc)) return

      lrinterngrid => interngrid%interngridSpecific%logRectIGrid
      call ESMF_LRIGridConstructSpecificNew(lrinterngrid, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      allocate(interngrid%interngridSpecific%logRectIGrid%coords(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "logRectIGrid%coords", &
                                     ESMF_CONTEXT, rc)) return
   
      coords => interngrid%interngridSpecific%logRectIGrid%coords

      ! Two ways to make a interngrid here: by coordinates or by minima and deltas
      ! by coordinates:
      if (present(coord1)) then    ! for now, assume if coord1 array is there then
                                   ! using coords for all directions
        allocate(coordsUse1(size(coord1)), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "coordsUse1", &
                                       ESMF_CONTEXT, rc)) return

        do i = 1,size(coord1)
          coordsUse1(i) = coord1(i)
        enddo
        coords(1) = ESMF_LocalArrayCreate(coordsUse1, ESMF_DATA_COPY, localrc)
        counts(1) = size(coord1) - 1   !TODO: coords indicate corner points
                                       !      or center points?
        minGlobalCoordPerDimUse(1) = minval(coord1)
        maxGlobalCoordPerDimUse(1) = maxval(coord1)
        if (present(coord2)) then
          if (dimCount.le.1) then
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "dimCount not consistent with coords arrays", &
                                 ESMF_CONTEXT, rc)) return
          endif
          allocate(coordsUse2(size(coord2)), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "coordsUse2", &
                                         ESMF_CONTEXT, rc)) return
          do i = 1,size(coord2)
            coordsUse2(i) = coord2(i)
          enddo
          coords(2) = ESMF_LocalArrayCreate(coordsUse2, ESMF_DATA_COPY, localrc)
          counts(2) = size(coord2) - 1
          minGlobalCoordPerDimUse(2) = minval(coord2)
          maxGlobalCoordPerDimUse(2) = maxval(coord2)
        endif
        if (present(coord3)) then
          if (dimCount.le.2) then
            dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                         "dimCount not consistent with coords arrays", &
                         ESMF_CONTEXT, rc)
            return
          endif
          allocate(coordsUse3(size(coord3)), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "Allocating IGrid ", &
                                         ESMF_CONTEXT, rc)) return

          do i = 1,size(coord3)
            coordsUse3(i) = coord3(i)
          enddo
          coords(3) = ESMF_LocalArrayCreate(coordsUse3, ESMF_DATA_COPY, localrc)
          counts(3) = size(coord3) - 1
          minGlobalCoordPerDimUse(3) = minval(coord3)
          maxGlobalCoordPerDimUse(3) = maxval(coord3)
        endif

      ! by deltas (and minCoordPerDim):     TODO: make it a starting value instead of min
      elseif (present(delta1)) then
        counts(1) = size(delta1)
        allocate(coordsUse1(size(delta1)+1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "coordsUse1", &
                                       ESMF_CONTEXT, rc)) return
        coordsUse1(1) = minGlobalCoordPerDim(1)    ! TODO: make sure it's here
        do i = 1,size(delta1)
          coordsUse1(i+1) = coordsUse1(i) + delta1(i)
        enddo
        coords(1) = ESMF_LocalArrayCreate(coordsUse1, ESMF_DATA_COPY, localrc)
        minGlobalCoordPerDimUse(1) = minGlobalCoordPerDimUse(1)
        maxGlobalCoordPerDimUse(1) = maxval(coordsUse1)
        ! TODO: redefine minGlobalCoordPerDim in case deltas are negative
        if (present(delta2)) then
          if (dimCount.le.1) then
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "dimCount not consistent with delta arrays", &
                                 ESMF_CONTEXT, rc)) return
          endif
          counts(2) = size(delta2)
          allocate(coordsUse2(size(delta2)+1), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "coordsUse2", &
                                         ESMF_CONTEXT, rc)) return
          coordsUse2(1) = minGlobalCoordPerDim(2)    ! TODO: make sure it's here
          do i = 1,size(delta2)
            coordsUse2(i+1) = coordsUse2(i) + delta2(i)
          enddo
          coords(2) = ESMF_LocalArrayCreate(coordsUse2, ESMF_DATA_COPY, localrc)
          minGlobalCoordPerDimUse(2) = minGlobalCoordPerDimUse(2)
          maxGlobalCoordPerDimUse(2) = maxval(coordsUse2)
        endif
        if (present(delta3)) then
          if (dimCount.le.2) then
            dummy =  ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                          "dimCount not consistent with delta arrays", &
                          ESMF_CONTEXT, rc)
            return
          endif
          counts(3) = size(delta3)
          allocate(coordsUse3(size(delta3)+1), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "coordsUse3", &
                                         ESMF_CONTEXT, rc)) return
          coordsUse3(1) = minGlobalCoordPerDim(3)    ! TODO: make sure it's here
          do i = 1,size(delta3)
            coordsUse3(i+1) = coordsUse3(i) + delta3(i)
          enddo
          coords(3) = ESMF_LocalArrayCreate(coordsUse3, ESMF_DATA_COPY, localrc)
          minGlobalCoordPerDimUse(3) = minGlobalCoordPerDimUse(3)
          maxGlobalCoordPerDimUse(3) = maxval(coordsUse3)
        endif
      else
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                     "must have either coords arrays or delta arrays", &
                     ESMF_CONTEXT, rc)) return
      endif

      ! either way, we have the counts now
      do i = 1,dimCount
        lrinterngrid%countPerDim(i) = counts(i)
      enddo

      ! Fill in interngrid derived type with subroutine arguments
      interngrid%dimCount      = dimCount
      interngrid%interngridStructure = ESMF_INTERNGRID_STRUCTURE_LOGRECT
      if (present(horzIGridType   )) interngrid%horzIGridType    = horzIGridType
      if (present(vertIGridType   )) interngrid%vertIGridType    = vertIGridType
      if (present(horzStagger    )) interngrid%horzStagger     = horzStagger
      if (present(vertStagger    )) interngrid%vertStagger     = vertStagger
      if (present(horzCoordSystem)) interngrid%horzCoordSystem = horzCoordSystem
      if (present(vertCoordSystem)) interngrid%vertCoordSystem = vertCoordSystem
      if (present(coordOrder     )) interngrid%coordOrder      = coordOrder

      ! Set dimension names and units for each dimension
      if (present(dimNames)) then
        do i=1,ESMF_MAXINTERNGRIDDIM
          if (i > size(dimNames)) exit
          interngrid%dimNames(i) = dimNames(i)
        enddo
      endif
      if (present(dimUnits)) then
        do i=1,ESMF_MAXINTERNGRIDDIM
          if (i > size(dimUnits)) exit
          interngrid%dimUnits(i) = dimUnits(i)
        enddo
      endif

      ! Set periodic flags for each dimension
      if (present(periodic)) then
        do i=1,ESMF_MAXINTERNGRIDDIM
          if (i > size(periodic)) exit
          interngrid%periodic(i) = periodic(i)
        enddo
      endif

      ! Set global domain limits
      do i=1,size(minGlobalCoordPerDimUse)
        interngrid%minGlobalCoordPerDim(i) = minGlobalCoordPerDimUse(i)
      enddo
      do i=1,size(maxGlobalCoordPerDimUse)
        interngrid%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDimUse(i)
      enddo

      interngrid%interngridStatus = ESMF_INTERNGRID_STATUS_INIT

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(interngrid)

      ! Clean up
      deallocate(coordsUse1, &
                 coordsUse2, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coordsUse arrays", &
                                     ESMF_CONTEXT, rc)) return
      if (dimCount.ge.3) then
        deallocate(coordsUse3, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coordsUse3", &
                                       ESMF_CONTEXT, rc)) return
      endif


      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridConstructSpecd

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridDistributeBlock"
!BOPI
! !IROUTINE: ESMF_LRIGridDistributeBlock - Distribute a IGrid with block storage

! !INTERFACE:
      subroutine ESMF_LRIGridDistributeBlock(interngrid, delayout, &
                                            countsPerDEDecomp1, &
                                            countsPerDEDecomp2, &
                                            decompIds, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), target :: interngrid
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in), optional :: countsPerDEDecomp1
      integer, dimension(:), intent(in), optional :: countsPerDEDecomp2
      integer, dimension(:), intent(in), optional :: decompIds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_IGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_IGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_IGridCreate}, which calls
!     {\tt ESMF\_IGridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid}
!     \item[delayout]
!         {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[countsPerDEDecomp1]}]
!          Array of number of interngrid increments (cells) per DE in the 
!          first decomposition axis.
!     \item[{[countsPerDEDecomp2]}]
!          Array of number of interngrid increments (cells) per DE in the 
!          second decomposition axis.
!     \item[{[decompIds]}]
!          Identifier for which IGrid axes are decomposed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: localrc                          ! Error status
      logical :: dummy
      character(len=ESMF_MAXSTR) :: internDGName, physIGridName
      character(len=ESMF_MAXSTR), dimension(:), allocatable :: dimNames, dimUnits
      integer :: internDGId, physIGridId, nDEs(0:2)
      integer :: localDE, myCount, myDEDecomp(0:2), myDE(3)
      integer :: i, dimCount, dimCountIGrid, aSize, ndim
      integer, dimension(:), allocatable :: decompIdsUse, counts
      integer, dimension(:), allocatable :: countsPerDEDecomp1Use, &
                                            countsPerDEDecomp2Use
      integer, dimension(:), allocatable :: countsPerDE1, countsPerDE2, &
                                            countsPerDE3
      real(ESMF_KIND_R8) :: delta
      real(ESMF_KIND_R8), dimension(:), allocatable :: min, max
      real(ESMF_KIND_R8), dimension(:), pointer :: coord1, coord2, coord3
      type(ESMF_Logical):: otoFlag, lrFlag
      type(ESMF_Logical), dimension(:), pointer :: periodic
      type(ESMF_LocalArray), dimension(:), pointer :: coords
      type(ESMF_RelLoc) :: relloc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

      ! validate the layout before going any further
      call ESMF_DELayoutValidate(delayout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! do some sanity error checking

      ! Extract some information from the IGrid
      dimCount = interngrid%dimCount
      allocate(  counts(dimCount), &
                    min(dimCount), &
                    max(dimCount), &
               dimNames(dimCount), &
               dimUnits(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating dimCount arrays", &
                                     ESMF_CONTEXT, rc)) return

      nullify(coords)
      if (associated(interngrid%interngridSpecific%logRectIGrid%coords)) then
        coords => interngrid%interngridSpecific%logRectIGrid%coords
      endif

      do i = 1,dimCount
        counts(i)   = interngrid%interngridSpecific%logRectIGrid%countPerDim(i)
        min(i)      = interngrid%minGlobalCoordPerDim(i)
        max(i)      = interngrid%maxGlobalCoordPerDim(i)
        dimNames(i) = interngrid%dimNames(i)
        dimUnits(i) = interngrid%dimUnits(i)
      enddo
      periodic => interngrid%periodic

      if (interngrid%horzIGridType.eq.ESMF_INTERNGRID_TYPE_LATLON_UNI .OR. &
          interngrid%horzIGridType.eq.ESMF_INTERNGRID_TYPE_XY_UNI) then ! uniform
        allocate(coord1(counts(1)+1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "coord1", &
                                       ESMF_CONTEXT, rc)) return
        coord1(1) = min(1)
        delta     = (max(1) - min(1)) / float(counts(1))
        do i = 1,counts(1)
          coord1(i+1) = min(1) + float(i)*delta
        enddo
        if (dimCount.ge.2) then
          allocate(coord2(counts(2)+1), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "coord2", &
                                         ESMF_CONTEXT, rc)) return
          coord2(1) = min(2)
          delta     = (max(2) - min(2)) / float(counts(2))
          do i = 1,counts(2)
            coord2(i+1) = min(2) + float(i)*delta
          enddo
        endif
      else   ! not uniform
        call ESMF_LocalArrayGetData(coords(1), coord1, ESMF_DATA_COPY, localrc)
        if (dimCount.ge.2) &
          call ESMF_LocalArrayGetData(coords(2), coord2, ESMF_DATA_COPY, localrc)
      endif

      ! the vertical interngrid is never uniform, so get its coordinates if 3d interngrid
      if (dimCount.ge.3) &
        call ESMF_LocalArrayGetData(coords(3), coord3, ESMF_DATA_COPY, localrc)

      ! Fill in defaults for necessary but optional variables
      allocate(decompIdsUse(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "decompIdsUse", &
                                     ESMF_CONTEXT, rc)) return
      decompIdsUse = 0
      if (present(decompIds)) then
        decompIdsUse = decompIds
      else
        do i = 1, 2
          decompIdsUse(i) = i
        enddo
      endif

      call ESMF_DELayoutGetDeprecated(delayout, dimCount=ndim, oneToOneFlag=otoFlag, &
                            logRectFlag=lrFlag, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Check layout attributes
      ! TODO: this is only interesting for DEs with non-0 interngridcounts.
      !if (otoFlag .ne. ESMF_TRUE) then    ! ensure this is 1-to-1 layout
      !  dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
      !                                "not a 1-to-1 layout", &
      !                                ESMF_CONTEXT, rc)
      !  return
      !endif
      ! if (ndim .ne. 2) then               ! ensure this is 2D Layout
      !  dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
      !                                "not a 2D layout", &
      !                                ESMF_CONTEXT, rc)
      !   return
      ! endif
      ! if (lrFlag .ne. ESMF_TRUE) then     ! ensure this is logical rect layout
      !  dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
      !                                "not a logically rectangular layout", &
      !                                ESMF_CONTEXT, rc)
      !   return
      ! endif

      call ESMF_DELayoutGetDeprecated(delayout, deCountPerDim=nDEs(1:2), rc=localrc)
      nDEs(0) = 1

      ! if there is an axis to decompose, either grab the specfied countsPerDE
      ! or parse the global count
      do i = 1,dimCount
        if (decompIdsUse(i).eq.1) then
          aSize = nDEs(1)
          allocate(countsPerDEDecomp1Use(aSize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "countsPerDEDecomp1Use", &
                                         ESMF_CONTEXT, rc)) return
          if (present(countsPerDEDecomp1)) then
            if (size(countsPerDEDecomp1).lt.aSize) then
              dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_SIZE, &
                           "countsPerDEDecomp1 array not large enough", &
                           ESMF_CONTEXT, rc)
              return
            endif
            if (size(countsPerDEDecomp1).gt.aSize) then
              call ESMF_LogWrite("countsPerDEDecomp1 array larger than layout", &
                                 ESMF_LOG_WARNING)
            endif
            countsPerDEDecomp1Use(1:aSize) = countsPerDEDecomp1(1:aSize)
          else
            call ESMF_LRIGridDecompose(nDEs(1), counts(i), countsPerDEDecomp1Use)
          endif
        endif
        if (decompIdsUse(i).eq.2) then
          aSize = nDEs(2)
          allocate(countsPerDEDecomp2Use(aSize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "countsPerDEDecomp2Use", &
                                         ESMF_CONTEXT, rc)) return
          if (present(countsPerDEDecomp2)) then
            if (size(countsPerDEDecomp2).lt.aSize) then
              dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_SIZE, &
                           "countsPerDEDecomp2 array not large enough", &
                           ESMF_CONTEXT, rc)
              return
            endif
            if (size(countsPerDEDecomp2).gt.aSize) then
              call ESMF_LogWrite("countsPerDEDecomp2 array larger than layout", &
                                 ESMF_LOG_WARNING)
            endif
            countsPerDEDecomp2Use(1:aSize) = countsPerDEDecomp2(1:aSize)
          else
            call ESMF_LRIGridDecompose(nDEs(2), counts(i), countsPerDEDecomp2Use)
          endif
        endif
      enddo

      ! Determine if the axis are decomposed and load counts arrays
      aSize = nDEs(decompIdsUse(1))
      allocate(countsPerDE1(aSize), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "countsPerDE1", &
                                     ESMF_CONTEXT, rc)) return
      if     (decompIdsUse(1).eq.0) then
        countsPerDE1(:) = counts(1)
      elseif (decompIdsUse(1).eq.1) then
        countsPerDE1(:) = countsPerDEDecomp1Use(:)
      elseif (decompIdsUse(1).eq.2) then
        countsPerDE1(:) = countsPerDEDecomp2Use(:)
      endif

      aSize = nDEs(decompIdsUse(2))
      allocate(countsPerDE2(aSize), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "countsPerDE2", &
                                     ESMF_CONTEXT, rc)) return
      if     (decompIdsUse(2).eq.0) then
        countsPerDE2(:) = counts(2)
      elseif (decompIdsUse(2).eq.1) then
        countsPerDE2(:) = countsPerDEDecomp1Use(:)
      elseif (decompIdsUse(2).eq.2) then
        countsPerDE2(:) = countsPerDEDecomp2Use(:)
      endif

      if (dimCount.eq.3) then
        aSize = nDEs(decompIdsUse(3))
        allocate(countsPerDE3(aSize), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "countsPerDE3", &
                                       ESMF_CONTEXT, rc)) return
        if     (decompIdsUse(3).eq.0) then
          countsPerDE3(:) = counts(3)
        elseif (decompIdsUse(3).eq.1) then
          countsPerDE3(:) = countsPerDEDecomp1Use(:)
        elseif (decompIdsUse(3).eq.2) then
          countsPerDE3(:) = countsPerDEDecomp2Use(:)
        endif
      endif

      ! Check to see if this DE has any data associated with it
      call ESMF_DELayoutGetDeprecated(delayout, localDE=localDE, rc=localrc)
      call ESMF_DELayoutGetDELocalInfo(delayout, de=localDE, &
                                       coord=myDEDecomp(1:2), rc=localrc)
      myDEDecomp(0) = 1
      ! modify myDE array by decompIds
      do i = 1,dimCount
        myDE(i) = myDEDecomp(decompIdsUse(i))
      enddo
      myCount = countsPerDE1(myDE(1))*countsPerDE2(myDE(2))
      if (dimCount.eq.3) myCount = myCount*countsPerDE3(myDE(3))
      interngrid%hasLocalData = ESMF_TRUE
      if (myCount.le.0) interngrid%hasLocalData = ESMF_FALSE

      ! Create InternDG and PhysGrid at cell center
      dimCountIGrid = dimCount
      if (dimCount.eq.3) dimCountIGrid = 2
      internDGId = 1
      internDGName = 'cell_center'
      physIGridId = 1
      physIGridName = 'cell_center'
      relloc = ESMF_CELL_CENTER
      call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, counts, &
                                       delayout, decompIdsUse(1:2), &
                                       countsPerDEDim1=countsPerDE1, &
                                       countsPerDEDim2=countsPerDE2, &
                                       periodic=periodic, &
                                       internDGName=internDGName, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, dimCountIGrid, &
                                       decompIdsUse(1:2), coord1, coord2, &
                                       countsPerDE1, countsPerDE2, &
                                       dimNames, dimUnits, physIGridName, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      interngrid%internDGIndex(physIGridId) = internDGId
      internDGId = internDGId + 1 
      physIGridId = physIGridId + 1 

      ! Create any other InternDGs and PhysGrids necessary for horizontal
      ! interngrid stagger
      ! TODO: finish filling out, look up D
      select case (interngrid%horzStagger%stagger)

        ! Arakawa A (centered velocity)
        case (1)

        ! Arakawa B_NE (velocities at NE interngrid corner)
        case (2)
          internDGName = 'cell_necorner'
          physIGridName = 'cell_necorner'
          relloc = ESMF_CELL_NECORNER
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1 
          physIGridId = physIGridId + 1 

        ! Arakawa B_SW (velocities at SW interngrid corner)
        case (3)
          internDGName = 'cell_swcorner'
          physIGridName = 'cell_swcorner'
          relloc = ESMF_CELL_SWCORNER
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa B_SE (velocities at SE interngrid corner)
        case (4)
          internDGName = 'cell_secorner'
          physIGridName = 'cell_secorner'
          relloc = ESMF_CELL_SECORNER
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa B_NW (velocities at NW interngrid corner)
        case (5)
          internDGName = 'cell_nwcorner'
          physIGridName = 'cell_nwcorner'
          relloc = ESMF_CELL_NWCORNER
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa C_NE (U at E face, V at N face) and
        ! Arakawa D_NE (V at E face, U at N face)
        case (6,10)
          internDGName = 'cell_eface'
          physIGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1
          internDGName = 'cell_nface'
          physIGridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa C_SW (U at W face, V at S face) and
        ! Arakawa D_SW (V at W face, U at S face)
        case (7,11)
          internDGName = 'cell_wface'
          physIGridName = 'cell_wface'
          relloc = ESMF_CELL_WFACE
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1
          internDGName = 'cell_sface'
          physIGridName = 'cell_sface'
          relloc = ESMF_CELL_SFACE
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa C_SE (U at E face, V at S face) and
        ! Arakawa D_SE (V at E face, U at S face)
        case (8,12)
          internDGName = 'cell_eface'
          physIGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1
          internDGName = 'cell_sface'
          physIGridName = 'cell_sface'
          relloc = ESMF_CELL_SFACE
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa C_NW (U at W face, V at N face) and
        ! Arakawa D_NW (V at W face, U at N face)
        case (9,13)
          internDGName = 'cell_wface'
          physIGridName = 'cell_wface'
          relloc = ESMF_CELL_WFACE
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1
          internDGName = 'cell_nface'
          physIGridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCountIGrid, &
                                           counts, delayout, decompIdsUse(1:2), &
                                           countsPerDEDim1=countsPerDE1, &
                                           countsPerDEDim2=countsPerDE2, &
                                           periodic=periodic, &
                                           internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                           dimCountIGrid, decompIdsUse(1:2), &
                                           coord1, coord2, countsPerDE1, &
                                           countsPerDE2, dimNames, dimUnits, &
                                           physIGridName, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

      end select

      ! Create vertical PhysGrid if requested
      if (dimCount.eq.3) then
        internDGName = 'vertical center'
        physIGridName = 'vertical center'
        relloc = ESMF_CELL_CELL    ! TODO: right relloc?
        call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, 1, counts(3:3), &
                                         delayout, decompIdsUse(3:3), &
                                         countsPerDEDim1=countsPerDE3, &
                                         periodic=periodic(3:3), &
                                         internDGName=internDGName, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_LRIGridAddVertPhysGrid(interngrid, physIGridId, relloc, coord3, &
                                        countsPerDE3, physIGridName, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        interngrid%internDGIndex(physIGridId) = internDGId
        internDGId = internDGId + 1
        physIGridId = physIGridId + 1

        select case (interngrid%vertStagger%stagger)

          ! ESMF_INTERNGRID_VERT_STAGGER_CENTER - vertical velocity at vertical midpoints
          case (1)

          ! ESMF_INTERNGRID_VERT_STAGGER_TOP - vertical velocity at top vertical face
          case (2)
            internDGName = 'vertical top face'
            physIGridName = 'vertical top face'
            relloc = ESMF_CELL_TOPFACE
            call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, 1, counts(3:3), &
                                             delayout, decompIdsUse(3:3), &
                                             countsPerDEDim1=countsPerDE3, &
                                             periodic=periodic(3:3), &
                                             internDGName=internDGName, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return

            call ESMF_LRIGridAddVertPhysGrid(interngrid, physIGridId, relloc, coord3, &
                                            countsPerDE3, physIGridName, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return

            interngrid%internDGIndex(physIGridId) = internDGId
            internDGId = internDGId + 1
            physIGridId = physIGridId + 1

        ! TODO: add default in case vertical stagger is not defined
        end select
      endif

      ! Create the BoundingBoxes structure
      call ESMF_LRIGridSetBoundBoxesBlock(interngrid, dimCount, coord1, coord2, &
                                         countsPerDE1, countsPerDE2, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Clean up
      deallocate(               counts, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(                   min, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(                   max, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(              dimNames, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(              dimUnits, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(          decompIdsUse, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate( countsPerDEDecomp1Use, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate( countsPerDEDecomp2Use, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(          countsPerDE1, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(          countsPerDE2, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      if (associated(coord1)) then
        deallocate(coord1, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coord1", &
                                       ESMF_CONTEXT, rc)) return
      endif
      if (dimCount.ge.2 .and. associated(coord2)) then
        deallocate(coord2, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coord2", &
                                       ESMF_CONTEXT, rc)) return
      endif
      if (dimCount.ge.3 .and. associated(coord3)) then
        deallocate(coord3, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coord3", &
                                       ESMF_CONTEXT, rc)) return
      endif
      if (dimCount.ge.3) then
        deallocate(countsPerDE3, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating countsPerDE3", &
                                       ESMF_CONTEXT, rc)) return
      endif

      interngrid%interngridStorage = ESMF_INTERNGRID_STORAGE_LOGRECT
      interngrid%interngridStatus  = ESMF_INTERNGRID_STATUS_READY
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridDistributeBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridDistributeArbitrary"
!BOPI
! !IROUTINE: ESMF_LRIGridDistributeArbitrary - Distribute a IGrid as an arbitrary vector of points
! !INTERFACE:
      subroutine ESMF_LRIGridDistributeArbitrary(interngrid, delayout, myCount, &
                                                myIndices, decompIds, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), target :: interngrid
      type(ESMF_DELayout), intent(in) :: delayout
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, dimension(:), intent(in), optional :: decompIds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_IGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_IGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_IGridCreate}, which calls
!     {\tt ESMF\_IGridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid}
!     \item[delayout]
!         {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[myCount]
!          Number of interngrid cells to be distributed to this DE.
!     \item[myIndices]
!          Array of interngrid indices to be distributed to this DE.  The size of this
!          array must be at least [myCount] in the first dimension and 2 in the
!          second.
!     \item[{[decompIds]}]
!          Identifier for which IGrid axes are decomposed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: localrc                          ! Error status
      logical :: dummy
      character(len=ESMF_MAXSTR) :: internDGName, physIGridName
      character(len=ESMF_MAXSTR), dimension(:), allocatable :: dimNames, dimUnits
      integer :: internDGId, physIGridId, nDEs(0:2)
      integer :: i, dimCount, dimCountIGrid, distCount, ndim
      integer, dimension(:), allocatable :: decompIdsUse, counts
      real(ESMF_KIND_R8) :: delta
      real(ESMF_KIND_R8), dimension(:), allocatable :: min, max
      real(ESMF_KIND_R8), dimension(:), pointer :: coord1, coord2, coord3
      type(ESMF_Logical):: otoFlag, lrFlag
      type(ESMF_Logical), dimension(:), pointer :: periodic
      type(ESMF_LocalArray), dimension(:), pointer :: coords
      type(ESMF_RelLoc) :: relloc

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

      ! validate the layout before going any further
      call ESMF_DELayoutValidate(delayout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! do some sanity error checking

      ! Extract some information from the IGrid
      dimCount = interngrid%dimCount
      allocate(  counts(dimCount), &
                    min(dimCount), &
                    max(dimCount), &
               dimNames(dimCount), &
               dimUnits(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Allocating dimCount arrays", &
                                     ESMF_CONTEXT, rc)) return

      ! Either get coordinates or calculate them from IGrid information
      nullify(coords)
      if (associated(interngrid%interngridSpecific%logRectIGrid%coords)) then
        coords => interngrid%interngridSpecific%logRectIGrid%coords
      endif

      do i = 1,dimCount
        counts(i)   = interngrid%interngridSpecific%logRectIGrid%countPerDim(i)
        min(i)      = interngrid%minGlobalCoordPerDim(i)
        max(i)      = interngrid%maxGlobalCoordPerDim(i)
        dimNames(i) = interngrid%dimNames(i)
        dimUnits(i) = interngrid%dimUnits(i)
      enddo
      periodic => interngrid%periodic

      if (interngrid%horzIGridType.eq.ESMF_INTERNGRID_TYPE_LATLON_UNI .OR. &
          interngrid%horzIGridType.eq.ESMF_INTERNGRID_TYPE_XY_UNI) then ! uniform
        allocate(coord1(counts(1)+1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "coord1", &
                                       ESMF_CONTEXT, rc)) return
        coord1(1) = min(1)
        delta     = (max(1) - min(1)) / float(counts(1))
        do i = 1,counts(1)
          coord1(i+1) = coord1(i) + delta
        enddo
        if (dimCount.ge.2) then
          allocate(coord2(counts(2)+1), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "coord2", &
                                         ESMF_CONTEXT, rc)) return
          coord2(1) = min(2)
          delta     = (max(2) - min(2)) / float(counts(2))
          do i = 1,counts(2)
            coord2(i+1) = coord2(i) + delta
          enddo
        endif
      else   ! not uniform
        call ESMF_LocalArrayGetData(coords(1), coord1, ESMF_DATA_COPY, localrc)
        if (dimCount.ge.2) &
          call ESMF_LocalArrayGetData(coords(2), coord2, ESMF_DATA_COPY, localrc)
      endif

      ! the vertical interngrid is never uniform, so get its coordinates if 3d interngrid
      if (dimCount.ge.3) &
        call ESMF_LocalArrayGetData(coords(3), coord3, ESMF_DATA_COPY, localrc)

      ! Fill in defaults for necessary but optional variables
      allocate(decompIdsUse(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "decompIdsUse", &
                                     ESMF_CONTEXT, rc)) return
      decompIdsUse = 0
      if (present(decompIds)) then
        decompIdsUse = decompIds
      else
        do i = 1, 2
          decompIdsUse(i) = i
        enddo
      endif

      call ESMF_DELayoutGetDeprecated(delayout, dimCount=ndim, oneToOneFlag=otoFlag, &
                            logRectFlag=lrFlag, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Check layout attributes
      if (otoFlag .ne. ESMF_TRUE) then    ! ensure this is 1-to-1 layout
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                      "not a 1-to-1 layout", &
                                      ESMF_CONTEXT, rc)
        return
      endif
      ! if (ndim .ne. 2) then               ! ensure this is 2D Layout
      !  dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
      !                                "not a 2D layout", &
      !                                ESMF_CONTEXT, rc)
      !   return
      ! endif
      ! if (lrFlag .ne. ESMF_TRUE) then     ! ensure this is logical rect layout
      !  dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
      !                                "not a logically rectangular layout", &
      !                                ESMF_CONTEXT, rc)
      !   return
      ! endif

      call ESMF_DELayoutGetDeprecated(delayout, deCountPerDim=nDEs(1:2), rc=localrc)
      nDEs(0) = 1

      ! Create InternDG and PhysGrid at cell center
      dimCountIGrid = dimCount
      distCount    = 1
      if (dimCount.eq.3) then
        dimCountIGrid = 2
        distCount    = 2
      endif
      internDGId = 1
      internDGName = 'cell_center'
      physIGridId = 1
      physIGridName = 'cell_center'
      relloc = ESMF_CELL_CENTER
      call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                     myCount, myIndices, counts, delayout, &
                                     decompIdsUse(1:2), &
                                     internDGName=internDGName, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, dimCountIGrid, &
                                     decompIdsUse(1:2), coord1, coord2, &
                                     myCount, myIndices, dimNames, dimUnits, &
                                     physIGridName, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      interngrid%internDGIndex(physIGridId) = internDGId
      internDGId = internDGId + 1 
      physIGridId = physIGridId + 1 

      ! Create any other InternDGs and PhysGrids necessary for horizontal
      ! interngrid stagger
      ! TODO: finish filling out, look up D
      select case (interngrid%horzStagger%stagger)

        ! Arakawa A (centered velocity)
        case (1)

        ! Arakawa B_NE (velocities at NE interngrid corner)
        case (2)
          internDGName = 'cell_necorner'
          physIGridName = 'cell_necorner'
          relloc = ESMF_CELL_NECORNER
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1 
          physIGridId = physIGridId + 1 

        ! Arakawa B_SW (velocities at SW interngrid corner)
        case (3)
          internDGName = 'cell_swcorner'
          physIGridName = 'cell_swcorner'
          relloc = ESMF_CELL_SWCORNER
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa B_SE (velocities at SE interngrid corner)
        case (4)
          internDGName = 'cell_secorner'
          physIGridName = 'cell_secorner'
          relloc = ESMF_CELL_SECORNER
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa B_NW (velocities at NW interngrid corner)
        case (5)
          internDGName = 'cell_nwcorner'
          physIGridName = 'cell_nwcorner'
          relloc = ESMF_CELL_NWCORNER
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa C_NE (U at E face, V at N face) and
        ! Arakawa D_NE (V at E face, U at N face)
        case (6,10)
          internDGName = 'cell_eface'
          physIGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1
          internDGName = 'cell_nface'
          physIGridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa C_SW (U at W face, V at S face) and
        ! Arakawa D_SW (V at W face, U at S face)
        case (7,11)
          internDGName = 'cell_wface'
          physIGridName = 'cell_wface'
          relloc = ESMF_CELL_WFACE
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1
          internDGName = 'cell_sface'
          physIGridName = 'cell_sface'
          relloc = ESMF_CELL_SFACE
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa C_SE (U at E face, V at S face) and
        ! Arakawa D_SE (V at E face, U at S face)
        case (8,12)
          internDGName = 'cell_eface'
          physIGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1
          internDGName = 'cell_sface'
          physIGridName = 'cell_sface'
          relloc = ESMF_CELL_SFACE
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

        ! Arakawa C_NW (U at W face, V at N face) and
        ! Arakawa D_NW (V at W face, U at N face)
        case (9,13)
          internDGName = 'cell_wface'
          physIGridName = 'cell_wface'
          relloc = ESMF_CELL_WFACE
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1
          internDGName = 'cell_nface'
          physIGridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_LRIGridAddInternDGArb(interngrid, internDGId, distCount, &
                                         myCount, myIndices, counts, delayout, &
                                         decompIdsUse(1:2), &
                                         internDGName=internDGName, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, &
                                         dimCountIGrid, decompIdsUse(1:2), &
                                         coord1, coord2, myCount, myIndices, &
                                         dimNames, dimUnits, physIGridName, &
                                         localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          interngrid%internDGIndex(physIGridId) = internDGId
          internDGId = internDGId + 1
          physIGridId = physIGridId + 1

      end select

      ! Create vertical PhysGrid if requested
      if (dimCount.eq.3) then
        internDGName = 'vertical center'
        physIGridName = 'vertical center'
        relloc = ESMF_CELL_CELL    ! TODO: right relloc?
        call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, 1, counts(3:3), &
                                         delayout, decompIdsUse(3:3), &
                                         countsPerDEDim1=counts(3:3), &
                                         internDGName=internDGName, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_LRIGridAddVertPhysGrid(interngrid, physIGridId, relloc, coord3, &
                                        counts(3:3), physIGridName, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        interngrid%internDGIndex(physIGridId) = internDGId
        internDGId = internDGId + 1
        physIGridId = physIGridId + 1

        select case (interngrid%vertStagger%stagger)

          ! ESMF_INTERNGRID_VERT_STAGGER_CENTER - vertical velocity at vertical midpoints
          case (1)

          ! ESMF_INTERNGRID_VERT_STAGGER_TOP - vertical velocity at top vertical face
          case (2)
            internDGName = 'vertical top face'
            physIGridName = 'vertical top face'
            relloc = ESMF_CELL_TOPFACE
            call ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, 1, counts(3:3), &
                                             delayout, decompIdsUse(3:3), &
                                             countsPerDEDim1=counts(3:3), &
                                             internDGName=internDGName, &
                                             rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return

            call ESMF_LRIGridAddVertPhysGrid(interngrid, physIGridId, relloc, coord3, &
                                            counts(3:3), physIGridName, localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return

            interngrid%internDGIndex(physIGridId) = internDGId
            internDGId = internDGId + 1
            physIGridId = physIGridId + 1

        ! TODO: add default in case vertical stagger is not defined
        end select
      endif

      ! Create the BoundingBoxes structure
      call ESMF_LRIGridSetBoundBoxesArb(interngrid, dimCount, coord1, coord2, &
                                       myCount, myIndices, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Clean up
      deallocate(               counts, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(                   min, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(                   max, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(              dimNames, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(              dimUnits, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(          decompIdsUse, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return
      if (associated(coord1)) then
        deallocate(coord1, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coord1", &
                                       ESMF_CONTEXT, rc)) return
      endif
      if (dimCount.ge.2 .and. associated(coord2)) then
        deallocate(coord2, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coord2", &
                                       ESMF_CONTEXT, rc)) return
      endif
      if (dimCount.ge.3 .and. associated(coord3)) then
        deallocate(coord3, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coord3", &
                                       ESMF_CONTEXT, rc)) return
      endif

      interngrid%interngridStorage = ESMF_INTERNGRID_STORAGE_ARBITRARY
      interngrid%interngridStatus  = ESMF_INTERNGRID_STATUS_READY
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridDistributeArbitrary

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridDestruct"
!BOPI
! !IROUTINE: ESMF_LRIGridDestruct - Free all resources associated with a IGrid

! !INTERFACE:
      subroutine ESMF_LRIGridDestruct(interngrid, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: interngrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_IGridConstruct}, does any additional cleanup before the
!     original {\tt ESMF\_Gri} object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_IGridDestroy}, which calls
!     {\tt ESMF\_LRIGridDestruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          The class to be destroyed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: n

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! If IGrid is unitialized, return with warning
      if (interngrid%interngridStatus.eq.ESMF_INTERNGRID_STATUS_UNINIT) then
        call ESMF_LogWrite("destroying uninitialized interngrid", ESMF_LOG_WARNING, &
                           ESMF_CONTEXT)
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      if (interngrid%interngridStatus.eq.ESMF_INTERNGRID_STATUS_READY) then
        do n = 1,interngrid%numPhysGrids
          call ESMF_PhysGridDestroy(interngrid%physgrids(n), rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        end do
        interngrid%numPhysGridsAlloc = 0
        deallocate(interngrid%physgrids, &
                   interngrid%internDGIndex, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating physgrids", &
                                       ESMF_CONTEXT, rc)) return

        do n = 1,interngrid%numInternDGs
          call ESMF_InternDGDestroy(interngrid%interndgs(n), rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        end do
        interngrid%numInternDGsAlloc = 0
        deallocate(interngrid%interndgs, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating interndgs", &
                                       ESMF_CONTEXT, rc)) return

        interngrid%minGlobalCoordPerDim(:) = 0
        interngrid%maxGlobalCoordPerDim(:) = 0

        call ESMF_LocalArrayDestroy(interngrid%boundingBoxes, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      !TODO: destruct these?
      !  type (ESMF_Base) :: base
      !  type (ESMF_Status) :: interngridStatus
      interngrid%horzIGridType    = ESMF_INTERNGRID_TYPE_UNKNOWN
      interngrid%vertIGridType    = ESMF_INTERNGRID_VERT_TYPE_UNKNOWN
      interngrid%horzStagger     = ESMF_INTERNGRID_HORZ_STAGGER_UNKNOWN
      interngrid%vertStagger     = ESMF_INTERNGRID_VERT_STAGGER_UNKNOWN
      interngrid%horzCoordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      interngrid%vertCoordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      interngrid%coordOrder      = ESMF_COORD_ORDER_UNKNOWN
      interngrid%periodic        = ESMF_FALSE
      interngrid%numPhysGrids    = 0
      interngrid%numInternDGs    = 0

      ! Set interngrid as deleted 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_DELETED(interngrid)
      
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridDestruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridAddInternDGBlock"
!BOPI
! !IROUTINE: ESMF_LRIGridAddInternDGBlock - Add a InternDG to a LogRectIGrid with block storage

! !INTERFACE:
      subroutine ESMF_LRIGridAddInternDGBlock(interngrid, internDGId, dimCount, &
                                             counts, delayout, decompIds, &
                                             countsPerDEDim1, countsPerDEDim2, &
                                             periodic, coversDomain, &
                                             internDGName, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), target :: interngrid
      integer, intent(out) :: internDGId
      integer, intent(in) :: dimCount 
      integer, dimension(dimCount), intent(in) :: counts
      type (ESMF_DELayout), intent(in) :: delayout
      integer, dimension(dimCount), intent(in) :: decompIds
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      type(ESMF_Logical), dimension(dimCount), intent(in), optional :: periodic
      type(ESMF_Logical), dimension(dimCount), intent(in), optional :: &
                                                     coversDomain
      character (len=*), intent(in), optional :: internDGName
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_InternDG} to a {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_IGrid} to add {\tt InternDG} to.
!     \item[internDGId]
!          Integer identifier for {\tt ESMF\_InternDG}.
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[counts]
!          Array of number of computational cells in each direction.
!     \item[delayout]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[decompIDs]}]
!          Identifier for which IGrid axes are decomposed.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[coversDomain]}]
!          Logical specifier (array) to denote if the InternDG covers the entire
!          physical domain in each direction.
!     \item[{[countsPerDEDim1]}]
!          Array of number of interngrid increments per DE in the first
!          decomposition direction.
!     \item[{[countsPerDEDim2]}]
!          Array of number of interngrid increments per DE in the second
!          decomposition direction.
!     \item[{[internDGName]}]
!          {\tt ESMF\_InternDG} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      type(ESMF_InternDG) :: internDG

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

      ! Create the InternDG
      internDG = ESMF_InternDGCreate(dimCount=dimCount, counts=counts, &
                                     delayout=delayout, decompIDs=decompIds, &
                                     countsPerDEDim1=countsPerDEDim1, &
                                     countsPerDEDim2=countsPerDEDim2, &
                                     periodic=periodic, &
                                     coversDomain=coversDomain, &
                                     name=internDGName, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! now that it's created, add the interndg to the interngrid
      call ESMF_IGridAddInternDG(interngrid, internDG, localrc)
      internDGId = interngrid%numInternDGs

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridAddInternDGBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridAddInternDGArb"
!BOPI
! !IROUTINE: ESMF_LRIGridAddInternDGArb - Add a InternDG to a LogRectIGrid with arbitrary storage

! !INTERFACE:
      subroutine ESMF_LRIGridAddInternDGArb(interngrid, internDGId, dimCount, &
                                           myCount, myIndices, counts, delayout, &
                                           decompIds, internDGName, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), target :: interngrid
      integer, intent(out) :: internDGId
      integer, intent(in) :: dimCount 
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, dimension(:), intent(in) :: counts
      type (ESMF_DELayout), intent(in) :: delayout
      integer, dimension(dimCount), intent(in), optional :: decompIds
      character (len=*), intent(in), optional :: internDGName
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_InternDG} to a {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_IGrid} to add {\tt InternDG} to.
!     \item[internDGId]
!          Integer identifier for {\tt ESMF\_InternDG}.
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[myCount]
!          Number of computational cells on this DE.
!     \item[myIndices]
!          Array of interngrid indices to be distributed to this DE.  The size of this
!          array must be at least [myCount] in the first dimension and 2 in the
!          second.
!     \item[counts]
!          Number of computational cells in the global 2D IGrid.
!     \item[delayout]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[decompIDs]
!          Identifier for which IGrid axes are decomposed.
!     \item[{[internDGName]}]
!          {\tt ESMF\_InternDG} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      type(ESMF_InternDG) :: internDG

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit,delayout,rc)

      ! Create the InternDG
      internDG = ESMF_InternDGCreate(dimCount, myCount, myIndices, &
                                     counts, delayout, decompIds, &
                                     internDGName, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! now that it's created, add the interndg to the interngrid
      call ESMF_IGridAddInternDG(interngrid, internDG, localrc)
      internDGId = interngrid%numInternDGs

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridAddInternDGArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridAddPhysGridBlock"
!BOPI
! !IROUTINE: ESMF_LRIGridAddPhysGridBlock - Add a PhysGrid to a LogRectIGrid with block storage

! !INTERFACE:
      subroutine ESMF_LRIGridAddPhysGridBlock(interngrid, physIGridId, relloc, &
                                             dimCount, decompIds, &
                                             coord1, coord2, &
                                             countsPerDEDim1, countsPerDEDim2, &
                                             dimNames, dimUnits, &
                                             physIGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), target :: interngrid
      integer, intent(out) :: physIGridId
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(in) :: dimCount 
      integer, dimension(:), intent(in) :: decompIds
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in) :: countsPerDEDim2
      character (len=*), dimension(:), intent(in), optional :: dimNames
      character (len=*), dimension(:), intent(in), optional :: dimUnits
      character (len=*), intent(in), optional :: physIGridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysGrid} to a {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_IGrid} to add {\tt PhysGrid} to.
!     \item[physIGridId]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[relloc]
!          Relative location of data at the centers, faces, and vertices of
!          the {\tt ESMF\_IGrid}.
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[decompIds]
!          Identifier for which IGrid axes are decomposed.
!     \item[coord1]
!          Array of physical coordinates in the first direction.
!     \item[coord2]
!          Array of physical coordinates in the second direction.
!     \item[countsPerDEDim1]
!          Array of number of interngrid increments per DE in the x-direction.
!     \item[countsPerDEDim2]
!          Array of number of interngrid increments per DE in the y-direction.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item [{[physIGridName]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, j, i1, i2, j1, j2, interngridBoundWidth, myDE(2), myDEDecomp(0:2)
      integer :: localDE
      integer, dimension(dimCount) :: counts, compCount, localStart
      integer, dimension(:), allocatable :: cellType1, cellType2
      character(len=ESMF_MAXSTR), dimension(dimCount) :: coordNames, coordUnits
      logical :: dummy
      logical, dimension(dimCount) :: coordAligned, coordEqualSpaced, coordCyclic
      real(ESMF_KIND_R8), dimension(dimCount) :: localMin, localMax
      real(ESMF_KIND_R8), dimension(:), allocatable :: coordUse1, coordUse2
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_CoordType), dimension(dimCount) :: coordType
      type(ESMF_DELayout) :: delayout
      type(ESMF_IGrid) :: interngridp
      type(ESMF_PhysGrid) :: physIGrid
      type(ESMF_PhysCoord) :: tempCoord

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! initialize some values
      interngridBoundWidth = 1   ! TODO: move into structure, make input?

      ! create temporary interngrid to pass into query subroutines
      interngridp%ptr => interngrid
      call ESMF_IGridSetInitCreated(interngridp, rc)

      ! figure out the position of myDE to get local counts
      call ESMF_LRIGridGet(interngridp, delayout=delayout)
      call ESMF_DELayoutGetDeprecated(delayout, localDE=localDE, rc=localrc)
      call ESMF_DELayoutGetDELocalInfo(delayout, de=localDE, &
                                       coord=myDEDecomp(1:2), rc=localrc)
      myDEDecomp(0) = 1

      ! modify myDE array by decompIds
      do i = 1,dimCount
        myDE(i) = myDEDecomp(decompIds(i))
      enddo

      localMin(1) = 0.0
      localMin(2) = 0.0
      localStart(1) = 0
      localStart(2) = 0
      if (interngrid%hasLocalData .eq. ESMF_TRUE) then

        ! set local starts for this DE
        if (myDE(1).ge.2) then
          do j = 1,myDE(1)-1
            localStart(1) = localStart(1) + countsPerDEDim1(j)
          enddo
        endif
        if (myDE(2).ge.2) then
          do j = 1,myDE(2)-1
            localStart(2) = localStart(2) + countsPerDEDim2(j)
          enddo
        endif

        ! modify global counts to include ghost region
        compCount(1) = size(coord1)
        compCount(2) = size(coord2)
        counts(1) = compCount(1) + 2*interngridBoundWidth
        counts(2) = compCount(2) + 2*interngridBoundWidth

        ! allocate and load coords
        allocate(coordUse1(counts(1)), &
                 coordUse2(counts(2)), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "coordUse arrays", &
                                       ESMF_CONTEXT, rc)) return

        do i = 1,compCount(1)
          coordUse1(i+interngridBoundWidth) = coord1(i)
        enddo
        do i = interngridBoundWidth,1,-1
          coordUse1(i) = coordUse1(i+1) - (coord1(2)-coord1(1))
        enddo
        do i = compCount(1)+interngridBoundWidth,compCount(1)+2*interngridBoundWidth-1
          coordUse1(i+1) = coordUse1(i) &
                         + (coord1(compCount(1))-coord1(compCount(1)-1))
        enddo
        do i = 1,compCount(2)
          coordUse2(i+interngridBoundWidth) = coord2(i)
        enddo
        do i = interngridBoundWidth,1,-1
          coordUse2(i) = coordUse2(i+1) - (coord2(2)-coord2(1))
        enddo
        do i = compCount(2)+interngridBoundWidth,compCount(2)+2*interngridBoundWidth-1
          coordUse2(i+1) = coordUse2(i) &
                         + (coord2(compCount(2))-coord2(compCount(2)-1))
        enddo

        ! allocate and load cell type masks -- these are by cell and not vertex,
        ! so the counts are all one less
        allocate(cellType1(counts(1)-1), &
                 cellType2(counts(2)-1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "cellType arrays", &
                                       ESMF_CONTEXT, rc)) return
        cellType1 = 0
        cellType2 = 0
        do i = 1,interngridBoundWidth
          cellType1(i) = 1
          cellType2(i) = 1
          cellType1(compCount(1)-1+interngridBoundWidth+i) = 1
          cellType2(compCount(2)-1+interngridBoundWidth+i) = 1
        enddo
      endif

      ! set parameters based on interngrid type
      select case (interngrid%horzIGridType%interngridType)

        ! ESMF_INTERNGRID_TYPE_LATLON
        case (1)
          coordSystem         = ESMF_COORD_SYSTEM_SPHERICAL
          coordNames(1)       = 'latitude'
          coordNames(2)       = 'longitude'
          coordType(1)        = ESMF_COORD_TYPE_LAT
          coordType(2)        = ESMF_COORD_TYPE_LON
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .false.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_INTERNGRID_TYPE_LATLON_UNI
        case (2)
          coordSystem         = ESMF_COORD_SYSTEM_SPHERICAL
          coordNames(1)       = 'latitude'
          coordNames(2)       = 'longitude'
          coordType(1)        = ESMF_COORD_TYPE_LAT
          coordType(2)        = ESMF_COORD_TYPE_LON
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .true.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_INTERNGRID_TYPE_XY
        case (8)
          coordSystem         = ESMF_COORD_SYSTEM_CARTESIAN
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordType(1)        = ESMF_COORD_TYPE_X
          coordType(2)        = ESMF_COORD_TYPE_Y
          coordUnits(:)       = ''
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .false.
          coordCyclic(:)      = .false.

        ! ESMF_INTERNGRID_TYPE_XY_UNI
        case (9)
          coordSystem         = ESMF_COORD_SYSTEM_CARTESIAN
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordType(1)        = ESMF_COORD_TYPE_X
          coordType(2)        = ESMF_COORD_TYPE_Y
          coordUnits(:)       = ''
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .true.
          coordCyclic(:)      = .false.

        case default
          dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                        "Unsupported IGrid Type", &
                                        ESMF_CONTEXT, rc)
          return

      end select

      ! Create the actual PhysGrid object
      physIGrid = ESMF_PhysGridCreate(dimCount, relloc, physIGridName, &
                                     coordSystem, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (interngrid%hasLocalData .eq. ESMF_TRUE) then
        ! set coordinate min/max using total cell count -- but don't include the halo
        ! region around the global interngrid
        counts(1) = countsPerDEDim1(myDE(1)) + 2*interngridBoundWidth
        counts(2) = countsPerDEDim2(myDE(2)) + 2*interngridBoundWidth
        i1 = localStart(1) + 1
        i2 = localStart(1) + counts(1) + 1
        j1 = localStart(2) + 1
        j2 = localStart(2) + counts(2) + 1
        i1 = max(i1,interngridBoundWidth + 1)
        j1 = max(j1,interngridBoundWidth + 1)
        i2 = min(i2,interngridBoundWidth + compCount(1))
        j2 = min(j2,interngridBoundWidth + compCount(2))
        localMin(1) = minval(coordUse1(i1:i2))
        localMax(1) = maxval(coordUse1(i1:i2))
        localMin(2) = minval(coordUse2(j1:j2))
        localMax(2) = maxval(coordUse2(j1:j2))
        do i = 1,dimCount
          tempCoord = ESMF_PhysCoordCreate(coordType(i), coordNames(i), &
                                           coordUnits(i), &
                                           coordAligned(i), coordEqualSpaced(i), &
                                           coordCyclic(i), localMin(i), &
                                           localMax(i), rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          call ESMF_PhysGridSetCoord(physIGrid, tempCoord, dimOrder=i, rc=localrc) 
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        enddo
      endif

      ! now that it's created, add the physgrid to the interngrid
      call ESMF_IGridAddPhysGrid(interngrid, physIGrid, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      physIGridId = interngrid%numPhysGrids

      if (interngrid%hasLocalData .eq. ESMF_TRUE) then
        ! set coordinates using total cell count
        counts(1) = countsPerDEDim1(myDE(1)) + 2*interngridBoundWidth
        counts(2) = countsPerDEDim2(myDE(2)) + 2*interngridBoundWidth
        i1 = localStart(1) + 1
        i2 = localStart(1) + counts(1) + 1
        j1 = localStart(2) + 1
        j2 = localStart(2) + counts(2) + 1
        if ((size(coordUse1) .lt. (i2-i1+1)) .or. &
            (lbound(coordUse1, 1) .gt. i1) .or. &
            (ubound(coordUse1, 1) .lt. i2)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
                         "not enough I vertex coordinates for I cell counts", &
                                     ESMF_CONTEXT, rc)
            return
         
        endif
        if ((size(coordUse2) .lt. (j2-j1+1)) .or. &
            (lbound(coordUse2, 1) .gt. j1) .or. &
            (ubound(coordUse2, 1) .lt. j2)) then
            call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
                         "not enough J vertex coordinates for J cell counts", &
                                     ESMF_CONTEXT, rc)
            return
        endif
        call ESMF_LRIGridSetCoord(interngrid, physIGridId, dimCount, counts, &
                                 interngridBoundWidth, relloc, coordUse1(i1:i2), &
                                 coordUse2(j1:j2), total=.true., rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! set coordinates using computational cell count
        counts(1) = countsPerDEDim1(myDE(1))
        counts(2) = countsPerDEDim2(myDE(2))
        i1 = localStart(1) + 1 + interngridBoundWidth
        i2 = localStart(1) + counts(1) + 1 + interngridBoundWidth
        j1 = localStart(2) + 1 + interngridBoundWidth
        j2 = localStart(2) + counts(2) + 1 + interngridBoundWidth
        call ESMF_LRIGridSetCoord(interngrid, physIGridId, dimCount, counts, &
                                 interngridBoundWidth, relloc, &
                                 coordUse1(i1:i2), coordUse2(j1:j2), &
                                 total=.false., rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! set mask using total cell count -- but masks are cell centered instead of
        ! on the vertices
        counts(1) = countsPerDEDim1(myDE(1)) + 2*interngridBoundWidth
        counts(2) = countsPerDEDim2(myDE(2)) + 2*interngridBoundWidth
        i1 = localStart(1) + 1
        i2 = localStart(1) + counts(1)
        j1 = localStart(2) + 1
        j2 = localStart(2) + counts(2)
        call ESMF_LRIGridSetCellMask(interngrid, physIGridId, dimCount, counts, &
                                    interngridBoundWidth, relloc, cellType1(i1:i2), &
                                    cellType2(j1:j2), localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        deallocate(coordUse1, &
                   coordUse2, &
                   cellType1, &
                   cellType2, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridAddPhysGridBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridAddPhysGridArb"
!BOPI
! !IROUTINE: ESMF_LRIGridAddPhysGridArb - Add a PhysGrid to a LogRectIGrid with arbitrary storage

! !INTERFACE:
      subroutine ESMF_LRIGridAddPhysGridArb(interngrid, physIGridId, relloc, dimCount, &
                                           decompIds, coord1, coord2, &
                                           myCount, myIndices, dimNames, &
                                           dimUnits, physIGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), target :: interngrid
      integer, intent(out) :: physIGridId
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(in) :: dimCount 
      integer, dimension(:), intent(in) :: decompIds
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      character (len=*), dimension(:), intent(in), optional :: dimNames
      character (len=*), dimension(:), intent(in), optional :: dimUnits
      character (len=*), intent(in), optional :: physIGridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysGrid} to a {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          {\tt ESMF\_IGrid} to add {\tt PhysGrid} to.
!     \item[physIGridId]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[relloc]
!          Relative location of data at the centers, faces, and vertices of
!          the {\tt ESMF\_IGrid}.
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[decompIds]
!          Identifier for which IGrid axes are decomposed.
!     \item[coord1]
!          Array of physical coordinates in the first direction.
!     \item[coord2]
!          Array of physical coordinates in the second direction.
!     \item[myCount]
!          Number of interngrid increments for this DE.
!     \item[myIndices]
!          Array of interngrid indices to be distributed to this DE.  The size of this
!          array must be at least [myCount] in the first dimension and 2 in the
!          second.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item [{[physIGridName]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, i1, j1, interngridBoundWidth
      integer, dimension(dimCount) :: counts, compCount
      integer, dimension(:), allocatable :: cellType
      character(len=ESMF_MAXSTR), dimension(dimCount) :: coordNames, coordUnits
      logical :: dummy
      logical, dimension(dimCount) :: coordAligned, coordEqualSpaced, coordCyclic
      real(ESMF_KIND_R8), dimension(dimCount) :: localMin, localMax
      real(ESMF_KIND_R8), dimension(:), allocatable :: coordUse1, coordUse2
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_CoordType), dimension(dimCount) :: coordType
      type(ESMF_IGrid) :: interngridp
      type(ESMF_PhysGrid) :: physIGrid
      type(ESMF_PhysCoord) :: tempCoord

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! create temporary interngrid to pass into subroutines
      interngridp%ptr => interngrid
      call ESMF_IGridSetInitCreated(interngridp, rc)

      ! initialize some values
      interngridBoundWidth = 1   ! TODO: move into structure, make input?

      ! interngridBoundWidth is not fully implemented for arbitrary interngrid.  It
      ! is not used to set up a totalArray for the physIGrid as did in
      ! ESMF_LRIGridAddPhysGridBlock.  Moreover, being set to 1 results in
      ! shifting the interngrid coordinates by 1 to the right, thus causing 
      ! ESMF_IGridGetCoord() returning wrong values.
      !  **PLi (10/11/2006)**
      ! interngridBoundWidth = 0
      !
      ! The extra coordinate boundary layer obtained by setting
      ! interngridBoundWidth=1 is used in ESMF_LRIGridSetCoord to set some 
      ! of the corner locations, so leave interngridBoundWidth=1, and
      ! instead offset the myIndices values. - Bob Oehmke 10/13/2006 
      interngridBoundWidth = 1

      localMin =  99999999.
      localMax = -99999999.

      ! loop over "my" cells and calculate local mins and maxs
      do i = 1,myCount
        i1 = myIndices(i,1)
        j1 = myIndices(i,2)
        if (coord1(i1  ).lt.localMin(1)) localMin(1) = coord1(i1  )
        if (coord1(i1+1).lt.localMin(1)) localMin(1) = coord1(i1+1)
        if (coord2(j1  ).lt.localMin(2)) localMin(2) = coord2(j1  )
        if (coord2(j1+1).lt.localMin(2)) localMin(2) = coord2(j1+1)
        if (coord1(i1  ).gt.localMax(1)) localMax(1) = coord1(i1  )
        if (coord1(i1+1).gt.localMax(1)) localMax(1) = coord1(i1+1)
        if (coord2(j1  ).gt.localMax(2)) localMax(2) = coord2(j1  )
        if (coord2(j1+1).gt.localMax(2)) localMax(2) = coord2(j1+1)
      enddo

      ! modify global counts to include ghost region
      compCount(1) = size(coord1)
      compCount(2) = size(coord2)
      counts(1) = compCount(1) + 2*interngridBoundWidth
      counts(2) = compCount(2) + 2*interngridBoundWidth

      ! allocate and load coords for use, which includes calculating the coordinates
      ! in the halo cells which might be needed for certain staggerings
      allocate(coordUse1(counts(1)), &
               coordUse2(counts(2)), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "coordUse arrays", &
                                     ESMF_CONTEXT, rc)) return

      do i = 1,compCount(1)
        coordUse1(i+interngridBoundWidth) = coord1(i)
      enddo
      do i = interngridBoundWidth,1,-1
        coordUse1(i) = coordUse1(i+1) - (coord1(2)-coord1(1))
      enddo
      do i = compCount(1)+interngridBoundWidth,compCount(1)+2*interngridBoundWidth-1
        coordUse1(i+1) = coordUse1(i) &
                       + (coord1(compCount(1))-coord1(compCount(1)-1))
      enddo
      do i = 1,compCount(2)
        coordUse2(i+interngridBoundWidth) = coord2(i)
      enddo
      do i = interngridBoundWidth,1,-1
        coordUse2(i) = coordUse2(i+1) - (coord2(2)-coord2(1))
      enddo
      do i = compCount(2)+interngridBoundWidth,compCount(2)+2*interngridBoundWidth-1
        coordUse2(i+1) = coordUse2(i) &
                       + (coord2(compCount(2))-coord2(compCount(2)-1))
      enddo

      ! allocate and load cell type masks
      allocate(cellType(myCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "cellType arrays", &
                                     ESMF_CONTEXT, rc)) return
      cellType = 0

      ! set parameters based on interngrid type
      select case (interngrid%horzIGridType%interngridType)

        ! ESMF_INTERNGRID_TYPE_LATLON
        case (1)
          coordSystem         = ESMF_COORD_SYSTEM_SPHERICAL
          coordNames(1)       = 'latitude'
          coordNames(2)       = 'longitude'
          coordType(1)        = ESMF_COORD_TYPE_LAT
          coordType(2)        = ESMF_COORD_TYPE_LON
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .false.
          coordEqualSpaced(:) = .false.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_INTERNGRID_TYPE_LATLON_UNI
        case (2)
          coordSystem         = ESMF_COORD_SYSTEM_SPHERICAL
          coordNames(1)       = 'latitude'
          coordNames(2)       = 'longitude'
          coordType(1)        = ESMF_COORD_TYPE_LAT
          coordType(2)        = ESMF_COORD_TYPE_LON
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .false.
          coordEqualSpaced(:) = .true.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_INTERNGRID_TYPE_XY
        case (8)
          coordSystem         = ESMF_COORD_SYSTEM_CARTESIAN
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordType(1)        = ESMF_COORD_TYPE_X
          coordType(2)        = ESMF_COORD_TYPE_Y
          coordUnits(:)       = ''
          coordAligned(:)     = .false.
          coordEqualSpaced(:) = .false.
          coordCyclic(:)      = .false.

        ! ESMF_INTERNGRID_TYPE_XY_UNI
        case (9)
          coordSystem         = ESMF_COORD_SYSTEM_CARTESIAN
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordType(1)        = ESMF_COORD_TYPE_X
          coordType(2)        = ESMF_COORD_TYPE_Y
          coordUnits(:)       = ''
          coordAligned(:)     = .false.
          coordEqualSpaced(:) = .true.
          coordCyclic(:)      = .false.

        case default
          dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                        "Unsupported IGrid Type", &
                                        ESMF_CONTEXT, rc)
          return

      end select

      ! Create the actual PhysGrid object
      physIGrid = ESMF_PhysGridCreate(dimCount, relloc, physIGridName, &
                                     coordSystem, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      do i = 1,dimCount
        tempCoord = ESMF_PhysCoordCreate(coordType(i), coordNames(i), &
                                         coordUnits(i), &
                                         coordAligned(i), coordEqualSpaced(i), &
                                         coordCyclic(i), localMin(i), &
                                         localMax(i), rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_PhysGridSetCoord(physIGrid, tempCoord, dimOrder=i, rc=localrc) 
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      enddo

      ! now that it's created, add the physgrid to the interngrid
      call ESMF_IGridAddPhysGrid(interngrid, physIGrid, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      physIGridId = interngrid%numPhysGrids

      ! set coordinates
      call ESMF_LRIGridSetCoord(interngrid, physIGridId, dimCount, myCount, &
                               myIndices,interngridBoundWidth, relloc,    &
                               coordUse1, coordUse2, &
                               total=.true., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_LRIGridSetCoord(interngrid, physIGridId, dimCount, myCount, &
                               myIndices, interngridBoundWidth, relloc,   &
                               coordUse1, coordUse2, &
                               total=.false., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! set mask
      call ESMF_LRIGridSetCellMask(interngrid, physIGridId, dimCount, myCount, &
                                  relloc, cellType, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      deallocate(cellType, &
                 coordUse1, &
                 coordUse2, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridAddPhysGridArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridAddVertPhysGrid"
!BOPI
! !IROUTINE: ESMF_LRIGridAddVertPhysGrid - Add a vertical PhysGrid to a LogRectIGrid

! !INTERFACE:
      subroutine ESMF_LRIGridAddVertPhysGrid(interngrid, physIGridId, relloc, coord, &
                                            countsPerDEDim, physIGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), target :: interngrid
      integer, intent(out) :: physIGridId
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord
      integer, dimension(:), intent(in) :: countsPerDEDim
      character (len=*), intent(in), optional :: physIGridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a vertical {\tt ESMF\_PhysGrid} to a {\tt ESMF\_IGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item [physIGridId]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[relloc]
!          Relative location of data at the centers, faces, and vertices of
!          the {\tt ESMF\_IGrid}.
!     \item[coord]
!          Array of physical coordinates in the vertical direction.
!     \item[countsPerDEDim]
!          Array of number of interngrid increments per DE in the vertical direction.
!     \item [{[physIGridName]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, i1, i2, interngridBoundWidth 
      !integer :: myDE(0:2)
      character(len=ESMF_MAXSTR) :: coordName, coordUnit
      logical :: coordAligned, coordEqualSpaced, coordCyclic
      integer :: count, compCount, localStart
      integer, dimension(1) :: localCount
      integer, dimension(:), allocatable :: cellType
      real(ESMF_KIND_R8) :: localMinCoord, localMaxCoord
      real(ESMF_KIND_R8), dimension(:), allocatable :: coordUse
      type(ESMF_CoordType) :: coordType
      !type(ESMF_DELayout) :: delayout
      type(ESMF_PhysCoord) :: tempCoord
      !type(ESMF_IGrid) :: interngridp
      type(ESMF_PhysGrid) :: physIGrid
      type(ESMF_CoordSystem) :: coordSystem

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! initialize some values
      interngridBoundWidth = 1   ! TODO: move into structure, make input?

#if 0
! gjt: I took this out because it seems that it is not used...
      ! figure out the position of myDE to get local counts
      ! create temporary interngrid to pass into subroutines
      interngridp%ptr => interngrid
      call ESMF_IGridSetInitCreated(interngridp, rc)

      call ESMF_IGridGetDELayout(interngridp, delayout, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_DELayoutGetDeprecated(layout, deCountPerDim=myDE, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

     ! TODO: make Fortran interface get come back in F90 style
      nDEs(2) = nDEs(1)                        
      nDEs(1) = nDEs(0)
      nDEs(0) = 1
#endif

      localMinCoord = 0.0
      localStart    = 0
  !    if (myDE(1).ge.2) then    TODO: modify to use decompId to indirect address
  !      do j = 1,myDE(1)-1
  !        localStart = localStart + countsPerDEDim(j)
  !      enddo
  !    endif
      i1 = localStart + 1
  !    i2 = localStart + countsPerDEDim(myDE(1))
      i2 = localStart + countsPerDEDim(1)      ! for now
      localMinCoord = minval(coord(i1:i2))
      localMaxCoord = maxval(coord(i1:i2))

      ! modify global counts to include ghost region
      compCount = size(coord)
      count     = compCount + 2*interngridBoundWidth

      ! allocate and load coords
      allocate(coordUse(count), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "coordUse", &
                                     ESMF_CONTEXT, rc)) return

      do i = 1,compCount
        coordUse(i+interngridBoundWidth) = coord(i)
      enddo
      do i = interngridBoundWidth,1,-1
        coordUse(i) = coordUse(i+1) - (coord(2)-coord(1))
      enddo
      do i = compCount+interngridBoundWidth,compCount+2*interngridBoundWidth-1
        coordUse(i+1) = coordUse(i) &
                      + (coord(compCount)-coord(compCount-1))
      enddo

      ! allocate and load cell type masks
      allocate(cellType(count-1), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "cellType", &
                                     ESMF_CONTEXT, rc)) return
      cellType = 0
      do i = 1,interngridBoundWidth
        cellType(i) = 1
        cellType(compCount-1+interngridBoundWidth+i) = 1
      enddo

      ! set parameters based on vertical coordinate system  TODO: better as a case
      if (interngrid%vertCoordSystem .eq. ESMF_COORD_SYSTEM_DEPTH) then
        ! ESMF_CoordSystem_Depth
        coordSystem      = ESMF_COORD_SYSTEM_DEPTH
        coordName        = 'depth'
        coordType        = ESMF_COORD_TYPE_DEPTH
        coordUnit        = ''
        coordAligned     = .true.
        coordEqualSpaced = .true.
        coordCyclic      = .false.

      elseif (interngrid%vertCoordSystem .eq. ESMF_COORD_SYSTEM_HEIGHT) then
        ! ESMF_CoordSystem_Height
        coordSystem      = ESMF_COORD_SYSTEM_HEIGHT
        coordName        = 'height'
        coordType        = ESMF_COORD_TYPE_HEIGHT
        coordUnit        = ''
        coordAligned     = .true.
        coordEqualSpaced = .true.
        coordCyclic      = .false.

      else
        if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "IGrid type not yet supported", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Create the actual PhysGrid object
      physIGrid = ESMF_PhysGridCreate(1, relloc, physIGridName, coordSystem, &
                                     rc=localrc)

      tempCoord = ESMF_PhysCoordCreate(coordType, name=coordName, &
                                       units=coordUnit, &
                                       aligned=coordAligned, &
                                       equalSpaced=coordEqualSpaced, &
                                       cyclic=coordCyclic, &
                                       minVal=localMinCoord, &
                                       maxVal=localMaxCoord, rc=localrc)
      call ESMF_PhysGridSetCoord(physIGrid, tempCoord, dimOrder=1, rc=localrc)

      ! now that it's created, add the physgrid to the interngrid
      call ESMF_IGridAddPhysGrid(interngrid, physIGrid, localrc)
      physIGridId = interngrid%numPhysGrids

      ! set coordinates using total cell count
      localCount(1) = countsPerDEDim(1) + 2*interngridBoundWidth  ! TODO: indirect address for countPer
      i1 = localStart + 1
      i2 = localStart + localCount(1) + 1
      call ESMF_LRIGridSetCoord(interngrid, physIGridId, 1, localCount, &
                               interngridBoundWidth, relloc, coordUse(i1:i2), &
                               total=.true., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set coordinates using computational cell count
      localCount(1) = countsPerDEDim(1)  ! TODO: indirect address for countPer
      i1 = localStart + 1 + interngridBoundWidth
      i2 = i1 + localCount(1)
      call ESMF_LRIGridSetCoord(interngrid, physIGridId, 1, localCount, &
                               0, relloc, coordUse(i1:i2), &
                               total=.false., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set mask using total cell count
      localCount(1) = countsPerDEDim(1) + 2*interngridBoundWidth  ! TODO: indirect address for countPer
      i1 = localStart + 1
      i2 = localStart + localCount(1) + 1
      ! TODO: fix setcellmask to work 1d
      ! call ESMF_LRIGridSetCellMask(interngrid, physIGridId, 1, localCount, &
      !                             interngridBoundWidth, relloc, cellType(i1:i2), &
      !                             rc=localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                          ESMF_ERR_PASSTHRU, &
      !                          ESMF_CONTEXT, rc)) return

      ! clean up
      deallocate(coordUse, &
                 cellType, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridAddVertPhysGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridGetCoord"
!BOPI
! !IROUTINE: ESMF_LRIGridGetCoord - Get the coordinates of a IGrid

! !INTERFACE:
      subroutine ESMF_LRIGridGetCoord(interngrid, horzRelLoc, vertRelLoc, centerCoord, &
                                     cornerCoord, faceCoord, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: interngrid
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      type(ESMF_InternArray), intent(out), dimension(:), optional :: centerCoord
      type(ESMF_InternArray), intent(out), dimension(:), optional :: cornerCoord
      type(ESMF_InternArray), intent(out), dimension(:), optional :: faceCoord
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Determines the appropriate physIGrid to query from either a physIGridId or
!     relloc and returns the requested information.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be queried.
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
!          but must be numbered consistently throughout interngrid.
!     \item[{[faceCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the face index.  Faces should
!          be numbered consistently with corners.  For example, face 1 should
!          correspond to the face between corners 1,2.
!     \item[{[reorder]}]
!          Logical flag.  If TRUE, reorder any results using a previously set
!          CoordOrder before returning.  If FALSE do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available primarily for
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
!EOPI

      integer :: localrc                          ! Error status
      integer :: i
      integer :: horzPhysIdUse, vertPhysIdUse
      integer :: aSize, interngridRank, index
      integer, dimension(3) :: order
      logical :: dummy
      logical :: reorderUse
      type(ESMF_InternArray) :: tempArray
      type(ESMF_InternArray), dimension(:), pointer :: coord, coord2

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Invalid IGrid object", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Initialize other variables
      horzPhysIdUse = -1
      vertPhysIdUse = -1
      reorderUse    = .TRUE.
      if (present(reorder)) reorderUse = reorder

      ! Get the interngrid rank -- to check if there is a vertical interngrid available
      interngridRank = interngrid%ptr%dimCount

      ! get physgrid identifiers from relative locations
      if (present(horzRelLoc)) then
        if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                       "undefined horizontal relloc", &
                       ESMF_CONTEXT, rc)
          return
        endif
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. interngridRank.eq.3) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
      endif

      ! Call PhysGridGet with valid PhysGrid
      if (present(centerCoord)) then
        index = 1
        aSize = min(interngridRank, size(centerCoord))
        allocate(coord(aSize), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "coord", &
                                       ESMF_CONTEXT, rc)) return
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index = 3
          call ESMF_PhysGridGetLocations(interngrid%ptr%physIGrids(horzPhysIdUse), &
                                         locationArray=coord(1:2), &
                                         total=total, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          call ESMF_PhysGridGetLocations(interngrid%ptr%physIGrids(vertPhysIdUse), &
                                         locationArray=coord(index:index), &
                                         total=total, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (reorderUse) then
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            ! if i and j are reordered, then the coordinate data arrays need to
            ! be shuffled as well
            if (interngridOrder(1,interngrid%ptr%coordOrder%order,2).eq.1) then
              centerCoord(order(i)) = coord(i)
            else
              call ESMF_LRIGridReshape(coord(i), tempArray, localrc)
              centerCoord(order(i)) = tempArray
            endif
          enddo
        else
          do i = 1,aSize
            centerCoord(i) = coord(i)
          enddo
        endif
        deallocate(coord, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coord", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (present(cornerCoord)) then
        index = 1
        aSize = min(interngridRank, size(cornerCoord))
        allocate(coord2(aSize), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "coord2", &
                                       ESMF_CONTEXT, rc)) return
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index = 3
          call ESMF_PhysGridGetRegions(interngrid%ptr%physIGrids(horzPhysIdUse), &
                                       vertexArray=coord2(1:2), rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          call ESMF_PhysGridGetRegions(interngrid%ptr%physIGrids(horzPhysIdUse), &
                                       vertexArray=coord2(index:index), &
                                       rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (reorderUse) then
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            cornerCoord(order(i)) = coord2(i)
          enddo
        else
          do i = 1,aSize
            cornerCoord(i) = coord2(i)
          enddo
        endif
        deallocate(coord2, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating coord2", &
                                       ESMF_CONTEXT, rc)) return
      endif

      ! TODO: face coords

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridGetCoord

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridGetDELocalInfo"
!BOPI
! !IROUTINE: ESMF_LRIGridGetDELocalInfo - Get DE (local) information for a IGrid

! !INTERFACE:
      subroutine ESMF_LRIGridGetDELocalInfo(interngrid, horzRelLoc, vertRelLoc, &
                                  myDE, localCellCount, localCellCountPerDim, &
                                  minLocalCoordPerDim, maxLocalCoordPerDim, &
                                  globalStartPerDim, globalAIPerDim, reorder, &
                                  total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: interngrid
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer, intent(inout), optional :: myDE
      integer, intent(inout), optional :: localCellCount
      integer, dimension(:), intent(inout), optional :: localCellCountPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minLocalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxLocalCoordPerDim
      integer, dimension(:), intent(inout), optional :: globalStartPerDim
      type(ESMF_AxisIndex), dimension(:), intent(out), &
                                            optional :: globalAIPerDim
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_InternDG} attribute with the given value.  Since a single
!     {\tt ESMF\_IGrid} can have many {\tt ESMF\_InternDGs}, the correct
!     {\tt ESMF\_InternDG} must be identified by this calling routine.  For a 3D
!     {\tt ESMF\_IGrid}, the user must supply identifiers for both the horizontal
!     and vertical interngrids if querying for an array of values, like
!     localCellCountPerDim.  The {\tt ESMF\_InternDG(s)} are identified
!     using the set of input variables:  horzRelLoc and/or vertRelLoc.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item[{[horzRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
!     \item[{[myDE]}]
!          Identifier for this {\tt ESMF\_DE}, zero-based.
!     \item[{[localCellCount]}]
!          Local (on this {\tt ESMF\_DE}) number of cells.
!     \item[{[localCellCountPerDim]}]
!          Local (on this {\tt ESMF\_DE}) number of cells per axis.
!     \item[{[minLocalCoordPerDim]}]
!          Array of minimum local physical coordinates in each direction.
!     \item[{[maxLocalCoordPerDim]}]
!          Array of maximum local physical coordinates in each direction.
!     \item[{[globalStartPerDim]}]
!          Global index of starting counts for each dimension.
!     \item[{[globalAIPerDim]}]
!          Global axis indices for each dimension.
!     \item[{[reorder]}]
!          Logical flag.  If TRUE, reorder any results using a previously set
!          CoordOrder before returning.  If FALSE do not reorder.  The default
!          value is TRUE and users should not need to reset this for most
!          applications.  This optional argument is available primarily for
!          internal use.
!     \item[{[total]}]
!          Logical flag to indicate getting InternDG information for total cells.
!          The default is the computational regime.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer :: aSize, interngridRank, distDimCount, index
      integer :: horzCellCount, vertCellCount
      integer, dimension(3) :: localCellCountPerDimUse, &
                               globalStartPerDimUse, order
      logical :: reorderUse, totalUse
      real(ESMF_KIND_R8), dimension(3) :: minLCPDUse, maxLCPDUse
      type(ESMF_AxisIndex), pointer :: globalAIPerDimUse(:)
!      type(ESMF_AxisIndex), dimension(3) :: globalAIPerDimUse
      type(ESMF_IGridStorage) :: interngridStorage
      type(ESMF_PhysCoord) :: coord
      type(ESMF_DELayout) :: delayout

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Invalid IGrid object", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! allocate local axis index list
     allocate(globalAIPerDimUse(3))

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1
      reorderUse    = .TRUE.
      totalUse      = .FALSE.
      if (present(reorder)) reorderUse = reorder
      if (present(total)) totalUse = total

      ! Get the interngrid rank -- to check if there is a vertical interngrid available
      interngridRank = interngrid%ptr%dimCount

      ! also get the number of dimensions in the distributed interngrid
      call ESMF_LRIGridGet(interngrid, distDimCount=distDimCount, &
                          interngridStorage=interngridStorage, rc=localrc)

      ! if this is an arbitrarily distributed interngrid, make sure total is false,
      ! since there is no difference and we have only made the computational
      ! domain
      if (interngridStorage.eq.ESMF_INTERNGRID_STORAGE_ARBITRARY) then
        totalUse = .FALSE.
      endif

      ! get physgrid and interndg identifiers from relative locations
      if (present(horzRelLoc)) then
        if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          horzDistIdUse = interngrid%ptr%internDGIndex(horzPhysIdUse)
        else
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "undefined horizontal relloc", &
                                 ESMF_CONTEXT, rc)) return
        endif
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. interngridRank.eq.3) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          vertDistIdUse = interngrid%ptr%internDGIndex(vertPhysIdUse)
        endif
      endif

      ! TODO: make sure the horzDistIdUse points to a horizontal interndg,
      !       same for vert

      ! use DELayout call instead of InternDG to get myDE to avoid zero-based
      ! vs. 1-based issues.  note: layout the same for all interndgs, so use 1
      if (present(myDE)) then
        call ESMF_LRIGridGet(interngrid, delayout=delayout)
        call ESMF_DELayoutGetDeprecated(delayout, localDE=myDE, rc=localrc)
      endif

      ! make InternDG calls first
      ! check maximum size of array variables
      aSize = 0
      if (present(localCellCountPerDim)) &
        aSize = max(aSize, size(localCellCountPerDim))
      if (present(   globalStartPerDim)) &
        aSize = max(aSize, size(   globalStartPerDim))
      if (present(      globalAIPerDim)) &
        aSize = max(aSize, size(      globalAIPerDim))
      aSize = min(interngridRank, aSize)

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level
      if (present(localCellCount)) then
        horzCellCount = 1
        vertCellCount = 1
        if (horzPhysIdUse.ne.-1) then
          call ESMF_InternDGGetDE(interngrid%ptr%interndgs(horzDistIdUse)%ptr, &
                                  horzCellCount, &
                                  total=totalUse, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (vertPhysIdUse.ne.-1) then
          call ESMF_InternDGGetDE(interngrid%ptr%interndgs(vertDistIdUse)%ptr, &
                                  vertCellCount, &
                                  total=totalUse, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        localCellCount = horzCellCount*vertCellCount
      endif
      if (aSize.ge.1) then
        index = 1
        if (distDimCount.eq.1 .AND. horzPhysIdUse.ne.-1) then
          index = 2
          call ESMF_InternDGGetDE(interngrid%ptr%interndgs(horzDistIdUse)%ptr, &
                    localCellCountPerDim=localCellCountPerDimUse(1:1), &
                    globalStartPerDim=globalStartPerDimUse(1:1), &
                    globalAIPerDim=globalAIPerDimUse(1:1), &
                    total=totalUse, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (distDimCount.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index         = 3
          call ESMF_InternDGGetDE(interngrid%ptr%interndgs(horzDistIdUse)%ptr, &
                    localCellCountPerDim=localCellCountPerDimUse(1:2), &
                    globalStartPerDim=globalStartPerDimUse(1:2), &
                    globalAIPerDim=globalAIPerDimUse(1:2), &
                    total=totalUse, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          call ESMF_InternDGGetDE(interngrid%ptr%interndgs(vertDistIdUse)%ptr, &
                    localCellCountPerDim=localCellCountPerDimUse(index:index), &
                    globalStartPerDim=globalStartPerDimUse(index:index), &
                    globalAIPerDim=globalAIPerDimUse(index:index), &
                    total=totalUse, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif

        ! load local values into return arguments
        if (reorderUse) then
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            if (present(localCellCountPerDim)) &
                localCellCountPerDim(order(i)) = localCellCountPerDimUse(i)
            if (present(   globalStartPerDim)) &
                globalStartPerDim(order(i)) =    globalStartPerDimUse(i)
            if (present(      globalAIPerDim)) &
                globalAIPerDim(order(i)) =       globalAIPerDimUse(i)
          enddo
        else
          do i = 1,aSize
            if (present(localCellCountPerDim)) &
                localCellCountPerDim(i) = localCellCountPerDimUse(i)
            if (present(   globalStartPerDim)) &
                globalStartPerDim(i) =    globalStartPerDimUse(i)
            if (present(      globalAIPerDim)) &
                globalAIPerDim(i) =       globalAIPerDimUse(i)
          enddo
        endif
      endif

      ! now make PhysGrid calls
      if (present(minLocalCoordPerDim)) then
        index = 1
        aSize = min(interngridRank, size(minLocalCoordPerDim))
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index = 3
          do i = 1,2
            coord = interngrid%ptr%physgrids(horzPhysIdUse)%ptr%coords(i)
            call ESMF_PhysCoordGetExtents(coord, minVal=minLCPDUse(i), &
                                          rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          enddo
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          coord = interngrid%ptr%physgrids(vertPhysIdUse)%ptr%coords(1)
          call ESMF_PhysCoordGetExtents(coord, minVal=minLCPDUse(index), &
                                        rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (reorderUse) then
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            minLocalCoordPerDim(order(i)) = minLCPDUse(i)
          enddo
        else
          do i = 1,aSize
            minLocalCoordPerDim(i) = minLCPDUse(i)
          enddo
        endif
      endif

      if (present(maxLocalCoordPerDim)) then
        index = 1
        aSize = min(interngridRank, size(maxLocalCoordPerDim))
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index = 3
          do i = 1,2
            coord = interngrid%ptr%physgrids(horzPhysIdUse)%ptr%coords(i)
            call ESMF_PhysCoordGetExtents(coord, maxVal=maxLCPDUse(i), &
                                          rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          enddo
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          coord = interngrid%ptr%physgrids(vertPhysIdUse)%ptr%coords(1)
          call ESMF_PhysCoordGetExtents(coord, maxVal=maxLCPDUse(index), &
                                        rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        if (reorderUse) then
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            maxLocalCoordPerDim(order(i)) = maxLCPDUse(i)
          enddo
        else
          do i = 1,aSize
            maxLocalCoordPerDim(i) = maxLCPDUse(i)
          enddo
        endif
      endif

      ! deallocate local axis index list
      deallocate(globalAIPerDimUse)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridGetDELocalInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridGetAllAxisIndex"
!BOPI
! !IROUTINE: ESMF_LRIGridGetAllAxisIndex - Get all axis indices for a InternDG

! !INTERFACE:
      subroutine ESMF_LRIGridGetAllAxisIndex(interngrid, globalAI, horzRelLoc, &
                                            vertRelLoc, AICountPerDE, total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: interngrid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: globalAI
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer, dimension(:), pointer, optional :: AICountPerDE
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_InternDG} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item[globalAI]
!          Global axis indices on all DE's.
!     \item[{[horzRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
!     \item[{[total]}]
!          Logical flag for whether the axis indices should be for total
!          cells or not.  Default is false, which infers computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: aSize, interngridRank, index, sizeAI
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer, dimension(ESMF_MAXINTERNGRIDDIM) :: order
      logical :: dummy
      type(ESMF_AxisIndex), dimension(:,:), pointer :: horzAI, vertAI

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Invalid IGrid object", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the interngrid rank and check against size of globalAI
      interngridRank = interngrid%ptr%dimCount
      if (size(globalAI,2).lt.interngridRank) then
        call ESMF_LogWrite("globalAI array size smaller than interngrid rank", &
                           ESMF_LOG_WARNING, ESMF_CONTEXT)
      endif

      ! Get the size of the AI array and allocate horz and vert temp AI arrays
      sizeAI = size(globalAI,1)
      aSize  = min(interngridRank, size(globalAI,2))
      allocate(horzAI(sizeAI,2), &
               vertAI(sizeAI,1), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "allocating AI arrays", &
                                     ESMF_CONTEXT, rc)) return

      ! get interndg identifiers from relative locations
      if (present(horzRelLoc)) then
        if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          horzDistIdUse = interngrid%ptr%internDGIndex(horzPhysIdUse)
        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                        "undefined horizontal relloc", &
                                        ESMF_CONTEXT, rc)
          return
        endif
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. interngridRank.eq.3) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          vertDistIdUse = interngrid%ptr%internDGIndex(vertPhysIdUse)
        endif
      endif

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level
      index = 1
      if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
        index = 3
        if (interngrid%ptr%interngridStorage.eq.ESMF_INTERNGRID_STORAGE_ARBITRARY) then
          if (present(AICountPerDE)) then
            call ESMF_InternDGGetAllAxisIndex(interngrid%ptr%interndgs(horzDistIdUse)%ptr, &
                                              horzAI, AICountPerDE, rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          else
            dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                          "AICountPerDE not present", &
                                          ESMF_CONTEXT, rc)
            return
          endif 
        else
          call ESMF_InternDGGetAllAxisIndex(interngrid%ptr%interndgs(horzDistIdUse)%ptr, &
                                            horzAI, total, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
      endif
      if (aSize.ge.index .AND. vertDistIdUse.ne.-1) then
        call ESMF_InternDGGetAllAxisIndex(interngrid%ptr%interndgs(vertDistIdUse)%ptr, &
                                          vertAI, total, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      else
        if (interngridRank.eq.3) then
           if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                 "valid vertical relloc required", &
                                  ESMF_CONTEXT, rc)) return
        endif
      endif

      ! Load temp values into input array and clean up
      order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
      if (aSize.ge.2) then
        globalAI(:,order(1)) = horzAI(:,1)
        globalAI(:,order(2)) = horzAI(:,2)
      endif
      if (aSize.ge.index) globalAI(:,order(3)) = vertAI(:,1)

      deallocate(horzAI, &
                 vertAI, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating AI arrays", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridGetAllAxisIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridGetAIsAllDEs"
!BOPI
! !IROUTINE: ESMF_LRIGridGetAIsAllDEs - Get all axis indices for a InternDG

! !INTERFACE:
      subroutine ESMF_LRIGridGetAIsAllDEs(interngrid, localGlobalFlag, &
                                         AIListPerDEPerRank, &
                                         horzRelLoc, vertRelLoc, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: interngrid
      type(ESMF_LocalGlobalFlag), intent(in) :: localGlobalFlag
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AIListPerDEPerRank
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_InternDG} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item[localGlobalFlag]
!          {\tt ESMF\_LocalGlobalFlag] identifier indicating whether the returned
!          array of {\tt ESMF\_AxisIndex} types should be in local or global
!          index space.
!     \item[AIListPerDEPerRank]
!          2D array of {\tt ESMF\_AxisIndex} types containing results.  If
!          allocated, it must be of size (nDEs,interngridrank).
!     \item[{[horzrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[{[vertrelloc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: AIrank, interngridRank, nDEs
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer, dimension(ESMF_MAXINTERNGRIDDIM) :: order
      logical :: dummy, horz, vert
      type(ESMF_AxisIndex), dimension(:,:), pointer :: horzAI, vertAI
      type(ESMF_DELayout) :: delayout

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1
      AIrank        =  0
      horz          = .false.
      vert          = .false.

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                   "Invalid IGrid object", &
                                   ESMF_CONTEXT, rc)) return
      endif

      interngridRank = interngrid%ptr%dimCount
      call ESMF_LRIGridGet(interngrid, delayout=delayout, rc=localrc)
      call ESMF_DELayoutGet(delayout, deCount=nDEs, rc=localrc)

      ! check for validity of horizontal and vertical rellocs and determine the
      ! required size of the AI array
      if (present(horzRelLoc)) then
 
        ! get the physgrid/interndg identifier from the horizontal relLoc.  If it
        ! is not valid return an error.
        if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          horzDistIdUse = interngrid%ptr%internDGIndex(horzPhysIdUse)
          AIrank = AIrank + 2
          horz   = .true.
        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                        "undefined horizontal relloc", &
                                        ESMF_CONTEXT, rc)
          return
        endif
      endif

      if (present(vertRelLoc)) then
	! get the physgrid/interndg identifier from the vertical relLoc.  If it
        ! is not valid return an error.
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .and. interngridRank.eq.3) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          vertDistIdUse = interngrid%ptr%internDGIndex(vertPhysIdUse)
          AIrank = AIrank + 1
          vert   = .true.
        endif
      endif
      ! check if the AI array pointer is associated 
      !  -  If it is, check that it is large enough to hold the requested data.
      !  -  If it is not, allocate it here
      if (associated(AIListPerDEPerRank)) then
        if (size(AIListPerDEPerRank,1).le.nDEs .OR. &
            size(AIListPerDEPerRank,2).le.AIrank) then
          dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                             "array not large enough for requested data", &
                             ESMF_CONTEXT, rc)
          return
        endif
      else
        allocate(AIListPerDEPerRank(nDEs,AIrank), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "allocating AIList array", &
                                       ESMF_CONTEXT, rc)) return
      endif

      ! first get the horizontal subIGrid AIs, if requested and identified with
      ! a proper internDGID
      if (horz) then
 
        ! allocate horz temp AI array
        allocate(horzAI(nDEs,2), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "allocating horz AI array", &
                                       ESMF_CONTEXT, rc)) return

        ! call the proper interndg to retrieve the horizontal AIs
        call ESMF_InternDGGetAIsAllDEs(interngrid%ptr%interndgs(horzDistIdUse)%ptr, &
                                       horzAI, localGlobalFlag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      ! next get the vertical subIGrid AIs, if requested and identified with
      ! a proper internDGID
      if (vert) then
 
        ! allocate vert temp AI array
        allocate(vertAI(nDEs,1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "allocating vert AI array", &
                                       ESMF_CONTEXT, rc)) return

        ! call the proper interndg to retrieve the vertical AIs
        call ESMF_InternDGGetAIsAllDEs(interngrid%ptr%interndgs(vertDistIdUse)%ptr, &
                                       vertAI, localGlobalFlag, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      endif

      ! Load temp values into return array
      order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,interngridRank)
      ! TODO: logic for a 3D interngrid and a request for the horizontal AIs,
      !       where the order array could reference outside of the AIList
      if (horz) then
        AIListPerDEPerRank(:,order(1)) = horzAI(:,1)
        AIListPerDEPerRank(:,order(2)) = horzAI(:,2)
      endif
      if (vert) then
        if (AIrank.ge.3) then
          AIListPerDEPerRank(:,order(3)) = vertAI(:,1)
        else
          AIListPerDEPerRank(:,1) = vertAI(:,1)
        endif
      endif

      ! clean up
      if (horz) then
        deallocate(horzAI, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating horz AI array", &
                                       ESMF_CONTEXT, rc)) return
      endif
      if (vert) then
        deallocate(vertAI, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating vert AI array", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridGetAIsAllDEs

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridGlobalToDELocalIndex"
!BOPI
! !IROUTINE: ESMF_LRIGridGlobalToDELocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_LRIGridGlobalToDELocalIndex(interngrid, horzRelLoc, vertRelLoc, &
                                                 global1D, local1D, &
                                                 global2D, local2D, &
                                                 dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer(ESMF_KIND_I4), dimension(:), intent(in),  optional :: global1D
      integer(ESMF_KIND_I4), dimension(:), intent(out), optional :: local1D
      integer(ESMF_KIND_I4), dimension(:,:), intent(in),  optional :: global2D
      integer(ESMF_KIND_I4), dimension(:,:), intent(out), optional :: local2D
      integer, dimension(:), intent(in), optional :: dimOrder
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_InternDG} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be used.
!     \item[horzRelLoc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i
      integer :: order(3)
      integer :: interngridRank, aSize, tempSize, tempSize2
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer, dimension(:), allocatable :: dimOrderUse
      integer(ESMF_KIND_I4), dimension(:,:), allocatable :: gTemp2D,   lTemp2D
      logical :: dummy
      type(ESMF_InternDGType), pointer :: hdgtype, vdgtype

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "Invalid IGrid object", &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the interngrid rank -- to check if there is a vertical interngrid available
      interngridRank = interngrid%ptr%dimCount

      ! determine the largest input array size and allocate temp arrays
      aSize = 0
      if (present(global1D)) then
        tempSize = size(global1D)
        aSize = max(aSize, tempSize)
      endif
      if (present(global2D)) then
        tempSize  = size(global2D,1)
        tempSize2 = size(global2D,2)
        aSize = max(aSize, tempSize2)
        allocate(gtemp2D(tempSize,tempSize2), &
                 ltemp2D(tempSize,tempSize2), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "temp2D arrays", &
                                       ESMF_CONTEXT, rc)) return
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          gtemp2D(:,i) = global2D(:,order(i))
        enddo
      endif
      aSize = min(interngridRank, aSize)

      ! calculate default if dimOrder is not present
      allocate(dimOrderUse(aSize), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dimOrderUse", &
                                     ESMF_CONTEXT, rc)) return
      if (present(dimOrder)) then
        dimOrderUse(:) = dimOrder(:)
      else
        do i = 1,size(dimOrderUse)
          dimOrderUse(i) = i
        enddo
      endif

      ! get interndg identifier from relative locations
      if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
        call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        horzDistIdUse = interngrid%ptr%internDGIndex(horzPhysIdUse)
      else
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "undefined horizontal relloc", &
                                 ESMF_CONTEXT, rc)) return
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. interngridRank.eq.3) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          vertDistIdUse = interngrid%ptr%internDGIndex(vertPhysIdUse)
        endif
      endif

      hdgtype => interngrid%ptr%interndgs(horzDistIdUse)%ptr
      if (vertDistIdUse.ne.-1) then
        vdgtype => interngrid%ptr%interndgs(vertDistIdUse)%ptr
      else
        if (aSize.ge.3 .and. interngridRank.eq.3) then
           if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                 "valid vertical relloc required", &
                                  ESMF_CONTEXT, rc)) return
        endif
      endif

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level
      ! can't send parts of optional arguments, so for now break out
      if (present(global1D)) then
        if (interngridRank.le.2) then
          call ESMF_InternDGGlobalToLocalIndex(hdgtype, &
                                               global1D=global1D, &
                                               local1D=local1D, &
                                               dimOrder=dimOrderUse(1:2), &
                                               rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                             "1D operation not yet defined for 3d interngrids", &
                              ESMF_CONTEXT, rc)
          return
        endif
      endif

      if (present(global2D)) then
        call ESMF_InternDGGlobalToLocalIndex(hdgtype, &
                                             global2D=gTemp2D(:,1:2), &
                                             local2D=lTemp2D(:,1:2), &
                                             dimOrder=dimOrderUse(1:2), &
                                             rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (vertDistIdUse.ne.-1) then
          call ESMF_InternDGGlobalToLocalIndex(vdgtype, &
                                               global2D=gTemp2D(:,3:3), &
                                               local2D=lTemp2D(:,3:3), &
                                               dimOrder=dimOrderUse(3:3), &
                                               rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif

        tempSize2 = size(global2D,2)
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          local2D(:,order(i)) = ltemp2D(:,i)
        enddo
        deallocate(gtemp2D, &
                   ltemp2D, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating temp2D arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      deallocate(dimOrderUse, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating dimOrderUse", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridGlobalToDELocalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridDELocalToGlobalIndex"
!BOPI
! !IROUTINE: ESMF_LRIGridDELocalToGlobalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_LRIGridDELocalToGlobalIndex(interngrid, horzRelLoc, vertRelLoc, &
                                                 local1D, global1D, &
                                                 local2D, global2D, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) ::  local1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: global1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) ::  local2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: global2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_InternDG} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be used.
!     \item[horzRelLoc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i
      integer :: order(3)
      integer :: interngridRank, aSize, tempSize, tempSize2
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer(ESMF_KIND_I4), dimension(:,:), allocatable :: gTemp2D,   lTemp2D
      logical :: dummy
      type(ESMF_InternDGType), pointer :: hdgtype, vdgtype

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Invalid IGrid object", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the interngrid rank -- to check if there is a vertical interngrid available
      interngridRank = interngrid%ptr%dimCount

      ! determine the largest input array size and allocate temp arrays
      aSize = 0
      if (present(local1D)) then
        tempSize = size(local1D)
        aSize = max(aSize, tempSize)
      endif
      if (present(local2D)) then
        tempSize  = size(local2D,1)
        tempSize2 = size(local2D,2)
        aSize = max(aSize, tempSize)
        allocate(gtemp2D(tempSize,tempSize2), &
                 ltemp2D(tempSize,tempSize2), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "temp2D arrays", &
                                       ESMF_CONTEXT, rc)) return
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          ltemp2D(:,i) = local2D(:,order(i))
        enddo
      endif
      aSize = min(interngridRank, aSize)

      ! get interndg identifiers from relative locations
      if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
        call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        horzDistIdUse = interngrid%ptr%internDGIndex(horzPhysIdUse)
      else
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                     "undefined horizontal relloc", &
                     ESMF_CONTEXT, rc)
        return
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. interngridRank.eq.3) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          vertDistIdUse = interngrid%ptr%internDGIndex(vertPhysIdUse)
        endif
      endif

      hdgtype => interngrid%ptr%interndgs(horzDistIdUse)%ptr
      if (vertDistIdUse.ne.-1) then
        vdgtype => interngrid%ptr%interndgs(vertDistIdUse)%ptr
      else
        if (aSize.ge.3 .and. interngridRank.eq.3) then
           if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                     "valid vertical relloc required", &
                                     ESMF_CONTEXT, rc)) return
        endif
      endif

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level
      ! can't send parts of optional arguments, so for now break out  TODO: fix
      if (present(local1D)) then
        if (interngridRank.le.2) then
          call ESMF_InternDGLocalToGlobalIndex(hdgtype, &
                                               local1D=local1D, &
                                               global1D=global1D, &
                                               rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                       "1D operation not yet defined for 3d interngrids", &
                       ESMF_CONTEXT, rc)
          return
        endif
      endif

      if (present(local2D)) then
        call ESMF_InternDGLocalToGlobalIndex(hdgtype, &
                                             local2D=lTemp2D(:,1:2), &
                                             global2D=gTemp2D(:,1:2), &
                                             rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (vertDistIdUse.ne.-1) then
          call ESMF_InternDGLocalToGlobalIndex(vdgtype, &
                                               local2D=lTemp2D(:,3:3), &
                                               global2D=gTemp2D(:,3:3), &
                                               rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        tempSize2 = size(local2D,2)
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          global2D(:,order(i)) = gtemp2D(:,i)
        enddo
        deallocate(gtemp2D, &
                   ltemp2D, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating temp2D arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridDELocalToGlobalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridGlobalToDELocalAI"
!BOPI
! !IROUTINE: ESMF_LRIGridGlobalToDELocalAI - translate global axis index to local

! !INTERFACE:
      subroutine ESMF_LRIGridGlobalToDELocalAI(interngrid, horzRelLoc, vertRelLoc, &
                                              globalAI1D, localAI1D, &
                                              globalAI2D, localAI2D, &
                                              dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      type(ESMF_AxisIndex), dimension(:), intent(in),  optional :: globalAI1D
      type(ESMF_AxisIndex), dimension(:), intent(out), optional :: localAI1D
      type(ESMF_AxisIndex), dimension(:,:), intent(in),  optional :: globalAI2D
      type(ESMF_AxisIndex), dimension(:,:), intent(out), optional :: localAI2D
      integer, dimension(:), intent(in), optional :: dimOrder
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_InternDG} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be used.
!     \item[horzRelLoc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
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

      integer :: localrc                          ! Error status
      integer :: i
      integer :: order(3)
      integer :: interngridRank, aSize, tempSize, tempSize2
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer, dimension(:), allocatable :: dimOrderUse
      logical :: dummy
      type(ESMF_AxisIndex),  dimension(:,:), allocatable :: gTempAI2D, lTempAI2D
      type(ESMF_InternDGType), pointer :: hdgtype, vdgtype
      integer :: j

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

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


      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "Invalid IGrid object", &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the interngrid rank -- to check if there is a vertical interngrid available
      interngridRank = interngrid%ptr%dimCount

      ! determine the largest input array size and allocate temp arrays
      aSize = 0
      if (present(globalAI1D)) then
        tempSize = size(globalAI1D)
        aSize = max(aSize, tempSize)
      endif
      if (present(globalAI2D)) then
        tempSize  = size(globalAI2D,1)
        tempSize2 = size(globalAI2D,2)
        aSize = max(aSize, tempSize2)
        allocate(gtempAI2D(tempSize,tempSize2), &
                 ltempAI2D(tempSize,tempSize2), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "tempAI2D arrays", &
                                       ESMF_CONTEXT, rc)) return
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          gtempAI2D(:,i) = globalAI2D(:,order(i))
        enddo
      endif
      aSize = min(interngridRank, aSize)

      ! calculate default if dimOrder is not present
      allocate(dimOrderUse(aSize), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dimOrderUse", &
                                     ESMF_CONTEXT, rc)) return
      if (present(dimOrder)) then
        dimOrderUse(:) = dimOrder(:)
      else
        do i = 1,size(dimOrderUse)
          dimOrderUse(i) = i
        enddo
      endif

      ! get interndg identifier from relative locations
      if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
        call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        horzDistIdUse = interngrid%ptr%internDGIndex(horzPhysIdUse)
      else
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "undefined horizontal relloc", &
                                 ESMF_CONTEXT, rc)) return
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. interngridRank.eq.3) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          vertDistIdUse = interngrid%ptr%internDGIndex(vertPhysIdUse)
        endif
      endif

      hdgtype => interngrid%ptr%interndgs(horzDistIdUse)%ptr
      if (vertDistIdUse.ne.-1) then
        vdgtype => interngrid%ptr%interndgs(vertDistIdUse)%ptr
      else
        if (aSize.ge.3 .and. interngridRank.eq.3) then
           if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                 "valid vertical relloc required", &
                                  ESMF_CONTEXT, rc)) return
        endif
      endif

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level
      ! can't send parts of optional arguments, so for now break out
      if (present(globalAI1D)) then
        if (interngridRank.le.2) then
          call ESMF_InternDGGlobalToLocalIndex(hdgtype, &
                                               globalAI1D=globalAI1D, &
                                               localAI1D=localAI1D, &
                                               dimOrder=dimOrderUse(1:2), &
                                               rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                       "1D operation not yet defined for 3d interngrids", &
                       ESMF_CONTEXT, rc)
          return
        endif
      endif

      if (present(globalAI2D)) then
        call ESMF_InternDGGlobalToLocalIndex(hdgtype, &
                                             globalAI2D=gTempAI2D(:,1:2), &
                                             localAI2D=lTempAI2D(:,1:2), &
                                             dimOrder=dimOrderUse(1:2), &
                                             rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (vertDistIdUse.ne.-1) then
          call ESMF_InternDGGlobalToLocalIndex(vdgtype, &
                                               globalAI2D=gTempAI2D(:,3:3), &
                                               localAI2D=lTempAI2D(:,3:3), &
                                               dimOrder=dimOrderUse(3:3), &
                                               rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        tempSize2 = size(globalAI2D,2)
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          localAI2D(:,order(i)) = ltempAI2D(:,i)
        enddo
        deallocate(gtempAI2D, &
                   ltempAI2D, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating tempAI2D arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      deallocate(dimOrderUse, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating dimOrderUse", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridGlobalToDELocalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridDELocalToGlobalAI"
!BOPI
! !IROUTINE: ESMF_LRIGridDELocalToGlobalAI - translate global axis indices to local

! !INTERFACE:
      subroutine ESMF_LRIGridDELocalToGlobalAI(interngrid, horzRelLoc, vertRelLoc, &
                                              localAI1D, globalAI1D, &
                                              localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: interngrid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) ::  localAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) :: globalAI2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_InternDG} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be used.
!     \item[horzRelLoc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          interngrid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          interngrid.
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

      integer :: localrc                          ! Error status
      integer :: i,j
      integer :: order(3)
      integer :: interngridRank, aSize, tempSize, tempSize2
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      logical :: dummy
      type(ESMF_AxisIndex),  dimension(:,:), allocatable :: gTempAI2D, lTempAI2D
      type(ESMF_InternDGType), pointer :: hdgtype, vdgtype

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

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

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Invalid IGrid object", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the interngrid rank -- to check if there is a vertical interngrid available
      interngridRank = interngrid%ptr%dimCount

      ! determine the largest input array size and allocate temp arrays
      aSize = 0
      if (present(localAI1D)) then
        tempSize = size(localAI1D)
        aSize = max(aSize, tempSize)
      endif
      if (present(localAI2D)) then
        tempSize  = size(localAI2D,1)
        tempSize2 = size(localAI2D,2)
        aSize = max(aSize, tempSize)
        allocate(gtempAI2D(tempSize,tempSize2), &
                 ltempAI2D(tempSize,tempSize2), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "tempAI2D arrays", &
                                       ESMF_CONTEXT, rc)) return
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          ltempAI2D(:,i) = localAI2D(:,order(i))
        enddo
      endif
      aSize = min(interngridRank, aSize)

      ! get interndg identifiers from relative locations
      if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
        call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        horzDistIdUse = interngrid%ptr%internDGIndex(horzPhysIdUse)
      else
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                     "undefined horizontal relloc", &
                     ESMF_CONTEXT, rc)
        return
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. interngridRank.eq.3) then
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          vertDistIdUse = interngrid%ptr%internDGIndex(vertPhysIdUse)
        endif
      endif

      hdgtype => interngrid%ptr%interndgs(horzDistIdUse)%ptr
      if (vertDistIdUse.ne.-1) then
        vdgtype => interngrid%ptr%interndgs(vertDistIdUse)%ptr
      else
        if (aSize.ge.3 .and. interngridRank.eq.3) then
           if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                 "valid vertical relloc required", &
                                  ESMF_CONTEXT, rc)) return
        endif
      endif

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level
      ! can't send parts of optional arguments, so for now break out  TODO: fix
      if (present(localAI1D)) then
        if (interngridRank.le.2) then
          call ESMF_InternDGLocalToGlobalIndex(hdgtype, &
                                               localAI1D=localAI1D, &
                                               globalAI1D=globalAI1D, &
                                               rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                       "1D operation not yet defined for 3d interngrids", &
                       ESMF_CONTEXT, rc)
          return
        endif
      endif

      if (present(localAI2D)) then
        call ESMF_InternDGLocalToGlobalIndex(hdgtype, &
                                             localAI2D=lTempAI2D(:,1:2), &
                                             globalAI2D=gTempAI2D(:,1:2), &
                                             rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        if (vertDistIdUse.ne.-1) then
          call ESMF_InternDGLocalToGlobalIndex(vdgtype, &
                                               localAI2D=lTempAI2D(:,3:3), &
                                               globalAI2D=gTempAI2D(:,3:3), &
                                               rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        endif
        tempSize2 = size(localAI2D,2)
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          globalAI2D(:,order(i)) = gtempAI2D(:,i)
        enddo
        deallocate(gtempAI2D, &
                   ltempAI2D, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating tempAI2D arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridDELocalToGlobalAI

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSetCoordComputeBlock"
!BOPI
! !IROUTINE: ESMF_LRIGridSetCoord - Compute coordinates for a IGrid with block storage

! !INTERFACE:
      ! Private name; call using ESMF_LRIGridSetCoord()
      subroutine ESMF_LRIGridSetCoordComputeBlock(interngrid, physIGridId, dimCount, &
                                                 counts, interngridBoundWidth, relloc, &
                                                 coord1, coord2, total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: interngrid
      integer, intent(in) :: physIGridId
      integer, intent(in) :: dimCount
      integer, dimension(dimCount), intent(in) :: counts
      integer, intent(in) :: interngridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord2
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes coordinates for a {\tt ESMF\_IGrid}
!     via a prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[physIGridId]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[counts]
!          Array of number of interngrid increments in each dimension.
!     \item[interngridBoundWidth]
!          Number of extra cell layers in the internal coordinate representation
!          for halo and ghost cells.  Used by {\tt ESMF\_Regrid}.
!     \item[relloc]
!          Relative location in interngrid cell for which this PhysGrid.
!     \item[coord1]
!          Array of specified interngrid coordinates in the first dimension.
!     \item[{[coord2]}]
!          Array of specified interngrid coordinates in the second dimension.
!     \item[{[total]}]
!          Logical flag to optionally set physical coordinate arrays of total cells.
!          Default is to set computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, j, i1, j1, hWidth
      integer, dimension(:), allocatable :: cornerCounts
      logical :: dummy
      real(ESMF_KIND_R8) :: centerUse1, centerUse2
      real(ESMF_KIND_R8) :: cornerUse11, cornerUse12, cornerUse21, cornerUse22
      real(ESMF_KIND_R8), dimension(:), pointer :: center
      real(ESMF_KIND_R8), dimension(:,:), pointer :: center1, center2, corner
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: corner1, corner2
      type(ESMF_InternArray), dimension(:), pointer :: centerArray, cornerArray
      type(ESMF_TypeKind) :: kind

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! TODO: could be a 1-D array for each coord axis later, but that
      !       would have to be supported by Regrid first

      ! allocate arrays
      allocate(cornerCounts(size(counts)+1), &
               centerArray(dimCount), &
               cornerArray(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "local arrays", &
                                     ESMF_CONTEXT, rc)) return

      ! set halo width
      hWidth = 0
      if (total) hWidth = interngridBoundWidth

      ! set up cornerCounts arrays
      cornerCounts(1) = dimCount*2
      do i = 1,size(counts)
        cornerCounts(i+1) = counts(i) - 2*hWidth
      enddo

      ! create ESMF_Arrays
      kind = ESMF_TYPEKIND_R8
      do i = 1,dimCount
        centerArray(i) = ESMF_InternArrayCreate(dimCount, kind, counts, &
                                          haloWidth=hWidth, rc=localrc)
        cornerArray(i) = ESMF_InternArrayCreate(dimCount+1, kind, cornerCounts, &
                                          haloWidth=hWidth, rc=localrc)
      enddo

      select case (dimCount)
      case(1)   ! 1D coordinates, assumed mostly for vertical interngrids

        ! get data
        call ESMF_InternArrayGetData(centerArray(1), center, ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(cornerArray(1), corner, ESMF_DATA_REF, localrc)

        ! For now, an if construct for the different relative locations
        if (relloc .eq. ESMF_CELL_UNDEFINED) then
          localrc = ESMF_FAILURE

        elseif (relloc.eq.ESMF_CELL_CENTER .or. relloc.eq.ESMF_CELL_CELL) then  ! TODO:?
          do i = 1,counts(1)
            center(  i) = 0.5d0*(coord1(i)+coord1(i+1))
          enddo
          if (total) then
            do i = 1+hWidth,counts(1)-hWidth
              i1 = i - hWidth
              corner(1,i1) = coord1(i  )
              corner(2,i1) = coord1(i+1)
            enddo
          endif
        elseif (relloc .eq. ESMF_CELL_TOPFACE) then   ! TODO: check bottom or top
          do i = 1,counts(1)
            center(  i) = coord1(i+1)
          enddo
          if (total) then
            do i = 1+hWidth,counts(1)-hWidth
              i1 = i - hWidth
              corner(1,i1) = 0.5d0*(coord1(i  )+coord1(i+1))
              corner(2,i1) = 0.5d0*(coord1(i+1)+coord1(i+2))
            enddo
          endif

        else
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                    "This relative location not yet supported for 1D interngrids", &
                    ESMF_CONTEXT, rc)) return
        endif


      case(2)   ! 2D coordinates

        ! get data
        call ESMF_InternArrayGetData(centerArray(1), center1, ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(centerArray(2), center2, ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(cornerArray(1), corner1, ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(cornerArray(2), corner2, ESMF_DATA_REF, localrc)

        ! For now, an if construct for the different relative locations
        ! TODO: also set corners and faces
        if (relloc .eq. ESMF_CELL_UNDEFINED) then
          localrc = ESMF_FAILURE

        elseif (relloc .eq. ESMF_CELL_CENTER) then
          do i = 1,counts(1)
            centerUse1  = 0.5d0*(coord1(i)+coord1(i+1))
            do j = 1,counts(2)
              centerUse2  = 0.5d0*(coord2(j)+coord2(j+1))
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = coord1(i  )
            cornerUse12 = coord1(i+1)
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = coord2(j  )
              cornerUse22 = coord2(j+1)
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

        elseif (relloc .eq. ESMF_CELL_NFACE) then
          do i = 1,counts(1)
            centerUse1  = 0.5d0*(coord1(i)+coord1(i+1))
            do j = 1,counts(2)
              centerUse2  = coord2(j+1)
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = coord1(i  )
            cornerUse12 = coord1(i+1)
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = 0.5d0*(coord2(j  )+coord2(j+1))
              cornerUse22 = 0.5d0*(coord2(j+1)+coord2(j+2))
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

        elseif (relloc .eq. ESMF_CELL_SFACE) then
          do i = 1,counts(1)
            centerUse1  = 0.5d0*(coord1(i)+coord1(i+1))
            do j = 1,counts(2)
              centerUse2  = coord2(j)
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = coord1(i  )
            cornerUse12 = coord1(i+1)
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = 0.5d0*(coord2(j-1)+coord2(j  ))
              cornerUse22 = 0.5d0*(coord2(j  )+coord2(j+1))
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

        elseif (relloc .eq. ESMF_CELL_EFACE) then
          do i = 1,counts(1)
            centerUse1  = coord1(i+1)
            do j = 1,counts(2)
              centerUse2  = 0.5d0*(coord2(j)+coord2(j+1))
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = 0.5d0*(coord1(i  )+coord1(i+1))
            cornerUse12 = 0.5d0*(coord1(i+1)+coord1(i+2))
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = coord2(j  )
              cornerUse22 = coord2(j+1)
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

        elseif (relloc .eq. ESMF_CELL_WFACE) then
          do i = 1,counts(1)
            centerUse1  = coord1(i)
            do j = 1,counts(2)
              centerUse2  = 0.5d0*(coord2(j)+coord2(j+1))
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = 0.5d0*(coord1(i-1)+coord1(i  ))
            cornerUse12 = 0.5d0*(coord1(i  )+coord1(i+1))
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = coord2(j  )
              cornerUse22 = coord2(j+1)
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

        elseif (relloc .eq. ESMF_CELL_NECORNER) then
          do i = 1,counts(1)
            centerUse1  = coord1(i+1)
            do j = 1,counts(2)
              centerUse2  = coord2(j+1)
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = 0.5d0*(coord1(i  )+coord1(i+1))
            cornerUse12 = 0.5d0*(coord1(i+1)+coord1(i+2))
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = 0.5d0*(coord2(j  )+coord2(j+1))
              cornerUse22 = 0.5d0*(coord2(j+1)+coord2(j+2))
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

        elseif (relloc .eq. ESMF_CELL_SWCORNER) then
          do i = 1,counts(1)
            centerUse1  = coord1(i)
            do j = 1,counts(2)
              centerUse2  = coord2(j)
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = 0.5d0*(coord1(i-1)+coord1(i  ))
            cornerUse12 = 0.5d0*(coord1(i  )+coord1(i+1))
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = 0.5d0*(coord2(j-1)+coord2(j  ))
              cornerUse22 = 0.5d0*(coord2(j  )+coord2(j+1))
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

        elseif (relloc .eq. ESMF_CELL_SECORNER) then
          do i = 1,counts(1)
            centerUse1  = coord1(i+1)
            do j = 1,counts(2)
              centerUse2  = coord2(j)
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = 0.5d0*(coord1(i  )+coord1(i+1))
            cornerUse12 = 0.5d0*(coord1(i+1)+coord1(i+2))
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = 0.5d0*(coord2(j-1)+coord2(j  ))
              cornerUse22 = 0.5d0*(coord2(j  )+coord2(j+1))
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

       elseif (relloc .eq. ESMF_CELL_NWCORNER) then
          do i = 1,counts(1)
            centerUse1  = coord1(i)
            do j = 1,counts(2)
              centerUse2  = coord2(1+1)
              center1(  i,j) = centerUse1
              center2(  i,j) = centerUse2
            enddo
          enddo
          if (total) then
          do i = 1+hWidth,counts(1)-hWidth
            i1 = i - hWidth
            cornerUse11 = 0.5d0*(coord1(i-1)+coord1(i  ))
            cornerUse12 = 0.5d0*(coord1(i  )+coord1(i+1))
            do j = 1+hWidth,counts(2)-hWidth
              j1 = j - hWidth
              cornerUse21 = 0.5d0*(coord2(j  )+coord2(j+1))
              cornerUse22 = 0.5d0*(coord2(j+1)+coord2(j+2))
              corner1(1,i1,j1) = cornerUse11
              corner1(2,i1,j1) = cornerUse12
              corner1(3,i1,j1) = cornerUse12
              corner1(4,i1,j1) = cornerUse11
              corner2(1,i1,j1) = cornerUse21
              corner2(2,i1,j1) = cornerUse21
              corner2(3,i1,j1) = cornerUse22
              corner2(4,i1,j1) = cornerUse22
            enddo
          enddo
          endif

        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                        "This relative location not yet supported", &
                                        ESMF_CONTEXT, rc)
          return
        endif

      case default
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "interngrids of this dimension are not yet supported", &
                                     ESMF_CONTEXT, rc)
        return
      end select

      ! now set the location array in PhysGrid
      call ESMF_PhysGridSetLocations(interngrid%physIGrids(physIGridId), &
                                     locationArray=centerArray, total=total, &
                                     rc=localrc)
            ! TODO: add name to set call
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set the region array in PhysGrid
      if (total) then
        call ESMF_PhysGridSetRegions(interngrid%physIGrids(physIGridId), &
                                     regionType=ESMF_REGION_TYPE_POLYGON, &
                                     vertexArray=cornerArray, &
                                     numVertices=4, rc=localrc)
              ! TODO: add name to set call
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      deallocate(cornerCounts, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating cornerCounts", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridSetCoordComputeBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSetCoordComputeArb"
!BOPI
! !IROUTINE: ESMF_LRIGridSetCoord - Compute coordinates for a IGrid with arbitrary storage

! !INTERFACE:
      ! Private name; call using ESMF_LRIGridSetCoord()
      subroutine ESMF_LRIGridSetCoordComputeArb(interngrid, physIGridId, dimCount, &
                                               myCount, myIndices,         &
                                               interngridBoundWidth, relloc,     &
                                               coord1, coord2, total, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: interngrid
      integer, intent(in) :: physIGridId
      integer, intent(in) :: dimCount
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, intent(in) :: interngridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord2
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes coordinates for a {\tt ESMF\_IGrid}
!     via a prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[physIGridId]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[myCount]
!          Number of interngrid increments on this DE.
!     \item[myIndices]
!          Array of interngrid indices to be distributed to this DE.  The size of this
!          array must be at least [myCount] in the first dimension and 2 in the
!          second.
!     \item[relloc]
!          Relative location in interngrid cell for which this PhysGrid.
!     \item[coord1]
!          Array of specified interngrid coordinates in the first dimension.
!     \item[{[coord2]}]
!          Array of specified interngrid coordinates in the second dimension.
!     \item[{[total]}]
!          Logical flag to optionally set physical coordinate arrays of total cells.
!          Default is to set computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, i1, j1
      integer :: counts(1), cornerCounts(2)
      logical :: dummy
      real(ESMF_KIND_R8) :: cornerUse11, cornerUse12, cornerUse21, cornerUse22
      real(ESMF_KIND_R8), dimension(:  ), pointer :: center, center1, center2
      real(ESMF_KIND_R8), dimension(:,:), pointer :: corner, corner1, corner2
      type(ESMF_InternArray), dimension(:), pointer :: centerArray, cornerArray
      type(ESMF_TypeKind) :: kind

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! allocate arrays
      allocate(centerArray(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "local arrays", &
                                     ESMF_CONTEXT, rc)) return
      allocate(cornerArray(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "local arrays", &
                                     ESMF_CONTEXT, rc)) return

      ! set up counts arrays
      counts(1)       = myCount
      cornerCounts(1) = dimCount*2
      cornerCounts(2) = myCount

      ! set up ESMF arrays
      kind = ESMF_TYPEKIND_R8
      do i = 1,dimCount
        centerArray(i) = ESMF_InternArrayCreate(1, kind, counts, &
                                          haloWidth=0, rc=localrc)
        cornerArray(i) = ESMF_InternArrayCreate(2, kind, cornerCounts, &
                                          haloWidth=0, rc=localrc)
      enddo


      ! NOTE: the +interngridBoundaryWidth is to offset the indices to
      !       account for the boundary layer, if the total=.true.
      !       ever becomes meaningful this will have to be
      !       adjusted to account for that - Bob Oehmke (10/13/2006)

      select case (dimCount)
      case(1)   ! 1D coordinates, assumed mostly for vertical interngrids

        ! get data
        call ESMF_InternArrayGetData(centerArray(1), center, ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(cornerArray(1), corner, ESMF_DATA_REF, localrc)

        ! For now, an if construct for the different relative locations
        if (relloc .eq. ESMF_CELL_UNDEFINED) then
          localrc = ESMF_FAILURE

        elseif (relloc.eq.ESMF_CELL_CENTER .or. relloc.eq.ESMF_CELL_CELL) then  ! TODO:?
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            center(  i) = 0.5d0*(coord1(i1)+coord1(i1+1))
            corner(1,i) = coord1(i1  )
            corner(2,i) = coord1(i1+1)
          enddo
        elseif (relloc .eq. ESMF_CELL_TOPFACE) then   ! TODO: check bottom or top
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            center(  i) = coord1(i1+1)
            corner(1,i) = 0.5d0*(coord1(i1  )+coord1(i1+1))
            corner(2,i) = 0.5d0*(coord1(i1+1)+coord1(i1+2))
          enddo

        else
          if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                    "This relative location not yet supported for 1D interngrids", &
                    ESMF_CONTEXT, rc)) return
        endif


      case(2)   ! 2D coordinates

        ! get data
        call ESMF_InternArrayGetData(centerArray(1), center1, ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(centerArray(2), center2, ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(cornerArray(1), corner1, ESMF_DATA_REF, localrc)
        call ESMF_InternArrayGetData(cornerArray(2), corner2, ESMF_DATA_REF, localrc)

        ! For now, an if construct for the different relative locations
        ! TODO: also set faces
        if (relloc .eq. ESMF_CELL_UNDEFINED) then
          localrc = ESMF_FAILURE

        elseif (relloc .eq. ESMF_CELL_CENTER) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   = 0.5d0*(coord1(i1)+coord1(i1+1))
            center2(i)   = 0.5d0*(coord2(j1)+coord2(j1+1))
            cornerUse11  =        coord1(i1  )
            cornerUse12  =        coord1(i1+1)
            cornerUse21  =        coord2(j1  )
            cornerUse22  =        coord2(j1+1)
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

        elseif (relloc .eq. ESMF_CELL_NFACE) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   = 0.5d0*(coord1(i1  )+coord1(i1+1))
            center2(i)   =        coord2(j1+1)
            cornerUse11  =        coord1(i1  )
            cornerUse12  =        coord1(i1+1)
            cornerUse21  = 0.5d0*(coord2(j1  )+coord2(j1+1))
            cornerUse22  = 0.5d0*(coord2(j1+1)+coord2(j1+2))
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

        elseif (relloc .eq. ESMF_CELL_SFACE) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   = 0.5d0*(coord1(i1  )+coord1(i1+1))
            center2(i)   =        coord2(j1  )
            cornerUse11  =        coord1(i1  )
            cornerUse12  =        coord1(i1+1)
            cornerUse21  = 0.5d0*(coord2(j1-1)+coord2(j1  ))
            cornerUse22  = 0.5d0*(coord2(j1  )+coord2(j1+1))
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

        elseif (relloc .eq. ESMF_CELL_EFACE) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   =        coord1(i1+1)
            center2(i)   = 0.5d0*(coord2(j1  )+coord2(j1+1))
            cornerUse11  = 0.5d0*(coord1(i1  )+coord1(i1+1))
            cornerUse12  = 0.5d0*(coord1(i1+1)+coord1(i1+2))
            cornerUse21  =        coord2(j1  )
            cornerUse22  =        coord2(j1+1)
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

        elseif (relloc .eq. ESMF_CELL_WFACE) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   =        coord1(i1  )
            center2(i)   = 0.5d0*(coord2(j1  )+coord2(j1+1))
            cornerUse11  = 0.5d0*(coord1(i1-1)+coord1(i1  ))
            cornerUse12  = 0.5d0*(coord1(i1  )+coord1(i1+1))
            cornerUse21  =        coord2(j1  )
            cornerUse22  =        coord2(j1+1)
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

        elseif (relloc .eq. ESMF_CELL_NECORNER) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   =        coord1(i1+1)
            center2(i)   =        coord2(j1+1)
            cornerUse11  = 0.5d0*(coord1(i1  )+coord1(i1+1))
            cornerUse12  = 0.5d0*(coord1(i1+1)+coord1(i1+2))
            cornerUse21  = 0.5d0*(coord2(j1  )+coord2(j1+1))
            cornerUse22  = 0.5d0*(coord2(j1+1)+coord2(j1+2))
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

        elseif (relloc .eq. ESMF_CELL_SWCORNER) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   =        coord1(i1  )
            center2(i)   =        coord2(j1  )
            cornerUse11  = 0.5d0*(coord1(i1-1)+coord1(i1  ))
            cornerUse12  = 0.5d0*(coord1(i1  )+coord1(i1+1))
            cornerUse21  = 0.5d0*(coord2(j1-1)+coord2(j1  ))
            cornerUse22  = 0.5d0*(coord2(j1  )+coord2(j1+1))
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

        elseif (relloc .eq. ESMF_CELL_SECORNER) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   =        coord1(i1+1)
            center2(i)   =        coord2(j1  )
            cornerUse11  = 0.5d0*(coord1(i1  )+coord1(i1+1))
            cornerUse12  = 0.5d0*(coord1(i1+1)+coord1(i1+2))
            cornerUse21  = 0.5d0*(coord2(j1-1)+coord2(j1  ))
            cornerUse22  = 0.5d0*(coord2(j1  )+coord2(j1+1))
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

       elseif (relloc .eq. ESMF_CELL_NWCORNER) then
          do i = 1,myCount
            i1 = myIndices(i,1)+interngridBoundWidth
            j1 = myIndices(i,2)+interngridBoundWidth
            center1(i)   =        coord1(i1  )
            center2(i)   =        coord2(j1+1)
            cornerUse11  = 0.5d0*(coord1(i1-1)+coord1(i1  ))
            cornerUse12  = 0.5d0*(coord1(i1  )+coord1(i1+1))
            cornerUse21  = 0.5d0*(coord2(j1  )+coord2(j1+1))
            cornerUse22  = 0.5d0*(coord2(j1+1)+coord2(j1+2))
            corner1(1,i) = cornerUse11
            corner1(2,i) = cornerUse12
            corner1(3,i) = cornerUse12
            corner1(4,i) = cornerUse11
            corner2(1,i) = cornerUse21
            corner2(2,i) = cornerUse21
            corner2(3,i) = cornerUse22
            corner2(4,i) = cornerUse22
          enddo

        else
          dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                        "This relative location not yet supported", &
                                        ESMF_CONTEXT, rc)
          return
        endif

      case default
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                      "interngrids of this dimension are not yet supported", &
                                     ESMF_CONTEXT, rc)
        return
      end select

      ! now set the location array in PhysGrid
      call ESMF_PhysGridSetLocations(interngrid%physIGrids(physIGridId), &
                                     locationArray=centerArray, total=total, &
                                     rc=localrc)
            ! TODO: add name to set call
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set the region array in PhysGrid
      call ESMF_PhysGridSetRegions(interngrid%physIGrids(physIGridId), &
                                   regionType=ESMF_REGION_TYPE_POLYGON, &
                                   vertexArray=cornerArray, &
                                   numVertices=4, rc=localrc)
            ! TODO: add name to set call
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridSetCoordComputeArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridGet"
!BOPI
! !IROUTINE: ESMF_LRIGridGet - Gets a variety of information about the interngrid

! !INTERFACE:
      subroutine ESMF_LRIGridGet(interngrid, horzRelLoc, vertRelLoc, &
                                horzIGridType, vertIGridType, &
                                horzStagger, vertStagger, &
                                horzCoordSystem, vertCoordSystem, coordOrder, &
                                dimCount, distDimCount, interngridStorage, &
                                minGlobalCoordPerDim, maxGlobalCoordPerDim, &
                                globalCellCountPerDim, maxLocalCellCountPerDim, &
                                globalStartPerDEPerDim, cellCountPerDEPerDim, &
                                periodic, delayout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: interngrid
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      type(ESMF_IGridType), intent(out), optional :: horzIGridType
      type(ESMF_IGridVertType), intent(out), optional :: vertIGridType
      type(ESMF_IGridHorzStagger), intent(out), optional :: horzStagger
      type(ESMF_IGridVertStagger), intent(out), optional :: vertStagger
      type(ESMF_CoordSystem), intent(out), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(out), optional :: vertCoordSystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordOrder
      integer, intent(out), optional :: dimCount
      integer, intent(out), optional :: distDimCount
      type(ESMF_IGridStorage), intent(out), optional :: interngridStorage
      real(ESMF_KIND_R8), intent(out), dimension(:), &
                            optional :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), &
                            optional :: maxGlobalCoordPerDim
      integer, intent(out), dimension(:), optional :: globalCellCountPerDim
      integer, intent(out), dimension(:), optional :: maxLocalCellCountPerDim
      integer, intent(out), dimension(:,:), optional :: globalStartPerDEPerDim
      integer, intent(out), dimension(:,:), optional :: cellCountPerDEPerDim
      type (ESMF_Logical), intent(out), optional :: periodic(:)
      type(ESMF_DELayout), intent(out), optional:: delayout
      character(len = *), intent(out), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version gets a variety of information about a {\tt ESMF\_IGrid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[{[horzIGridType]}]
!          Integer specifier to denote horizontal interngrid type.
!     \item[{[vertIGridType]}]
!          Integer specifier to denote vertical interngrid type.
!     \item[{[horzStagger]}]
!          Integer specifier to denote horizontal interngrid stagger.
!     \item[{[vertStagger]}]
!          Integer specifier to denote vertical interngrid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal interngrid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical interngrid.
!     \item[{[coordOrder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote the default coordinate
!          ordering for the IGrid and all related Fields (i.e. KIJ).
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[globalCellCountPerDim]}]
!          Array of numbers of global interngrid increments in each direction.
!     \item[{[maxLocalCellCountPerDim]}]
!          Array of maximum interngrid counts on any DE in each direction.
!     \item[{[globalStartPerDEPerDim]}]
!          Array of global starting locations for each DE and in each direction.
!     \item[{[cellCountPerDEPerDim]}]
!          2-D array of interngrid counts on each DE and in each direction.
!     \item[{[periodic]}]
!          Returns the periodicity along the coordinate axes - logical array.
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, aSize, interngridRank
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer, dimension(ESMF_MAXINTERNGRIDDIM) :: order
      integer, dimension(:), allocatable :: gCCPDUse, mLCCPDUse
      integer, dimension(:,:), allocatable :: gSPDEPDUse, cCPDEPDUse
      type(ESMF_IGridClass), pointer :: interngridp

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Invalid IGrid object", &
                                 ESMF_CONTEXT, rc)) return
      endif

      interngridp => interngrid%ptr

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the interngrid rank -- to check if there is a vertical interngrid available
      interngridRank = interngridp%dimCount

      ! if present, gets information from the interngrid derived type
      if (present(horzIGridType   )) horzIGridType    = interngridp%horzIGridType
      if (present(vertIGridType   )) vertIGridType    = interngridp%vertIGridType
      if (present(horzStagger    )) horzStagger     = interngridp%horzStagger
      if (present(vertStagger    )) vertStagger     = interngridp%vertStagger
      if (present(horzCoordSystem)) horzCoordSystem = interngridp%horzCoordSystem
      if (present(vertCoordSystem)) vertCoordSystem = interngridp%vertCoordSystem
      if (present(coordOrder     )) coordOrder      = interngridp%coordOrder
      if (present(dimCount       )) dimCount        = interngridp%dimCount
      if (present(interngridStorage    )) interngridStorage     = interngridp%interngridStorage

      ! get name from base obj
      if (present(name)) then
        call ESMF_GetName(interngridp%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! Get global coordinate extents
      if (present(minGlobalCoordPerDim)) then
        aSize = min(interngridRank, size(minGlobalCoordPerDim))
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
        do i=1,aSize
          minGlobalCoordPerDim(order(i)) = interngridp%minGlobalCoordPerDim(i)
        enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
        aSize = min(interngridRank, size(maxGlobalCoordPerDim))
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
        do i=1,aSize
          maxGlobalCoordPerDim(order(i)) = interngridp%maxGlobalCoordPerDim(i)
        enddo
      endif

      ! get the periodicity
      if (present(periodic)) then
        aSize = min(interngridRank, size(periodic))
        order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
        do i=1,aSize
          periodic(order(i)) = interngridp%periodic(i)
        enddo
      endif

      ! if InternDG info is being queried, make sure there is a valid internDGId
      if (present(globalCellCountPerDim)   .or. &
          present(globalStartPerDEPerDim)  .or. &
          present(maxLocalCellCountPerDim) .or. &
          present(cellCountPerDEPerDim)) then

        ! determine the largest input array size
        aSize = 0
        if (present(globalCellCountPerDim)) &
            aSize = max(aSize, size(globalCellCountPerDim))
        if (present(maxLocalCellCountPerDim)) &
            aSize = max(aSize, size(maxLocalCellCountPerDim))
        if (present(globalStartPerDEPerDim)) &
            aSize = max(aSize, size(globalStartPerDEPerDim,2))
        if (present(cellCountPerDEPerDim)) &
            aSize = max(aSize, size(cellCountPerDEPerDim,2))
        aSize = min(interngridRank, aSize)

        ! get interndg identifiers from relative locations
        if (.not.(present(horzRelLoc))) then
           if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "invalid horizontal relloc", &
                                 ESMF_CONTEXT, rc)) return
        endif
        call ESMF_IGridGetPhysGridId(interngrid%ptr, horzRelLoc, horzPhysIdUse, &
                                    localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        horzDistIdUse = interngrid%ptr%internDGIndex(horzPhysIdUse)

        if (aSize.ge.3) then
          if (.not.(present(vertRelLoc))) then
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                 "no valid vertical InternDG identifier", &
                                 ESMF_CONTEXT, rc)) return
          endif
          call ESMF_IGridGetPhysGridId(interngrid%ptr, vertRelLoc, vertPhysIdUse, &
                                      localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          vertDistIdUse = interngrid%ptr%internDGIndex(vertPhysIdUse)
        endif

        ! Get interndg info with global coordinate counts
        if (present(globalCellCountPerDim)) then
          aSize = min(interngridRank, size(globalCellCountPerDim))
          allocate(gCCPDUse(aSize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "gCCPDUse", &
                                         ESMF_CONTEXT, rc)) return
          call ESMF_InternDGGet(interngridp%interndgs(horzDistIdUse), &
                                globalCellCountPerDim=gCCPDUse(1:2), &
                                rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          if (aSize.ge.3) then
            call ESMF_InternDGGet(interngridp%interndgs(vertDistIdUse), &
                                  globalCellCountPerDim=gCCPDUse(3:3), &
                                  rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          endif
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            globalCellCountPerDim(order(i)) = gCCPDUse(i)
          enddo
          deallocate(gCCPDUse, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocating gCCPDUse", &
                                         ESMF_CONTEXT, rc)) return
        endif

        if (present(globalStartPerDEPerDim)) then
          aSize = min(interngridRank, size(globalStartPerDEPerDim,2))
          allocate(gSPDEPDUse(size(globalStartPerDEPerDim,1), aSize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "gSPDEPUse", &
                                         ESMF_CONTEXT, rc)) return
          call ESMF_InternDGGet(interngridp%interndgs(horzDistIdUse), &
                                globalStartPerDEPerDim=gSPDEPDUse(:,1:2), &
                                rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          if (aSize.ge.3) then
            call ESMF_InternDGGet(interngridp%interndgs(vertDistIdUse), &
                                  globalStartPerDEPerDim=gSPDEPDUse(:,3:3), &
                                  rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          endif
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            globalStartPerDEPerDim(:,order(i)) = gSPDEPDUse(:,i)
          enddo
          deallocate(gSPDEPDUse, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocating gSPDEPUse", &
                                         ESMF_CONTEXT, rc)) return
        endif

        if (present(maxLocalCellCountPerDim)) then
          aSize = min(interngridRank, size(maxLocalCellCountPerDim))
          allocate(mLCCPDUse(aSize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "mLCCPDUse", &
                                         ESMF_CONTEXT, rc)) return
          call ESMF_InternDGGet(interngridp%interndgs(horzDistIdUse), &
                                maxLocalCellCountPerDim=mLCCPDUse(1:2), &
                                rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          if (aSize.ge.3) then
            call ESMF_InternDGGet(interngridp%interndgs(vertDistIdUse), &
                                  maxLocalCellCountPerDim=mLCCPDUse(3:3), &
                                  rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          endif
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            maxLocalCellCountPerDim(order(i)) = mLCCPDUse(i)
          enddo
          deallocate(mLCCPDUse, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocating mLCCPDUse", &
                                         ESMF_CONTEXT, rc)) return
        endif

        if (present(cellCountPerDEPerDim)) then
          aSize = min(interngridRank, size(cellCountPerDEPerDim,2))
          allocate(cCPDEPDUse(size(cellCountPerDEPerDim,1), aSize), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "cCPDEPDUse", &
                                         ESMF_CONTEXT, rc)) return
          call ESMF_InternDGGetAllCounts(interngridp%interndgs(horzDistIdUse)%ptr, &
                                         cCPDEPDUse(:,1:2), rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

          if (aSize.ge.3) then
            call ESMF_InternDGGetAllCounts(interngridp%interndgs(vertDistIdUse)%ptr, &
                                           cCPDEPDUse(:,3:3), rc=localrc)
            if (ESMF_LogMsgFoundError(localrc, &
                                      ESMF_ERR_PASSTHRU, &
                                      ESMF_CONTEXT, rc)) return
          endif
          order(:) = interngridOrder(:,interngrid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            cellCountPerDEPerDim(:,order(i)) = cCPDEPDUse(:,i)
          enddo
          deallocate(cCPDEPDUse, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocating cCPDEPDUse", &
                                         ESMF_CONTEXT, rc)) return
        endif
      endif

      if (present(distDimCount)) then
        distDimCount = interngridRank
        ! arbitrarily distributed interngrids are a special case
        if (interngridp%interngridStorage.eq.ESMF_INTERNGRID_STORAGE_ARBITRARY) &
            distDimCount = interngridRank - 1
      endif

      if (present(delayout)) then
        call ESMF_InternDGGet(interngridp%interndgs(1), delayout=delayout, rc=localrc)
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSet"
!BOPI
! !IROUTINE: ESMF_LRIGridSet - Sets a variety of information about the interngrid

! !INTERFACE:
      subroutine ESMF_LRIGridSet(interngrid, horzIGridType, vertIGridType, &
                                horzStagger, vertStagger, &
                                horzCoordSystem, vertCoordSystem, &
                                coordOrder, minGlobalCoordPerDim, &
                                maxGlobalCoordPerDim, periodic, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: interngrid
      type(ESMF_IGridType),     intent(in), optional :: horzIGridType
      type(ESMF_IGridVertType), intent(in), optional :: vertIGridType
      type(ESMF_IGridHorzStagger), intent(in), optional :: horzStagger
      type(ESMF_IGridVertStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      type(ESMF_CoordOrder),  intent(in), optional :: coordOrder
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                      minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                      maxGlobalCoordPerDim
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version sets a variety of information about a {\tt ESMF\_IGrid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[{[horzIGridType]}]
!          Integer specifier to denote horizontal interngrid type.
!     \item[{[vertIGridType]}]
!          Integer specifier to denote vertical interngrid type.
!     \item[{[horzStagger]}]
!          Integer specifier to denote horizontal interngrid stagger.
!     \item[{[vertStagger]}]
!          Integer specifier to denote vertical interngrid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal interngrid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical interngrid.
!     \item[{[coordOrder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote the default coordinate
!          ordering for the IGrid and all related Fields (i.e. KIJ).
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
!EOPI

      !integer :: localrc                          ! Error status
      integer :: i                                ! loop index

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! if present, set information filling in interngrid derived type
      if (present(horzIGridType)) interngrid%horzIGridType = horzIGridType
      if (present(vertIGridType)) interngrid%vertIGridType = vertIGridType
      if (present(horzStagger)) interngrid%horzStagger = horzStagger
      if (present(vertStagger)) interngrid%vertStagger = vertStagger
      if (present(horzCoordSystem)) interngrid%horzCoordSystem = horzCoordSystem
      if (present(vertCoordSystem)) interngrid%vertCoordSystem = vertCoordSystem
      if (present(coordOrder)) interngrid%coordOrder = coordOrder
      if (present(periodic)) then
        do i=1,ESMF_MAXINTERNGRIDDIM
          if (i > size(periodic)) exit
          interngrid%periodic(i) = periodic(i)
        enddo
      endif

      if (present(minGlobalCoordPerDim)) then
   !      if (size(minGlobalCoordPerDim) .gt. ESMF_MAXINTERNGRIDDIM) exit  ! TODO
        do i=1,size(minGlobalCoordPerDim)
          interngrid%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
        enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
   !      if (size(maxGlobalCoordPerDim) .gt. ESMF_MAXINTERNGRIDDIM) exit  ! TODO
        do i=1,size(maxGlobalCoordPerDim)
          interngrid%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
        enddo
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridGetCellMask"
!BOPI
! !IROUTINE: ESMF_LRIGridGetCellMask - Retrieves cell identifier mask for a IGrid

! !INTERFACE:
      subroutine ESMF_LRIGridGetCellMask(interngrid, maskArray, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: interngrid
      type(ESMF_InternArray), intent(out) :: maskArray !BOB changed to out from inout
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
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[maskArray]
!          {\tt ESMF\_Array} to contain the internally-used cell array denoting
!          whether cells are in the computational regime, a ghost region, or a
!          halo region.
!     \item[relloc]
!          Relative location in interngrid cell for this PhysGrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: physIdUse

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      ! some basic error checking    TODO: more
      if (.not.associated(interngrid%ptr)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Invalid IGrid object", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Initialize other variables
      physIdUse = -1

      if (relloc.ne.ESMF_CELL_UNDEFINED) then
        call ESMF_IGridGetPhysGridId(interngrid%ptr, relloc, physIdUse, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      else
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "invalid horizontal relloc", &
                                 ESMF_CONTEXT, rc)) return
      endif
      if (physIdUse.eq.-1) then
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                            "No PhysGrid corresponding to horizontal relloc", &
                            ESMF_CONTEXT, rc)) return
      endif

      ! call PhysGrid with the valid Id
      call ESMF_PhysGridGetMask(interngrid%ptr%physIGrids(physIdUse), maskArray, id=1, &
                                rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridGetCellMask

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSetCellMaskBlock"
!BOPI
! !IROUTINE: ESMF_LRIGridSetCellMask - Compute cell identifier mask for a IGrid with block storage

! !INTERFACE:
      ! Private name; call using ESMF_LRIGridSetCellMask()
      subroutine ESMF_LRIGridSetCellMaskBlock(interngrid, physIGridId, dimCount, counts, &
                                             interngridBoundWidth, relloc, cellType1, &
                                             cellType2, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: interngrid
      integer, intent(in) :: physIGridId
      integer, intent(in) :: dimCount
      integer, dimension(dimCount), intent(in) :: counts
      integer, intent(in) :: interngridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, dimension(:), intent(in) :: cellType1
      integer, dimension(:), intent(in), optional :: cellType2
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes cell types for an {\tt ESMF\_IGrid}
!     and sets them as an integer mask in a corresponding {\tt ESMF\_PhysGrid}.
!     This mask is intended for internal use to indicate which cells are in
!     the computational regime (cellType=0), a ghost region (cellType=1), or a
!     halo region (cellType=2).
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[physIGridId]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[counts]
!          Array of number of interngrid increments in each dimension.
!     \item[interngridBoundWidth]
!          Width, in cells, of the ficticious boundary around the interngrid for
!          halo and ghost regions.
!     \item[relloc]
!          Relative location in interngrid cell for which this PhysGrid.
!     \item[cellType1]
!          Array of cell type identifiers in the first dimension.
!     \item[{[cellType2]}]
!          Array of cell type identifiers in the second dimension.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      character(len=ESMF_MAXSTR) :: name
      integer :: i, j, iMax1, jMax1, iType, jType
      integer, dimension(:,:), pointer :: temp
      type(ESMF_InternArray) :: arrayTemp
      type(ESMF_TypeKind) :: kind

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! TODO: different subroutines for different dimCount?  or case?

      ! create ESMF_Array
      kind = ESMF_TYPEKIND_I4
      arrayTemp = ESMF_InternArrayCreate(dimCount, kind, counts, &
                                   haloWidth=interngridBoundWidth, rc=localrc)
      call ESMF_InternArrayGetData(arrayTemp, temp, ESMF_DATA_REF, localrc)

      ! TODO: should this be different for different relative locations?
      iMax1 = counts(1) - interngridBoundWidth + 1
      jMax1 = counts(2) - interngridBoundWidth + 1
      do i = 1,counts(1)
        iType = cellType1(i)
        do j = 1,counts(2)
          jType = cellType2(j)
          ! default is computational
          temp(i,j) = 0
          ! identify ghost cells
          if (iType.eq.1 .or. jType.eq.1) then
            temp(i,j) = 1
          ! identify halo cells
          elseif (i.le.interngridBoundWidth .OR. j.le.interngridBoundWidth .OR. &
                  i.ge.iMax1          .OR. j.ge.jMax1) then
            temp(i,j) = 2
          endif
        enddo
      enddo

      ! now set the mask array in PhysGrid
      name = 'cell type total'
      call ESMF_PhysGridSetMask(interngrid%physIGrids(physIGridId), &
                                maskArray=arrayTemp, &
                                maskType=ESMF_GRID_MASKTYPE_REGION_ID, &
                                name=name, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridSetCellMaskBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSetCellMaskArb"
!BOPI
! !IROUTINE: ESMF_LRIGridSetCellMask - Compute cell identifier mask for a IGrid with arbitrary storage

! !INTERFACE:
      ! Private name; call using ESMF_LRIGridSetCellMask()
      subroutine ESMF_LRIGridSetCellMaskArb(interngrid, physIGridId, dimCount, &
                                           myCount, relloc, cellType, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: interngrid
      integer, intent(in) :: physIGridId
      integer, intent(in) :: dimCount
      integer, intent(in) :: myCount
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, dimension(:), intent(in) :: cellType
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes cell types for an {\tt ESMF\_IGrid}
!     and sets them as an integer mask in a corresponding {\tt ESMF\_PhysGrid}.
!     This mask is intended for internal use to indicate which cells are in
!     the computational regime (cellType=0), a ghost region (cellType=1), or a
!     halo region (cellType=2).
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[physIGridId]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[dimCount]
!          Number of interngrid dimensions.
!     \item[myCount]
!          Number of interngrid increments on this DE.
!     \item[relloc]
!          Relative location in interngrid cell for which this PhysGrid.
!     \item[cellType]
!          Array of cell type identifiers corresponding to the list of
!          arbitrary points.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      character(len=ESMF_MAXSTR) :: name
      integer :: i
      integer, dimension(1) :: counts
      integer, dimension(:), pointer :: temp
      type(ESMF_InternArray) :: arrayTemp
      type(ESMF_TypeKind) :: kind

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! create ESMF_Array
      kind = ESMF_TYPEKIND_I4
      counts(1) = myCount
      arrayTemp = ESMF_InternArrayCreate(1, kind, counts, &
                                   haloWidth=0, rc=localrc)
      call ESMF_InternArrayGetData(arrayTemp, temp, ESMF_DATA_REF, localrc)

      do i = 1,myCount
        ! default is computational for arbitrary storage
        temp(i) = 0
      enddo

      ! now set the mask array in PhysGrid
      name = 'cell type total'
      call ESMF_PhysGridSetMask(interngrid%physIGrids(physIGridId), &
                                maskArray=arrayTemp, &
                                maskType=ESMF_GRID_MASKTYPE_REGION_ID, &
                                name=name, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridSetCellMaskArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSetBoundBoxesBlock"
!BOPI
! !IROUTINE: ESMF_LRIGridSetBoundBoxesBlock - Set the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_LRIGridSetBoundBoxesBlock(interngrid, dimCount, coord1, &
                                               coord2, countsPerDEDim1, &
                                               countsPerDEDim2, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), intent(inout) :: interngrid
      integer, intent(in) :: dimCount
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in) :: countsPerDEDim2
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the region identifier data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[dimCount]
!          Number of interngrid dimensions (directions).
!     \item[coord1]
!          Array of physical coordinates in the first direction.
!     \item[coord2]
!          Array of physical coordinates in the second direction.
!     \item[countsPerDEDim1]
!          Array of number of interngrid increments per DE in the x-direction.
!     \item[countsPerDEDim2]
!          Array of number of interngrid increments per DE in the y-direction.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: DE, numDE1, numDE2, numDEs, npts, count(2)
      integer :: i, i1, i2, j
      real(ESMF_KIND_R8) :: start, stop, huge
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)


      huge     = 999999.     !TODO: could be an ESMF constant -- OK for now
      numDE1   = size(countsPerDEDim1)
      numDE2   = size(countsPerDEDim2)
      numDEs   = numDE1*numDE2
      npts     = 2**dimCount
      count(1) = size(coord1)
      count(2) = size(coord2)

      ! TODO: break out by rank?
      ! Assume the following starage for bounding boxes:
      !   number of DEs * npts * dimCount
      !   where npts is the number of points necessary to describe a bounding box
      !   and rank is the number of dimensions.  For the time being, the points
      !   are stored in the following order:
      !                 1. (Xmin,Ymin,Zmin)
      !                 2. (Xmax,Ymin,Zmin)
      !                 3. (Xmax,Ymax,Zmin)
      !                 4. (Xmin,Ymax,Zmin)
      !                 5. (Xmin,Ymin,Zmax)
      !                 6. (Xmax,Ymin,Zmax)
      !                 7. (Xmax,Ymax,Zmax)
      !                 8. (Xmin,Ymax,Zmax)
      allocate(boxes(numDEs,npts,dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "boxes", &
                                     ESMF_CONTEXT, rc)) return

      ! Calculate box for each DE
      ! Direction 1 first
      start = 0.0
      stop  = 0.0
      i1    = 0
      do j = 1,numDE1
        i2 = i1+countsPerDEDim1(j)
        start = minval(coord1(i1+1:i2+1))
        stop  = maxval(coord1(i1+1:i2+1))
        if (i1.ge.1         ) start = minval(coord1(i1+0:i2+1))
        if (i2.le.count(1)-2) stop  = maxval(coord1(i1+1:i2+2))
        if (countsPerDEDim1(j).eq.0) then
          start = -huge
          stop  = -huge
        endif
        do i = 1,numDE2
          DE = (i-1)*numDE1 + j
          boxes(DE,1,1) = start
          boxes(DE,2,1) = stop
          boxes(DE,3,1) = stop
          boxes(DE,4,1) = start
        enddo
        start = stop
        i1    = i1 + countsPerDEDim1(j)
      enddo

      ! Direction 2 next
      start = 0.0
      stop  = 0.0
      i1    = 0
      do j = 1,numDE2
        i2 = i1+countsPerDEDim2(j)
        start = minval(coord2(i1+1:i2+1))
        stop  = maxval(coord2(i1+1:i2+1))
        if (i1.ge.1         ) start = minval(coord2(i1+0:i2+1))
        if (i2.le.count(2)-2) stop  = maxval(coord2(i1+1:i2+2))
        if (countsPerDEDim2(j).eq.0) then
          start = -huge
          stop  = -huge
        endif
        do i = 1,numDE1
          DE = (j-1)*numDE1 + i
          boxes(DE,1,2) = start
          boxes(DE,2,2) = start
          boxes(DE,3,2) = stop
          boxes(DE,4,2) = stop
        enddo
        start = stop
        i1    = i1 + countsPerDEDim2(j)
      enddo

      interngrid%boundingBoxes = ESMF_LocalArrayCreate(boxes, ESMF_DATA_COPY, localrc)

      ! Clean up
      deallocate(boxes, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dellocating boxes", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridSetBoundBoxesBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSetBoundBoxesArb"
!BOPI
! !IROUTINE: ESMF_LRIGridSetBoundBoxesArb - Set the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_LRIGridSetBoundBoxesArb(interngrid, dimCount, coord1, &
                                             coord2, myCount, myIndices, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), target :: interngrid
      integer, intent(in) :: dimCount
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the region identifier data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[dimCount]
!          Number of interngrid dimensions (directions).
!     \item[coord1]
!          Array of physical coordinates in the first direction.
!     \item[coord2]
!          Array of physical coordinates in the second direction.
!     \item[countsPerDEDim1]
!          Array of number of interngrid increments per DE in the x-direction.
!     \item[countsPerDEDim2]
!          Array of number of interngrid increments per DE in the y-direction.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      integer :: npts, nDEs
      real(ESMF_KIND_R8) :: start, stop
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes
      type(ESMF_DELayout) :: delayout
      type(ESMF_IGrid) :: interngridp

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! TODO: break out by rank?
      ! Assume the following starage for bounding boxes:
      !   number of DEs * npts * dimCount
      !   where npts is the number of points necessary to describe a bounding box
      !   and rank is the number of dimensions.  For the time being, the points
      !   are stored in the following order:
      !                 1. (Xmin,Ymin,Zmin)
      !                 2. (Xmax,Ymin,Zmin)
      !                 3. (Xmax,Ymax,Zmin)
      !                 4. (Xmin,Ymax,Zmin)
      !                 5. (Xmin,Ymin,Zmax)
      !                 6. (Xmax,Ymin,Zmax)
      !                 7. (Xmax,Ymax,Zmax)
      !                 8. (Xmin,Ymax,Zmax)
      ! create temporary interngrid to pass into subroutines
      interngridp%ptr => interngrid
      call ESMF_IGridSetInitCreated(interngridp, rc)

      call ESMF_LRIGridGet(interngridp, delayout=delayout, rc=localrc)
      call ESMF_DELayoutGet(delayout, deCount=nDEs, rc=localrc)
      npts = 2**dimCount
      allocate(boxes(nDEs,npts,dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "boxes", &
                                     ESMF_CONTEXT, rc)) return

      ! Calculate box for each DE
      ! TODO: eventually should be communicated, but for now just set each to the
      !       min and max of the coordinate arrays
      start = minval(coord1)
      stop  = maxval(coord1)
      boxes(:,1,1) = start
      boxes(:,2,1) = stop
      boxes(:,3,1) = stop
      boxes(:,4,1) = start
      start = minval(coord2)
      stop  = maxval(coord2)
      boxes(:,1,2) = start
      boxes(:,2,2) = stop
      boxes(:,3,2) = stop
      boxes(:,4,2) = start

      interngrid%boundingBoxes = ESMF_LocalArrayCreate(boxes, ESMF_DATA_COPY, localrc)

      ! Clean up
      deallocate(boxes, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dellocating boxes", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridSetBoundBoxesArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridValidate"
!BOPI
! !IROUTINE: ESMF_LRIGridValidate - Check internal consistency of a IGrid

! !INTERFACE:
      subroutine ESMF_LRIGridValidate(interngrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in) :: interngrid
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that a {\tt ESMF\_IGrid} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item[{[opt]}]
!          Validation options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      character(len=ESMF_MAXSTR) :: name
      type(ESMF_IGridClass), pointer :: gp
      integer :: localrc                          ! Error status

      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)

      if (.not. associated(interngrid%ptr)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Invalid IGrid object", &
                                 ESMF_CONTEXT, rc)) return
      endif

      gp => interngrid%ptr
      if (gp%interngridStatus /= ESMF_INTERNGRID_STATUS_READY) then
        return
      endif

      call ESMF_GetName(gp%base, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! TODO: add calls to PhysGrid and interndg validates

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridBoxIntersectRecv"
!BOPI
! !IROUTINE: ESMF_LRIGridBoxIntersectRecv - Determine a DomainList covering a box
!
! !INTERFACE:
      subroutine ESMF_LRIGridBoxIntersectRecv(srcIGrid, dstIGrid, &
                                             parentVM, domainList, &
                                             hasSrcData, hasDstData, &
                                             total, layer, &
                                             srcRelloc, dstRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: srcIGrid
      type(ESMF_IGrid) :: dstIGrid
      type(ESMF_VM), intent(in) :: parentVM
      type(ESMF_DomainList), intent(out) :: domainList !BOB changed from inout to out
      logical, intent(in) :: hasSrcData
      logical, intent(in) :: hasDstData
      logical, intent(in) :: total
      logical, intent(in) :: layer
      type(ESMF_RelLoc), intent(in), optional :: srcRelloc
      type(ESMF_RelLoc), intent(in), optional :: dstRelloc
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
!          Logical flag to indicate the domainList should use total cells
!          instead of computational cells.
!     \item[layer]
!          Logical flag to indicate the domainList should add an extra layer
!          of cells, which in necessary for some regridding algorithms in
!          some situations.
!     \item[{[srcRelloc]}]
!          Relative location of the source data.  The default value is
!          ESMF_CELL_CENTER.
!     \item[{[dstRelloc]}]
!          Relative location of the destination data.  The default value is
!          ESMF_CELL_CENTER.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  SSSn.n, GGGn.n
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, i1, j, rank, nDEs, numDomains
      integer :: size, totalPoints
      integer :: counts(ESMF_MAXDIM)
      integer :: physId
      integer :: myPET, nPETs, thisPET, srcDEs, srcDE, srcPID
      integer(ESMF_KIND_I4) :: localIndex(2)
      integer(ESMF_KIND_I4), dimension(:), allocatable :: globalMinIndex, &
                                                          globalMaxIndex
      logical :: hasMin, hasMax
      real(ESMF_KIND_R8) :: point(2)
      real(ESMF_KIND_R8), dimension(:), pointer :: localMinPerDim
      real(ESMF_KIND_R8), dimension(:), pointer :: localMaxPerDim
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes
      type(ESMF_AxisIndex) :: thisAI(2)
      type(ESMF_AxisIndex), dimension(:,:), pointer :: interngridAI, localAI
      type(ESMF_LocalArray) :: array
      type(ESMF_RelLoc) :: srcRellocUse, dstRellocUse
      type(ESMF_DELayout) :: srcDELayout

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVM,rc)

      ! set defaults and overwrite with optional arguments
      srcRellocUse = ESMF_CELL_CENTER
      dstRellocUse = ESMF_CELL_CENTER
      if (present(srcRelloc)) srcRellocUse = srcRelloc
      if (present(dstRelloc)) dstRellocUse = dstRelloc

      ! allocate local min/max arrays to the rank of the dst IGrid.
      ! Must still do this even if this DE does not have any dst data for
      ! use below, so set huge default values.
      call ESMF_LRIGridGet(dstIGrid, dimCount=rank, rc=localrc)
      allocate(localMinPerDim(rank), &
               localMaxPerDim(rank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "min/max arrays", &
                                     ESMF_CONTEXT, rc)) return
      localMinPerDim =  9876543.0d0          ! TODO: use ESMF_Huge once it is
      localMaxPerDim = -9876543.0d0          !       defined

      ! If this DE does not have any destination data, then there is nothing to
      ! receive.  However, it cannot simply return because it may be involved in
      ! collective communication calls if it has source data.
      if (hasDstData) then

        ! get set of bounding boxes from the source interngrid
        call ESMF_IGridGetBoundingBoxes(srcIGrid%ptr, array, localrc)

        ! get rank and counts from the bounding boxes array
        call ESMF_LocalArrayGet(array, counts=counts, rc=localrc)
        nDEs = counts(1)

        ! get pointer to the actual bounding boxes data
        call ESMF_LocalArrayGetData(array, boxes, rc=localrc)

        ! allocate arrays now
        allocate( interngridAI(nDEs,rank), &
                 localAI(nDEs,rank), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "AI arrays", &
                                       ESMF_CONTEXT, rc)) return

        ! get DE-local min/max
        call ESMF_LRIGridGetDELocalInfo(dstIGrid, horzRelLoc=dstRellocUse, &
                                       minLocalCoordPerDim=localMinPerDim, &
                                       maxLocalCoordPerDim=localMaxPerDim, &
                                       reorder=.false., rc=localrc)

        ! get set of axis indices from source interngrid
        call ESMF_LRIGridGetAllAxisIndex(srcIGrid, interngridAI, &
                                        horzRelLoc=srcRellocUse, total=total, &
                                        rc=localrc)

        ! translate the AIs from global to local
        call ESMF_LRIGridGlobalToDELocalAI(srcIGrid, horzRelLoc=srcRellocUse, &
                                          globalAI2D=interngridAI, &
                                          localAI2D=localAI, rc=localrc)

        ! loop through bounding boxes, looking for overlap with our "box"
        ! go through list of DEs to calculate the number of domains
        numDomains = 0
        do i = 1,nDEs
          if ((localMinPerDim(1).gt.max(boxes(i,2,1),boxes(i,3,1))) .OR. &
              (localMaxPerDim(1).lt.min(boxes(i,1,1),boxes(i,4,1))) .OR. &
              (localMinPerDim(2).gt.max(boxes(i,3,2),boxes(i,4,2))) .OR. &
              (localMaxPerDim(2).lt.min(boxes(i,1,2),boxes(i,2,2)))) cycle
          numDomains = numDomains + 1
        enddo

        domainList = ESMF_DomainListCreate(numDomains)

        ! now fill in the domain list
        !  TODO: only one loop instead of two, one that figures the number
        !        of domains and one that fills it in
        ! TODO: move some of this code to Base and add a DomainList method?
        numDomains   = 0
        totalPoints  = 0

      endif

      ! branch on different domainOption, which is a more or less private used
      ! to select the algorithm for deciding how much data from each DE will be
      ! gathered.  If 0, then the entire domain of any DE overlapping this one
      ! will be gathered.  If 1, then the framework will use a different set of
      ! coding to identify the smallest logically rectangular domain (with some
      ! minor efficiency adjustments) to gather.
      select case(domainOption)

      ! whole domain from each overlapping DE.  If there is an overlap, then use 
      ! the entire local source domain for the domainList.
      case(0)

        ! only PETs with destination data need to check for overlap
        if (hasDstData) then

          do j = 1,nDEs
            if ((localMinPerDim(1).gt.max(boxes(j,2,1),boxes(j,3,1))) .OR. &
                (localMaxPerDim(1).lt.min(boxes(j,1,1),boxes(j,4,1))) .OR. &
                (localMinPerDim(2).gt.max(boxes(j,3,2),boxes(j,4,2))) .OR. &
                (localMaxPerDim(2).lt.min(boxes(j,1,2),boxes(j,2,2)))) cycle
            numDomains = numDomains + 1
            domainList%domains(numDomains)%DE   = j - 1  ! DEs start with 0
            domainList%domains(numDomains)%rank = rank
            size = 1
            do i = 1,rank
              domainList%domains(numDomains)%ai(i) = localAI(j,i)
              size = size * (localAI(j,i)%max - localAI(j,i)%min + 1)
            enddo
            totalPoints = totalPoints + size
          enddo
          domainList%total_points = totalPoints

        endif

      !  reduced domain from each overlapping DE
      case(1)

        ! allocate arrays to store intersection results
        call ESMF_VMGet(parentVM, localPet=myPET, petCount=nPETs, rc=localrc)
        allocate(globalMinIndex(nPETs*2), &
                 globalMaxIndex(nPETs*2), stat=rc)

        ! determine the right physgrid to look at in calls below
        call ESMF_IGridGetPhysGridId(srcIGrid%ptr, srcRellocUse, physId, localrc)

        ! loop over PETs in the parent VM, where the PET matching the loop
        ! counter broadcasts its minima and maxima.  All PETs that have source
        ! data then search for those points in the intended physgrid and return
        ! the row and column if found.
        do j  = 1,nPETs
          localIndex = 0
          thisPET    = j - 1
          if (myPET.eq.thisPET) then
            point(1) = localMinPerDim(1)
            point(2) = localMinPerDim(2)
          endif
          call ESMF_VMBroadcast(parentVM, point, 2, thisPET, rc=localrc)
          if (hasSrcData) then
            call ESMF_PhysGridSearchMyDERowCol(srcIGrid%ptr%physIGrids(physId), &
                                               localIndex, point, &
                                               option='min', total=total, rc=rc)
          endif
          call ESMF_VMGather(parentVM, localIndex, globalMinIndex, 2, thisPET, &
                             rc=localrc)
          if (myPET.eq.thisPET) then
            point(1) = localMaxPerDim(1)
            point(2) = localMaxPerDim(2)
          endif
          call ESMF_VMBroadcast(parentVM, point, 2, thisPET, rc=localrc)
          if (hasSrcData) then
            call ESMF_PhysGridSearchMyDERowCol(srcIGrid%ptr%physIGrids(physId), &
                                               localIndex, point, &
                                               option='max', total=total, rc=rc)
          endif
          call ESMF_VMGather(parentVM, localIndex, globalMaxIndex, 2, thisPET, &
                             rc=localrc)
        enddo

        ! only PETs with destination data have to worry about a recv domainList
        if (hasDstData) then

          ! first get the appropriate layout
          call ESMF_LRIGridGet(srcIGrid, delayout=srcDELayout, rc=localrc)
          call ESMF_DELayoutGet(srcDELayout, deCount=srcDEs, rc=localrc)

          ! loop over PETs, checking for intersections
          size = 0
          do i = 1,nPETs
            i1 = (i-1)*2 + 1
            hasMin = .false.
            hasMax = .false.
            if (globalMinIndex(i1  ).le.globalMaxIndex(i1  ) .AND. &
                globalMinIndex(i1+1).le.globalMaxIndex(i1+1) .AND. &
                globalMinIndex(i1  ).ne.12345678             .AND. &
                globalMinIndex(i1+1).ne.12345678) &
                hasMin = .true.
            if (globalMaxIndex(i1  ).ge.globalMinIndex(i1  ) .AND. &
                globalMaxIndex(i1+1).ge.globalMinIndex(i1+1) .AND. &
                globalMaxIndex(i1  ).ne.0                    .AND. &
                globalMaxIndex(i1+1).ne.0) &
                hasMax = .true.

            ! in here only if an intersection is found
            if (hasMin .AND. hasMax) then

              ! modify the gathered indices with a layer of cells, if requested
              if (layer) then
                globalMinIndex(i1  ) = globalMinIndex(i1  ) - 1
                globalMinIndex(i1+1) = globalMinIndex(i1+1) - 1
                globalMaxIndex(i1  ) = globalMaxIndex(i1  ) + 1
                globalMaxIndex(i1+1) = globalMaxIndex(i1+1) + 1
              endif

              ! so this is a domain -- figure out the corresponding DE in
              ! the source layout
              do j = 1,srcDEs
                srcDE = j - 1              ! DE's are 0-based
                call ESMF_DELayoutGetDELocalInfo(srcDELayout, srcDE, &
                                                 pid=srcPID, rc=localrc)
                if (srcPID.eq.i-1) exit   ! TODO: fix once we talk to Gerhard
              enddo

              ! set AIs for the domainList.  Note that since we are looking for data
              ! to receive and being efficient, the AI corresponds only to the chunk
              ! of data to transfer.  That way, the data is packed as it is received.
              thisAI(1) = localAI(j,1)
              thisAI(2) = localAI(j,2)
              thisAI(1)%min = 1
              thisAI(2)%min = 1
              thisAI(1)%max = globalMaxIndex(i1  ) - globalMinIndex(i1  ) + 1
              thisAI(2)%max = globalMaxIndex(i1+1) - globalMinIndex(i1+1) + 1
              thisAI(1)%stride = thisAI(1)%max
              thisAI(2)%stride = thisAI(2)%max

              numDomains = numDomains + 1

              ! load AI and information into the domainList
              domainList%domains(numDomains)%DE    = srcDE
              domainList%domains(numDomains)%rank  = rank
              domainList%domains(numDomains)%ai(1) = thisAI(1)
              domainList%domains(numDomains)%ai(2) = thisAI(2)
              size = (thisAI(1)%max - thisAI(1)%min + 1) &
                   * (thisAI(2)%max - thisAI(2)%min + 1)
              totalPoints = totalPoints + size
            endif
          enddo
          domainList%num_domains  = numDomains
          domainList%total_points = totalPoints
        endif

        deallocate(globalMinIndex, &
                   globalMaxIndex, stat=rc)

      end select

      ! TODO:  the code below is taken from Phil's regrid routines and needs
      !        to be incorporated at some point
      !
      ! if spherical coordinates, set up constants for longitude branch cut
      !

      !if (dstPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !   if (units = 'degrees') then
      !      lon_thresh = 270.0
      !      lon_cycle  = 360.0
      !   else if (units = 'radians') then
      !      lon_thresh = 1.5*pi
      !      lon_cycle  = 2.0*pi
      !   endif
      !endif
      !
      ! correct for longitude crossings if spherical coords
      ! assume degrees and x is longitude
      !
      !if (dstPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) - lon_cycle
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) + lon_cycle
      !endif
      !
      ! make sure src bbox is in same longitude range as dst bbox
      ! assume degrees and x is longitude
      !
      !   if (srcPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) - lon_cycle
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) + lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) - lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) + lon_cycle
      !   endif ! Spherical coords

      ! clean up
      deallocate(localMinPerDim, &
                 localMaxPerDim, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating min/max arrays", &
                                     ESMF_CONTEXT, rc)) return

      if (hasDstData) then
        deallocate( interngridAI, &
                   localAI, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating AI arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridBoxIntersectRecv

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridBoxIntersectSend"
!BOPI
! !IROUTINE: ESMF_LRIGridBoxIntersectSend - Determine a DomainList covering a box
!
! !INTERFACE:
      subroutine ESMF_LRIGridBoxIntersectSend(srcIGrid, dstIGrid, domainList, &
                                             total, layer, &
                                             srcRelloc, dstRelloc, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: srcIGrid
      type(ESMF_IGrid) :: dstIGrid
      type(ESMF_DomainList), intent(inout) :: domainList
      logical, intent(in) :: total
      logical, intent(in) :: layer
      type(ESMF_RelLoc), intent(in), optional :: srcRelloc
      type(ESMF_RelLoc), intent(in), optional :: dstRelloc
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
!     \item[dstIGrid]
!          Destination {\tt ESMF\_IGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[srcIGrid]
!          Source {\tt ESMF\_IGrid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[domainList]
!          Resulting {\tt ESMF\_DomainList} containing the set of
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[total]
!          Logical flag to indicate the domainList should use total cells
!          instead of computational cells.
!     \item[layer]
!          Logical flag to indicate the domainList should add an extra layer
!          of cells, which in necessary for some regridding algorithms in
!          some situations.
!     \item[{[srcRelloc]}]
!          Relative location of the source data.  The default value is
!          ESMF_CELL_CENTER.
!     \item[{[dstRelloc]}]
!          Relative location of the destination data.  The default value is
!          ESMF_CELL_CENTER.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  SSSn.n, GGGn.n
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, i1, j, rank, nDEs, numDomains
      integer :: size, totalPoints
      integer :: counts(ESMF_MAXDIM)
      integer :: physId
      integer(ESMF_KIND_I4) :: localIndex(2)
      integer(ESMF_KIND_I4), dimension(:), pointer :: globalMinIndex, &
                                                      globalMaxIndex
      logical :: hasMin, hasMax
      real(ESMF_KIND_R8) :: point(2)
      real(ESMF_KIND_R8), dimension(:), pointer :: localMinPerDim
      real(ESMF_KIND_R8), dimension(:), pointer :: localMaxPerDim
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes
      type(ESMF_AxisIndex), dimension(:), pointer :: myGlobalAI, myLocalAI, thisAI
      type(ESMF_LocalArray) :: array
      type(ESMF_RelLoc) :: srcRellocUse, dstRellocUse

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)

      ! set defaults and overwrite with optional arguments
      srcRellocUse = ESMF_CELL_CENTER
      dstRellocUse = ESMF_CELL_CENTER
      if (present(srcRelloc)) srcRellocUse = srcRelloc
      if (present(dstRelloc)) dstRellocUse = dstRelloc

      ! get set of bounding boxes from the destination interngrid
      call ESMF_IGridGetBoundingBoxes(dstIGrid%ptr, array, localrc)

      ! get rank and counts from the bounding boxes array
      call ESMF_LocalArrayGet(array, counts=counts, rc=localrc)
      nDEs = counts(1)
      rank = counts(3)

      ! allocate arrays now
      allocate(localMinPerDim(rank), &
               localMaxPerDim(rank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "min/max per dim arrays", &
                                     ESMF_CONTEXT, rc)) return
      allocate(myGlobalAI(rank), &
                myLocalAI(rank), &
                   thisAI(rank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "AI arrays", &
                                     ESMF_CONTEXT, rc)) return

      ! get local min/max and myGlobalAI
      call ESMF_LRIGridGetDELocalInfo(srcIGrid, horzRelLoc=srcRellocUse, &
                                     minLocalCoordPerDim=localMinPerDim, &
                                     maxLocalCoordPerDim=localMaxPerDim, &
                                     globalAIPerDim=myGlobalAI, &
                                     total=total, reorder=.false., rc=localrc)

      ! translate myGlobalAI to local index
      call ESMF_LRIGridGlobalToDELocalAI(srcIGrid, horzRelLoc=srcRellocUse, &
                                        globalAI1D=myGlobalAI, &
                                        localAI1D=myLocalAI, rc=localrc)

      ! get pointer to the actual bounding boxes data
      call ESMF_LocalArrayGetData(array, boxes, rc=localrc)

      ! loop through bounding boxes, looking for overlap with our "box".
      ! go through list of DEs to calculate the number of domains
      numDomains = 0
      do i = 1,nDEs
        if ((localMinPerDim(1).gt.max(boxes(i,2,1),boxes(i,3,1))) .or. &
            (localMaxPerDim(1).lt.min(boxes(i,1,1),boxes(i,4,1))) .or. &
            (localMinPerDim(2).gt.max(boxes(i,3,2),boxes(i,4,2))) .or. &
            (localMaxPerDim(2).lt.min(boxes(i,1,2),boxes(i,2,2)))) cycle
        numDomains = numDomains + 1
      enddo

      ! create the domainList
      domainList = ESMF_DomainListCreate(numDomains)

      ! now fill in the domain list  TODO: only one loop instead of two, one that
      ! figures the number of domains and one that fills it in
      ! TODO: move some of this code to Base and add a DomainList method
      numDomains  = 0
      totalPoints = 0

      ! branch on different domainOption, which is a more or less private used
      ! to select the algorithm for deciding how much data from each DE will be
      ! gathered.  If 0, then the entire domain of any DE overlapping this one
      ! will be gathered.  If 1, then the framework will use a different set of
      ! coding to identify the smallest logically rectangular domain (with some
      ! minor efficiency adjustments) to gather.
      select case(domainOption)

      ! whole domain from each overlapping DE
      case(0)

        ! loop over the DEs in the destination interngrid
        do j = 1,nDEs
          if ((localMinPerDim(1).gt.max(boxes(j,2,1),boxes(j,3,1))) .or. &
              (localMaxPerDim(1).lt.min(boxes(j,1,1),boxes(j,4,1))) .or. &
              (localMinPerDim(2).gt.max(boxes(j,3,2),boxes(j,4,2))) .or. &
              (localMaxPerDim(2).lt.min(boxes(j,1,2),boxes(j,2,2)))) cycle
          numDomains = numDomains + 1
          domainList%domains(numDomains)%DE   = j - 1  ! DEs start with 0
          domainList%domains(numDomains)%rank = rank
          size = 1
          do i = 1,rank
            domainList%domains(numDomains)%ai(i) = myLocalAI(i)
            size = size * (myLocalAI(i)%max - myLocalAI(i)%min + 1)
          enddo
          totalPoints = totalPoints + size
        enddo
        domainList%total_points = totalPoints

      !  reduced domain from each overlapping DE
      case(1)

        ! allocate arrays to store intersection data, sized by the number of
        ! DEs for the destination IGrid and the rank, assumed to be two
        allocate(globalMinIndex(nDEs*2), &
                 globalMaxIndex(nDEs*2), stat=rc)

        ! determine the right physgrid to look at in calls below
        call ESMF_IGridGetPhysGridId(srcIGrid%ptr, srcRellocUse, physId, localrc)

        ! loop over the DEs in the destination IGrid
        do j  = 1,nDEs
          localIndex = 0

          ! pick out min and max for each DE from its bounding box and search for
          ! that point in my source IGrid
          point(1) = min(boxes(j,1,1),boxes(j,4,1))
          point(2) = min(boxes(j,1,2),boxes(j,2,2))
          call ESMF_PhysGridSearchMyDERowCol(srcIGrid%ptr%physIGrids(physId), &
                                             localIndex, point, &
                                             option='min', total=total, rc=rc)
          globalMinIndex((j-1)*2 + 1) = localIndex(1)
          globalMinIndex((j-1)*2 + 2) = localIndex(2)
          point(1) = max(boxes(j,2,1),boxes(j,3,1))
          point(2) = max(boxes(j,3,2),boxes(j,4,2))
          call ESMF_PhysGridSearchMyDERowCol(srcIGrid%ptr%physIGrids(physId), &
                                             localIndex, point, &
                                             option='max', total=total, rc=rc)
          globalMaxIndex((j-1)*2 + 1) = localIndex(1)
          globalMaxIndex((j-1)*2 + 2) = localIndex(2)
        enddo

        ! unload index array info to AI's
        size = 0
        do i = 1,nDEs
          i1 = (i-1)*2 + 1
          hasMin = .false.
          hasMax = .false.
          if (globalMinIndex(i1  ).le.globalMaxIndex(i1  ) .AND. &
              globalMinIndex(i1+1).le.globalMaxIndex(i1+1) .AND. &
              globalMinIndex(i1  ).ne.12345678             .AND. &
              globalMinIndex(i1+1).ne.12345678) &
              hasMin = .true.
          if (globalMaxIndex(i1  ).ge.globalMinIndex(i1  ) .AND. &
              globalMaxIndex(i1+1).ge.globalMinIndex(i1+1) .AND. &
              globalMaxIndex(i1  ).ne.0                    .AND. &
              globalMaxIndex(i1+1).ne.0) &
              hasMax = .true.
          if (hasMin .AND. hasMax) then

            ! modify the gathered indices with a layer of cells, if requested
            if (layer) then
              globalMinIndex(i1  ) = globalMinIndex(i1  ) - 1
              globalMinIndex(i1+1) = globalMinIndex(i1+1) - 1
              globalMaxIndex(i1  ) = globalMaxIndex(i1  ) + 1
              globalMaxIndex(i1+1) = globalMaxIndex(i1+1) + 1

            endif

            ! load the appropriate information into thisAI, using the indices
            ! calculated earlier for min and max and the local stride
            thisAI(1)%min    = globalMinIndex(i1)
            thisAI(2)%min    = globalMinIndex(i1+1)
            thisAI(1)%max    = globalMaxIndex(i1)
            thisAI(2)%max    = globalMaxIndex(i1+1)
            thisAI(1)%stride = myLocalAI(1)%stride
            thisAI(2)%stride = myLocalAI(2)%stride

            ! add the AI and related information to the domainList
            numDomains = numDomains + 1
            domainList%domains(numDomains)%DE    = i-1  ! DEs start with 0
            domainList%domains(numDomains)%rank  = rank
            domainList%domains(numDomains)%ai(1) = thisAI(1)
            domainList%domains(numDomains)%ai(2) = thisAI(2)
            size = (thisAI(1)%max - thisAI(1)%min + 1) &
                 * (thisAI(2)%max - thisAI(2)%min + 1)
            totalPoints = totalPoints + size
          endif
        enddo
        domainList%num_domains  = numDomains
        domainList%total_points = totalPoints

        deallocate(globalMinIndex, &
                   globalMaxIndex, stat=rc)

      end select
      ! TODO:  the code below is taken from Phil's regrid routines and needs
      !        to be incorporated at some point
      !
      ! if spherical coordinates, set up constants for longitude branch cut
      !

      !if (dstPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !   if (units = 'degrees') then
      !      lon_thresh = 270.0
      !      lon_cycle  = 360.0
      !   else if (units = 'radians') then
      !      lon_thresh = 1.5*pi
      !      lon_cycle  = 2.0*pi
      !   endif
      !endif
      !
      ! correct for longitude crossings if spherical coords
      ! assume degrees and x is longitude
      !
      !if (dstPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) - lon_cycle
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) + lon_cycle
      !endif
      !
      ! make sure src bbox is in same longitude range as dst bbox
      ! assume degrees and x is longitude
      !
      !   if (srcPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) - lon_cycle
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) + lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) - lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) + lon_cycle
      !   endif ! Spherical coords

      ! clean up
      deallocate(localMaxPerDim, &
                 localMinPerDim, &
                     myGlobalAI, &
                      myLocalAI, &
                         thisAI, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating local arrays", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridBoxIntersectSend

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridReshape"
!BOPI
! !IROUTINE: ESMF_LRIGridReshape - Switch the dimensions of the data of an Array

 !INTERFACE:
      subroutine ESMF_LRIGridReshape(array1, array2, rc)
!
! !ARGUMENTS:

      type(ESMF_InternArray), intent(in)  :: array1   ! source array
      type(ESMF_InternArray), intent(out) :: array2   ! dest array
      integer, intent(out), optional :: rc  ! return code

! !DESCRIPTION:
!     This routine takes the data from one {\tt ESMF\_Array} and reorders it,
!     switching ranks, to create a destination Array.
!
!     The arguments are:
!     \begin{description}
!     \item[array1]
!          Source {\tt ESMF\_Array}.
!     \item[array2]
!          Destination {\tt ESMF\_Array}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc                          ! Error status
      integer :: i, i1, j, j1
      real(ESMF_KIND_R8), dimension(:,:), pointer :: temp1, temp2

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit,array1,rc)

      ! get data in source array
      call ESMF_InternArrayGetData(array1, temp1, rc=localrc)
      i1 = size(temp1,1)
      j1 = size(temp1,2)

      ! allocate data for destination array
      allocate(temp2(j1,i1), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "temp2", &
                                     ESMF_CONTEXT, rc)) return

      ! transfer data
      do j = 1,j1
        do i = 1,i1
          temp2(j,i) = temp1(i,j)
        enddo
      enddo
 
      ! make destination array from data
      array2 = ESMF_InternArrayCreate(temp2, ESMF_DATA_COPY, rc=localrc)

      deallocate(temp2, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating temp2", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridReshape

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSearchPoint"
!!BOPI
!! !IROUTINE: ESMF_LRIGridSearchPoint - Search the interngrid for a cell containing point
!
! !INTERFACE:
!      subroutine ESMF_LRIGridSearchPoint(dstAdd, x, y, DEId, searchIGrid, &
!                                        physIGridId, rc)
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) :: dstAdd       ! location in interngrid of interngrid cell
!                                            ! containing search point
!      real (kind=?), intent(in) :: x        ! x coordinates of search point 
!      real (kind=?), intent(in) :: y        ! y coordinates of search point 
!      integer, intent(in) :: DEId           ! DE which owns the search point
!      type(ESMF_IGrid), intent(in) :: searchIGrid
!                                            ! interngrid to search for location of
!                                            ! point
!      integer, intent(in), optional :: physIGridId
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
!!     \item[DEId]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[searchIGrid]
!!          ESMF {\tt ESMF\_IGrid} to search for location.
!!     \item[{[physIGridId]}]
!!          If more than one {\tt ESMF\_PhysGrid} is contained in 
!!          {\tt ESMF\_IGrid}, choose which interngrid to search (default is 1st
!!          {\tt ESMF\_PhysGrid}?).
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOPI
!! !REQUIREMENTS:  SSSn.n, GGGn.n
!
!!
!!    ! Initialize return code; assume routine not implemented
!!    if (present(rc)) rc = ESMF_RC_NOT_IMPL
!
!!      ! check input variables
!!      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,interngrid,rc)
!!
!!     extract appropriate PhysGrid and InternDG to search
!!     extract various other interngrid properties
!!
!      if (.not. present(physIGridId)) physIGridId = 1 (or whatever default)
!      ! combine these queries?  format of query functions?
!      call ESMF_LRIGridGet(searchIGrid, physIGrid=physIGridId) ??? 
!      call ESMF_LRIGridGet(searchIGrid, internDG = ??)
!      call ESMF_LRIGridGet(searchIGrid, horzIGridType = searchIGridType)
!!
!!     Call appropriate search routine based on coordinate system and
!!     interngrid type.
!!
! 
!      select case (srchIGridType)
!      case(ESMF_INTERNGRID_TYPE_LATLON,       ESMF_INTERNGRID_TYPE_LATLON_MERCATOR, &
!           ESMF_INTERNGRID_TYPE_LATLON_GAUSS, ESMF_INTERNGRID_TYPE_REDUCED)
!         !*** simple search adequate for these cases
!         call ESMF_PhysGridSearchBboxSpherical(dstAdd, x, y, DEId, physIGrid, &
!                                               internDG, localrc)
!
!      case(ESMF_INTERNGRID_TYPE_DIPOLE,   ESMF_INTERNGRID_TYPE_TRIPOLE, &
!           ESMF_INTERNGRID_TYPE_GEODESIC, ESMF_INTERNGRID_TYPE_CUBEDSPHERE)
!         !*** must use more general algorithm for these cases
!         call ESMF_PhysGridSearchGeneralSpherical(dstAdd, x, y, DEId, physIGrid, &
!                                                  internDG, localrc)
!
!      case(ESMF_INTERNGRIDTYPE_XY, ESMF_INTERNGRIDTYPE_XY_UNI)
!         call ESMF_PhysGridSearchBboxCartesian(dstAdd, x, y, DEId, physIGrid, &
!                                               internDG, localrc)
!
!      case default
!         print *,'IGridSearchPoint: search of this interngrid type not supported'
!         localrc = ESMF_Failure
!      end select
!
!      end subroutine ESMF_LRIGridSearchPoint
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridDecompose"
!BOPI
! !IROUTINE: ESMF_LRIGridDecompose - 

 !INTERFACE:
      subroutine ESMF_LRIGridDecompose(nDEs, count, countsPerDE, rc)
!
! !ARGUMENTS:

      integer, intent(in)                 :: nDEs
      integer, intent(in)                 :: count
      integer, dimension(:), intent(out)  :: countsPerDE
      integer, intent(out), optional      :: rc             

! !DESCRIPTION:
!
!EOPI
! !REQUIREMENTS:

      ! Local variables.
      !integer :: localrc                          ! Error status
      integer :: total, i

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Determine the decomposition 
      total = 0
      do i  = 1, nDEs
        countsPerDE(i) = ((count*i + nDEs/2)/nDEs) - total
        total = total + countsPerDE(i)
      enddo

      ! set return code if user specified it
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridDecompose

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridSerialize"

!BOPI
! !IROUTINE: ESMF_LRIGridSerialize - Serialize lr interngrid specific info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_LRIGridSerialize(interngrid, buffer, length, offset, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), pointer :: interngrid
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
!     \item [interngrid]
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
      integer :: localrc                         ! Error status
      integer :: i
      type(ESMF_LogRectIGrid), pointer :: lrinterngrid  ! lrinterngrid class

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClassGetInit,interngrid,rc)

      ! shortcut to internals
      lrinterngrid => interngrid%interngridSpecific%logRectIGrid

      ! serialize the interngrid derived type
      call c_ESMC_LRIGridSerialize(interngrid%dimCount, &
                                  lrinterngrid%countPerDim(1), &
                                  lrinterngrid%deltaPerDim(1), &
                                  buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! loop over the arrays of coords (only for non-uniform interngrids)
      if (interngrid%horzIGridType.ne.ESMF_INTERNGRID_TYPE_XY_UNI .AND. &
          interngrid%horzIGridType.ne.ESMF_INTERNGRID_TYPE_LATLON_UNI) then
        do i = 1,interngrid%dimCount
          call c_ESMC_LocalArraySerialize(lrinterngrid%coords(i), buffer(1), length, &
                                     offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        enddo
      endif

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LRIGridDeserialize"

!BOPI
! !IROUTINE: ESMF_LRIGridDeserialize - Deserialize a byte stream into an LRIGrid
!
! !INTERFACE:
      subroutine ESMF_LRIGridDeserialize(interngrid, buffer, offset, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), pointer :: interngrid
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
      integer :: i
      type(ESMF_LogRectIGrid), pointer :: lrinterngrid  ! lrinterngrid class

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! allocate logRectIGrid derived type
      allocate(interngrid%interngridSpecific%logRectIGrid, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "logRectIGrid", &
                                     ESMF_CONTEXT, rc)) return
      
      ! shortcut to internals
      lrinterngrid => interngrid%interngridSpecific%logRectIGrid
      call ESMF_LRIGridConstructSpecificNew(lrinterngrid, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! serialize the interngrid derived type
      call c_ESMC_LRIGridDeserialize(lrinterngrid%countPerDim(1), &
                                    lrinterngrid%deltaPerDim(1), &
                                    buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! loop over the arrays of coords
      if (interngrid%horzIGridType.ne.ESMF_INTERNGRID_TYPE_XY_UNI .AND. &
          interngrid%horzIGridType.ne.ESMF_INTERNGRID_TYPE_LATLON_UNI) then
        allocate(lrinterngrid%coords(interngrid%dimCount), stat=status)
        if (ESMF_LogMsgFoundAllocError(status, "logRectIGrid", &
                                     ESMF_CONTEXT, rc)) return
        do i = 1,interngrid%dimCount
          call c_ESMC_LocalArrayDeserialize(lrinterngrid%coords(i), buffer(1), &
                                       offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
        enddo
      endif

      ! call the appropriate create function -- is this necessary or is it all there?

      ! Set interngrid as created 
      ! (this may be done twice if this sub. is called from a general
      !  interngrid create subroutine, but that should be ok, since
      !  it doesn't matter how often a var. is set to the same value.)
      ESMF_INIT_SET_CREATED(interngrid)

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRIGridDeserialize

!------------------------------------------------------------------------------
 
      end module ESMF_LogRectIGridMod
