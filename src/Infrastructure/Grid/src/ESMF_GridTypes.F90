! $Id: ESMF_GridTypes.F90,v 1.22 2004/04/09 17:44:43 jwolfe Exp $
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
!     ESMF Grid Types Module
      module ESMF_GridTypesMod
!
!==============================================================================
!
! This file contains the Grid class definition and basic Grid class
! methods.  These are used by the main Grid module interfaces which
! provide the user interface for grid functions. 
!
!------------------------------------------------------------------------------
! INCLUDES
!!#include "ESMF_Grid.h"  ! unneeded
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_GridTypesMod - Grid class data types
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the 
! {\tt ESMF\_Grid} class.  This class provides utilities for the Grid
! class that are used by the main {\tt ESMF\_Grid} module.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_DataMapMod     ! ESMF data map class
      use ESMF_DELayoutMod    ! ESMF layout class
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_PhysCoordMod   ! ESMF physical coord class
      use ESMF_PhysGridMod    ! ESMF physical grid class
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!  For now, add derived types for specific grid structures here:
!  These derived types contain all the necessary information beyond the
!  GridClass derived type.
!------------------------------------------------------------------------------
!     ! ESMF_LogRectGrid
!
!     ! Type to contain extra information for Logically rectangular grids.

      type ESMF_LogRectGrid
      sequence
        integer, dimension(ESMF_MAXGRIDDIM) :: countPerDim
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: deltaPerDim
        type(ESMF_LocalArray), dimension(:), pointer :: coords
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridSpecific
!
!     ! Type to hold pointers to all available specific grid derived types

      type ESMF_GridSpecific
      sequence
        type (ESMF_LogRectGrid), pointer :: logRectGrid
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridStatus
!
!     ! Type to specify overall status of grid.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_GridStatus
      sequence
        integer :: gridStatus
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridStructure
!
!     ! Type to specify overall structure of grid for supported ESMF Grids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_GridStructure
      sequence
!      private
        integer :: gridStructure
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridType
!
!     ! Type to specify kind of grid for supported ESMF Grids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_GridType
      sequence
!      private
        integer :: gridType
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridStagger
!
!     ! Type to specify type of grid staggering for supported ESMF Grids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_GridStagger
      sequence
!      private
        integer :: stagger
      end type

!------------------------------------------------------------------------------
!     ! ESMF_CoordOrder
!
!     ! Type to specify logical ordering of coordinate in ESMF Grids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_CoordOrder
      sequence
!      private
        integer :: order
      end type

!------------------------------------------------------------------------------
!     ! ESMF_CoordIndex
!
!     ! Type to specify global or local indexing of ESMF Grids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_CoordIndex
      sequence
!      private
        integer :: index
      end type

!------------------------------------------------------------------------------
!     !  ESMF_GridClass
!
!     ! Definition for the Grid class.

      type ESMF_GridClass
      sequence
!      private

        type (ESMF_Base) :: base               ! base class object
        type (ESMF_GridStatus) :: gridStatus   ! uninitialized, init ok, etc
        integer :: dimCount
        type (ESMF_GridStructure) :: gridStructure
                                               ! enum for structure of grid
                                               ! i.e. logically rectangular, etc
        type (ESMF_GridType) :: horzGridType   ! enum for type of horizontal grid
        type (ESMF_GridType) :: vertGridType   ! enum for type of vertical grid
        type (ESMF_GridStagger) :: horzStagger ! enum for horizontal grid staggering
        type (ESMF_GridStagger) :: vertStagger ! enum for vertical grid staggering
        type (ESMF_CoordSystem) :: horzCoordSystem  
                                               ! identifier for horizontal
                                               ! physical coordinate system
        type (ESMF_CoordSystem) :: vertCoordSystem  
                                               ! identifier for vertical
                                               ! physical coordinate system
        type (ESMF_CoordOrder) :: coordOrder   ! enum for mapping of xyz to ijk
        type (ESMF_CoordIndex) :: coordIndex   ! enum for global, local indexing
        type (ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
                                               ! logical identifier to indicate
                                               ! periodic boundary conditions in
                                               ! each direction
        integer :: numPhysGrids                ! number of grid descriptors
                                               ! necessary to support
                                               ! staggering, vertical
                                               ! grids, background grids
        integer :: numPhysGridsAlloc           ! number of physgrids allocated
        type (ESMF_PhysGrid), dimension(:), pointer :: physgrids
                                               ! info for all grid descriptions
                                               ! necessary to define horizontal,
                                               ! staggered and vertical grids
        integer, dimension(:), pointer :: distGridIndex
                                               ! for each physgrid, the index of
                                               ! the corresponding DistGrid
        integer :: numDistGrids                ! number of grid descriptors
                                               ! necessary to support
                                               ! staggering, vertical
                                               ! grids, background grids
        integer :: numDistGridsAlloc           ! number of DistGrids allocated
        type (ESMF_DistGrid), dimension(:), pointer :: distgrids       
                                               ! decomposition and other
                                               ! logical space info for grid
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: minGlobalCoordPerDim
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: maxGlobalCoordPerDim
        character(len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: dimNames
        character(len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: dimUnits
        type (ESMF_LocalArray) :: boundingBoxes
                                            ! array of bounding boxes on each DE
                                            ! used for search routines
        type (ESMF_GridSpecific) :: gridSpecific
!       type (???) :: searchStructure

      end type

!------------------------------------------------------------------------------
!     !  ESMF_Grid
!
!     ! The Grid data structure that is passed between languages.

      type ESMF_Grid
      sequence
!      private
#ifndef ESMF_NO_INITIALIZERS
        type (ESMF_GridClass), pointer :: ptr => NULL()
#else
        type (ESMF_GridClass), pointer :: ptr
#endif
      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

      public ESMF_LogRectGrid, ESMF_GridSpecific, ESMF_GridStatus
      public ESMF_GridStructure
      public ESMF_GridClass,    ESMF_GridType,     ESMF_GridStagger
      public ESMF_CoordOrder,  ESMF_CoordIndex,   ESMF_Grid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    ! These functions are generally accessed only by the main Grid module
    ! and are not meant to be accessed by the user.  Well...except for
    ! the overloaded operators.
    
    public ESMF_GridConstructNew
    public ESMF_GridGetDELayout
    public ESMF_GridAddAttribute
    public ESMF_GridGetAttributes
    public ESMF_GridAddDistGrid
    public ESMF_GridMakeDistGridSpace
    public ESMF_GridAddPhysGrid
    public ESMF_GridMakePhysGridSpace
    public ESMF_GridGetPhysGrid
    public ESMF_GridGetPhysGridID
    public ESMF_GridGetDistGrid
    public ESMF_GridGetBoundingBoxes

    public operator(==), operator(/=) ! for overloading 
                                      ! comparison functions

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:
  ! Supported ESMF structure kinds:
  !   ESMF_GridStatus_Unknown           ! unknown or undefined status
  !   ESMF_GridStatus_Uninit            ! uninitialized
  !   ESMF_GridStatus_Ready             ! completely ready for use
  !   ESMF_GridStatus_Unallocated       ! unallocated
  !   ESMF_GridStatus_Allocated         ! allocated
  !   ESMF_GridStatus_Init              ! initialized but not distributed
  !   ESMF_GridStatus_Invalid           ! invalid

   type (ESMF_GridStatus), parameter, public ::              &
      ESMF_GridStatus_Unknown      =  ESMF_GridStatus(0), &
      ESMF_GridStatus_Uninit       =  ESMF_GridStatus(1), &
      ESMF_GridStatus_Ready        =  ESMF_GridStatus(2), &
      ESMF_GridStatus_Unallocated  =  ESMF_GridStatus(3), &
      ESMF_GridStatus_Allocated    =  ESMF_GridStatus(4), &
      ESMF_GridStatus_Init         =  ESMF_GridStatus(5), &
      ESMF_GridStatus_Invalid      =  ESMF_GridStatus(6)

  ! Supported ESMF structure kinds:
  !   ESMF_GridStructure_Unknown           ! unknown or undefined grid
  !   ESMF_GridStructure_LogRect           ! logically rectangular grid
  !   ESMF_GridStructure_LogRectBlock      ! logically rectangular blocked grid
  !   ESMF_GridStructure_Unstruct          ! unstructured grid
  !   ESMF_GridStructure_User              ! user-defined grid

   type (ESMF_GridStructure), parameter, public ::              &
      ESMF_GridStructure_Unknown      =  ESMF_GridStructure(0), &
      ESMF_GridStructure_LogRect      =  ESMF_GridStructure(1), &
      ESMF_GridStructure_LogRectBlock =  ESMF_GridStructure(2), &
      ESMF_GridStructure_Unstruct     =  ESMF_GridStructure(3), &
      ESMF_GridStructure_User         =  ESMF_GridStructure(4)

  ! TODO: add kinds for vertical coordinates
  ! Supported ESMF grid kinds:
  !   ESMF_GridType_Unknown           ! unknown or undefined grid
  !   ESMF_GridType_LatLon            ! aligned with longitude,latitude
  !   ESMF_GridType_LatLonGauss       ! LatLon with gaussian-spaced latitudes
  !   ESMF_GridType_LatLonMercator    ! LatLon with Mercator-spaced latitudes
  !   ESMF_GridType_Reduced           ! LatLon with num lon pts a fcn of lat
  !   ESMF_GridType_Dipole            ! Displaced-pole dipole grid
  !   ESMF_GridType_Tripole           ! Tripolar grids
  !   ESMF_GridType_XY                ! aligned with Cartesian x-y coords
  !   ESMF_GridType_XYVar             ! XY grid with unequal spacing
  !   ESMF_GridType_DataStream        ! Data stream - set of locations
  !   ESMF_GridType_PhysFourier       ! Mixed Fourier/Phys Space grid
  !   ESMF_GridType_SphericalSpectral ! spectral space:spherical harmonics
  !   ESMF_GridType_CartSpectral      ! spectral space:Cartesian coords
  !   ESMF_GridType_Geodesic          ! spherical geodesic grid
  !   ESMF_GridType_CubedSphere       ! cubed sphere grid
  !   ESMF_GridType_Exchange          ! intersection of two grids

   type (ESMF_GridType), parameter, public ::              &
      ESMF_GridType_Unknown           = ESMF_GridType( 0), &
      ESMF_GridType_LatLon            = ESMF_GridType( 1), &
      ESMF_GridType_LatLonGauss       = ESMF_GridType( 2), &
      ESMF_GridType_LatLonMercator    = ESMF_GridType( 3), &
      ESMF_GridType_Reduced           = ESMF_GridType( 4), &
      ESMF_GridType_Dipole            = ESMF_GridType( 5), &
      ESMF_GridType_Tripole           = ESMF_GridType( 6), &
      ESMF_GridType_XY                = ESMF_GridType( 7), &
      ESMF_GridType_XYVar             = ESMF_GridType( 8), &
      ESMF_GridType_DataStream        = ESMF_GridType( 9), &
      ESMF_GridType_PhysFourier       = ESMF_GridType(10), &
      ESMF_GridType_SphericalSpectral = ESMF_GridType(11), &
      ESMF_GridType_CartSpectral      = ESMF_GridType(12), &
      ESMF_GridType_Geodesic          = ESMF_GridType(13), &
      ESMF_GridType_CubedSphere       = ESMF_GridType(14), &
      ESMF_GridType_Exchange          = ESMF_GridType(15)

   ! Recognized ESMF staggering types
   !   ESMF_GridStagger_Unknown    ! unknown or undefined staggering
   !   ESMF_GridStagger_A          ! Arakawa A (centered velocity)
   !   ESMF_GridStagger_B_NE       ! Arakawa B (U,V at NE corner)
   !   ESMF_GridStagger_B_SW       ! Arakawa B (U,V at SW corner)
   !   ESMF_GridStagger_B_SE       ! Arakawa B (U,V at SE corner)
   !   ESMF_GridStagger_B_NW       ! Arakawa B (U,V at NW corner)
   !   ESMF_GridStagger_C_NE       ! Arakawa C (U at E face, V at N face)
   !   ESMF_GridStagger_C_SW       ! Arakawa C (U at W face, V at S face)
   !   ESMF_GridStagger_C_SE       ! Arakawa C (U at E face, V at S face)
   !   ESMF_GridStagger_C_NW       ! Arakawa C (U at W face, V at N face)
   !   ESMF_GridStagger_D_NE       ! Arakawa D (V at E face, U at N face)
   !   ESMF_GridStagger_D_SW       ! Arakawa D (V at W face, U at S face)
   !   ESMF_GridStagger_D_SE       ! Arakawa D (V at E face, U at S face)
   !   ESMF_GridStagger_D_NW       ! Arakawa D (V at W face, U at N face)
   !   ESMF_GridStagger_E          ! Arakawa E
   !   ESMF_GridStagger_Z          ! C grid equiv for geodesic grid
   !   ESMF_GridStagger_VertCenter ! vertical midpoints
   !   ESMF_GridStagger_VertTop    ! at top    face of vertical grid
   !   ESMF_GridStagger_VertBottom ! at bottom face of vertical grid

   type (ESMF_GridStagger), parameter, public ::          &
      ESMF_GridStagger_Unknown    = ESMF_GridStagger( 0), &
      ESMF_GridStagger_A          = ESMF_GridStagger( 1), &
      ESMF_GridStagger_B_NE       = ESMF_GridStagger( 2), &
      ESMF_GridStagger_B_SW       = ESMF_GridStagger( 3), &
      ESMF_GridStagger_B_SE       = ESMF_GridStagger( 4), &
      ESMF_GridStagger_B_NW       = ESMF_GridStagger( 5), &
      ESMF_GridStagger_C_NE       = ESMF_GridStagger( 6), &
      ESMF_GridStagger_C_SW       = ESMF_GridStagger( 7), &
      ESMF_GridStagger_C_SE       = ESMF_GridStagger( 8), &
      ESMF_GridStagger_C_NW       = ESMF_GridStagger( 9), &
      ESMF_GridStagger_D_NE       = ESMF_GridStagger(10), &
      ESMF_GridStagger_D_SW       = ESMF_GridStagger(11), &
      ESMF_GridStagger_D_SE       = ESMF_GridStagger(12), &
      ESMF_GridStagger_D_NW       = ESMF_GridStagger(13), &
      ESMF_GridStagger_E          = ESMF_GridStagger(14), &
      ESMF_GridStagger_Z          = ESMF_GridStagger(15), &
      ESMF_GridStagger_VertCenter = ESMF_GridStagger(16), &
      ESMF_GridStagger_VertTop    = ESMF_GridStagger(17), &
      ESMF_GridStagger_VertBottom = ESMF_GridStagger(18)

   ! Recognized coordinate orderings
   !   ESMF_CoordOrder_Unknown  ! unknown or undefined coord ordering
   !   ESMF_CoordOrder_XYZ      ! IJK maps to XYZ
   !   ESMF_CoordOrder_XZY      ! IJK maps to XZY
   !   ESMF_CoordOrder_YXZ      ! IJK maps to YXZ
   !   ESMF_CoordOrder_YZX      ! IJK maps to YZX
   !   ESMF_CoordOrder_ZXY      ! IJK maps to ZXY
   !   ESMF_CoordOrder_ZYX      ! IJK maps to ZYX

   type (ESMF_CoordOrder), parameter, public ::      &
      ESMF_CoordOrder_Unknown = ESMF_CoordOrder( 0), &
      ESMF_CoordOrder_XYZ     = ESMF_CoordOrder( 1), &
      ESMF_CoordOrder_XZY     = ESMF_CoordOrder( 2), &
      ESMF_CoordOrder_YXZ     = ESMF_CoordOrder( 3), &
      ESMF_CoordOrder_YZX     = ESMF_CoordOrder( 4), &
      ESMF_CoordOrder_ZXY     = ESMF_CoordOrder( 5), &
      ESMF_CoordOrder_ZYX     = ESMF_CoordOrder( 6)

   ! Recognized coordinate indexings
   !   ESMF_CoordIndex_Unknown  ! unknown or undefined coord indexing
   !   ESMF_CoordIndex_Local    ! uses local indexing
   !   ESMF_CoordIndex_Global   ! uses global indexing

   type (ESMF_CoordIndex), parameter, public ::      &
      ESMF_CoordIndex_Unknown = ESMF_CoordIndex( 0), &
      ESMF_CoordIndex_Local   = ESMF_CoordIndex( 1), &
      ESMF_CoordIndex_Global  = ESMF_CoordIndex( 2)

   integer, dimension(3, 6, 3), parameter, public :: &
      gridOrder = reshape((/ 1, 1, 1, &
                             1, 1, 1, &
                             1, 1, 1, &
                             1, 1, 1, &
                             1, 1, 1, &
                             1, 1, 1, &
                             1, 2, 1, &
                             1, 2, 1, &
                             2, 1, 1, &
                             2, 1, 1, &
                             1, 2, 1, &
                             2, 1, 1, &
                             1, 2, 3, &
                             1, 3, 2, &
                             2, 1, 3, &
                             2, 3, 1, &
                             3, 1, 2, &
                             3, 2, 1  /), (/ 3, 6, 3 /))

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_GridTypes.F90,v 1.22 2004/04/09 17:44:43 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridStatusEqual
         module procedure ESMF_GridStructureEqual
         module procedure ESMF_GridTypeEqual
         module procedure ESMF_GridStaggerEqual
         module procedure ESMF_CoordOrderEqual
         module procedure ESMF_CoordIndexEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF Grid ids (enums).  It is provided for easy comparisons of 
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
         module procedure ESMF_GridStatusNotEqual
         module procedure ESMF_GridStructureNotEqual
         module procedure ESMF_GridTypeNotEqual
         module procedure ESMF_GridStaggerNotEqual
         module procedure ESMF_CoordOrderNotEqual
         module procedure ESMF_CoordIndexNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF Grid ids (enums).  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
!BOPI
! !IROUTINE: ESMF_GridConstructNew - Construct the internals of an allocated Grid

! !INTERFACE:
      subroutine ESMF_GridConstructNew(grid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_Grid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_GridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_GridCreate}, which calls
!     {\tt ESMF\_GridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid}
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i
      character (len = ESMF_MAXSTR) :: defaultname ! default grid name

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Set the Grid name if present, otherwise construct a default one
      call ESMF_BaseCreate(grid%base, "Grid", name, 0, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructNew: BaseCreate"
        return
      endif

      ! Initialize grid contents
      grid%gridStatus      = ESMF_GridStatus_Ready
      grid%gridStructure   = ESMF_GridStructure_Unknown
      grid%horzGridType    = ESMF_GridType_Unknown
      grid%vertGridType    = ESMF_GridType_Unknown
      grid%horzStagger     = ESMF_GridStagger_Unknown
      grid%vertStagger     = ESMF_GridStagger_Unknown
      grid%horzCoordSystem = ESMF_CoordSystem_Unknown
      grid%vertCoordSystem = ESMF_CoordSystem_Unknown
      grid%coordOrder      = ESMF_CoordOrder_XYZ
      do i=1,ESMF_MAXGRIDDIM
        grid%periodic(i)     = ESMF_FALSE
      !   grid%coversDomain(i) = ESMF_TRUE
      enddo
      grid%numDistGrids = 0
      grid%numDistGridsAlloc = 0
      nullify(grid%distGrids)
      grid%numPhysGrids = 0
      grid%numPhysGridsAlloc = 0
      nullify(grid%physGrids)
      nullify(grid%distGridIndex)
      ! nullify(grid%gridSpecific)

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetDELayout - Get pointer to a DELayout from a Grid

! !INTERFACE:
      subroutine ESMF_GridGetDELayout(grid, layout, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_DELayout) :: layout
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[layout]
!          Pointer to the {\tt ESMF\_DELayout} corresponding to the
!          {\tt ESMF\_Grid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! call DistGrid method to retrieve information otherwise not available
      ! to the application level -- does not matter which one since they all share
      ! the same layout    !TODO: move layout to Grid class?
      call ESMF_DistGridGetDELayout(grid%ptr%distgrids(1)%ptr, layout, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetDELayout: distgrid get delayout"
        return
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDELayout

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridAddAttribute - sets some attributes of Grid

! !INTERFACE:
      subroutine ESMF_GridAddAttribute(grid,  name,            &
                              horzGridType,    vertGridType,    &
                              horzStagger,     vertStagger,     &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder,      coordIndex,      &
                              numPhysGrids,    numDistGrids,    &
                              periodic,                         &
                              minGlobalCoordPerDim,  maxGlobalCoordPerDim,  rc)

!
! !ARGUMENTS:

      type(ESMF_Grid), intent(inout) :: grid

      character (len=*),       intent(in), optional :: name
      type (ESMF_GridType),    intent(in), optional :: horzGridType
      type (ESMF_GridType),    intent(in), optional :: vertGridType
      type (ESMF_GridStagger), intent(in), optional :: horzStagger
      type (ESMF_GridStagger), intent(in), optional :: vertStagger
      type (ESMF_CoordSystem), intent(in), optional :: horzCoordSystem  
      type (ESMF_CoordSystem), intent(in), optional :: vertCoordSystem  
      type (ESMF_CoordOrder),  intent(in), optional :: coordOrder
      type (ESMF_CoordIndex),  intent(in), optional :: coordIndex
      integer,                 intent(in), optional :: numPhysGrids
      integer,                 intent(in), optional :: numDistGrids

      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                       minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                       maxGlobalCoordPerDim
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic

      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     This routine sets a variety of information about an {\tt ESMF\_Grid}.
!     It is meant to set only general attribute information; more specific
!     information about coordinates, physical grids, etc. are accessed from
!     other routines.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          The {\tt ESMF\_Grid} to be modified.
!     \item[{[name]}]
!          Character name to be assigned to this grid.
!     \item[{[horzGridType]}]
!          {\tt ESMF\_GridType} describing the horizontal grid.
!     \item[{[vertGridType]}]
!          {\tt ESMF\_GridType} describing the vertical   grid.
!     \item[{[horzStagger]}]
!          {\tt ESMF\_GridStagger} describing staggering of variables on
!          horizontal grid.
!     \item[{[vertStagger]}]
!          {\tt ESMF\_GridStagger} describing staggering of variables on
!          vertical grid.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} identifying the ESMF standard physical
!          coordinate system for the horizontal grid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} identifying the ESMF standard physical
!          coordinate system for the vertical grid.
!     \item[{[coordOrder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote coordinate ordering.
!     \item[{[coordIndex]}]
!          {\tt ESMF\_CoordIndex} specifier to denote global or local indexing.
!     \item[{[numPhysGrids]}]
!          Integer specifying the number of {\tt ESMF\_PhysGrids} necessary
!          to support the staggered grids and vertical grids aggregated in
!          this grid structure.
!     \item[{[numDistGrids]}]
!          Integer specifying the number of {\tt ESMF\_DistGrids} necessary
!          to support the staggered grids and vertical grids aggregated in
!          this grid structure.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along each coordinate
!          axis.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! loop index

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! if present, set information filling in grid derived type
      if (present(horzGridType   )) grid%ptr%horzGridType    = horzGridType 
      if (present(vertGridType   )) grid%ptr%vertGridType    = vertGridType 
      if (present(horzStagger    )) grid%ptr%horzStagger     = horzStagger 
      if (present(vertStagger    )) grid%ptr%vertStagger     = vertStagger 
      if (present(horzCoordSystem)) grid%ptr%horzCoordSystem = horzCoordSystem  
      if (present(vertCoordSystem)) grid%ptr%vertCoordSystem = vertCoordSystem  
      if (present(coordOrder     )) grid%ptr%coordOrder      = coordOrder 
      if (present(coordIndex     )) grid%ptr%coordIndex      = coordIndex 

      ! Set the Grid name if present, otherwise construct a default one
      if (present(name)) then
         call ESMF_SetName(grid%ptr%base, name, "Grid", status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridAddAttribute: Setname"
            return
         endif
      endif

      ! Set number of physical grids - check to make sure number of
      ! allocated grids does not already exceed this value.
      if (present(numPhysGrids)) then
         if (numPhysGrids > grid%ptr%numPhysGridsAlloc) &
            grid%ptr%numPhysGrids = numPhysGrids
      endif
      
      ! Set number of dist grids - check to make sure number of
      ! allocated grids does not already exceed this value.
      if (present(numDistGrids)) then
         if (numDistGrids > grid%ptr%numDistGridsAlloc) &
            grid%ptr%numDistGrids = numDistGrids
      endif
      
      ! Set periodic flags for each dimension
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            grid%ptr%periodic(i) = periodic(i)
         enddo
      endif

      ! Set global domain limits
      if (present(minGlobalCoordPerDim)) then
         if (size(minGlobalCoordPerDim) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridAddAttribute: minGlobalCoordPerDim too big'
            return
         endif
         do i=1,size(minGlobalCoordPerDim)
            grid%ptr%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
         enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
         if (size(maxGlobalCoordPerDim) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridAddAttribute: maxGlobalCoordPerDim too big'
            return
         endif
         do i=1,size(maxGlobalCoordPerDim)
            grid%ptr%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
         enddo
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddAttribute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetAttributes - retrieves some attributes of Grid

! !INTERFACE:
      subroutine ESMF_GridGetAttributes(grid,  name,            &
                              horzGridType,    vertGridType,    &
                              horzStagger,     vertStagger,     &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder,      coordIndex,      &
                              numPhysGrids,    numDistGrids,    &
                              periodic,                         &
                              minGlobalCoordPerDim,  maxGlobalCoordPerDim,  rc)

!
! !ARGUMENTS:

      type(ESMF_Grid), intent(in) :: grid

      character(len=ESMF_MAXSTR), intent(out), optional :: name
      type (ESMF_GridType),       intent(out), optional :: horzGridType
      type (ESMF_GridType),       intent(out), optional :: vertGridType
      type (ESMF_GridStagger),    intent(out), optional :: horzStagger
      type (ESMF_GridStagger),    intent(out), optional :: vertStagger
      type (ESMF_CoordSystem),    intent(out), optional :: horzCoordSystem  
      type (ESMF_CoordSystem),    intent(out), optional :: vertCoordSystem  
      type (ESMF_CoordOrder),     intent(out), optional :: coordOrder
      type (ESMF_CoordIndex),     intent(out), optional :: coordIndex
      integer,                    intent(out), optional :: numPhysGrids
      integer,                    intent(out), optional :: numDistGrids

      real(ESMF_KIND_R8), dimension(:), intent(out), optional :: &
                                                           minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(out), optional :: &
                                                           maxGlobalCoordPerDim
      type(ESMF_Logical), dimension(:), intent(out), optional :: periodic

      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     This routine retrieves information about an {\tt ESMF\_Grid}, determined
!     through a set of optional arguments.
!     It is meant to retrieve only general attribute information; more specific
!     information about coordinates, physical grids, etc. are accessed from
!     other routines.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          The {\tt ESMF\_Grid} to be queried.
!     \item[{[name]}]
!          Character name assigned to this grid.
!     \item[{[horzGridType]}]
!          {\tt ESMF\_GridType} describing the horizontal grid.
!     \item[{[vertGridType]}]
!          {\tt ESMF\_GridType} describing the vertical   grid.
!     \item[{[horzStagger]}]
!          {\tt ESMF\_GridStagger} describing staggering of variables on
!          horizontal grid.
!     \item[{[vertStagger]}]
!          {\tt ESMF\_GridStagger} describing staggering of variables on
!          vertical grid.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} identifying the ESMF standard physical
!          coordinate system for the horizontal grid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} identifying the ESMF standard physical
!          coordinate system for the vertical grid.
!     \item[{[coordOrder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote coordinate ordering.
!     \item[{[coordIndex]}]
!          {\tt ESMF\_CoordIndex} specifier to denote global or local indexing.
!     \item[{[numPhysGrids]}]
!          Integer specifying the number of {\tt ESMF\_PhysGrids} necessary
!          to support the staggered grids and vertical grids aggregated in
!          this grid structure.
!     \item[{[numDistGrids]}]
!          Integer specifying the number of {\tt ESMF\_DistGrids} necessary
!          to support the staggered grids and vertical grids aggregated in
!          this grid structure.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along each coordinate
!          axis.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! loop index

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Get the Grid name if present
      if (present(name)) then
         call ESMF_GetName(grid%ptr%base, name, status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridGetAttributes: GetName"
            return
         endif
      endif

      ! if present, get information from grid derived type
      if (present(horzGridType   )) horzGridType    = grid%ptr%horzGridType
      if (present(vertGridType   )) vertGridType    = grid%ptr%vertGridType
      if (present(horzStagger    )) horzStagger     = grid%ptr%horzStagger
      if (present(vertStagger    )) vertStagger     = grid%ptr%vertStagger
      if (present(horzCoordSystem)) horzCoordSystem = grid%ptr%horzCoordSystem
      if (present(vertCoordSystem)) vertCoordSystem = grid%ptr%vertCoordSystem
      if (present(coordOrder     )) coordOrder      = grid%ptr%coordOrder
      if (present(coordIndex     )) coordIndex      = grid%ptr%coordIndex
      if (present(numPhysGrids   )) numPhysGrids    = grid%ptr%numPhysGrids
      if (present(numDistGrids   )) numDistGrids    = grid%ptr%numDistGrids

      ! Get periodic flags for each dimension
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            periodic(i) = grid%ptr%periodic(i)
         enddo
      endif

      ! Get global domain limits
      if (present(minGlobalCoordPerDim)) then
         if (size(minGlobalCoordPerDim) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridGetAttributes: minGlobalCoordPerDim too big'
            return
         endif
         do i=1,size(minGlobalCoordPerDim)
            minGlobalCoordPerDim(i) = grid%ptr%minGlobalCoordPerDim(i)
         enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
         if (size(maxGlobalCoordPerDim) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridGetAttributes: maxGlobalCoordPerDim too big'
            return
         endif
         do i=1,size(maxGlobalCoordPerDim)
            maxGlobalCoordPerDim(i) = grid%ptr%maxGlobalCoordPerDim(i)
         enddo
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetAttributes

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridAddDistGrid - adds a complete DistGrid to Grid type

! !INTERFACE:
      subroutine ESMF_GridAddDistGrid(grid, distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(inout) :: grid
      type(ESMF_DistGrid), intent(in)    :: distgrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine attaches an {\tt ESMF\_DistGrid} object to an
!     {\tt ESMF\_Grid} object.  It is only meant to be called by
!     grid creation routines in the processes of building a grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Grid structure to which new DistGrid is to be added.
!     \item[distgrid]
!          Complete {\tt ESMF\_DistGrid} to be added.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Update the DistGridAlloc counter and check to see if DistGrid
      ! array needs to be resized to add the new distgrid
      call ESMF_GridMakeDistGridSpace(grid, grid%numDistGrids+1, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridAddDistGrid: make distgrid space"
        return
      endif
      grid%numDistGrids = grid%numDistGrids + 1

      ! Add the DistGrid
      grid%distgrids(grid%numDistGrids) = distgrid

      ! Set return values.
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddDistGrid

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridMakeDistGridSpace - Allocate or extend DistGrid array

! !INTERFACE:
      subroutine ESMF_GridMakeDistGridSpace(gridp, newcount, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: gridp
      integer, intent(in) :: newcount
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Internal routine which verifies there is enough array space to
!     hold the specified number of DistGrids.  Allocates more space and
!     copies the contents into the new space if not.
!
!     The arguments are:
!     \begin{description}
!     \item[gridp]
!          Pointer to an {\tt ESMF\_GridClass}, the internal structure
!          which holds the {\tt Grid} information.
!     \item[newcount]
!          Make sure there are enough space in the array to hold
!          {\tt newcount} items.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      ! note: this is an internal routine.  rc isn't optional - so we
      ! don't have to fool with rcpresent and status here.

      ! the save attribute is to be sure that the temp space isn't
      ! deallocated automatically by the compiler at the return of
      ! this routine. it's going to be pointed to by the DistGrids pointer
      ! in the grid structure, but that might not be obvious to the compiler.
      type(ESMF_DistGrid), dimension(:), pointer, save :: temp_dgrids
      integer :: i, oldcount, alloccount, allocrc

      ! number of currently used and available entries
      oldcount = gridp%numDistGrids
      alloccount = gridp%numDistGridsAlloc

      ! if there are already enough, we are done.
      if (alloccount .ge. newcount) then
         rc = ESMF_SUCCESS
         return
      endif

#define CHUNK 4

      ! if none are allocated yet, allocate and return.
      ! the chunksize is 4 because it is a round number in base 2.
      if (alloccount .eq. 0) then
        allocate(gridp%distGrids(CHUNK), stat=allocrc)
        if (allocrc .ne. 0) then
          print *, "cannot allocate DistGrids, first try"
          print *, "ERROR in ESMF_GridAddDistGrid: DistGrids allocate"
          rc = ESMF_FAILURE
          return
        endif
        gridp%numDistGridsAlloc = CHUNK
        rc = ESMF_SUCCESS
        return
      endif

     ! ok, if you're here then there's already a list and you
     ! need to extend it.  i don't know of any fortran intrinsic functions
     ! to do that, so this: allocates a longer temp list, copies the old
     ! contents over, deallocates the old array, and sets the pointer to
     ! point to the new list.

     alloccount = alloccount + CHUNK

     ! make larger temp space
     allocate(temp_dgrids(alloccount), stat=allocrc)
     if (allocrc .ne. 0) then
       print *, "cannot allocate temp_dgrids, alloc=", alloccount
       print *, "ERROR in ESMF_GridAddDistGrid: temp_dgrids allocate"
       return
     endif

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_dgrids(i) = gridp%distGrids(i)
     enddo

     ! deallocate old array
     deallocate(gridp%distGrids, stat=allocrc)
     if (allocrc .ne. 0) then
       print *, "ERROR in ESMF_GridAddDistGrid: DistGrids deallocate"
       return
     endif

     ! and set original pointer to the new space
     gridp%DistGrids => temp_dgrids

     ! update count of how many items are currently allocated
     gridp%numDistGridsAlloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_GridMakeDistGridSpace

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridAddPhysGrid - adds a complete PhysGrid to Grid type

! !INTERFACE:
      subroutine ESMF_GridAddPhysGrid(grid, physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(inout) :: grid
      type(ESMF_PhysGrid), intent(in)    :: physgrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine attaches an {\tt ESMF\_PhysGrid} object to an
!     {\tt ESMF\_Grid} object.  It is only meant to be called by
!     grid creation routines in the processes of building a grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Grid structure to which new PhysGrid is to be added.
!     \item[physgrid]
!          Complete {\tt ESMF\_PhysGrid} to be added.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

     ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Update the PhysGridAlloc counter and check to see if PhysGrid
      ! array needs to be resized to add the new physgrid
      call ESMF_GridMakePhysGridSpace(grid, grid%numPhysGrids+1, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridAddPhysGrid: make physgrid space"
        return
      endif
      grid%numPhysGrids = grid%numPhysGrids + 1

      ! Add the PhysGrid
      grid%physgrids(grid%numPhysGrids) = physgrid

      ! Set return values.
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddPhysGrid

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridMakePhysGridSpace - Allocate or extend PhysGrid array

! !INTERFACE:
      subroutine ESMF_GridMakePhysGridSpace(gridp, newcount, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: gridp
      integer, intent(in) :: newcount
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Internal routine which verifies there is enough array space to
!     hold the specified number of PhysGrids.  Allocates more space and
!     copies the contents into the new space if not.
!
!     The arguments are:
!     \begin{description}
!     \item[gridp]
!          Pointer to an {\tt ESMF\_GridClass}, the internal structure
!          which holds the {\tt Grid} information.
!     \item[newcount]
!          Make sure there are enough space in the array to hold
!          {\tt newcount} items.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      ! note: this is an internal routine.  rc isn't optional - so we
      ! don't have to fool with rcpresent and status here.

      ! the save attribute is to be sure that the temp space isn't
      ! deallocated automatically by the compiler at the return of
      ! this routine. it's going to be pointed to by the PhysGrids pointer
      ! in the grid structure, but that might not be obvious to the compiler.
      type(ESMF_PhysGrid), dimension(:), pointer, save :: temp_pgrids
      integer, dimension(:), pointer, save :: temp_dgIndex
      integer :: i, oldcount, alloccount, allocrc

      ! number of currently used and available entries
      oldcount = gridp%numPhysGrids
      alloccount = gridp%numPhysGridsAlloc

      ! if there are already enough, we are done.
      if (alloccount .ge. newcount) then
         rc = ESMF_SUCCESS
         return
      endif

#define CHUNK 4

      ! if none are allocated yet, allocate and return.
      ! the chunksize is 4 because it is a round number in base 2.
      if (alloccount .eq. 0) then
        allocate(gridp%physGrids(CHUNK), stat=allocrc)
        if (allocrc .ne. 0) then
          print *, "cannot allocate PhysGrids, first try"
          print *, "ERROR in ESMF_GridAddPhysGrid: PhysGrids allocate"
          rc = ESMF_FAILURE
          return
        endif
        allocate(gridp%distGridIndex(CHUNK), stat=allocrc)
        if (allocrc .ne. 0) then
          print *, "cannot allocate distGridIndex, first try"
          print *, "ERROR in ESMF_GridAddPhysGrid: distGridIndex allocate"
          rc = ESMF_FAILURE
          return
        endif
        gridp%numPhysGridsAlloc = CHUNK
        rc = ESMF_SUCCESS
        return
      endif

     ! ok, if you're here then there's already a list and you
     ! need to extend it.  i don't know of any fortran intrinsic functions
     ! to do that, so this: allocates a longer temp list, copies the old
     ! contents over, deallocates the old array, and sets the pointer to
     ! point to the new list.

     alloccount = alloccount + CHUNK

     ! make larger temp space
     allocate(temp_pgrids(alloccount), stat=allocrc)
     if (allocrc .ne. 0) then
       print *, "cannot allocate temp_pgrids, alloc=", alloccount
       print *, "ERROR in ESMF_GridAddPhysGrid: temp_pgrids allocate"
       return
     endif
     allocate(temp_dgIndex(alloccount), stat=allocrc)
     if (allocrc .ne. 0) then
       print *, "cannot allocate temp_dgIndex, alloc=", alloccount
       print *, "ERROR in ESMF_GridAddPhysGrid: temp_dgIndex allocate"
       return
     endif

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_pgrids(i)  = gridp%physGrids(i)
       temp_dgIndex(i) = gridp%distGridIndex(i)
     enddo

     ! deallocate old arrays
     deallocate(gridp%physGrids, stat=allocrc)
     if (allocrc .ne. 0) then
       print *, "ERROR in ESMF_GridAddPhysGrid: PhysGrids deallocate"
       return
     endif
     deallocate(gridp%distGridIndex, stat=allocrc)
     if (allocrc .ne. 0) then
       print *, "ERROR in ESMF_GridAddPhysGrid: distGridIndex deallocate"
       return
     endif

     ! and set original pointers to the new space
     gridp%physGrids => temp_pgrids
     gridp%distGridIndex => temp_dgIndex

     ! update count of how many items are currently allocated
     gridp%numPhysGridsAlloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_GridMakePhysGridSpace

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetPhysGrid - retrieves complete PhysGrid from Grid type

! !INTERFACE:
      subroutine ESMF_GridGetPhysGrid(physgrid, grid, relloc, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(out) :: physgrid
      type(ESMF_GridClass), intent(in)  :: grid
      type(ESMF_RelLoc), intent(in), optional :: relloc
      character(*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine retrieves an {\tt ESMF\_PhysGrid} object from an
!     {\tt ESMF\_Grid} object.  A PhysGrid can be retrieved either
!     by name or by relative location.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} to be retrieved.
!     \item[grid]
!          Grid structure from which PhysGrid is to be extracted.
!     \item[{[relloc]}]
!          Relative location ({\tt ESMF_RelLoc}) to identify which
!          PhysGrid to retrieve.
!     \item[{[name]}]
!          Optional name to identify which PhysGrid to retrieve.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: n                    ! search loop index
      integer :: status               ! Error status
      logical :: rcpresent            ! Return code present
      logical :: found                ! found flag for searches
      character (len=ESMF_MAXSTR) :: nameTmp    ! temporary name variable
      type(ESMF_RelLoc) :: rellocTmp

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! If name supplied, search by name and return selected PhysGrid
      if (present(name)) then
         found = .false.
         name_search: do n=1,grid%numPhysGridsAlloc
            call ESMF_PhysGridGet(grid%physgrids(n), name=nameTmp, rc=status)
            if (status /= ESMF_SUCCESS) then
               print *, "ERROR in ESMF_GridGetPhysGrid: error getting name"
               return
            endif
            if (name == nameTmp) then
               physgrid = grid%physgrids(n)
               found = .true.
               exit name_search
            endif
         end do name_search
         if (.not. found) then
            print *,'ERROR in ESMF_GridGetPhysGrid: unknown name'
         endif

      ! If relloc supplied, search by relloc and return selected PhysGrid
      else if (present(relloc)) then
         found = .false.
         relloc_search: do n=1,grid%numPhysGridsAlloc
            call ESMF_PhysGridGet(grid%physgrids(n), relloc=rellocTmp, rc=status)
            if (status /= ESMF_SUCCESS) then
               print *, "ERROR in ESMF_GridGetPhysGrid: error getting relloc"
               return
            endif
            if (relloc == rellocTmp) then
               physgrid = grid%physgrids(n)
               found = .true.
               exit relloc_search
            endif
         end do relloc_search
         if (.not. found) then
            print *,'ERROR in ESMF_GridGetPhysGrid: unknown relloc'
            return
         endif
      
      ! If neither supplied, return error.
      else
         print *,'ERROR in GridGetPhysGrid: must supply name or relloc'
         return
      endif

      ! Set return values.
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetPhysGrid

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetPhysGridID - Get PhysGrid Id for a given relative location

! !INTERFACE:
      subroutine ESMF_GridGetPhysGridID(grid, relloc, physGridId, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(in) :: grid
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(out) :: physGridId
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return the {\tt ESMF\_PhysGridId} associated with the given relative
!     location.  Return error if the grid contains no PhysGrid at the
!     specified location.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[relloc]
!          Relative location to query
!     \item[physGridId]
!          Returned physGrid identifier.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      type(ESMF_RelLoc) :: thisRelloc
      integer :: i

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      physGridId = -1

      ! Loop through physGrids comparing rellocs  TODO: make part of the Grid obj?
      do i = 1,grid%numPhysGrids
        call ESMF_PhysGridGet(grid%physGrids(i), relloc=thisRelloc, &
                              rc=status)
        if (relloc.eq.thisRelloc) then
          physGridId = i
          status = ESMF_SUCCESS
          exit
        endif
      enddo

      if (rcpresent) rc = status

      end subroutine ESMF_GridGetPhysGridID

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetDistGrid - retrieves complete DistGrid from Grid type

! !INTERFACE:
      subroutine ESMF_GridGetDistGrid(distgrid, grid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(out) :: distgrid
      type(ESMF_GridClass), intent(in)  :: grid
      character(*), intent(in) :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine retrieves an {\tt ESMF\_DistGrid} object from an
!     {\tt ESMF\_Grid} object, given the name of the distgrid.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          {\tt ESMF\_DistGrid} to be retrieved.
!     \item[grid]
!          Grid structure from which DistGrid is to be extracted.
!     \item[name]
!          Name to identify which DistGrid to retrieve.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: n                    ! search loop index
      integer :: status               ! Error status
      logical :: rcpresent            ! Return code present
      logical :: found                ! found flag for searches
      character (len=ESMF_MAXSTR) :: nameTmp    ! temporary name variable

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! Search by name and return selected DistGrid
      found = .false.
      name_search: do n=1,grid%numDistGridsAlloc
         call ESMF_DistGridGet(grid%distgrids(n), name=nameTmp, rc=status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridGetDistGrid: error getting name"
            return
         endif
         if (name == nameTmp) then
            distgrid = grid%distgrids(n)
            found = .true.
            exit name_search
         endif
      end do name_search
      if (.not. found) then
         print *,'ERROR in ESMF_GridGetDistGrid: unknown name'
      endif

      ! Set return values.
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDistGrid

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetBoundingBoxes - Get the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_GridGetBoundingBoxes(grid, array, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(in) :: grid
      type(ESMF_LocalArray), intent(inout) :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the region identifier data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

      ! Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      array = grid%boundingBoxes

      ! Set return values.
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetBoundingBoxes

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridStatusEqual - equality of Grid statuses
!
! !INTERFACE:
      function ESMF_GridStatusEqual(GridStatus1, GridStatus2)

! !RETURN VALUE:
      logical :: ESMF_GridStatusEqual

! !ARGUMENTS:

      type (ESMF_GridStatus), intent(in) :: &
         GridStatus1,      &! Two grid statuses to compare for
         GridStatus2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStatus1, GridStatus2]
!          Two grid statuses to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStatusEqual = (GridStatus1%gridStatus == &
                              GridStatus2%gridStatus)

      end function ESMF_GridStatusEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridStructureEqual - equality of Grid structures
!
! !INTERFACE:
      function ESMF_GridStructureEqual(GridStructure1, GridStructure2)

! !RETURN VALUE:
      logical :: ESMF_GridStructureEqual

! !ARGUMENTS:

      type (ESMF_GridStructure), intent(in) :: &
         GridStructure1,      &! Two grid structures to compare for
         GridStructure2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid structures to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStructure1, GridStructure2]
!          Two region types to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStructureEqual = (GridStructure1%gridStructure == &
                                 GridStructure2%gridStructure)

      end function ESMF_GridStructureEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridTypeEqual - equality of Grid types
!
! !INTERFACE:
      function ESMF_GridTypeEqual(GridType1, GridType2)

! !RETURN VALUE:
      logical :: ESMF_GridTypeEqual

! !ARGUMENTS:

      type (ESMF_GridType), intent(in) :: &
         GridType1,      &! Two grid kinds to compare for
         GridType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid kinds to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridType1, GridType2]
!          Two region types to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridTypeEqual = (GridType1%gridType == &
                            GridType2%gridType)

      end function ESMF_GridTypeEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridStaggerEqual - equality of Grid staggerings
!
! !INTERFACE:
      function ESMF_GridStaggerEqual(GridStagger1, GridStagger2)

! !RETURN VALUE:
      logical :: ESMF_GridStaggerEqual

! !ARGUMENTS:

      type (ESMF_GridStagger), intent(in) :: &
         GridStagger1,      &! Two grid staggerings to compare for
         GridStagger2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid staggerings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStagger1, GridStagger2]
!          Two grid staggerings to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStaggerEqual = (GridStagger1%stagger == &
                               GridStagger2%stagger)

      end function ESMF_GridStaggerEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CoordOrderEqual - equality of Grid coordinate orders
!
! !INTERFACE:
      function ESMF_CoordOrderEqual(CoordOrder1, CoordOrder2)

! !RETURN VALUE:
      logical :: ESMF_CoordOrderEqual

! !ARGUMENTS:

      type (ESMF_CoordOrder), intent(in) :: &
         CoordOrder1,      &! Two grid coordinate orders to compare for
         CoordOrder2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid coordinate orderings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordOrder1, CoordOrder2]
!          Two coordinate orders to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordOrderEqual = (CoordOrder1%order == &
                              CoordOrder2%order)

      end function ESMF_CoordOrderEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CoordIndexEqual - equality of Grid coordinate indexing
!
! !INTERFACE:
      function ESMF_CoordIndexEqual(CoordIndex1, CoordIndex2)

! !RETURN VALUE:
      logical :: ESMF_CoordIndexEqual

! !ARGUMENTS:

      type (ESMF_CoordIndex), intent(in) :: &
         CoordIndex1,      &! Two grid coordinate indexings to compare
         CoordIndex2        ! for equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid coordinate indexings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordIndex1, CoordIndex2]
!          Two coordinate indexings to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordIndexEqual = (CoordIndex1%index == &
                              CoordIndex2%index)

      end function ESMF_CoordIndexEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridStatusNotEqual - non-equality of Grid statuses
!
! !INTERFACE:
      function ESMF_GridStatusNotEqual(GridStatus1, GridStatus2)

! !RETURN VALUE:
      logical :: ESMF_GridStatusNotEqual

! !ARGUMENTS:

      type (ESMF_GridStatus), intent(in) :: &
         GridStatus1,      &! Two Grid Statuses to compare for
         GridStatus2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStatus1, GridStatus2]
!          Two statuses of Grids to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStatusNotEqual = (GridStatus1%gridStatus /= &
                                 GridStatus2%gridStatus)

      end function ESMF_GridStatusNotEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridStructureNotEqual - non-equality of Grid structures
!
! !INTERFACE:
      function ESMF_GridStructureNotEqual(GridStructure1, GridStructure2)

! !RETURN VALUE:
      logical :: ESMF_GridStructureNotEqual

! !ARGUMENTS:

      type (ESMF_GridStructure), intent(in) :: &
         GridStructure1,      &! Two Grid Structures to compare for
         GridStructure2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid structures to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStructure1, GridStructure2]
!          Two structures of Grids to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStructureNotEqual = (GridStructure1%gridStructure /= &
                                    GridStructure2%gridStructure)

      end function ESMF_GridStructureNotEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridTypeNotEqual - non-equality of Grid kinds
!
! !INTERFACE:
      function ESMF_GridTypeNotEqual(GridType1, GridType2)

! !RETURN VALUE:
      logical :: ESMF_GridTypeNotEqual

! !ARGUMENTS:

      type (ESMF_GridType), intent(in) :: &
         GridType1,      &! Two Grid kinds to compare for
         GridType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid kinds to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridType1, GridType2]
!          Two kinds of Grids to compare for inequality
!     \end{description}
!
!EOP

      ESMF_GridTypeNotEqual = (GridType1%gridType /= &
                               GridType2%gridType)

      end function ESMF_GridTypeNotEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridStaggerNotEqual - inequality of Grid staggerings
!
! !INTERFACE:
      function ESMF_GridStaggerNotEqual(GridStagger1, GridStagger2)

! !RETURN VALUE:
      logical :: ESMF_GridStaggerNotEqual

! !ARGUMENTS:

      type (ESMF_GridStagger), intent(in) :: &
         GridStagger1,      &! Two grid staggerings to compare for
         GridStagger2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid staggerings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStagger1, GridStagger2]
!          Two grid staggerings to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStaggerNotEqual = (GridStagger1%stagger /= &
                                  GridStagger2%stagger)

      end function ESMF_GridStaggerNotEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CoordOrderNotEqual - inequality of Grid coordinate orders
!
! !INTERFACE:
      function ESMF_CoordOrderNotEqual(CoordOrder1, CoordOrder2)

! !RETURN VALUE:
      logical :: ESMF_CoordOrderNotEqual

! !ARGUMENTS:

      type (ESMF_CoordOrder), intent(in) :: &
         CoordOrder1,      &! Two grid coordinate orders to compare for
         CoordOrder2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid coordinate orderings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordOrder1, CoordOrder2]
!          Two coordinate orders to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordOrderNotEqual = (CoordOrder1%order /= &
                                 CoordOrder2%order)

      end function ESMF_CoordOrderNotEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CoordIndexNotEqual - inequality of Grid coordinate indexing
!
! !INTERFACE:
      function ESMF_CoordIndexNotEqual(CoordIndex1, CoordIndex2)

! !RETURN VALUE:
      logical :: ESMF_CoordIndexNotEqual

! !ARGUMENTS:

      type (ESMF_CoordIndex), intent(in) :: &
         CoordIndex1,      &! Two grid coordinate indexings to compare
         CoordIndex2        ! for inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid coordinate indexings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordIndex1, CoordIndex2]
!          Two coordinate indexings to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordIndexNotEqual = (CoordIndex1%index /= &
                                 CoordIndex2%index)

      end function ESMF_CoordIndexNotEqual

!------------------------------------------------------------------------------

      end module ESMF_GridTypesMod
