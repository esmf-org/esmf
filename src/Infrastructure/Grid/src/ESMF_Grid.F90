! $Id: ESMF_Grid.F90,v 1.104 2003/10/13 23:07:10 jwolfe Exp $
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
#include "ESMF_Grid.h"
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_GridMod - Grid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_Grid} class.  This class
! provides a unified interface for both {\tt ESMF\_PhysGrid} and {\tt ESMF\_DistGrid}
! information for model grids.  Functions for defining and computing {\tt ESMF\_Grid}
! information are available through this class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_ArrayBaseMod
      use ESMF_ArrayExpandMod
      use ESMF_IOMod          ! ESMF I/O class
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
!     !  ESMF_GridType
!
!     ! Definition for the Grid class.  A Grid
!     ! is passed back to the user at Grid creation.

      type ESMF_GridType
      sequence
      private

        type (ESMF_Base) :: base            ! base class object
        type (ESMF_Status) :: gridstatus    ! uninitialized, init ok, etc
        integer :: horz_gridtype            ! enum for type of horizontal grid
        integer :: vert_gridtype            ! enum for type of vertical grid
        integer :: horz_stagger             ! enum for horizontal grid staggering
        integer :: vert_stagger             ! enum for vertical grid staggering
        type (ESMF_CoordSystem) :: horz_coord_system
                                            ! enum for horizontal physical
                                            ! coordinate system
        type (ESMF_CoordSystem) :: vert_coord_system
                                            ! enum for vertical physical
                                            ! coordinate system
        integer :: coord_order              ! enum for mapping of xyz 
                                            ! to ijk
        integer :: coord_index              ! enum for global or local indexing
        type (ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
                                            ! logical identifier to indicate
                                            ! periodic boundary conditions in
                                            ! each direction
        integer :: num_physgrids            ! number of grid descriptors
                                            ! necessary to support
                                            ! staggering, vertical
                                            ! grids, background grids
        integer :: num_physgrids_alloc      ! number of physgrids allocated
        type (ESMF_PhysGrid), dimension(:), pointer :: physgrids
                                            ! info for all grid descriptions
                                            ! necessary to define horizontal, 
                                            ! staggered and vertical grids
        type (ESMF_DistGrid) :: distgrid    ! decomposition and other
                                            ! logical space info for grid
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: globalMinCoord
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: globalMaxCoord
        type (ESMF_LocalArray) :: boundingBoxes
                                            ! array of bounding boxes on each DE
                                            ! used for search routines
!       type (???) :: search_structure

      end type

!------------------------------------------------------------------------------
!     !  ESMF_Grid
!
!     ! The Grid data structure that is passed between languages.

      type ESMF_Grid
      sequence
      private
#ifndef ESMF_NO_INITIALIZERS
        type (ESMF_GridType), pointer :: ptr => NULL()
#else
        type (ESMF_GridType), pointer :: ptr
#endif
      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

      public ESMF_Grid
      public ESMF_GridType

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    public ESMF_GridCreate
    public ESMF_GridDestroy
    public ESMF_GridAddPhysGrid
    public ESMF_GridGetConfig
    public ESMF_GridSetConfig
    public ESMF_GridGetCoord
    public ESMF_GridSetCoord
    public ESMF_GridGetDE            ! access DistGrid from above
    public ESMF_GridGetAllAxisIndex  ! access DistGrid from above
    public ESMF_GridGetDELayout      ! access DistGrid from above
    ! TODO:  combine all get subroutines into one
    public ESMF_GridGlobalToLocalIndex
    public ESMF_GridLocalToGlobalIndex
    public ESMF_GridGet
    public ESMF_GridSet
    !public ESMF_GridGetLMask
    public ESMF_GridSetLMask
    !public ESMF_GridGetMMask
    public ESMF_GridSetMMask
    !public ESMF_GridGetMetric
    public ESMF_GridSetMetric
    !public ESMF_GridGetRegionID
    public ESMF_GridSetRegionID
    public ESMF_GridGetBoundingBoxes
    public ESMF_GridSetBoundingBoxes
    public ESMF_GridGetPhysGrid
    public ESMF_GridGetPhysGridID
    public ESMF_GridValidate
    public ESMF_GridPrint
    public ESMF_GridComputeDistance
    public ESMF_GridBoxIntersectRecv
    public ESMF_GridBoxIntersectSend
    !public ESMF_GridSearch

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

   integer, parameter, public ::            &! recognized grid types
      ESMF_GridType_Unknown           =  0, &! unknown or undefined grid
      ESMF_GridType_LatLon            =  1, &! equally-spaced lat/lon grid
      ESMF_GridType_Mercator          =  2, &! Mercator lat/lon grid
      ESMF_GridType_Dipole            =  3, &! Displaced-pole dipole grid
      ESMF_GridType_Tripole           =  4, &! Tripolar grids
      ESMF_GridType_XY                =  5, &! Cartesian equally-space x-y grid
      ESMF_GridType_DataStream        =  6, &! Data stream
      ESMF_GridType_PhysFourier       =  7, &! Mixed Fourier Space/Phys Space grid
      ESMF_GridType_LatLonGauss       =  8, &! lat/lon grid with Gaussian latitudes
      ESMF_GridType_Reduced           =  9, &! reduced grid
      ESMF_GridType_SphericalSpectral = 10, &! spectral space for spherical harmonics
      ESMF_GridType_Geodesic          = 11, &! spherical geodesic grid
      ESMF_GridType_CubedSphere       = 12   ! cubed sphere grid

   integer, parameter, public ::            &! recognized staggering types
      ESMF_GridStagger_Unknown        =  0, &! unknown or undefined staggering
      ESMF_GridStagger_A              =  1, &! Arakawa A (centered velocity)
      ESMF_GridStagger_B              =  2, &! Arakawa B (velocities at grid corner)
      ESMF_GridStagger_C              =  3, &! Arakawa C (velocities at cell faces)
      ESMF_GridStagger_D              =  4, &! Arakawa D
      ESMF_GridStagger_E              =  5, &! Arakawa E
      ESMF_GridStagger_Z              =  6, &! C grid equiv for geodesic grid
      ESMF_GridStagger_VertCenter     =  7, &! vert velocity at vertical midpoints
      ESMF_GridStagger_VertFace       =  8   ! vert velocity/Pgrad at top(bottom)face

!   integer, parameter, public ::            &! recognized coordinate systems
!      ESMF_CoordSystem_Unknown        =  0, &! unknown or undefined coord system
!      ESMF_CoordSystem_User           =  1, &! user defined coord system
!      ESMF_CoordSystem_Spherical      =  2, &! spherical coordinates (lat/lon)
!      ESMF_CoordSystem_Cartesian      =  3, &! Cartesian coordinates (x,y)
!      ESMF_CoordSystem_Cylindrical    =  4, &! cylindrical coordinates
!      ESMF_CoordSystem_LatFourier     =  5, &! mixed latitude/spectral space
!      ESMF_CoordSystem_Spectral       =  6, &! wavenumber space
!      ESMF_CoordSystem_Depth          =  7, &! vertical z coord. depth (0 at surface)
!      ESMF_CoordSystem_Height         =  8, &! vertical z coord. height (0 at bottom)
!      ESMF_CoordSystem_Pressure       =  9, &! vertical pressure coordinate
!      ESMF_CoordSystem_Sigma          = 10, &! vertical sigma coordinate
!      ESMF_CoordSystem_Theta          = 11, &! vertical theta coordinate
!      ESMF_CoordSystem_Eta            = 12, &! vertical eta coordinate
!      ESMF_CoordSystem_Isopycnal      = 13, &! vertical density coordinate
!      ESMF_CoordSystem_Hybrid         = 14, &! hybrid vertical coordinates
!      ESMF_CoordSystem_Lagrangian     = 15   ! Lagrangian coordinates
      ! I'm sure there are more - I'm not sure
      ! what the atmospheric ESMF models are using for vertical coords

   integer, parameter, public ::            &! recognized coordinate orderings
      ESMF_CoordOrder_Unknown         =  0, &! unknown or undefined coord ordering
      ESMF_CoordOrder_XYZ             =  1, &! IJK maps to XYZ
      ESMF_CoordOrder_XZY             =  2, &! IJK maps to XZY
      ESMF_CoordOrder_YXZ             =  3, &! IJK maps to YXZ
      ESMF_CoordOrder_YZX             =  4, &! IJK maps to YZX
      ESMF_CoordOrder_ZXY             =  5, &! IJK maps to ZXY
      ESMF_CoordOrder_ZYX             =  6   ! IJK maps to ZYX

   integer, parameter, public ::            &! recognized coordinate indexings
      ESMF_CoordIndex_Unknown         =  0, &! unknown or undefined coord indexing
      ESMF_CoordIndex_Local           =  1, &! uses local indexing
      ESMF_CoordIndex_Global          =  2   ! uses global indexing

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.104 2003/10/13 23:07:10 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_GridCreate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridCreateEmpty
         module procedure ESMF_GridCreateSpecd
         module procedure ESMF_GridCreateUniform
         module procedure ESMF_GridCreateRead
         module procedure ESMF_GridCreateCopy
         module procedure ESMF_GridCreateCutout
         module procedure ESMF_GridCreateChangeResolution
         module procedure ESMF_GridCreateExchange

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Grid} create
!     methods.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_GridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridConstructNew
         module procedure ESMF_GridConstructSpecd
         module procedure ESMF_GridConstructUniform

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct a
!     complete {\tt ESMF\_Grid}.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_GridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetCoordFromArray
         module procedure ESMF_GridSetCoordFromBuffer
         module procedure ESMF_GridSetCoordSpecd
         module procedure ESMF_GridSetCoordUniform
         module procedure ESMF_GridSetCoordCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     coordinates as part of a {\tt ESMF\_Grid}.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_GridSetLMask

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetLMaskFromArray
         module procedure ESMF_GridSetLMaskFromBuffer
         module procedure ESMF_GridSetLMaskFromMMask
         module procedure ESMF_GridSetLMaskCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     logical masks as part of a {\tt ESMF\_Grid}.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_GridSetMMask

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetMMaskFromArray
         module procedure ESMF_GridSetMMaskFromBuffer
         module procedure ESMF_GridSetMMaskFromLMask
         module procedure ESMF_GridSetMMaskCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     multiplicative masks as part of a {\tt ESMF\_Grid}.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
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
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_GridSetRegionID

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetRegionIDFromArray
         module procedure ESMF_GridSetRegionIDFromBuffer
         module procedure ESMF_GridSetRegionIDCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     region id's as part of a {\tt ESMF\_Grid}.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!!BOP
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
!!EOP
!      end interface
!!
!------------------------------------------------------------------------------
!!BOP
!! !INTERFACE:
       interface ESMF_GridAddPhysGrid
!
!! !PRIVATE MEMBER FUNCTIONS:
          module procedure ESMF_GridAddPhysGridSpecd
          module procedure ESMF_GridAddPhysGridUniform
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     search a {\tt ESMF\_Grid} for point(s).
!!
!!EOP
       end interface
!!
!------------------------------------------------------------------------------
!!BOP
!! !INTERFACE:
       interface ESMF_GridSetBoundingBoxes
!
!! !PRIVATE MEMBER FUNCTIONS:
          module procedure ESMF_GridSetBoundingBoxesUni
          module procedure ESMF_GridSetBoundingBoxesSpecd
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     set bounding boxes for all the DEs in a {\tt ESMF\_Grid}.
!!
!!EOP
       end interface
!!
!------------------------------------------------------------------------------

!    < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Grid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateEmpty - Create a new Grid with no contents

! !INTERFACE:
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
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateEmpty%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateEmpty: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateEmpty: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateEmpty%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateEmpty

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateUniform - Create a new uniform Grid

! !INTERFACE:
      function ESMF_GridCreateUniform(numDims, counts, min, max, layout, &
                                      horz_gridtype, vert_gridtype, &
                                      horz_stagger, vert_stagger, &
                                      horz_coord_system, vert_coord_system, &
                                      periodic, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateUniform
!
! !ARGUMENTS:
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: max
      type (ESMF_DELayout), intent(in) :: layout
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      type(ESMF_CoordSystem), intent(in), optional :: horz_coord_system
      type(ESMF_CoordSystem), intent(in), optional :: vert_coord_system
      type (ESMF_Logical), intent(in), optional :: periodic(:)
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and internally generates the {\tt ESMF\_Grid}.  Return a pointer to
!     the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[numDims]}]
!          Number of grid dimensions.
!     \item[{[counts]}]
!          Array of number of grid increments in each dimension.
!     \item[{[min]}]
!          Array of minimum physical coordinates in each dimension.
!     \item[{[max]}]
!          Array of maximum physical coordinates in each direction.
!     \item[{[layout]}]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[horz\_gridtype]}]
!          Integer specifier to denote horizontal gridtype.
!     \item[{[vert\_gridtype]}]
!          Integer specifier to denote vertical gridtype.
!     \item[{[horz\_stagger]}]
!          Integer specifier to denote horizontal grid stagger.
!     \item[{[vert\_stagger]}]
!          Integer specifier to denote vertical grid stagger.
!     \item[{[horz\_coord\_system]}]
!          Integer specifier to denote horizontal coordinate system.
!     \item[{[vert\_coord\_system]}]
!          Integer specifier to denote vertical coordinate system.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate axes.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateUniform%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateUniform: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstruct(grid, numDims, counts, min, max, layout, &
                              horz_gridtype, vert_gridtype, &
                              horz_stagger, vert_stagger, &
                              horz_coord_system, vert_coord_system, &
                              periodic, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateUniform: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateUniform%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateUniform

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateSpecd - Create a new specified Grid internally

! !INTERFACE:
      function ESMF_GridCreateSpecd(numDims, min, delta1, delta2, layout, &
                                    countsPerDE1, countsPerDE2, dim_names, &
                                    dim_units, horz_gridtype, vert_gridtype, &
                                    horz_stagger, vert_stagger, &
                                    horz_coord_system, vert_coord_system, &
                                    periodic, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateSpecd
!
! !ARGUMENTS:
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta2
      type (ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in), optional :: countsPerDE1
      integer, dimension(:), intent(in), optional :: countsPerDE2
      character (len=*), dimension(numDims), intent(in), optional :: dim_names
      character (len=*), dimension(numDims), intent(in), optional :: dim_units
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      type(ESMF_CoordSystem), intent(in), optional :: horz_coord_system
      type(ESMF_CoordSystem), intent(in), optional :: vert_coord_system
      type(ESMF_Logical), dimension(numDims), intent(in), optional :: periodic
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and internally generates the {\tt ESMF\_Grid}.  Return a pointer to
!     the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[min]}]
!          Array of minimum physical coordinate in each direction.
!     \item[{[delta1]}]
!          Array of physical increments between nodes in the first direction.
!     \item[{[delta2]}]
!          Array of physical increments between nodes in the second direction.
!     \item[{[layout]}]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[countsPerDE1]}]
!          Array of number of grid increments per DE in the first direction.
!     \item[{[countsPerDE2]}]
!          Array of number of grid increments per DE in the second direction.
!     \item[{[dim\_names]}]
!          Array of dimension names.
!     \item[{[dim\_units]}]
!          Array of dimension units.
!     \item[{[horz\_gridtype]}]
!          Integer specifier to denote horizontal gridtype.
!     \item[{[vert\_gridtype]}]
!          Integer specifier to denote vertical gridtype.
!     \item[{[horz\_stagger]}]
!          Integer specifier to denote horizontal grid stagger.
!     \item[{[vert\_stagger]}]
!          Integer specifier to denote vertical grid stagger.
!     \item[{[horz\_coord\_system]}]
!          Integer specifier to denote horizontal coordinate system.
!     \item[{[vert\_coord\_system]}]
!          Integer specifier to denote vertical coordinate system.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid    ! Pointer to new grid
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateSpecd%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateSpecd: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstruct(grid, numDims, min, delta1, delta2, layout, &
                              countsPerDE1, countsPerDE2, &
                              dim_names, dim_units, &
                              horz_gridtype, vert_gridtype, &
                              horz_stagger, vert_stagger, &
                              horz_coord_system, vert_coord_system, &
                              periodic, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateSpecd: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateSpecd%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateSpecd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateRead - Create a new Grid read in from a file

! !INTERFACE:
      function ESMF_GridCreateRead(iospec, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateRead
!
! !ARGUMENTS:
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
!     \item[{[iospec]}]
!          File I/O specification.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateRead%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateRead: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateRead: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateRead%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateCopy - Create a new Grid by copying another Grid

! !INTERFACE:
      function ESMF_GridCreateCopy(grid_in, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateCopy
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid_in
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
!     \item[{[grid\_in]}]
!          {\tt ESMF\_Grid} to be copied.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateCopy%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateCopy: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateCopy: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateCopy%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateCutout - Create a new Grid as a subset of an existing Grid

! !INTERFACE:
      function ESMF_GridCreateCutout(grid_in, i_min, i_max, j_min, j_max, &
                                     name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateCutout
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid_in
      integer, intent(in) :: i_min
      integer, intent(in) :: i_max
      integer, intent(in) :: j_min
      integer, intent(in) :: j_max
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
!     \item[{[grid\_in]}]
!          {\tt ESMF\_Grid} to be partially copied.
!     \item[{[i\_min]}]
!          Minimum global i-index for the region of the grid to be cutout.
!     \item[{[i\_max]}]
!          Maximum global i-index for the region of the grid to be cutout.
!     \item[{[j\_min]}]
!          Minimum global j-index for the region of the grid to be cutout.
!     \item[{[j\_max]}]
!          Maximum global j-index for the region of the grid to be cutout.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateCutout%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateCutout: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateCutout: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateCutout%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateCutout

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateChangeResolution - Create a new Grid by coarsening or refining an existing Grid

! !INTERFACE:
      function ESMF_GridCreateChangeResolution(grid_in, i_resolution, &
                                               j_resolution, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateChangeResolution
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid_in
      integer, intent(in) :: i_resolution
      integer, intent(in) :: j_resolution
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
!     \item[{[grid\_in]}]
!          Source {\tt ESMF\_Grid} to be coarsened or refined.
!     \item[{[i\_resolution]}]
!          Integer resolution factor in the i-direction.
!     \item[{[j\_resolution]}]
!          Integer resolution factor in the j-direction.
!          Note:  The above arguments assume refinement by factor if positive
!          and coarsening by absolute value of the factor if negative.  For
!          example, i\_resolution=4 indicates the new {\tt ESMF\_Grid} will be four
!          times as resolved in the i-direction as the source {\tt ESMF\_Grid},
!          whereas j\_resolution=-3 means the new {\tt ESMF\_Grid} will sample every
!          third point in the j-direction.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateChangeResolution%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateChangeResolution: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateChangeResolution: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateChangeResolution%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateChangeResolution

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateExchange - Create a new Grid from the intersection of two existing grids

! !INTERFACE:
      function ESMF_GridCreateExchange(grid_in1, grid_in2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateExchange
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid_in1
      type(ESMF_Grid), intent(in) :: grid_in2
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
!     \item[{[grid\_in1]}]
!          First source {\tt ESMF\_Grid}.
!     \item[{[grid\_in2]}]
!          Second source {\tt ESMF\_Grid}.
!     \item[{[name]}]
!          New {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateExchange%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateExchange: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateExchange: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateExchange%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateExchange

!------------------------------------------------------------------------------
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
!EOP
! !REQUIREMENTS:
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! If already destroyed or never created, return ok
      if (.not. associated(grid%ptr)) then
        print *, "GridDestroy called on uninitialized or destroyed Grid"
        if(rcpresent) rc = ESMF_FAILURE   ! should this really be an error?
        return
      endif

      ! Destruct all grid internals and then free field memory.
      call ESMF_GridDestruct(grid%ptr, status)
      ! If error write message and return.
      if(status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridDestroy from ESMF_GridDestruct"
        return
      endif

      deallocate(grid%ptr, stat=status)
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridDestroy: Grid deallocate"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDestroy

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridConstructUniform - Construct a uniform Grid

! !INTERFACE:
      subroutine ESMF_GridConstructUniform(grid, numDims, counts, min, max, &
                                           layout, horz_gridtype, vert_gridtype, &
                                           horz_stagger, vert_stagger, &
                                           horz_coord_system, vert_coord_system, &
                                           periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: max
      type (ESMF_DELayout), intent(in) :: layout
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      type(ESMF_CoordSystem), intent(in), optional :: horz_coord_system
      type(ESMF_CoordSystem), intent(in), optional :: vert_coord_system
      type (ESMF_Logical), intent(in), optional :: periodic(:)
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
!     \item[{[numDims]}]
!          Number of grid dimensions.
!     \item[{[counts]}]
!          Array of number of grid increments in each dimension.
!     \item[{[min]}]
!          Array of minimum physical coordinates in each dimension.
!     \item[{[max]}]
!          Array of maximum physical coordinates in each direction.
!     \item[{[layout]}]
!         {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[horz\_gridtype]}]
!          Integer specifier to denote horizontal gridtype.
!     \item[{[vert\_gridtype]}]
!          Integer specifier to denote vertical gridtype.
!     \item[{[horz\_stagger]}]
!          Integer specifier to denote horizontal grid stagger.
!     \item[{[vert\_stagger]}]
!          Integer specifier to denote vertical grid stagger.
!     \item[{[horz\_coord\_system]}]
!          Integer specifier to denote horizontal coordinate system.
!     \item[{[vert\_coord\_system]}]
!          Integer specifier to denote vertical coordinate system.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate axes.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      character(len=ESMF_MAXSTR) :: physgridName
      integer :: physgridId                   ! integer identifier for physgrid
      integer :: i, numDE1, numDE2
      integer, dimension(:,:), pointer :: countsPerAxis
      real(ESMF_KIND_R8), dimension(numDims) :: delta
      type(ESMF_RelLoc) :: relloc

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructUniform: Grid construct"
        return
      endif

!     Fill in grid derived type with subroutine arguments
      call ESMF_GridSet(grid, horz_gridtype, vert_gridtype, &
                        horz_stagger, vert_stagger, &
                        horz_coord_system, vert_coord_system, &
                        global_min_coord=min, global_max_coord=max, &
                        periodic=periodic, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructUniform: Grid set"
        return
      endif

!     Create the DistGrid
      grid%distgrid = ESMF_DistGridCreate(counts=counts, layout=layout, &
                                          periodic=periodic, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructUniform: Distgrid create"
        return
      endif

!     Create the BoundingBoxes structure
      do i = 1,numDims
        if (counts(i).ne.0) then
          delta(i) = (max(i)-min(i)) / real(counts(i))
        endif
      enddo
      call ESMF_DELayoutGetSize(layout, numDE1, numDE2, status)
      allocate(countsPerAxis(numDE1*numDE2, ESMF_MAXGRIDDIM), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, countsperaxis(DE1*DE2,maxgrid) =", &
                              numDE1, numDE2, ESMF_MAXGRIDDIM
         return
      endif
      call ESMF_DistGridGetAllCounts(grid%distgrid%ptr, countsPerAxis, rc=status)
      call ESMF_GridSetBoundingBoxes(grid, numDims, min, delta, countsPerAxis, &
                                     numDE1, numDE2, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructUniform: Grid set boxes"
        return
      endif

!     Create physgrid at cell center
      physgridId = 1
      physgridName = 'cell_center'
      relloc = ESMF_CELL_CENTER
      call ESMF_GridAddPhysGrid(grid, numDims, counts, physgridId, relloc, &
                                min, max, physgridName, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructUniform: Add physgrid"
        return
      endif
      physgridId = physgridId + 1 

!     Create any other physgrids necessary for horizontal grid stagger
!     TODO: finish filling out, look up D
      select case (horz_stagger)

        ! Arakawa A (centered velocity)
        case (1)

        ! Arakawa B (velocities at grid corner)
        case (2)
          physgridName = 'cell_necorner'
          relloc = ESMF_CELL_NECORNER
          call ESMF_GridAddPhysGrid(grid, numDims, counts, physgridId, relloc, &
                                    min, max, physgridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructUniform: Add physgrid"
            return
          endif
          physgridId = physgridId + 1 

        ! Arakawa C (velocities at cell faces)
        case (3)
          physgridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_GridAddPhysGrid(grid, numDims, counts, physgridId, relloc, &
                                    min, max, physgridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructUniform: Add physgrid"
            return
          endif
          physgridId = physgridId + 1 

        ! Arakawa D
        case (4)
          physgridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_GridAddPhysGrid(grid, numDims, counts, physgridId, relloc, &
                                    min, max, physgridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructUniform: Add physgrid"
            return
          endif
          physgridId = physgridId + 1 

      end select

!     Create vertical physgrid if requested  TODO

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridConstructUniform

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridConstructSpecd - Construct a specified Grid

! !INTERFACE:
      subroutine ESMF_GridConstructSpecd(grid, numDims, min, delta1, delta2, &
                                         layout,  countsPerDE1, countsPerDE2, &
                                         dim_names, dim_units, &
                                         horz_gridtype, vert_gridtype, &
                                         horz_stagger, vert_stagger, &
                                         horz_coord_system, &
                                         vert_coord_system, &
                                         periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta2
      type (ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in), optional :: countsPerDE1
      integer, dimension(:), intent(in), optional :: countsPerDE2
      character (len=*), dimension(numDims), intent(in), optional :: dim_names
      character (len=*), dimension(numDims), intent(in), optional :: dim_units
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      type(ESMF_CoordSystem), intent(in), optional :: horz_coord_system
      type(ESMF_CoordSystem), intent(in), optional :: vert_coord_system
      type(ESMF_Logical), dimension(numDims), intent(in), optional :: periodic
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
!     \item[min]
!          Array of minimum physical coordinate in each direction.
!     \item[delta1]
!          Array of physical increments between nodes in the first direction.
!     \item[delta2]
!          Array of physical increments between nodes in the second direction.
!     \item[layout]
!         {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[countsPerDE1]}]
!          Array of number of grid increments per DE in the x-direction.
!     \item[{[countsPerDE2]}]
!          Array of number of grid increments per DE in the y-direction.
!     \item[{[dim\_names]}]
!          Array of dimension names.
!     \item[{[dim\_units]}]
!          Array of dimension units.
!     \item[{[horz\_gridtype]}]
!          Integer specifier to denote horizontal gridtype.
!     \item[{[vert\_gridtype]}]
!          Integer specifier to denote vertical gridtype.
!     \item[{[horz\_stagger]}]
!          Integer specifier to denote horizontal grid stagger.
!     \item[{[vert\_stagger]}]
!          Integer specifier to denote vertical grid stagger.
!     \item[{[horz\_coord\_system]}]
!          Integer specifier to denote horizontal coordinate system.
!     \item[{[vert\_coord\_system]}]
!          Integer specifier to denote vertical coordinate system.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      character(len=ESMF_MAXSTR) :: physgrid_name       !
      integer :: physgridId                   ! integer identifier for physgrid
      integer :: i, counts(numDims)
      real(ESMF_KIND_R8), dimension(numDims) :: max
      type(ESMF_RelLoc) :: relloc

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructSpecd: Grid construct"
        return
      endif

      ! First calculate global maxs
      do i = 1,numDims
        max(i) = min(i)
      enddo
      do i = 1,size(delta1)
        max(1) = max(1) + delta1(i)
      enddo
      do i = 1,size(delta2)
        max(2) = max(2) + delta2(i)
      enddo

!     Fill in grid derived type with subroutine arguments
      call ESMF_GridSet(grid, horz_gridtype, vert_gridtype, &
                        horz_stagger, vert_stagger, &
                        horz_coord_system, vert_coord_system, &
                        global_min_coord=min, global_max_coord=max, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructSpecd: Grid set"
        return
      endif

!     Create the BoundingBoxes structure
      call ESMF_GridSetBoundingBoxes(grid, numDims, min, delta1, delta2, &
                                     countsPerDE1, countsPerDE2, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructSpecd: Grid set boxes"
        return
      endif

!     Create the DistGrid
      if(present(countsPerDE1) .and. present(countsPerDE2)) then
        grid%distgrid = ESMF_DistGridCreate(countsPerDE1=countsPerDE1, &
                                            countsPerDE2=countsPerDE2, &
                                            periodic=periodic, layout=layout, &
                                            rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridConstructSpecd: Distgrid create"
          return
        endif
      else
        counts(1) = size(delta1)
        counts(2) = size(delta2)
        grid%distgrid = ESMF_DistGridCreate(counts=counts, layout=layout, &
                                            periodic=periodic, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridConstructUniform: Distgrid create"
          return
        endif
      endif

!     Create physgrid at cell center
      physgridId = 1
      physgrid_name = 'cell_center'
      relloc = ESMF_CELL_CENTER
      call ESMF_GridAddPhysGrid(grid, physgridId, relloc, numDims, delta1, &
                                delta2, countsPerDE1, countsPerDE2, min, &
                                dim_names, dim_units, physgrid_name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructSpecd: Add physgrid"
        return
      endif
      physgridId = physgridId + 1 

!     Create any other physgrids necessary for horizontal grid stagger
!     TODO: finish filling out, look up D
      select case (horz_stagger)

        ! Arakawa A (centered velocity)
        case (1)

        ! Arakawa B (velocities at grid corner)
        case (2)
          physgrid_name = 'cell_necorner'
          relloc = ESMF_CELL_NECORNER
          call ESMF_GridAddPhysGrid(grid, physgridId, relloc, numDims, delta1, &
                                    delta2, countsPerDE1, countsPerDE2, min, &
                                    dim_names, dim_units, physgrid_name, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructSpecd: Add physgrid"
            return
          endif
          physgridId = physgridId + 1 

        ! Arakawa C (velocities at cell faces)
        case (3)
          physgrid_name = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_GridAddPhysGrid(grid, physgridId, relloc, numDims, delta1, &
                                    delta2, countsPerDE1, countsPerDE2, min, &
                                    dim_names, dim_units, physgrid_name, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructSpecd: Add physgrid"
            return
          endif
          physgridId = physgridId + 1 

        ! Arakawa D
        case (4)
          physgrid_name = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_GridAddPhysGrid(grid, physgridId, relloc, numDims, delta1, &
                                    delta2, countsPerDE1, countsPerDE2, min, &
                                    dim_names, dim_units, physgrid_name, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructSpecd: Add physgrid"
            return
          endif
          physgridId = physgridId + 1 

      end select

!     Create vertical physgrid if requested  TODO

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructSpecd: Grid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridConstructSpecd

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridConstructNew - Construct the internals of an allocated Grid

! !INTERFACE:
      subroutine ESMF_GridConstructNew(grid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
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

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Set the Grid name if present, otherwise construct a default one
      call ESMF_SetName(grid%base, name, "Grid", status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructNew: Setname"
        return
      endif

!     Initialize grid contents
      grid%gridstatus = ESMF_STATE_READY
      grid%horz_gridtype = ESMF_GridType_Unknown
      grid%vert_gridtype = ESMF_GridType_Unknown
      grid%horz_stagger = ESMF_GridStagger_Unknown
      grid%vert_stagger = ESMF_GridStagger_Unknown
      grid%horz_coord_system = ESMF_CoordSystem_Unknown
      grid%vert_coord_system = ESMF_CoordSystem_Unknown
      grid%coord_order = ESMF_CoordOrder_Unknown
      do i=1,ESMF_MAXGRIDDIM
        grid%periodic(i) = ESMF_FALSE
      enddo
      grid%num_physgrids = 0
      grid%num_physgrids_alloc = 0
      nullify(grid%physgrids)

      grid%distgrid = ESMF_DistGridCreate(name, rc)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridConstructNew

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridDestruct - Free any Grid memory allocated internally

! !INTERFACE:
      subroutine ESMF_GridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), pointer :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_GridConstruct}, does any additional cleanup before the
!     original {\tt ESMF\_Gri} object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_GridDestroy}, which calls
!     {\tt ESMF\_GridDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          The class to be destructed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_DistGridDestroy(grid%distgrid, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridDestruct: distgrid destroy"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridAddPhysGridUniform - Add a uniform PhysGrid to a Grid

! !INTERFACE:
      subroutine ESMF_GridAddPhysGridUniform(grid, numDims, globalCounts, &
                                             physgridId, relloc, min, max, &
                                             physgridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: globalCounts
      integer, intent(out) :: physgridId
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: min
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: max
      character (len=*), intent(in), optional :: physgridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysGrid} to a {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[numDims]}]
!          Number of grid dimensions.
!     \item[{[globalCounts]}]
!          Array of global number of grid increments in each direction.
!     \item [{[physgridId]}]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[{[min]}]
!          Array of minimum physical coordinates in each direction.
!     \item[{[max]}]
!          Array of maximum physical coordinates in each direction.
!     \item [{[physgridName]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, gridBoundWidth
      character(len=ESMF_MAXSTR), dimension(numDims) :: coordNames, coordUnits
      logical, dimension(numDims) :: coordAligned, coordEqualSpaced, coordCyclic
      integer, dimension(numDims) :: countsPlus, localCounts
      real(ESMF_KIND_R8) :: delta(numDims)
      real(ESMF_KIND_R8) :: localMinCoord(numDims)
      real(ESMF_KIND_R8) :: localMaxCoord(numDims)
      type(ESMF_PhysCoord) :: tempCoord
      type(ESMF_CoordSystem) :: coordSystem

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Increment the number of physgrids in the grid and allocate memory
      ! This subroutine should be called before incrementing num_physgrids
      ! because it uses the old value internally.
      call ESMF_GridMakePhysGridSpace(grid, grid%num_physgrids+1, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridAddPhysGridUniform: temp_pgrids allocate"
        return
      endif
      grid%num_physgrids = grid%num_physgrids + 1

      ! Now initialize some values
      gridBoundWidth = 1   ! TODO: move into structure, make input?
      physgridId = grid%num_physgrids 

      do i = 1,numDims
        if (globalCounts(i).ne.0) then
          delta(i) = (max(i) - min(i)) / real(globalCounts(i))
        else
          print *, "ERROR in ESMF_GridAddPhysGridUniform: counts=0"
          return
        endif

        localMinCoord(i) = delta(i) &
                         * real(grid%distgrid%ptr%MyDE_comp%ai_global(i)%min - 1)
        localMaxCoord(i) = delta(i) &
                         * real(grid%distgrid%ptr%MyDE_comp%ai_global(i)%max)
        localCounts(i)   = grid%distgrid%ptr%MyDE_comp%ai_global(i)%max &
                         - grid%distgrid%ptr%MyDE_comp%ai_global(i)%min + 1
      enddo

      ! set parameters based on grid type
      select case (grid%horz_gridtype)

        ! ESMF_GridType_LatLon
        case (1)
          coordSystem         = ESMF_CoordSystem_Spherical
          coordNames(1)       = 'longitude'
          coordNames(2)       = 'latitude'
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .true.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_GridType_XY
        case (5)
          coordSystem         = ESMF_CoordSystem_Cartesian
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordUnits(:)       = ''
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .true.
          coordCyclic(:)      = .false.

        case default
           print *,'Grid type not yet supported in GridAddPhysGrid'
           status = ESMF_FAILURE

      end select

      ! Create the actual physgrid object
      grid%physgrids(physgridId) = ESMF_PhysGridCreate(numDims, relloc, &
                                     physgridName, coordSystem, status)

      do i = 1,numDims
        tempCoord = ESMF_PhysCoordCreate(coordNames(i), coordUnits(i), &
                                         coordAligned(i), coordEqualSpaced(i), &
                                         coordCyclic(i), localMinCoord(i), &
                                         localMaxCoord(i), rc=status)
        call ESMF_PhysGridSetCoord(grid%physgrids(physgridId), tempCoord, &
                                   dim_order=i, rc=status)
      enddo

      countsPlus(1) = localCounts(1) + 2*gridBoundWidth
      countsPlus(2) = localCounts(2) + 2*gridBoundWidth
      call ESMF_GridSetCoord(grid, physgridId, numDims, countsPlus, &
                             gridBoundWidth, relloc, delta, localMinCoord, &
                             status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridAddPhysGridUniform: Grid set coord"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddPhysGridUniform

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridAddPhysGridSpecd - Add a specified PhysGrid to a Grid

! !INTERFACE:
      subroutine ESMF_GridAddPhysGridSpecd(grid, physgridId, relloc, numDims, &
                                           delta1, delta2, &
                                           countsPerDE1, countsPerDE2, &
                                           min, dimNames, dimUnits, &
                                           physgridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), target :: grid
      integer, intent(out) :: physgridId
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta2
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: min
      character (len=*), dimension(:), intent(in), optional :: dimNames
      character (len=*), dimension(:), intent(in), optional :: dimUnits
      character (len=*), intent(in), optional :: physgridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysGrid} to a {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt Grid} to add {\tt PhysGrid} to.
!     \item[physgridId]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[relloc]
!          Relative location of data at the centers, faces, and vertices of
!          the {\tt Grid}.
!     \item[{[numDims]}]
!          Number of grid dimensions.
!     \item[{[delta1]}]
!          Array of physical increments between nodes in the first direction.
!     \item[{[delta2]}]
!          Array of physical increments between nodes in the second direction.
!     \item[{[countsPerDE1]}]
!          Array of number of grid increments per DE in the x-direction.
!     \item[{[countsPerDE2]}]
!          Array of number of grid increments per DE in the y-direction.
!     \item[{[min]}]
!          Array of minimum physical coordinates in each direction.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item [{[physgridName]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, j, gridBoundWidth, myDE(2)
      integer, dimension(numDims) :: counts, localStart
      character(len=ESMF_MAXSTR), dimension(numDims) :: coordNames, coordUnits
      logical, dimension(numDims) :: coordAligned, coordEqualSpaced, coordCyclic
      real(ESMF_KIND_R8) :: last
      real(ESMF_KIND_R8), dimension(numDims) :: localMin, localMax
      real(ESMF_KIND_R8), dimension(:), allocatable :: coord1, coord2
      real(ESMF_KIND_R8), dimension(:), allocatable :: deltaUse1, deltaUse2
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: gridp
      type(ESMF_PhysCoord) :: tempCoord

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Increment the number of physgrids in the grid and allocate memory
      ! This subroutine should be called before incrementing num_physgrids
      ! because it uses the old value internally.
      call ESMF_GridMakePhysGridSpace(grid, grid%num_physgrids+1, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridAddPhysGrid: temp_pgrids allocate"
        return
      endif
      grid%num_physgrids = grid%num_physgrids + 1

      ! Now initialize some values
      gridBoundWidth = 1   ! TODO: move into structure, make input?
      physgridId = grid%num_physgrids 

      ! figure out the position of myDE to get local counts
      gridp%ptr => grid
      call ESMF_GridGetDELayout(gridp, layout, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridAddPhysGridSpecd: get delayout"
        return
      endif
      call ESMF_DELayoutGetDEPosition(layout, myDE(1), myDE(2), status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridAddPhysGridSpecd: delayout get position"
        return
      endif

      localMin(1) = min(1)
      localMin(2) = min(2)
      localStart(1) = 0
      localStart(2) = 0
      if(myDE(1).ge.2) then
        do j = 1,myDE(1)-1
          do i = 1,countsPerDE1(j)
            localMin(1) = localMin(1) + delta1(i+localStart(1))
          enddo
          localStart(1) = localStart(1) + countsPerDE1(j)
        enddo
      endif
      if(myDE(2).ge.2) then
        do j = 1,myDE(2)-1
          do i = 1,countsPerDE2(j)
            localMin(2) = localMin(2) + delta2(i+localStart(2))
          enddo
          localStart(2) = localStart(2) + countsPerDE2(j)
        enddo
      endif

      ! The following is a bit rough and should be replaced by functionality
      ! that creates local coordinate arrays and halos them

      ! modify global counts to include ghost region
      counts(1) = size(delta1) + 2*gridBoundWidth
      counts(2) = size(delta2) + 2*gridBoundWidth

      ! allocate and load delta arrays with ghost region
      allocate(deltaUse1(counts(1)), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(1) =", counts(1)
        return
      endif
      allocate(deltaUse2(counts(2)), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(2) =", counts(2)
        return
      endif
      do i = 1,size(delta1)
        deltaUse1(i+gridBoundWidth) = delta1(i)
      enddo
      do i = 1,size(delta2)
        deltaUse2(i+gridBoundWidth) = delta2(i)
      enddo
      do i = 1,gridBoundWidth
        deltaUse1(i) = delta1(1)
        deltaUse2(i) = delta2(1)
        deltaUse1(i+size(delta1)+gridBoundWidth) = delta1(size(delta1))
        deltaUse2(i+size(delta2)+gridBoundWidth) = delta2(size(delta2))
      enddo

      ! modify local counts to include ghost/halo region
      counts(1) = countsPerDE1(myDE(1)) + 2*gridBoundWidth
      counts(2) = countsPerDE2(myDE(2)) + 2*gridBoundWidth

      ! allocate and load local coords -- plus one for extra vertex
      allocate(coord1(counts(1)+1), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(1) =", counts(1)+1
        return
      endif
      allocate(coord2(counts(2)+1), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(2) =", counts(2)+1
        return
      endif

      last = localMin(1)
      do i = 1,gridBoundWidth+1
        last = last - deltaUse1(i+localStart(1))
      enddo
      coord1(1) = last
      do i = 1,counts(1)
        coord1(i+1) = last + deltaUse1(i+localStart(1))
      enddo
      localMax(1) = coord1(counts(1)+1-gridBoundWidth)
      last = localMin(2)
      do i = 1,gridBoundWidth+1
        last = last - deltaUse2(i+localStart(2))
      enddo
      coord2(1) = last
      do i = 1,counts(2)
        coord2(i+1) = last + deltaUse2(i+localStart(2))
      enddo
      localMax(2) = coord2(counts(2)+1-gridBoundWidth)

      ! set parameters based on grid type
      select case (grid%horz_gridtype)

        ! ESMF_GridType_LatLon
        case (1)
          coordSystem         = ESMF_CoordSystem_Spherical
          coordNames(1)       = 'longitude'
          coordNames(2)       = 'latitude'
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .false.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_GridType_XY
        case (5)
          coordSystem         = ESMF_CoordSystem_Cartesian
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordUnits(:)       = ''
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .false.
          coordCyclic(:)      = .false.

        case default
           print *,'Grid type not yet supported in GridAddPhysGrid'
           status = ESMF_FAILURE

      end select

      ! Create the actual physgrid object
      grid%physgrids(physgridId) = ESMF_PhysGridCreate(numDims, relloc, &
                                     physgridName, coordSystem, status)

      do i = 1,numDims
        tempCoord = ESMF_PhysCoordCreate(coordNames(i), coordUnits(i), &
                                         coordAligned(i), coordEqualSpaced(i), &
                                         coordCyclic(i), localMin(i), &
                                         localMax(i), rc=status)
        call ESMF_PhysGridSetCoord(grid%physgrids(physgridId), tempCoord, &
                                   dim_order=i, rc=status) 
      enddo

      call ESMF_GridSetCoord(grid, physgridId, numDims, counts, gridBoundWidth, &
                             relloc, coord1, coord2, localMin, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridAddPhysGridSpecd: Grid set coord"
        return
      endif

      deallocate(coord1)
      deallocate(coord2)
      deallocate(deltaUse1)
      deallocate(deltaUse2)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddPhysGridSpecd

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridMakePhysGridSpace - Allocate or extend physgrid array

! !INTERFACE:
      subroutine ESMF_GridMakePhysGridSpace(gridp, newcount, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: gridp
      integer, intent(in) :: newcount
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Internal routine which verifies there is enough array space to
!     hold the specified number of physgrids.  Allocates more space and
!     copies the contents into the new space if not.
!
!     The arguments are:
!     \begin{description}
!     \item[gridp]
!          Pointer to an {\tt ESMF\_GridType}, the internal structure
!          which holds the {\tt Grid} information.
!     \item[newcount]
!          Make sure there are enough space in the array to hold 
!          {\tt newcount} items.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      ! note: this is an internal routine.  rc isn't optional - so we
      ! don't have to fool with rcpresent and status here.

      ! the save attribute is to be sure that the temp space isn't
      ! deallocated automatically by the compiler at the return of
      ! this routine. it's going to be pointed to by the physgrids pointer
      ! in the grid structure, but that might not be obvious to the compiler.
      type(ESMF_PhysGrid), dimension(:), pointer, save :: temp_pgrids
      integer :: i, oldcount, alloccount, allocrc

      ! number of currently used and available entries
      oldcount = gridp%num_physgrids
      alloccount = gridp%num_physgrids_alloc

      ! if there are already enough, we are done. 
      if (alloccount .ge. newcount) then
         rc = ESMF_SUCCESS
         return
      endif

#define CHUNK 4

      ! if none are allocated yet, allocate and return.
      ! the chunksize is 4 because it is a round number in base 2.
      if (alloccount .eq. 0) then
        allocate(gridp%physgrids(CHUNK), stat=allocrc)
        if(allocrc .ne. 0) then
          print *, "cannot allocate physgrids, first try"
          print *, "ERROR in ESMF_GridAddPhysGrid: physgrids allocate"
          rc = ESMF_FAILURE
          return
        endif
        gridp%num_physgrids_alloc = CHUNK
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
     if(allocrc .ne. 0) then
       print *, "cannot allocate temp_pgrids, alloc=", alloccount
       print *, "ERROR in ESMF_GridAddPhysGrid: temp_pgrids allocate"
       return
     endif

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_pgrids(i) = gridp%physgrids(i) 
     enddo

     ! deallocate old array
     deallocate(gridp%physgrids, stat=allocrc)
     if(allocrc .ne. 0) then
       print *, "ERROR in ESMF_GridAddPhysGrid: physgrids deallocate"
       return
     endif

     ! and set original pointer to the new space
     gridp%physgrids => temp_pgrids
 
     ! update count of how many items are currently allocated
     gridp%num_physgrids_alloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_GridMakePhysGridSpace

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetConfig - Get configuration information from a Grid

! !INTERFACE:
      subroutine ESMF_GridGetConfig(grid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out) :: config
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Returns the set of resources the {\tt ESMF\_Grid} object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[config]
!          Configuration information.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetConfig - Set configuration information for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetConfig(grid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: config
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Configures the {\tt ESMF\_Grid} object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be configured.
!     \item[config]
!          Configuration information.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetValue - Get <Value> for a Grid

! !INTERFACE:
      subroutine ESMF_GridGetValue(grid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns the value of {\tt ESMF\_Grid} attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[value]
!          Value to be retrieved.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridGetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get the coordinates of a Grid

! !INTERFACE:
      subroutine ESMF_GridGetCoord(grid, physgridId, relloc, centerCoord, &
                                   cornerCoord, faceCoord, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in), optional :: physgridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_Array), dimension(:), pointer, optional :: centerCoord
      type(ESMF_Array), dimension(:,:), pointer, optional :: cornerCoord
      type(ESMF_Array), optional :: faceCoord
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Determines the appropriate physgrid to query from either a physgridId or
!     relloc and returns the requested information.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be queried.
!     \item[{[physgridId]}]
!          Identifier for the {\tt ESMF\_PhysGrid} to be queried.
!     \item[{[relloc]}]
!          Relative location of the {\tt ESMF\_PhysGrid} to be queried.
!          Note: either the physgridId or relloc must be specified.  If both
!                are, the physgridId will take precedence.
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: physIdUse
      logical :: rellocIsValid, physIdIsValid

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initialize other variables
      physIdUse = -1
      rellocIsValid = .false.
      physIdIsValid = .false.

      ! Either the relative location or physgridId must be present and valid
      if (present(relloc)) then
!        rellocIsValid = ESMF_RelLocIsValid(relloc)  TODO: assume OK if there for now
        rellocIsValid = .true.
      endif
      if (present(physgridId)) then
        if ((physgridId.ge.1) .and. (physgridId.le.grid%ptr%num_physgrids)) then
          physIdIsValid = .true.
          physIdUse = physgridId
       endif
      endif
      if (.not.(rellocIsValid .or. physIdIsValid)) then
        print *, "ERROR in ESMF_GridGetCoord: need either relloc or physgridId"
        return
      endif

      ! If there is a relloc but no physgrid id, then get the id from the relloc
      if (rellocIsValid .and. .not.(physIdIsValid)) then
        call ESMF_GridGetPhysGridID(grid, relloc, physIdUse, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetCoord: get physgrid id"
          return
        endif
        if (physIdUse.eq.-1) then
          print *, "ERROR in ESMF_GridGetCoord: no physgrid corresponding", &
                   " to relloc"
          return
        endif
      endif

      ! Call PhysGridGet with valid physgrid
      if (present(centerCoord)) then
        call ESMF_PhysGridGetLocations(grid%ptr%physgrids(physIdUse), &
                                       location_array=centerCoord, &
                                       rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetLocations: physgrid get locations"
          return
        endif
      endif
      if (present(cornerCoord)) then
        call ESMF_PhysGridGetRegions(grid%ptr%physgrids(physIdUse), &
                                     vertex_array=cornerCoord, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetRegions: physgrid get regions"
          return
        endif
      endif
      ! TODO: face coords

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCoord

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordFromArray - Set the coordinates of a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetCoordFromArray(Grid, array, id, rc)
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
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetDE - Get DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_GridGetDE(grid, MyDE, local_cell_count, &
                                local_axis_length, global_start, ai_global, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(inout), optional :: MyDE
      integer, intent(inout), optional :: local_cell_count
      integer, dimension(:), intent(inout), optional :: local_axis_length
      integer, dimension(:,:), intent(inout), optional :: global_start
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM), intent(inout), &
                        optional :: ai_global
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[MyDE]}]
!          Identifier for this {\tt ESMF\_DE}.
!     \item[{[local\_cell\_count]}]
!          Local (on this {\tt ESMF\_DE}) number of cells.
!     \item[{[local\_axis\_length]}]
!          Local (on this {\tt ESMF\_DE}) number of cells per axis.
!     \item[{[global\_start]}]
!          Global index of starting counts for each dimension.
!     \item[{[ai\_global]}]
!          Global axis indices for each dimension.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGetDE(grid%ptr%distgrid%ptr, MyDE, local_cell_count, &
                              local_axis_length, global_start, ai_global, &
                              rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetDE: distgrid get de"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetAllAxisIndex - Get all axis indices for a DistGrid

! !INTERFACE:
      subroutine ESMF_GridGetAllAxisIndex(grid, globalAI, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: globalAI
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[globalAI]}]
!          Global axis indices on all DE's.
!     \item[{[total]}]
!          Logical flag for whether the axis indices should be for total
!          cells or not.  Default is false, which infers computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGetAllAxisIndex(grid%ptr%distgrid%ptr, globalAI, &
                                        total, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetAllAxisIndex: distgrid get all axis index"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetAllAxisIndex

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
!     \item[{[layout]}]
!          Pointer to the {\tt ESMF\_DELayout} corresponding to the {\tt ESMF\_Grid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGetDELayout(grid%ptr%distgrid%ptr, layout, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetDELayout: distgrid get delayout"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDELayout

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetPhysGrid - Get PhysGrid information

! !INTERFACE:
      subroutine ESMF_GridGetPhysGrid(grid, relloc, physgridId, name, numDims, &
                                      numCorners, numFaces, coordSystem, &
                                      dimNames, dimUnits, localMin, localMax, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid 
      type(ESMF_RelLoc), intent(inout), optional :: relloc
      integer, intent(inout), optional :: physgridId
      character(*), intent(inout), optional :: name  ! name of grid
      integer, intent(inout), optional :: numDims
                                         ! number of physical dimensions
      integer, intent(inout), optional :: numCorners
                                         ! number of corners in each cell
      integer, intent(inout), optional :: numFaces
                                         ! number of faces for each cell
      type(ESMF_CoordSystem), intent(inout), optional :: coordSystem
                                         ! TODO
      character(*), dimension(:), intent(inout), optional :: dimNames
                                         ! names for each dimension
      character(*), dimension(:), intent(inout), optional :: dimUnits
                                         ! units for each dimension
      real(ESMF_KIND_R8), dimension(:), intent(inout), optional :: localMin
                                         ! local minimum in each coord direction
      real(ESMF_KIND_R8), dimension(:), intent(inout), optional :: localMax
                                         ! local maximum in each coord direction
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return specified {\tt ESMF\_PhysGrid} information associated with either
!     a physgridId or a given relative location.  Return error if the grid
!     contains no physgrid at the specified location.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[physgridId]
!          Physgrid identifier.
!     \item[relloc]
!           Relative location to query
!     \item[[name]]
!          {\tt ESMF\_PhysGrid} name.
!     \item[[numDims]]
!          Number of physical dimensions for this PhysGrid.
!     \item[[dimNames]]
!          Names for each physical dimension of this PhysGrid.
!     \item[[dimUnits]]
!          Units for each physical dimension of this PhysGrid.
!     \item[[numCorners]]
!          Number of corners for each PhysGrid cell (can be degenerate).
!     \item[[numFaces]]
!          Number of faces for each PhysGrid cell.
!     \item[[localMin]]
!          Minimum local physical coordinate in each coordinate direction.
!     \item[[localMax]]
!          Maximum local physical coordinate in each coordinate direction.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      integer :: physIdUse, i
      logical :: rellocIsValid, physIdIsValid
      type(ESMF_RelLoc) :: thisRelloc
      type(ESMF_PhysCoord) :: tempCoord

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize other variables
      physIdUse = -1
      rellocIsValid = .false.
      physIdIsValid = .false.

!     Either the relative location or physgridId must be present and valid
      if (present(relloc)) then
!        rellocIsValid = ESMF_RelLocIsValid(relloc)  TODO: assume OK if there for now
        rellocIsValid = .true.
      endif
      if (present(physgridId)) then
        if ((physgridId.ge.1) .and. (physgridId.le.grid%ptr%num_physgrids)) then
          physIdIsValid = .true.
          physIdUse = physgridId
       endif
      endif
      if (.not.(rellocIsValid .or. physIdIsValid)) then
        print *, "ERROR in ESMF_GridGetPhysGrid: need either relloc or physgridId"
        return
      endif

!     Loop through physgrids comparing rellocs  TODO: make part of the Grid obj?
      if (rellocIsValid .and. .not.(physIdIsValid)) then
        do i = 1,grid%ptr%num_physgrids
          call ESMF_PhysGridGet(grid%ptr%physgrids(i), relloc=thisRelloc, &
                                rc=status)
          if (relloc.eq.thisRelloc) then
            physIdUse = i
            exit
          endif
        enddo
        if (physIdUse.eq.-1) then
          print *, "ERROR in ESMF_GridGetPhysGrid: no physgrid corresponding", &
                   " to relloc"
          return
        endif
      endif

!     Call PhysGridGet with valid physgrid
      if(present(relloc) .or. present(name) .or. present(numDims) &
                         .or. present(coordSystem)) then
        call ESMF_PhysGridGet(grid%ptr%physgrids(physIdUse), &
                              relloc=relloc, name=name, num_dims=numDims, &
                              coord_system=coordSystem, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetPhysGrid: physgrid get"
          return
        endif
      endif
      if(present(numCorners)) then
        ! TODO: add numFaces to this call
        call ESMF_PhysGridGetRegions(grid%ptr%physgrids(physIdUse), &
                                     num_vertices=numCorners, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetPhysGrid: physgrid get regions"
          return
        endif
      endif
      if(present(dimNames)) then
        do i = 1,numDims
          ! TODO: fix for reordered dimensions
          call ESMF_PhysGridGetCoord(grid%ptr%physgrids(physIdUse), tempCoord, &
                                     dim_order=i, rc=status)
          dimNames(i) = ESMF_PhysCoordGetName(tempCoord, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridGetPhysGrid: physcoord get name"
            return
          endif
        enddo
      endif
      if(present(dimUnits)) then
        do i = 1,numDims
          ! TODO: fix for reordered dimensions
          call ESMF_PhysGridGetCoord(grid%ptr%physgrids(physIdUse), tempCoord, &
                                     dim_order=i, rc=status)
          dimUnits(i) = ESMF_PhysCoordGetUnits(tempCoord, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridGetPhysGrid: physcoord get units"
            return
          endif
        enddo
      endif
      if(present(localMin) .or. present(localMax)) then
        do i = 1,numDims
          ! TODO: fix for reordered dimensions
          call ESMF_PhysGridGetCoord(grid%ptr%physgrids(physIdUse), tempCoord, &
                                     dim_order=i, rc=status)
          call ESMF_PhysCoordGetExtents(tempCoord, min_val=localMin(i), &
                                        max_val=localMax(i), rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridGetPhysGrid: physcoord get extents"
            return
          endif
        enddo
      endif
 
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetPhysGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetPhysGridID - Get PhysGrid Id for a given relative location

! !INTERFACE:
      subroutine ESMF_GridGetPhysGridID(grid, relloc, physgridId, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid 
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(out) :: physgridId
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return the {\tt ESMF\_PhysGridId} associated with the given relative
!     location.  Return error if the grid contains no physgrid at the
!     specified location.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[physgrid]
!          Returned physgrid.
!     \item[relloc]
!           Relative location to query
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      type(ESMF_RelLoc) :: thisRelloc
      integer :: i

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      physgridId = -1

!     Loop through physgrids comparing rellocs  TODO: make part of the Grid obj?
      do i = 1,grid%ptr%num_physgrids
        call ESMF_PhysGridGet(grid%ptr%physgrids(i), relloc=thisRelloc, &
                              rc=status)
        if (relloc.eq.thisRelloc) then
          physgridId = i
          status = ESMF_SUCCESS
          exit
        endif
      enddo

      if (rcpresent) rc = status

      end subroutine ESMF_GridGetPhysGridID

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_GridGlobalToLocalIndex(grid, global1D, local1D, &
                                             global2D, local2D, &
                                             globalAI1D, localAI1D, &
                                             globalAI2D, localAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) :: global1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: local1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) :: global2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: local2D
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) :: globalAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) ::  localAI2D
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
!          One-dimensional {\tt ESMF\_LocalArray} of global identifiers to be translated.
!          Infers translating between positions in memory.
!     \item[{[local1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of local identifiers corresponding to
!          global identifiers.
!     \item[{[global2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of global identifiers to be translated.
!          Infers translating between indices in ij space.
!     \item[{[local2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of local identifiers corresponding to
!          global identifiers.
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
!EOP
! !REQUIREMENTS:

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGlobalToLocalIndex(grid%ptr%distgrid%ptr, &
                                           global1D, local1D, &
                                           global2D, local2D, &
                                           globalAI1D, localAI1D, &
                                           globalAI2D, localAI2D, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGlobalToLocalIndex: distgrid global to local"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGlobalToLocalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridLocalToGlobalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_GridLocalToGlobalIndex(grid, local1D, global1D, &
                                             local2D, global2D, &
                                             localAI1D, globalAI1D, &
                                             localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
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
!          One-dimensional {\tt ESMF\_LocalArray} of local identifiers to be translated.
!          Infers translating between positions in memory.
!     \item[{[global1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of global identifiers corresponding to
!          local identifiers.
!     \item[{[local2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of local identifiers to be translated.
!          Infers translating between indices in ij space.
!     \item[{[global2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of global identifiers corresponding to
!          local identifiers.
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
!EOP
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridLocalToGlobalIndex(grid%ptr%distgrid%ptr, &
                                           local1D, global1D, &
                                           local2D, global2D, &
                                           localAI1D, globalAI1D, &
                                           localAI2D, globalAI2D, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridLocalToGlobalIndex: distgrid local to global"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridLocalToGlobalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordFromBuffer - Set the coordinates of a Grid from an existing data buffer

! !INTERFACE:
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
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordSpecd - Compute coordinates for a specified Grid

! !INTERFACE:
      subroutine ESMF_GridSetCoordSpecd(grid, physgridId, numDims, counts, &
                                        gridBoundWidth, relloc, coord1, coord2, &
                                        min, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: physgridId
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      integer, intent(in) :: gridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes coordinates for a {\tt ESMF\_Grid} via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[{[physgridId]}]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[{[numDims]}]
!          Number of grid dimensions.
!     \item[{[counts]}]
!          Array of number of grid increments in each dimension.
!     \item[{[relloc]}]
!          Relative location in grid cell for which this PhysGrid.
!     \item[{[delta1]}]
!          Array of specified grid coordinates in the first dimension.
!     \item[{[delta2]}]
!          Array of specified grid coordinates in the second dimension.
!     \item[{[min]}]
!          Array of minimum local physical coordinates in each dimension.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, j, i1, j1
      real(ESMF_KIND_R8) :: coordUse1, coordUse2
      real(ESMF_KIND_R8), dimension(:,:), pointer :: temp1, temp2
      type(ESMF_Array), dimension(:), pointer :: arrayTemp
      type(ESMF_DataKind) :: kind
      type(ESMF_DataType) :: type

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: different subroutines for different numDims?  or case?
      ! TODO: could be a 1-D array for each coord axis later, but that
      !       would have to be supported by Regrid first

      ! allocate arrays
      allocate(arrayTemp(numDims))

      ! create ESMF_Arrays
      kind = ESMF_R8
      type = ESMF_DATA_REAL
      arrayTemp(1) = ESMF_ArrayCreate(numDims, type, kind, counts, &
                                      halo_width=gridBoundWidth, rc=status)
      arrayTemp(2) = ESMF_ArrayCreate(numDims, type, kind, counts, &
                                      halo_width=gridBoundWidth, rc=status)
      call ESMF_ArrayGetData(arrayTemp(1), temp1, ESMF_DATA_REF, status)
      call ESMF_ArrayGetData(arrayTemp(2), temp2, ESMF_DATA_REF, status)

!     For now, an if construct for the different relative locations
!     TODO: also set corners and faces
      if (relloc .eq. ESMF_CELL_UNDEFINED) then
        status = ESMF_FAILURE

      elseif (relloc .eq. ESMF_CELL_CENTER) then
        do i = 1,counts(1)
          coordUse1 = 0.5d0*(coord1(i)+coord1(i+1))
          do j = 1,counts(2)
            coordUse2 = 0.5d0*(coord2(j)+coord2(j+1))
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_NFACE) then
        do i = 1,counts(1)
          coordUse1 = 0.5d0*(coord1(i)+coord1(i+1))
          do j = 1,counts(2)
            coordUse2 = coord2(j+1)
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_SFACE) then
        do i = 1,counts(1)
          coordUse1 = 0.5d0*(coord1(i)+coord1(i+1))
          do j = 1,counts(2)
            coordUse2 = coord2(j)
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_EFACE) then
        do i = 1,counts(1)
          coordUse1 = coord1(i+1)
          do j = 1,counts(2)
            coordUse2 = 0.5d0*(coord2(j)+coord2(j+1))
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_WFACE) then
        do i = 1,counts(1)
          coordUse1 = coord1(i)
          do j = 1,counts(2)
            coordUse2 = 0.5d0*(coord2(j)+coord2(j+1))
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_NECORNER) then
        do i = 1,counts(1)
          coordUse1 = coord1(i+1)
          do j = 1,counts(2)
            coordUse2 = coord2(j+1)
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

        ! TODO: rest of the corners

      else
        print *, "This relative location not yet supported in ", &
                 "GridSetCoordSpecd"
        return
      endif

      ! now set the location array in PhysGrid
      arrayTemp(1) = ESMF_ArrayCreate(temp1, ESMF_DATA_COPY, halo_width=1, &
                                      rc=status)
      arrayTemp(2) = ESMF_ArrayCreate(temp2, ESMF_DATA_COPY, halo_width=1, &
                                      rc=status)
      call ESMF_PhysGridSetLocations(grid%physgrids(physgridId), &
                                     location_array=arrayTemp, rc=status)
            ! TODO: add name to set call
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridSetCoordSpecd: PhysGrid set locations"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetCoordSpecd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordUniform - Compute coordinates for a uniform Grid

! !INTERFACE:
      subroutine ESMF_GridSetCoordUniform(grid, physgridId, numDims, counts, &
                                          gridBoundWidth, relloc, delta, min, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: physgridId
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      integer, intent(in) :: gridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: delta
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes coordinates for a {\tt ESMF\_Grid} via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[{[physgridId]}]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[{[numDims]}]
!          Number of grid dimensions.
!     \item[{[counts]}]
!          Array of number of grid increments in each dimension.
!     \item[{[relloc]}]
!          Relative location in grid cell for which this PhysGrid.
!     \item[{[delta]}]
!          Array of uniform grid increments in each dimension.
!     \item[{[min]}]
!          Array of minimum physical coordinates in each dimension.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, j, i1, j1
      real(ESMF_KIND_R8) :: coord1, coord2
      real(ESMF_KIND_R8), dimension(:,:), pointer :: temp1, temp2
      type(ESMF_Array), dimension(:), pointer :: arrayTemp
      type(ESMF_DataKind) :: kind
      type(ESMF_DataType) :: type

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: different subroutines for different numDims?  or case?
      ! TODO: could be a 1-D array for each coord axis later, but that
      !       would have to be supported by Regrid first

      ! allocate arrays
      allocate(arrayTemp(numDims))

      ! create ESMF_Arrays
      kind = ESMF_R8
      type = ESMF_DATA_REAL
      arrayTemp(1) = ESMF_ArrayCreate(numDims, type, kind, counts, &
                                      halo_width=gridBoundWidth, rc=status)
      arrayTemp(2) = ESMF_ArrayCreate(numDims, type, kind, counts, &
                                      halo_width=gridBoundWidth, rc=status)
      call ESMF_ArrayGetData(arrayTemp(1), temp1, ESMF_DATA_REF, status)
      call ESMF_ArrayGetData(arrayTemp(2), temp2, ESMF_DATA_REF, status)

!     For now, an if construct for the different relative locations
!     TODO: also set corners and faces
      if (relloc .eq. ESMF_CELL_UNDEFINED) then
        status = ESMF_FAILURE

      elseif (relloc .eq. ESMF_CELL_CENTER) then
        do i = 1,counts(1)
          i1 = i - gridBoundWidth
          coord1 = delta(1)*0.5*real(i1+i1-1) + min(1)
          do j = 1,counts(2)
            j1 = j - gridBoundWidth
            coord2 = delta(2)*0.5*real(j1+j1-1) + min(2)
            temp1(i,j) = coord1 
            temp2(i,j) = coord2 
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_NFACE) then
        do i = 1,counts(1)
          i1 = i - gridBoundWidth
          coord1 = delta(1)*0.5*real(i1+i1-1) + min(1)
          do j = 1,counts(2)
            j1 = j - gridBoundWidth
            coord2 = delta(2)*real(j1) + min(2)
            temp1(i,j) = coord1 
            temp2(i,j) = coord2 
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_SFACE) then
        do i = 1,counts(1)
          i1 = i - gridBoundWidth
          coord1 = delta(1)*0.5*real(i1+i1-1) + min(1)
          do j = 1,counts(2)
            j1 = j - gridBoundWidth
            coord2 = delta(2)*real(j1-1) + min(2)
            temp1(i,j) = coord1 
            temp2(i,j) = coord2 
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_EFACE) then
        do i = 1,counts(1)
          i1 = i - gridBoundWidth
          coord1 = delta(1)*real(i1) + min(1)
          do j = 1,counts(2)
            j1 = j - gridBoundWidth
            coord2 = delta(2)*0.5*real(j1+j1-1) + min(2)
            temp1(i,j) = coord1 
            temp2(i,j) = coord2 
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_WFACE) then
        do i = 1,counts(1)
          i1 = i - gridBoundWidth
          coord1 = delta(1)*real(i1-1) + min(1)
          do j = 1,counts(2)
            j1 = j - gridBoundWidth
            coord2 = delta(2)*0.5*real(j1+j1-1) + min(2)
            temp1(i,j) = coord1 
            temp2(i,j) = coord2 
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_NECORNER) then
        do i = 1,counts(1)
          i1 = i - gridBoundWidth
          coord1 = delta(1)*real(i1) + min(1)
          do j = 1,counts(2)
            j1 = j - gridBoundWidth
            coord2 = delta(2)*real(j1) + min(2)
            temp1(i,j) = coord1 
            temp2(i,j) = coord2 
          enddo
        enddo

      ! TODO: rest of the corners

      else
        print *, "This relative location not yet supported in ", &
                 "GridSetCoordUnifrom"
        return
      endif

      ! now set the location array in PhysGrid
      call ESMF_PhysGridSetLocations(grid%physgrids(physgridId), &
                                     location_array=arrayTemp, rc=status)
            ! TODO: add name to set call
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridSetCoordUnifrom: PhysGrid set locations"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetCoordUniform

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordCopy - Copies coordinates from one grid to another

! !INTERFACE:
      subroutine ESMF_GridSetCoordCopy(Grid, Grid_in, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: grid_in
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
!     \item[grid\_in]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
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
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGet - Gets a variety of information about the grid

! !INTERFACE:
      subroutine ESMF_GridGet(grid, horz_gridtype, vert_gridtype, &
                              horz_stagger, vert_stagger, &
                              horz_coord_system, vert_coord_system, &
                              coord_order, global_min_coords, &
                              global_max_coords, global_cell_dim, &
                              global_start, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out), optional :: horz_gridtype
      integer, intent(out), optional :: vert_gridtype
      integer, intent(out), optional :: horz_stagger
      integer, intent(out), optional :: vert_stagger
      type(ESMF_CoordSystem), intent(out), optional :: horz_coord_system
      type(ESMF_CoordSystem), intent(out), optional :: vert_coord_system
      integer, intent(out), optional :: coord_order
      real(ESMF_KIND_R8), intent(out), dimension(ESMF_MAXGRIDDIM), &
                            optional :: global_min_coords
      real(ESMF_KIND_R8), intent(out), dimension(ESMF_MAXGRIDDIM), &
                            optional :: global_max_coords
      integer, intent(out), dimension(ESMF_MAXGRIDDIM), &
                            optional :: global_cell_dim
      integer, intent(out), dimension(:,:), optional :: global_start
      type (ESMF_Logical), intent(out), optional :: periodic(:)
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
!     \item[{[horz\_gridtype]}]
!          Integer specifier to denote horizontal gridtype
!     \item[{[vert\_gridtype]}]
!          Integer specifier to denote vertical gridtype
!     \item[{[horz\_stagger]}]
!          Integer specifier to denote horizontal grid stagger
!     \item[{[vert\_stagger]}]
!          Integer specifier to denote vertical grid stagger
!     \item[{[horz\_coord\_system]}]
!          Integer specifier to denote horizontal coordinate system
!     \item[{[vert\_coord\_system]}]
!          Integer specifier to denote vertical coordinate system
!     \item[{[coord\_order]}]
!          Integer specifier to denote coordinate ordering
!     \item[{[global\_min\_coords]}]
!          Minimum global physical coordinates.
!     \item[{[global\_max\_coords]}]
!          Maximum global physical coordinates.
!     \item[{[global\_nmax]}]
!          Numbers of global grid increments.
!     \item[{[periodic]}]
!          Returns the periodicity along the coordinate axes - logical array.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: physgridId
      integer :: i                                ! Loop index
      type(ESMF_GridType), pointer :: gridp

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
 
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_GridGet called with invalid grid object"
        return
      endif

      gridp => grid%ptr

      ! if present, gets information from the grid derived type
      if(present(horz_gridtype)) horz_gridtype = gridp%horz_gridtype
      if(present(vert_gridtype)) vert_gridtype = gridp%vert_gridtype
      if(present(horz_stagger)) horz_stagger = gridp%horz_stagger
      if(present(vert_stagger)) vert_stagger = gridp%vert_stagger
      if(present(horz_coord_system)) horz_coord_system = gridp%horz_coord_system
      if(present(vert_coord_system)) vert_coord_system = gridp%vert_coord_system
      if(present(coord_order)) coord_order = gridp%coord_order
    
      ! Get physgrid info with global coordinate extents
      if(present(global_min_coords) .or. present(global_max_coords)) then
        physgridId = gridp%num_physgrids  ! TODO: fix so passed in?
      !  call ESMF_PhysGridGet(gridp%physgrids(physgridId)%ptr, &
      !                        global_min=global_min_coords, &
      !                        global_max=global_max_coords, rc=status)
      ! TODO: fix once we figure out where global min/max is
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGet: PhysGrid get"
          return
        endif
      endif

      ! Get distgrid info with global coordinate counts
      if(present(global_cell_dim) .or. present(global_start)) then
        call ESMF_DistGridGet(gridp%distgrid%ptr, &
                              global_cell_dim=global_cell_dim, &
                              global_start=global_start, &
                              rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGet: DistGrid get"
          return
        endif
      endif

      ! get name from base obj
      if (present(name)) then
        call ESMF_GetName(gridp%base, name, status)
        if(status .ne. ESMF_SUCCESS) then
           print *, "ERROR in ESMF_GridGetName"
           return
        endif
      endif

      ! Get global coordinate extents
      if(present(global_min_coords)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(global_min_coords)) exit
          global_min_coords(i) = gridp%globalMinCoord(i)
        enddo
      endif
      if(present(global_max_coords)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(global_max_coords)) exit
          global_max_coords(i) = gridp%globalMaxCoord(i)
        enddo
      endif

      ! get the periodicity
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            periodic(i) = gridp%periodic(i)
         enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSet - Sets a variety of information about the grid

! !INTERFACE:
      subroutine ESMF_GridSet(grid, horz_gridtype, vert_gridtype, &
                              horz_stagger, vert_stagger, &
                              horz_coord_system, vert_coord_system, &
                              coord_order, global_min_coord, global_max_coord, &
                              periodic, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      type(ESMF_CoordSystem), intent(in), optional :: horz_coord_system
      type(ESMF_CoordSystem), intent(in), optional :: vert_coord_system
      integer, intent(in), optional :: coord_order
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: global_min_coord
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: global_max_coord
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
!     \item[{[horz\_gridtype]}]
!          Integer specifier to denote horizontal gridtype
!     \item[{[vert\_gridtype]}]
!          Integer specifier to denote vertical gridtype
!     \item[{[horz\_stagger]}]
!          Integer specifier to denote horizontal grid stagger
!     \item[{[vert\_stagger]}]
!          Integer specifier to denote vertical grid stagger
!     \item[{[horz\_coord\_system]}]
!          Integer specifier to denote horizontal coordinate system
!     \item[{[vert\_coord\_system]}]
!          Integer specifier to denote vertical coordinate system
!     \item[{[coord\_order]}]
!          Integer specifier to denote coordinate ordering
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate axes.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! loop index

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     if present, set information filling in grid derived type
      if(present(horz_gridtype)) grid%horz_gridtype = horz_gridtype
      if(present(vert_gridtype)) grid%vert_gridtype = vert_gridtype
      if(present(horz_stagger)) grid%horz_stagger = horz_stagger
      if(present(vert_stagger)) grid%vert_stagger = vert_stagger
      if(present(horz_coord_system)) grid%horz_coord_system = horz_coord_system
      if(present(vert_coord_system)) grid%vert_coord_system = vert_coord_system
      if(present(coord_order)) grid%coord_order = coord_order
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            grid%periodic(i) = periodic(i)
         enddo
      endif

      if (present(global_min_coord)) then
   !      if (size(global_min_coord) .gt. ESMF_MAXGRIDDIM) exit  ! TODO
         do i=1,size(global_min_coord)
            grid%globalMinCoord(i) = global_min_coord(i)
         enddo
      endif
      if (present(global_max_coord)) then
   !      if (size(global_max_coord) .gt. ESMF_MAXGRIDDIM) exit  ! TODO
         do i=1,size(global_max_coord)
            grid%globalMaxCoord(i) = global_max_coord(i)
         enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetLMaskFromArray - Set a logical mask in a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetLMaskFromArray(Grid, array, name, rc)
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
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetLMaskFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetLMaskFromBuffer - Set a logical mask in a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetLMaskFromBuffer(Grid, buffer, name, rc)
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
!           {\tt ESMF\_LMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetLMaskFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetLMaskFromMMask - Set a logical mask in a Grid from an existing multiplicative mask

! !INTERFACE:
      subroutine ESMF_GridSetLMaskFromMMask(Grid, mmask, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: mmask        ! TODO: name?
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the logical mask data will be
!     created from an existing multiplicative mask.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[{[mmask]}]
!          Multiplicative mask identifier.
!     \item [{[name]}]
!           {\tt ESMF\_LMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetLMaskFromMMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetLMaskCopy - Copies a logical mask from one grid to another.

! !INTERFACE:
      subroutine ESMF_GridSetLMaskCopy(Grid, Grid_in, name, name_in, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: grid_in
      character (len=*), intent(in), optional :: name
      character (len=*), intent(in), optional :: name_in
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a logical mask for a {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[grid\_in]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [{[name]}]
!           {\tt ESMF\_LMask} name to be set.
!     \item [{[name\_in]}]
!           {\tt ESMF\_LMask} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetLMaskCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskFromArray - Set a multiplicative mask in a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetMMaskFromArray(Grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the multiplicative mask data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item [{[name]}]
!           {\tt ESMF\_MMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskFromBuffer - Set a multiplicative mask in a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetMMaskFromBuffer(Grid, buffer, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real, dimension (:), pointer :: buffer
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the multiplicative mask data exists already
!     and is being passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt ESMF\_MMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskFromLMask - Set a multiplicative mask in a Grid from an existing logical mask

! !INTERFACE:
      subroutine ESMF_GridSetMMaskFromLMask(Grid, lmask, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: lmask
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the multiplicative mask data will be
!     created from an existing logical mask.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[lmask]
!          Logical mask identifier.
!     \item [{[name]}]
!           {\tt ESMF\_MMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskFromLMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskCopy - Copies a multiplicative mask from one grid to another.

! !INTERFACE:
      subroutine ESMF_GridSetMMaskCopy(Grid, Grid_in, name, name_in, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: grid_in
      character (len=*), intent(in), optional :: name
      character (len=*), intent(in), optional :: name_in
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a multiplicative mask for a {\tt ESMF\_Grid} from another
!     {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[grid\_in]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [{[name]}]
!           {\tt ESMF\_MMask} name to be set.
!     \item [{[name\_in]}]
!           {\tt ESMF\_MMask} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetricFromArray - Set a metric for a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetMetricFromArray(Grid, array, name, rc)
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
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetricFromBuffer - Set a metric for a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetMetricFromBuffer(Grid, buffer, name, rc)
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
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetricCompute - Compute a metric for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetMetricCompute(Grid, name, id, rc)
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
!     \item[{[id]}]
!          Identifier for predescribed metrics.  TODO: make list
!     \item [{[name]}]
!           {\tt ESMF\_Metric} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricCompute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetricCopy - Copies a metric from one grid to another

! !INTERFACE:
      subroutine ESMF_GridSetMetricCopy(Grid, name, Grid_in, name_in, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      type(ESMF_Grid), intent(in) :: grid_in
      character (len=*), intent(in) :: name_in  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a metric for a {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item [{[name]}]
!           {\tt ESMF\_Metric} name to be set.
!     \item[grid\_in]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [{[name\_in]}]
!           {\tt ESMF\_Metric} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetRegionIDFromArray - Set a region identifier in a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetRegionIDFromArray(Grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      character (len=*), intent(in) :: name  ! TODO: optional?
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
!     \item [{[name]}]
!           {\tt ESMF\_RegionID} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetRegionIDFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetRegionIDFromBuffer - Set a region identifier in a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetRegionIDFromBuffer(Grid, buffer, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real, dimension (:), pointer :: buffer
      character (len=*), intent(in) :: name  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the multiplicative mask data exists already
!     and is being passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt ESMF\_RegionID} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetRegionIDFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetRegionIDCopy - Copies a region identifier from one grid to another

! !INTERFACE:
      subroutine ESMF_GridSetRegionIDCopy(Grid, name, Grid_in, name_in, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      type(ESMF_Grid), intent(in) :: grid_in
      character (len=*), intent(in) :: name_in  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a region identifier for a {\tt ESMF\_Grid} from another
!     {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item [{[name]}]
!           {\tt ESMF\_RegionID} name to be set.
!     \item[grid\_in]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [{[name\_in]}]
!           {\tt ESMF\_RegionID} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetRegionIDCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetBoundingBoxes - Get the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_GridGetBoundingBoxes(grid, array, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), intent(in) :: grid
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
!EOP
! !REQUIREMENTS:
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      array = grid%boundingBoxes

      end subroutine ESMF_GridGetBoundingBoxes

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetBoundingBoxesUni - Set the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_GridSetBoundingBoxesUni(grid, numDims, min, delta, &
                                              countsPerAxis, numDE1, numDE2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), intent(inout) :: grid
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: delta
      integer, dimension(:,:), intent(in) :: countsPerAxis
      integer, intent(in) :: numDE1
      integer, intent(in) :: numDE2
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
!     \item[numDims]
!          Number of grid dimensions (directions).
!     \item[min]
!          Array of minimum physical coordinates in each direction.
!     \item[delta]
!          Array of uniform physical increments between nodes in each direction.
!     \item[countsPerAxis]
!          Array of number of grid increments per DE in each direction.
!     \item[numDE1]
!          Number of DEs in the first direction of the decomposition.
!     \item[numDE2]
!          Number of DEs in the second direction of the decomposition.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: DE, numDEs, rank, npts
      integer :: i, j, jDE
      real(ESMF_KIND_R8) :: start, stop
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      numDEs = numDE1*numDE2
      rank   = 2   ! TODO: hard-coded for now
      npts   = 2**rank

!     TODO: break out by rank?
!     Assume the following starage for bounding boxes:
!       number of DEs * npts * rank
!       where npts is the number of points necessary to describe a bounding box
!       and rank is the number of dimensions.  For the time being, the points
!       are stored in the following order:
!                     1. (Xmin,Ymin,Zmin)
!                     2. (Xmax,Ymin,Zmin)
!                     3. (Xmax,Ymax,Zmin)
!                     4. (Xmin,Ymax,Zmin)
!                     5. (Xmin,Ymin,Zmax)
!                     6. (Xmax,Ymin,Zmax)
!                     7. (Xmax,Ymax,Zmax)
!                     8. (Xmin,Ymax,Zmax)
      allocate(boxes(numDEs,npts,rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, boxes(nDE,npt,rank) = ", numDEs,npts,rank
         return
      endif
      

!     Calculate box for each DE
!     Direction 1 first
      start = min(1)
      stop  = min(1)
      do j = 1,numDE1
        stop = stop + delta(1)*real(countsPerAxis(j,1))
        do i = 1,numDE2
          DE = (i-1)*numDE1 + j
          boxes(DE,1,1) = start
          boxes(DE,2,1) = stop
          boxes(DE,3,1) = stop
          boxes(DE,4,1) = start
        enddo
        start = stop
      enddo

!     Direction 2 next
      start = min(2)
      stop  = min(2)
      do j = 1,numDE2
        jDE  = (j-1)*numDE1 + 1
        stop = stop + delta(2)*real(countsPerAxis(jDE,2))
        do i = 1,numDE1
          DE = (j-1)*numDE1 + i
          boxes(DE,1,2) = start
          boxes(DE,2,2) = stop
          boxes(DE,3,2) = stop
          boxes(DE,4,2) = start
        enddo
        start = stop
      enddo

      grid%boundingBoxes = ESMF_LocalArrayCreate(boxes, ESMF_DATA_REF, status)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetBoundingBoxesUni

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetBoundingBoxesSpecd - Set the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_GridSetBoundingBoxesSpecd(grid, numDims, min, delta1, &
                                                delta2, countsPerDE1, &
                                                countsPerDE2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), intent(inout) :: grid
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta2
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
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
!     \item[numDims]
!          Number of grid dimensions (directions).
!     \item[min]
!          Array of minimum physical coordinate in each direction.
!     \item[delta1]
!          Array of physical increments between nodes in the first direction.
!     \item[delta2]
!          Array of physical increments between nodes in the second direction.
!     \item[countsPerDE1]
!          Array of number of grid increments per DE in the x-direction.
!     \item[countsPerDE2]
!          Array of number of grid increments per DE in the y-direction.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: DE, numDE1, numDE2, numDEs, npts
      integer :: i, i1, j
      real(ESMF_KIND_R8) :: start, stop
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      numDE1 = size(countsPerDE1)
      numDE2 = size(countsPerDE2)
      numDEs = numDE1*numDE2
      npts   = 2**numDims

!     TODO: break out by rank?
!     Assume the following starage for bounding boxes:
!       number of DEs * npts * numDims
!       where npts is the number of points necessary to describe a bounding box
!       and rank is the number of dimensions.  For the time being, the points
!       are stored in the following order:
!                     1. (Xmin,Ymin,Zmin)
!                     2. (Xmax,Ymin,Zmin)
!                     3. (Xmax,Ymax,Zmin)
!                     4. (Xmin,Ymax,Zmin)
!                     5. (Xmin,Ymin,Zmax)
!                     6. (Xmax,Ymin,Zmax)
!                     7. (Xmax,Ymax,Zmax)
!                     8. (Xmin,Ymax,Zmax)
      allocate(boxes(numDEs,npts,numDims), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, boxes(nDE,npt,numDims) = ", numDEs,npts, &
                   numDims
         return
      endif

!     Calculate box for each DE
!     Direction 1 first
      start = min(1)
      stop  = min(1)
      i1    = 0
      do j = 1,numDE1
        do i = i1+1,i1+countsPerDE1(j)
          stop = stop + delta1(i)
        enddo
        do i = 1,numDE2
          DE = (i-1)*numDE1 + j
          boxes(DE,1,1) = start
          boxes(DE,2,1) = stop
          boxes(DE,3,1) = stop
          boxes(DE,4,1) = start
        enddo
        start = stop
        i1    = i1 + countsPerDE1(j)
      enddo
!     Direction 2 next
      start = min(2)
      stop  = min(2)
      i1    = 0
      do j = 1,numDE2
        do i = i1+1,i1+countsPerDE2(j)
          stop = stop + delta2(i)
        enddo
        do i = 1,numDE1
          DE = (j-1)*numDE1 + i
          boxes(DE,1,2) = start
          boxes(DE,2,2) = stop
          boxes(DE,3,2) = stop
          boxes(DE,4,2) = start
        enddo
        start = stop
        i1    = i1 + countsPerDE2(j)
      enddo

      grid%boundingBoxes = ESMF_LocalArrayCreate(boxes, ESMF_DATA_REF, status)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetBoundingBoxesSpecd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridValidate - Check internal consistency of a Grid

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

      character(len=ESMF_MAXSTR) :: name, str
      type(ESMF_GridType), pointer :: gp
      integer :: status

      if (present(rc)) rc = ESMF_FAILURE

      if (.not. associated(grid%ptr)) then
        print *, "Empty or Uninitialized Grid"
        return
      endif

      gp => grid%ptr
      if (gp%gridstatus .ne. ESMF_STATE_READY) then
        return
      endif

      call ESMF_GetName(gp%base, name, status)
      if(status .NE. ESMF_SUCCESS) then
        return
      endif

      ! TODO: add calls to physgrid and distgrid validates

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridValidate

!------------------------------------------------------------------------------
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
!     \item[{[opt]}]
!          Print options that control the type of information and level of
!          detail.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      character(len=ESMF_MAXSTR) :: name, str
      type(ESMF_GridType), pointer :: gp
      integer :: i
      integer :: status

      if (present(rc)) rc = ESMF_FAILURE

      print *, "********Begin Grid Print:"
      if (.not. associated(grid%ptr)) then
        print *, "Empty or Uninitialized Grid"
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      gp => grid%ptr
      call ESMF_StatusString(gp%gridstatus, str, rc)
      print *, "Grid status = ", trim(str)

      if (gp%gridstatus .ne. ESMF_STATE_READY) then
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      call ESMF_GetName(gp%base, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetName"
        return
      endif
      print *, "  Name = '",  trim(name), "'"

      ! TODO: add calls to //physgrid-Done\\ and distgrid prints

      ! Print the Associated physgrids

      print *, 'PhysGrids associated with this grid:'
      do i=1, gp%num_physgrids
        call ESMF_PhysGridPrint(gp%physgrids(i), 'no-opt')
      enddo

      ! Print the DistGrid
      print *, 'DistGrid associated with this Grid:'
      call ESMF_DistGridPrint(gp%distgrid, 'noopt')


      print *, "*********End Grid Print"

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridComputeDistance - Compute distance between points
!
! !INTERFACE:
      function ESMF_GridComputeDistance(x1, y1, x2, y2, coord_system, rc)

! !RETURN VALUE:
      real(ESMF_KIND_R8) :: ESMF_GridComputeDistance

! !ARGUMENTS:

      real(ESMF_KIND_R8), intent(in) :: &
         x1,y1,      &! x,y coordinates of two points between which 
         x2,y2        !   the distance is to be computed

      type(ESMF_CoordSystem) :: &
         coord_system ! coordinate system in which the points are given

      integer, optional :: &
         rc           ! return code

! !DESCRIPTION:
!     This routine computes the distance between two points given the
!     coordinates of the two points.
!
!     The arguments are:
!     \begin{description}
!     \item[x1,y1,x2,y2]
!          Coordinates of two points between which to compute distance.
!     \item[coord\_system]
!          Coordinate system in which the points are given
!          (e.g. spherical, Cartesian)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!     local variables
!
      integer :: status
!
!     branch to appropriate PhysGrid routine to compute
!     distance
!
      status = ESMF_SUCCESS

      if(coord_system .eq. ESMF_CoordSystem_Spherical) then
        ESMF_GridComputeDistance = &
          ESMF_PhysGridCompDistSpherical(x1, y1, x2, y2, rc=status)
      elseif(coord_system .eq. ESMF_CoordSystem_Cartesian) then
        ESMF_GridComputeDistance = &
          ESMF_PhysGridCompDistCartesian(x1, y1, x2, y2, rc=status)
      else
        print *,'Distance in coordinate system not yet supported'
        status = ESMF_FAILURE
      endif
!
!     set return code and exit
!
      if (present(rc)) rc = status
      return

      end function ESMF_GridComputeDistance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridBoxIntersectRecv - Determine a DomainList covering a box
!
! !INTERFACE:
      subroutine ESMF_GridBoxIntersectRecv(grid, local_min, local_max, &
                                           domainList, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), dimension(:), intent(in) :: local_min
                                                         ! array of local mins
      real(ESMF_KIND_R8), dimension(:), intent(in) :: local_max
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
!     \item[local\_min]
!          Array of local minimum coordinates, one per rank of the array,
!          defining the "box."
!     \item[local\_max]
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i, j, rank, nDEs, num_domains
      integer :: size, totalPoints
      integer :: counts(ESMF_MAXDIM)
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes
      type(ESMF_AxisIndex), dimension(:,:), pointer :: grid_ai, localAI
      type(ESMF_Domain) :: domain
      type(ESMF_LocalArray) :: array

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! get set of bounding boxes from the grid
      call ESMF_GridGetBoundingBoxes(grid%ptr, array, status)

      ! get rank and counts from the bounding boxes array
      call ESMF_LocalArrayGet(array, counts=counts, rc=status)
      nDEs = counts(1)
      rank = counts(3)

      ! allocate arrays now
      allocate(grid_ai(nDEs,rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, grid_ai(nDE,rank) =", nDEs, rank
         return
      endif
      allocate(localAI(nDEs,rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error"
         return
      endif
      allocate(boxes(nDEs,2**rank,rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, boxes(nDE,2^rank,rank) =", nDEs,2**rank,rank
         return
      endif

      ! get pointer to the actual bounding boxes data
      call ESMF_LocalArrayGetData(array, boxes, rc=status)

      ! get set of axis indices from grid
      call ESMF_GridGetAllAxisIndex(grid, grid_ai, total, rc=status)

      ! translate the AIs from global to local
      call ESMF_GridGlobalToLocalIndex(grid, globalAI2D=grid_ai, &
                                       localAI2D=localAI, rc=status)

      ! loop through bounding boxes, looking for overlap with our "box"
      ! TODO: a better algorithm

      ! go through list of DEs to calculate the number of domains
      ! TODO: use David's DomainList routines, but they are untested
      num_domains = 0
      do i = 1,nDEs
        if ((local_min(1).gt.max(boxes(i,2,1),boxes(i,3,1))) .or. &
            (local_max(1).lt.min(boxes(i,1,1),boxes(i,4,1))) .or. &
            (local_min(2).gt.max(boxes(i,3,2),boxes(i,4,2))) .or. &
            (local_max(2).lt.min(boxes(i,1,2),boxes(i,2,2)))) cycle
        num_domains = num_domains + 1
      enddo

      domainList = ESMF_DomainListCreate(num_domains)

      ! now fill in the domain list
      !  TODO: only one loop instead of two, one that figures the number
      !        of domains and one that fills it in
      ! TODO: move some of this code to Base and add a DomainList method?
      num_domains = 0
      totalPoints  = 0
      do j = 1,nDEs
        if ((local_min(1).gt.max(boxes(j,2,1),boxes(j,3,1))) .or. &
            (local_max(1).lt.min(boxes(j,1,1),boxes(j,4,1))) .or. &
            (local_min(2).gt.max(boxes(j,3,2),boxes(j,4,2))) .or. &
            (local_max(2).lt.min(boxes(j,1,2),boxes(j,2,2)))) cycle
        num_domains = num_domains + 1
        domainList%domains(num_domains)%DE   = j - 1  ! DEs start with 0
        domainList%domains(num_domains)%rank = rank
        size = 1
        do i = 1,rank
          domainList%domains(num_domains)%ai(i) = localAI(j,i)
          size = size * (localAI(j,i)%max - localAI(j,i)%min + 1)
        enddo
        totalPoints = totalPoints + size
      enddo
      domainList%total_points = totalPoints

      ! TODO:  the code below is taken from Phil's regrid routines and needs
      !        to be incorporated at some point
      !
      ! if spherical coordinates, set up constants for longitude branch cut
      !

      !if (dst_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
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
      !if (dst_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) - lon_cycle
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) + lon_cycle
      !endif
      !
      ! make sure src bbox is in same longitude range as dst bbox
      ! assume degrees and x is longitude
      !
      !   if (src_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) - lon_cycle
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) + lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) - lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) + lon_cycle
      !   endif ! Spherical coords

      deallocate(grid_ai)
      deallocate(localAI)
      deallocate(boxes)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridBoxIntersectRecv

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridBoxIntersectSend - Determine a DomainList covering a box
!
! !INTERFACE:
      subroutine ESMF_GridBoxIntersectSend(dstGrid, srcGrid, local_min, &
                                           local_max, myAI, domainList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: dstGrid
      type(ESMF_Grid) :: srcGrid
      real(ESMF_KIND_R8), dimension(:), intent(in) :: local_min
                                                         ! array of local mins
      real(ESMF_KIND_R8), dimension(:), intent(in) :: local_max
                                                         ! array of local maxs
      type(ESMF_AxisIndex), dimension(:), intent(in) :: myAI
      type(ESMF_DomainList), intent(inout) :: domainlist ! domain list
      integer, intent(out), optional :: rc               ! return code

! !DESCRIPTION:
!     This routine computes the DomainList necessary to cover a given "box"
!     described by an array of min/max's.  This routine is for the case of
!     a DE that is part of a source Grid determining which DEs it will send
!     its data to.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Destination {\tt ESMF\_Grid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[local\_min]
!          Array of local minimum coordinates, one per rank of the array,
!          defining the "box."
!     \item[local\_max]
!          Array of local maximum coordinates, one per rank of the array,
!          defining the "box."
!     \item[myAI]
!          {\tt ESMF\_AxisIndex} for this DE on the sending (source)
!          {\tt ESMF\_Grid}, assumed to be in global indexing.
!     \item[domainlist]
!          Resulting {\tt ESMF\_DomainList} containing the set of 
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i, j, rank, nDEs, num_domains
      integer :: size, totalPoints
      integer :: counts(ESMF_MAXDIM)
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes
      type(ESMF_AxisIndex), dimension(:), pointer :: myLocalAI
      type(ESMF_Domain) :: domain
      type(ESMF_LocalArray) :: array

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! get set of bounding boxes from the grid
      call ESMF_GridGetBoundingBoxes(dstGrid%ptr, array, status)

      ! get rank and counts from the bounding boxes array
      call ESMF_LocalArrayGet(array, counts=counts, rc=status)
      nDEs = counts(1)
      rank = counts(3)

      ! allocate arrays now
      allocate(boxes(nDEs,2**rank,rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error"
         return
      endif
      allocate(myLocalAI(rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error"
         return
      endif

      ! translate myAI to local index
      call ESMF_GridGlobalToLocalIndex(srcGrid, globalAI1D=myAI, &
                                       localAI1D=myLocalAI, rc=status)

      ! get pointer to the actual bounding boxes data
      call ESMF_LocalArrayGetData(array, boxes, rc=status)

      ! loop through bounding boxes, looking for overlap with our "box"
      ! TODO: a better algorithm

      ! go through list of DEs to calculate the number of domains
      ! TODO: use David's DomainList routines, but they are untested
      num_domains = 0
      do i = 1,nDEs
        if ((local_min(1).gt.max(boxes(i,2,1),boxes(i,3,1))) .or. &
            (local_max(1).lt.min(boxes(i,1,1),boxes(i,4,1))) .or. &
            (local_min(2).gt.max(boxes(i,3,2),boxes(i,4,2))) .or. &
            (local_max(2).lt.min(boxes(i,1,2),boxes(i,2,2)))) cycle
        num_domains = num_domains + 1
      enddo

      domainList = ESMF_DomainListCreate(num_domains)

      ! now fill in the domain list  TODO: only one loop instead of two, one that
      ! figures the number of domains and one that fills it in
      ! TODO: move some of this code to Base and add a DomainList method
      num_domains = 0
      totalPoints  = 0
      do j = 1,nDEs
        if ((local_min(1).gt.max(boxes(j,2,1),boxes(j,3,1))) .or. &
            (local_max(1).lt.min(boxes(j,1,1),boxes(j,4,1))) .or. &
            (local_min(2).gt.max(boxes(j,3,2),boxes(j,4,2))) .or. &
            (local_max(2).lt.min(boxes(j,1,2),boxes(j,2,2)))) cycle
        num_domains = num_domains + 1
        domainList%domains(num_domains)%DE   = j - 1  ! DEs start with 0
        domainList%domains(num_domains)%rank = rank
        size = 1
        do i = 1,rank
          domainList%domains(num_domains)%ai(i) = myLocalAI(i)
          size = size * (myLocalAI(i)%max - myLocalAI(i)%min + 1)
        enddo
        totalPoints = totalPoints + size
      enddo
      domainList%total_points = totalPoints

      ! TODO:  the code below is taken from Phil's regrid routines and needs
      !        to be incorporated at some point
      !
      ! if spherical coordinates, set up constants for longitude branch cut
      !

      !if (dst_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
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
      !if (dst_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) - lon_cycle
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) + lon_cycle
      !endif
      !
      ! make sure src bbox is in same longitude range as dst bbox
      ! assume degrees and x is longitude
      !
      !   if (src_phys_grid%coord_system == ESMF_CoordSystem_Spherical) then
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) - lon_cycle
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) + lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) - lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) + lon_cycle
      !   endif ! Spherical coords

      deallocate(boxes)
      deallocate(myLocalAI)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridBoxIntersectSend

!------------------------------------------------------------------------------
!!BOP
!! !IROUTINE: ESMF_GridSearchPoint - Search the grid for a cell containing point
!
! !INTERFACE:
!      subroutine ESMF_GridSearchPoint(dst_add, x, y, DEid, search_grid, &
!                                      phys_grid_id, rc)
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) ::
!         dst_add      ! location in grid of grid cell containing search point
!
!      real (kind=?), intent(in) :: &
!         x,y          ! x,y coordinates of search point 
!
!      integer, intent(in) :: &
!         DEid         ! DE which owns the search point
!
!      type(ESMF_Grid), intent(in) :: &
!         search_grid  ! grid to search for location of point
!
!      integer, intent(in), optional :: &
!         phys_grid_id ! id of the subgrid to search (if more than one subgrid)
!
!      integer, intent(out), optional :: rc  ! return code
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the grid of a grid cell 
!!     containing the point given by the input x,y coordinates.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dst\_add]
!!          Address of grid cell containing the search point.
!!     \item[x,y]
!!          Coordinates of search point.
!!     \item[DEid]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[search\_grid]
!!          ESMF {\tt ESMF\_Grid} to search for location.
!!     \item[{[phys\_grid\_id]}]
!!          If more than one {\tt ESMF\_PhysGrid} is contained in {\tt ESMF\_Grid}, choose which
!!          grid to search (default is 1st {\tt ESMF\_PhysGrid}?).
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOP
!! !REQUIREMENTS:  SSSn.n, GGGn.n
!
!!
!!     extract appropriate PhysGrid and DistGrid to search
!!     extract various other grid properties
!!
!      if (.not. present(phys_grid_id)) phys_grid_id = 1 (or whatever default)
!      ! combine these queries?  format of query functions?
!      call ESMF_GridGet(search_grid, phys_grid=phys_grid_id) ??? 
!      call ESMF_GridGet(search_grid, dist_grid = ??)
!      call ESMF_GridGet(search_grid, horis_gridtype = search_grid_type)
!!
!!     Call appropriate search routine based on coordinate system and
!!     grid type.
!!
! 
!      select case (srch_grid_type)
!      case(ESMF_GridType_LatLon, ESMF_GridType_Mercator, ESMF_GridType_LatLonGauss, &
!           ESMF_GridType_Reduced)
!         !*** simple search adequate for these cases
!         call ESMF_PhysGridSearchBboxSpherical(dst_add, x, y, DEid, phys_grid, &
!                                               dist_grid, status)
!
!      case(ESMF_GridType_Dipole, ESMF_GridType_Tripole, ESMF_GridType_Geodesic, &
!           ESMF_GridType_CubedSphere)
!         !*** must use more general algorithm for these cases
!         call ESMF_PhysGridSearchGeneralSpherical(dst_add, x, y, DEid, phys_grid, &
!                                                  dist_grid, status)
!
!      case(ESMF_GridType_XY)
!         call ESMF_PhysGridSearchBboxCartesian(dst_add, x, y, DEid, phys_grid, &
!                                               dist_grid, status)
!
!      case default
!         print *,'GridSearchPoint: search of this grid type not supported'
!         status = ESMF_Failure
!      end select
!
!      end subroutine ESMF_GridSearchPoint
!
!------------------------------------------------------------------------------

      end module ESMF_GridMod
