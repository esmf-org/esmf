! $Id: ESMF_Grid.F90,v 1.55 2003/06/06 14:36:44 nscollins Exp $
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
#include "ESMF_Macros.inc"
!==============================================================================
!BOPI
! !MODULE: ESMF_GridMod - Grid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt Grid} class.  This class
! provides a unified interface for both {\tt PhysGrid} and {\tt DistGrid}
! information for model grids.  Functions for defining and computing {\tt Grid}
! information are available through this class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_ArrayMod       ! ESMF array class
      use ESMF_IOMod          ! ESMF I/O class
      use ESMF_DELayoutMod      ! ESMF layout class
      use ESMF_DistGridMod    ! ESMF distributed grid class
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
        integer :: horz_coord_system        ! enum for horizontal physical
                                            ! coordinate system
        integer :: vert_coord_system        ! enum for vertical physical
                                            ! coordinate system
        integer :: coord_order              ! enum for mapping of xyz 
                                            ! to ijk
        integer :: coord_index              ! enum for global or local indexing
        integer :: num_physgrids            ! number of grid descriptors
                                            ! necessary to support
                                            ! staggering, vertical
                                            ! grids, background grids
        logical, dimension(3) :: periodic   ! logical identifier to indicate
                                            ! periodic boundary conditions in
                                            ! each direction
        type (ESMF_PhysGrid), dimension(:), pointer :: &
           physgrids         ! grid info for all grid descriptions necessary
                             ! to define horizontal, staggered and vertical grids
        type (ESMF_DistGrid) :: distgrid    ! decomposition and other
                                            ! logical space info for grid
!       type (???) :: search_structure

      end type

!------------------------------------------------------------------------------
!     !  ESMF_Grid
!
!     ! The Grid data structure that is passed between languages.

      type ESMF_Grid
      sequence
      private
        type (ESMF_GridType), pointer :: ptr     ! pointer to a grid type
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
    !public ESMF_GridGetCoord
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
    public ESMF_GridHalo
    public ESMF_GridAllGather
    public ESMF_GridGather
    public ESMF_GridScatter
    public ESMF_GridValidate
    public ESMF_GridPrint
    public ESMF_GridComputeDistance
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

   integer, parameter, public ::            &! recognized coordinate systems
      ESMF_CoordSystem_Unknown        =  0, &! unknown or undefined coord system
      ESMF_CoordSystem_Spherical      =  1, &! spherical coordinates (lat/lon)
      ESMF_CoordSystem_Cartesian      =  2, &! Cartesian coordinates (x,y)
      ESMF_CoordSystem_Cylindrical    =  3, &! cylindrical coordinates
      ESMF_CoordSystem_LatFourier     =  4, &! mixed latitude/spectral space
      ESMF_CoordSystem_Spectral       =  5, &! wavenumber space
      ESMF_CoordSystem_Depth          =  6, &! vertical z coord. depth (0 at surface)
      ESMF_CoordSystem_Height         =  7, &! vertical z coord. height (0 at bottom)
      ESMF_CoordSystem_Pressure       =  8, &! vertical pressure coordinate
      ESMF_CoordSystem_Sigma          =  9, &! vertical sigma coordinate
      ESMF_CoordSystem_Theta          = 10, &! vertical theta coordinate
      ESMF_CoordSystem_Eta            = 11, &! vertical eta coordinate
      ESMF_CoordSystem_Isopycnal      = 12, &! vertical density coordinate
      ESMF_CoordSystem_Hybrid         = 13, &! hybrid vertical coordinates
      ESMF_CoordSystem_Lagrangian     = 14   ! Lagrangian coordinates
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
      '$Id: ESMF_Grid.F90,v 1.55 2003/06/06 14:36:44 nscollins Exp $'

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
         module procedure ESMF_GridCreateInternal
         module procedure ESMF_GridCreateRead
         module procedure ESMF_GridCreateCopy
         module procedure ESMF_GridCreateCutout
         module procedure ESMF_GridCreateChangeResolution
         module procedure ESMF_GridCreateExchange

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt Grid} create
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
         module procedure ESMF_GridConstructInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct a
!     complete {\tt Grid}.
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
         module procedure ESMF_GridSetCoordCompute
         module procedure ESMF_GridSetCoordCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     coordinates as part of a {\tt Grid}.
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
!     logical masks as part of a {\tt Grid}.
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
!     multiplicative masks as part of a {\tt Grid}.
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
!     region id's as part of a {\tt Grid}.
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
!!     search a grid for point(s).
!!
!!EOP
!      end interface
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
!     Allocates memory for a new {\tt Grid} object and constructs its
!     internals, but does not fill in any contents.  Return a pointer to
!     the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          {\tt Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateEmpty%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
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
! !IROUTINE: ESMF_GridCreateInternal - Create a new Grid internally

! !INTERFACE:
      function ESMF_GridCreateInternal(i_max, j_max, x_min, x_max, &
                                       y_min, y_max, layout, &
                                       horz_gridtype, vert_gridtype, &
                                       horz_stagger, vert_stagger, &
                                       horz_coord_system, vert_coord_system, &
                                       halo_width, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateInternal
!
! !ARGUMENTS:
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      real, intent(in) :: x_min
      real, intent(in) :: x_max
      real, intent(in) :: y_min
      real, intent(in) :: y_max
      type (ESMF_DELayout), intent(in) :: layout
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      integer, intent(in), optional :: horz_coord_system
      integer, intent(in), optional :: vert_coord_system
      integer, intent(in), optional :: halo_width
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and internally generates the Grid.  Return a pointer to
!     the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[i\_max]}]
!          Number of grid increments in the i-direction.
!     \item[{[j\_max]}]
!          Number of grid increments in the j-direction.
!     \item[{[x\_min]}]
!          Minimum physical coordinate in the x-direction.
!     \item[{[x\_max]}]
!          Maximum physical coordinate in the x-direction.
!     \item[{[y\_min]}]
!          Minimum physical coordinate in the y-direction.
!     \item[{[y\_max]}]
!          Maximum physical coordinate in the y-direction.
!     \item[{[layout]}]
!          DELayout of DE's.
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
!          {\tt Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid    ! Pointer to new grid
      integer :: status=ESMF_FAILURE          ! Error status
      logical :: rcpresent=.FALSE.            ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateInternal%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateInternal: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstruct(grid, i_max, j_max, x_min, x_max, y_min, y_max, &
                              layout, &
                              horz_gridtype, vert_gridtype, &
                              horz_stagger, vert_stagger, &
                              horz_coord_system, vert_coord_system, &
                              halo_width, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateInternal: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateInternal%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateInternal

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
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and reads a {\tt Grid} in from a file.  Return a pointer to
!     the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[iospec]}]
!          File I/O specification.
!     \item[{[name]}]
!          {\tt Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateRead%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
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
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and copies attributes from another {\tt Grid}.  Return a
!     pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[grid\_in]}]
!          {\tt Grid} to be copied.
!     \item[{[name]}]
!          {\tt Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateCopy%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
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
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and copies a region from an existing {\tt Grid}.
!     Return a pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[grid\_in]}]
!          {\tt Grid} to be partially copied.
!     \item[{[i\_min]}]
!          Minimum global i-index for the region of the grid to be cutout.
!     \item[{[i\_max]}]
!          Maximum global i-index for the region of the grid to be cutout.
!     \item[{[j\_min]}]
!          Minimum global j-index for the region of the grid to be cutout.
!     \item[{[j\_max]}]
!          Maximum global j-index for the region of the grid to be cutout.
!     \item[{[name]}]
!          {\tt Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateCutout%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
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
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and creates a {\tt Grid} by either coarsening or refining an
!     existing {\tt Grid}.  Return a pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[grid\_in]}]
!          Source {\tt Grid} to be coarsened or refined.
!     \item[{[i\_resolution]}]
!          Integer resolution factor in the i-direction.
!     \item[{[j\_resolution]}]
!          Integer resolution factor in the j-direction.
!          Note:  The above arguments assume refinement by factor if positive
!          and coarsening by absolute value of the factor if negative.  For
!          example, i\_resolution=4 indicates the new {\tt Grid} will be four
!          times as resolved in the i-direction as the source {\tt Grid},
!          whereas j\_resolution=-3 means the new {\tt Grid} will sample every
!          third point in the j-direction.
!     \item[{[name]}]
!          {\tt Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateChangeResolution%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
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
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and creates a new {\tt Grid} from the intersection of two
!     existing {\tt Grids}.  Return a pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[grid\_in1]}]
!          First source {\tt Grid}.
!     \item[{[grid\_in2]}]
!          Second source {\tt Grid}.
!     \item[{[name]}]
!          New {\tt Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateExchange%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
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
!     Destroys a {\tt Grid} object previously allocated
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
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridDestroy: Grid deallocate"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDestroy

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridConstructInternal - Construct the internals of an allocated Grid

! !INTERFACE:
      subroutine ESMF_GridConstructInternal(grid, i_max, j_max, &
                                            x_min, x_max, y_min, y_max, &
                                            layout, &
                                            horz_gridtype, vert_gridtype, &
                                            horz_stagger, vert_stagger, &
                                            horz_coord_system, vert_coord_system, &
                                            halo_width, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      real, intent(in) :: x_min
      real, intent(in) :: x_max
      real, intent(in) :: y_min
      real, intent(in) :: y_max
      type (ESMF_DELayout), intent(in) :: layout
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      integer, intent(in), optional :: horz_coord_system
      integer, intent(in), optional :: vert_coord_system
      integer, intent(in), optional :: halo_width
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt Grid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF\_GridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_GridCreate}, which calls
!     {\tt ESMF\_GridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid}
!     \item[{[i\_max]}]
!          Number of grid increments in the i-direction.
!     \item[{[j\_max]}]
!          Number of grid increments in the j-direction.
!     \item[{[x\_min]}]
!          Minimum physical coordinate in the x-direction.
!     \item[{[x\_max]}]
!          Maximum physical coordinate in the x-direction.
!     \item[{[y\_min]}]
!          Minimum physical coordinate in the y-direction.
!     \item[{[y\_max]}]
!          Maximum physical coordinate in the y-direction.
!     \item[{[layout]}]
!         DELayout of DE's.
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

      character(len=4) :: physgrid_name       !
      integer :: physgrid_id                  ! integer identifier for physgrid
      integer :: status=ESMF_SUCCESS          ! Error status
      logical :: rcpresent=.FALSE.            ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternal: Grid construct"
        return
      endif

!     Fill in grid derived type with subroutine arguments
      call ESMF_GridSet(grid, horz_gridtype, vert_gridtype, &
                        horz_stagger, vert_stagger, &
                        horz_coord_system, vert_coord_system, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternal: Grid set"
        return
      endif

!     Create the DistGrid
      grid%distgrid = ESMF_DistGridCreate(i_max=i_max, j_max=j_max, &
                                          layout=layout, halo_width=halo_width, &
                                          rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternal: Distgrid create"
        return
      endif

!     Create main physgrid
      physgrid_name = 'base'
      call ESMF_GridAddPhysGrid(grid, i_max, j_max, physgrid_id, &
                                x_min, x_max, y_min, y_max, &
                                physgrid_name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternal: Add physgrid"
        return
      endif

!     Call SetCoord to create physical coordinates of subgrid
      call ESMF_GridSetCoord(grid, physgrid_id, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternal: Grid set coord compute"
        return
      endif

!     Create any necessary physgrids to support staggering  TODO

!     Create vertical physgrid if requested

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternal: Grid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridConstructInternal

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
!     allocated {\tt Grid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF\_GridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_GridCreate}, which calls
!     {\tt ESMF\_GridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid}
!     \item[{[name]}]
!          {\tt Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status=ESMF_SUCCESS               ! Error status
      logical :: rcpresent=.FALSE.                 ! Return code present
      character (len = ESMF_MAXSTR) :: defaultname ! default grid name

!     Initialize return code
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
      grid%num_physgrids = 0
      grid%periodic(1) = .FALSE.
      grid%periodic(2) = .FALSE.
      grid%periodic(3) = .FALSE.
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
!     original Grid object is freed.  Intended for internal ESMF
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
! !IROUTINE: ESMF_GridAddPhysGrid - Add a PhysGrid to a Grid

! !INTERFACE:
      subroutine ESMF_GridAddPhysGrid(grid, i_max, j_max, physgrid_id, &
                                      x_min, x_max, y_min, y_max, &
                                      physgrid_name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      integer, intent(out) :: physgrid_id
      real, intent(in), optional :: x_min
      real, intent(in), optional :: x_max
      real, intent(in), optional :: y_min
      real, intent(in), optional :: y_max
      character (len=*), intent(in), optional :: physgrid_name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt PhysGrid} to a {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[i\_max]}]
!          Number of grid increments in the i-direction.
!     \item[{[j\_max]}]
!          Number of grid increments in the j-direction.
!     \item [{[physgrid\_id]}]
!          Integer identifier for {\tt PhysGrid}.
!     \item[{[x\_min]}]
!          Minimum physical coordinate in the x-direction.
!     \item[{[x\_max]}]
!          Maximum physical coordinate in the x-direction.
!     \item[{[y\_min]}]
!          Minimum physical coordinate in the y-direction.
!     \item[{[y\_max]}]
!          Maximum physical coordinate in the y-direction.
!     \item [{[physgrid\_name]}]
!          {\tt PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_SUCCESS          ! Error status
      logical :: rcpresent=.FALSE.            ! Return code present
      real :: delta(2)
      real :: local_min_coord(2)
      real :: local_max_coord(2)
      integer :: local_nmax(2)
      real :: global_min_coord(2)
      real :: global_max_coord(2)
      integer :: global_nmax(2)
      integer :: i
      type(ESMF_PhysGrid), dimension(:), allocatable, target :: temp_pgrids
                                             ! temporary array of physgrids

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Increment the number of physgrids in the grid and allocate memory
      grid%num_physgrids = grid%num_physgrids + 1
      if(grid%num_physgrids .eq. 1) then
!       allocate(grid%physgrids(1), stat=status)
!       if(status .NE. ESMF_SUCCESS) then
!         print *, "ERROR in ESMF_GridAddPhysGrid: physgrids allocate"
!         return
!       endif
        allocate(temp_pgrids(grid%num_physgrids), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridAddPhysGrid: temp_pgrids allocate"
          return
        endif
      else
        allocate(temp_pgrids(grid%num_physgrids), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridAddPhysGrid: temp_pgrids allocate"
          return
        endif
        do i = 1, grid%num_physgrids - 1
          temp_pgrids(i) = grid%physgrids(i)
        enddo
        deallocate(grid%physgrids, stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridAddPhysGrid: physgrids deallocate"
          return
        endif
      endif
      physgrid_id = grid%num_physgrids 

      delta(1) = (x_max - x_min) / real(i_max)
      delta(2) = (y_max - y_min) / real(j_max)
      do i = 1,2
        local_min_coord(i) = delta(i) * &
                             real(grid%distgrid%ptr%MyDE%lcelltot_index(i)%l - 1)
        local_max_coord(i) = delta(i) * &
                             real(grid%distgrid%ptr%MyDE%lcelltot_index(i)%r)
        local_nmax(i) = grid%distgrid%ptr%MyDE%lcelltot_index(i)%r &
                      - grid%distgrid%ptr%MyDE%lcelltot_index(i)%l + 1
      enddo
      global_min_coord(1)=x_min
      global_max_coord(1)=x_max
      global_nmax(1)=i_max
      global_min_coord(2)=y_min
      global_max_coord(2)=y_max
      global_nmax(2)=j_max
      temp_pgrids(physgrid_id)=ESMF_PhysGridCreate(dim_num=2, &
                                      local_min=local_min_coord, &
                                      local_max=local_max_coord, &
                                      local_nmax=local_nmax, &
                                      global_min=global_min_coord, &
                                      global_max=global_max_coord, &
                                      global_nmax=global_nmax, &
                                      name=physgrid_name, rc=status)
      grid%physgrids => temp_pgrids

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddPhysGrid

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
!     Returns the set of resources the Grid object was configured with.
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
!     Configures the Grid object with set of resources given.
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
!     Returns the value of Grid attribute <Value>.
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
! !IROUTINE: ESMF_GridSetCoordFromArray - Set the coordinates of a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetCoordFromArray(Grid, array, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Array), intent(in) :: array
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the coordinates exist already and are being
!     passed in through an {\tt Array}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[array]
!          ESMF Array of data.
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
      subroutine ESMF_GridGetDE(grid, MyDE, &
                                lcelltot_count, lcellexc_count, &
                                gcelltot_start, gcellexc_start, &
                                lcelltot_index, lcellexc_index, &
                                rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(inout), optional :: MyDE
      integer, intent(inout), optional :: lcelltot_count
      integer, intent(inout), optional :: lcellexc_count
      integer, intent(inout), optional :: gcelltot_start
      integer, intent(inout), optional :: gcellexc_start
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM), intent(inout), &
                        optional :: lcelltot_index
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM), intent(inout), &
                        optional :: lcellexc_index
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a DistGrid attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[MyDE]}]
!          Identifier for this DE.
!     \item[{[lcelltot\_count]}]
!          Local (on this DE) number of total cells.
!     \item[{[lcellexc\_count]}]
!          Local (on this DE) number of exclusive cells.
!     \item[{[gcelltot\_start]}]
!          Global index of starting count for total cells.
!     \item[{[gcellexc\_start]}]
!          Global index of starting count for exclusive cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGetDE(grid%ptr%distgrid%ptr, MyDE, &
                              lcelltot_count, lcellexc_count, &
                              gcelltot_start,  gcellexc_start, &
                              lcelltot_index, lcellexc_index, &
                              status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetDE: distgrid get de"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetAllAxisIndex - Get array of AxisIndices from a Grid

! !INTERFACE:
      subroutine ESMF_GridGetAllAxisIndex(grid, AI, AI_tot, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI
      type(ESMF_AxisIndex), dimension(:,:), optional, pointer :: AI_tot
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a DistGrid attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[AI]}]
!          Array of {\tt AxisIndices} corresponding to the {\tt Grid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGetAllAxisIndex(grid%ptr%distgrid%ptr, AI, AI_tot, &
                                        status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetAllAxisIndex: distgrid get"
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
!     Get a DistGrid attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[layout]}]
!          Pointer to the {\tt DELayout} corresponding to the {\tt Grid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
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
! !IROUTINE: ESMF_GridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_GridGlobalToLocalIndex(grid, global1D, local1D, &
                                             global2D, local2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer(ESMF_IKIND_I4), dimension(:), optional, intent(in) :: global1D
      integer(ESMF_IKIND_I4), dimension(:), optional, intent(out) :: local1D
      integer(ESMF_IKIND_I4), dimension(:,:), optional, intent(in) :: global2D
      integer(ESMF_IKIND_I4), dimension(:,:), optional, intent(out) :: local2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a DistGrid routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be used.
!     \item[{[global1D]}]
!          One-dimensional Array of global identifiers to be translated.
!          Infers translating between positions in memory.
!     \item[{[local1D]}]
!          One-dimensional Array of local identifiers corresponding to
!          global identifiers.
!     \item[{[global2D]}]
!          Two-dimensional Array of global identifiers to be translated.
!          Infers translating between indices in ij space.
!     \item[{[local2D]}]
!          Two-dimensional Array of local identifiers corresponding to
!          global identifiers.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGlobalToLocalIndex(grid%ptr%distgrid%ptr, &
                                           global1D, local1D, &
                                           global2D, local2D, status)
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
                                             local2D, global2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer(ESMF_IKIND_I4), dimension(:), optional, intent(in) :: local1D
      integer(ESMF_IKIND_I4), dimension(:), optional, intent(out) :: global1D
      integer(ESMF_IKIND_I4), dimension(:,:), optional, intent(in) :: local2D
      integer(ESMF_IKIND_I4), dimension(:,:), optional, intent(out) :: global2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a DistGrid routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be used.
!     \item[{[local1D]}]
!          One-dimensional Array of local identifiers to be translated.
!          Infers translating between positions in memory.
!     \item[{[global1D]}]
!          One-dimensional Array of global identifiers corresponding to
!          local identifiers.
!     \item[{[local2D]}]
!          Two-dimensional Array of local identifiers to be translated.
!          Infers translating between indices in ij space.
!     \item[{[global2D]}]
!          Two-dimensional Array of global identifiers corresponding to
!          local identifiers.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridLocalToGlobalIndex(grid%ptr%distgrid%ptr, &
                                           local1D, global1D, &
                                           local2D, global2D, status)
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
      real, dimension (:), pointer :: buffer
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
!          Pointer to a {\tt Grid} to be modified.
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
! !IROUTINE: ESMF_GridSetCoordCompute - Compute coordinates for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetCoordCompute(grid, physgrid_id, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: physgrid_id
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes coordinates for a Grid via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[{[physgrid\_id]}]
!          Identifier of the {\tt PhysGrid} to be modified.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!TODO: figure out the argument list necessary to completely describe the
!      internal calculation of the coordinates of a simple grid.
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_SUCCESS              ! Error status
      integer :: ncoord_locs
      integer, dimension(6) :: coord_loc
      integer :: DE_id
      integer, dimension(ESMF_MAXGRIDDIM) :: gcell_dim
      integer, dimension(ESMF_MAXGRIDDIM) :: lcellexc_start
      integer, dimension(ESMF_MAXGRIDDIM) :: lcellexc_end
      real :: delta1
      real :: delta2
      real, dimension(ESMF_MAXGRIDDIM) :: global_min_coords
      real, dimension(ESMF_MAXGRIDDIM) :: global_max_coords
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize local data
      coord_loc = 0
      ncoord_locs = 0
      delta1 = 0.0
      delta2 = 0.0
     
!     Get distgrid information, including global size in each direction and local
!     grid size indexed globally
      call ESMF_DistGridGet(grid%distgrid%ptr, &
                            gcell_dim=gcell_dim, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridSetCoordCompute: Distgrid get"
        return
      endif
      DE_id = grid%distgrid%ptr%MyDE%MyDE
      call ESMF_DistGridGetCounts(grid%distgrid%ptr, DE_id, &
                                  lcellexc_start=lcellexc_start, &
                                  lcellexc_end=lcellexc_end, &
                                  rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridSetCoordCompute: Distgrid get counts"
        return
      endif

!     Get physgrid info with global coordinate extents
      call ESMF_PhysGridGet(grid%physgrids(physgrid_id)%ptr, &
                            global_min=global_min_coords, &
                            global_max=global_max_coords, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridSetCoordCompute: PhysGrid get"
        return
      endif

!     Calculate global cell sizes
      if (gcell_dim(1).ne.0) then
        delta1 = (global_max_coords(1)-global_min_coords(1)) / real(gcell_dim(1))
      else
        print *, "ERROR in ESMF_GridSetCoordCompute: gcell_dim1=0"
        return
      endif
      if (gcell_dim(2).ne.0) then
        delta2 = (global_max_coords(2)-global_min_coords(2)) / real(gcell_dim(2))
      else
        print *, "ERROR in ESMF_GridSetCoordCompute: gcell_dim2=0"
        return
      endif

!     For now, set coord_loc by hand.  TODO:  automate for the different grid types
      coord_loc(1) = 1
      coord_loc(2) = 2
      ncoord_locs = 2
      call ESMF_PhysGridSetCoord(grid%physgrids(physgrid_id)%ptr, &
                                 ncoord_locs, coord_loc, &
                                 lcellexc_start(1), lcellexc_end(1), &
                                 lcellexc_start(2), lcellexc_end(2), &
                                 delta1, delta2, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridSetCoordCompute: PhysGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetCoordCompute

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
!     This version of set copies the coordinates of a Grid from another Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
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
                              global_max_coords, global_nmax, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out), optional :: horz_gridtype
      integer, intent(out), optional :: vert_gridtype
      integer, intent(out), optional :: horz_stagger
      integer, intent(out), optional :: vert_stagger
      integer, intent(out), optional :: horz_coord_system
      integer, intent(out), optional :: vert_coord_system
      integer, intent(out), optional :: coord_order
      real, intent(out), dimension(ESMF_MAXGRIDDIM), optional :: global_min_coords
      real, intent(out), dimension(ESMF_MAXGRIDDIM), optional :: global_max_coords
      integer, intent(out), dimension(ESMF_MAXGRIDDIM), optional :: global_nmax
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version sets a variety of information about a grid, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: physgrid_id
      type(ESMF_GridType), pointer :: gridp

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
 
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_GridGet called with invalid grid object"
        return
      endif

      gridp => grid%ptr

!     if present, gets information from the grid derived type
      if(present(horz_gridtype)) horz_gridtype = gridp%horz_gridtype
      if(present(vert_gridtype)) vert_gridtype = gridp%vert_gridtype
      if(present(horz_stagger)) horz_stagger = gridp%horz_stagger
      if(present(vert_stagger)) vert_stagger = gridp%vert_stagger
      if(present(horz_coord_system)) horz_coord_system = gridp%horz_coord_system
      if(present(vert_coord_system)) vert_coord_system = gridp%vert_coord_system
      if(present(coord_order)) coord_order = gridp%coord_order

!     Get physgrid info with global coordinate extents
      if(present(global_min_coords) .or. present(global_max_coords)) then
        physgrid_id = gridp%num_physgrids  ! TODO: fix so passed in?
        call ESMF_PhysGridGet(gridp%physgrids(physgrid_id)%ptr, &
                              global_min=global_min_coords, &
                              global_max=global_max_coords, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGet: PhysGrid get"
          return
        endif
      endif

!     Get distgrid info with global coordinate counts
      if(present(global_nmax)) then
        call ESMF_DistGridGet(gridp%distgrid%ptr, gcell_dim=global_nmax, &
                              rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGet: DistGrid get"
          return
        endif
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
                              coord_order, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      integer, intent(in), optional :: horz_coord_system
      integer, intent(in), optional :: vert_coord_system
      integer, intent(in), optional :: coord_order
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version sets a variety of information about a grid, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
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
      type(ESMF_Array), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the logical mask data exists already and is
!     being passed in through an {\tt Array}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[array]
!          ESMF Array of data.
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
!          Pointer to a {\tt Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
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
!          Pointer to a {\tt Grid} to be modified.
!     \item[{[mmask]}]
!          Multiplicative mask identifier.
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
!     This version of set copies a logical mask for a Grid from another Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
!     \item [{[name]}]
!           {\tt LMask} name to be set.
!     \item [{[name\_in]}]
!           {\tt LMask} name to be copied.
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
      type(ESMF_Array), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the multiplicative mask data exists already
!     and is being passed in through an {\tt Array}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[array]
!          ESMF Array of data.
!     \item [{[name]}]
!           {\tt MMask} name.
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
!          Pointer to a {\tt Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt MMask} name.
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
!          Pointer to a {\tt Grid} to be modified.
!     \item[lmask]
!          Logical mask identifier.
!     \item [{[name]}]
!           {\tt MMask} name.
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
!     This version of set copies a multiplicative mask for a Grid from another
!     Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
!     \item [{[name]}]
!           {\tt MMask} name to be set.
!     \item [{[name\_in]}]
!           {\tt MMask} name to be copied.
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
      type(ESMF_Array), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the metric data exists already and is being
!     passed in through an {\tt Array}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[array]
!          ESMF Array of data.
!     \item [{[name]}]
!           {\tt Metric} name.
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
!          Pointer to a {\tt Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt Metric} name.
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
!     This version of set internally computes a metric for a Grid via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[{[id]}]
!          Identifier for predescribed metrics.  TODO: make list
!     \item [{[name]}]
!           {\tt Metric} name.
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
!     This version of set copies a metric for a Grid from another Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item [{[name]}]
!           {\tt Metric} name to be set.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
!     \item [{[name\_in]}]
!           {\tt Metric} name to be copied.
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
      type(ESMF_Array), intent(in) :: array
      character (len=*), intent(in) :: name  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the region identifier data exists already
!     and is being passed in through an {\tt Array}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item[array]
!          ESMF Array of data.
!     \item [{[name]}]
!           {\tt RegionID} name.
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
!          Pointer to a {\tt Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt RegionID} name.
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
!     This version of set copies a region identifier for a Grid from another
!     Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item [{[name]}]
!           {\tt RegionID} name to be set.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
!     \item [{[name\_in]}]
!           {\tt RegionID} name to be copied.
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
! !IROUTINE: ESMF_GridAllGather - Data AllGather operation on a Grid

! !INTERFACE:
      subroutine ESMF_GridAllGather(grid, srcarray, dstarray, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Array), intent(inout) :: srcarray
      type(ESMF_Array), intent(out) :: dstarray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Call {\tt DistGrid} routines to perform a AllGather operation over the 
!     data in a {\tt Grid}.  It returns the data in the {\tt dstarray}.
!
!     \begin{description}
!     \item [grid]
!           Grid on which data is defined.
!     \item [srcarray]
!           Array containing data to be allgather'ed.  The data inside the
!           array is not altered, but annotation is attached to the array
!           for later use (thus the intent must be 'inout'.)
! 
!     \item [dstarray]
!           Array containing the resulting data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Call DistGrid method to perform actual work
      call ESMF_DistGridAllGather(grid%ptr%distgrid%ptr, srcarray, dstarray, &
                                                                       status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in GridAllGather: DistGrid AllGather returned failure"
        return
      endif

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAllGather

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGather - Data Gather operation on a Grid

! !INTERFACE:
      subroutine ESMF_GridGather(grid, srcarray, deid, dstarray, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Array), intent(inout) :: srcarray
      integer, intent(in) :: deid
      type(ESMF_Array), intent(out) :: dstarray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Call {\tt DistGrid} routines to perform a Gather operation over the 
!     data in a {\tt Grid}.  It returns the data in the {\tt dstarray}.
!
!     \begin{description}
!     \item [grid]
!           Grid on which data is defined.
!     \item [srcarray]
!           Array containing data to be gathered.  The data inside the
!           array is not altered, but annotation is attached to the array
!           for later use (thus the intent must be 'inout'.)
!     \item [deid]
!           Destination DE number to gather array onto.  The {\tt dstarray}
!           return object is only valid on this DE.  On all other DEs in this
!           layout the {\tt dstarray} return is an invalid {\tt Array}.
!     \item [dstarray]
!           Array containing the resulting data on DE {\tt deid}.  On all other
!           DE numbers in this it is an invalid array.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Call DistGrid method to perform actual work
      call ESMF_DistGridGather(grid%ptr%distgrid%ptr, srcarray, deid, &
                                                             dstarray, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in GridGather: DistGrid Gather returned failure"
        return
      endif

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGather

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridScatter - Data Scatter operation on a Grid

! !INTERFACE:
      subroutine ESMF_GridScatter(grid, deid, srcarray, dimorder, dstarray, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: deid
      type(ESMF_Array), intent(inout) :: srcarray
      integer, dimension(:), intent(in) :: dimorder
      type(ESMF_Array), intent(out) :: dstarray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Call {\tt DistGrid} routines to perform a Scatter operation over the 
!     data in a {\tt Grid}.  It returns the data in the {\tt dstarray}.
!
!     \begin{description}
!     \item [grid]
!           Grid on which output data will be defined.
!     \item [deid]
!           Destination DE number to scatter array from.  The {\tt srcarray}
!           input object is only valid on this DE.  On all other DEs in this
!           layout the {\tt srcarray} argument is ignored.
!     \item [srcarray]
!           Array containing data to be scattered.  The data inside the
!           array is not altered, but annotation is attached to the array
!           for later use (thus the intent must be 'inout'.)
!     \item [dimorder]
!           Integer dimension list same length as rank of srcarray, specifying 
!           how to spread dimensions of input array to output arrays.
!           0 indicates no decomposition, 1 to Ndims indicate index order.
!     \item [dstarray]
!           Array containing the resulting data on DE {\tt deid}.  On all other
!           DE numbers this is an invalid array.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Call DistGrid method to perform actual work
      call ESMF_DistGridScatter(grid%ptr%distgrid%ptr, deid, srcarray, &
                                                    dimorder, dstarray, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in GridScatter: DistGrid Scatter returned failure"
        return
      endif

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridScatter

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridHalo - Data Halo operation on a Grid

! !INTERFACE:
      subroutine ESMF_GridHalo(grid, array, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_Array) :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Call {\tt DistGrid} routines to perform a Halo operation over the data
!     in a {\tt Grid}.  This routine updates the data inside the {\tt Array}
!     so there is no separate return argument.
!
!     \begin{description}
!     \item [grid]
!           Grid on which data is defined.
!     \item [array]
!           Array containing data to be haloed.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Call DistGrid method to perform actual work
      call ESMF_DistGridHalo(grid%ptr%distgrid%ptr, array, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in GridHalo: DistGrid Halo returned failure"
        return
      endif

      ! Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridHalo

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
!     Validates that a Grid is internally consistent.
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
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
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
!      Print information about a Grid.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      print *, "Grid Print:"
      print *, "  (coming soon)"

      end subroutine ESMF_GridPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridComputeDistance - Compute distance between points
!
! !INTERFACE:
      function ESMF_GridComputeDistance(x1, y1, x2, y2, coord_system, rc)

! !RETURN VALUE:
      real (kind=ESMF_IKIND_R8) :: ESMF_GridComputeDistance

! !ARGUMENTS:

      real (kind=ESMF_IKIND_R8), intent(in) :: &
         x1,y1,      &! x,y coordinates of two points between which 
         x2,y2        !   the distance is to be computed

      integer :: &
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

      select case (coord_system)
      case (ESMF_CoordSystem_Spherical)
         ESMF_GridComputeDistance = &
         ESMF_PhysGridCompDistSpherical(x1, y1, x2, y2, status)
      case (ESMF_CoordSystem_Cartesian)
         ESMF_GridComputeDistance = &
         ESMF_PhysGridCompDistCartesian(x1, y1, x2, y2, status)
      case default
         print *,'Distance in coordinate system not yet supported'
         status = ESMF_FAILURE
      end select
!
!     set return code and exit
!
      if (present(rc)) then
         rc = status
      endif
      return

      end function ESMF_GridComputeDistance

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
!!          id of DE that owns search point.
!!     \item[search\_grid]
!!          ESMF Grid to search for location.
!!     \item[{[phys\_grid\_id]}]
!!          If more than one PhysGrid is contained in Grid, choose which
!!          grid to search (default is 1st PhysGrid?).
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
