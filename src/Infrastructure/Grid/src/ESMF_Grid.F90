! $Id: ESMF_Grid.F90,v 1.12 2002/12/06 16:23:40 nscollins Exp $
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
!BOP
! !MODULE: ESMF_GridMod - Grid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt Grid} class.  This class
! provides a unified interface for both PhysGrid and DistGrid information
! for model grids.  Functions for defining and computing grid information
! are available through this class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_ArrayMod       ! ESMF array class
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_IOMod          ! ESMF I/O class
      use ESMF_PhysGridMod    ! ESMF physical grid class
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_GridConfig
!
!     ! Description of ESMF_GridConfig

      type ESMF_GridConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     !  ESMF_GridType
!
!     ! Definition for the Grid class.  A Grid
!     ! is passed back to the user at Grid creation.

      type ESMF_GridType
      sequence
      private

        type (ESMF_Base) :: base                     ! base class object
        type (ESMF_Status) :: gridstatus             ! uninitialized, init ok,
                                                     ! etc
        integer :: horz_gridtype                     ! enum for type of horizontal grid
        integer :: vert_gridtype                     ! enum for type of vertical grid
        integer :: horz_stagger                      ! enum for horizontal grid staggering
        integer :: vert_stagger                      ! enum for vertical grid staggering
        integer :: horz_coord_system                 ! enum for horizontal physical
                                                     ! coordinate system
        integer :: vert_coord_system                 ! enum for vertical physical
                                                     ! coordinate system
        integer :: coord_order                       ! enum for mapping of xyz 
                                                     ! to ijk
        integer :: num_physgrids                     ! number of grid descriptors
                                                     ! necessary to support
                                                     ! staggering, vertical
                                                     ! grids, background grids
        type (ESMF_PhysGrid), dimension(:), pointer :: &
           physgrids         ! grid info for all grid descriptions necessary
                             ! to define horizontal, staggered and vertical grids
        type (ESMF_DistGrid), pointer :: distgrid    ! decomposition and other
                                                     ! logical space info for grid

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

      public ESMF_GridConfig
      public ESMF_Grid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    public ESMF_GridCreate
    public ESMF_GridDestroy
    public ESMF_GridGetConfig
    public ESMF_GridSetConfig
    !public ESMF_GridGetCoordinate
    public ESMF_GridSetCoordinate
    !public ESMF_GridGetLMask
    public ESMF_GridSetLMask
    !public ESMF_GridGetMMask
    public ESMF_GridSetMMask
    !public ESMF_GridGetMetric
    public ESMF_GridSetMetric
    !public ESMF_GridGetRegionID
    public ESMF_GridSetRegionID
    public ESMF_GridValidate
    public ESMF_GridPrint

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

   integer, parameter ::                   &! recognized grid types
      ESMF_GridType_Unknown           =  0, &! unknown or undefined grid
      ESMF_GridType_LatLon            =  1, &! equally-spaced lat/lon grid
      ESMF_GridType_Mercator          =  2, &! Mercator lat/lon grid
      ESMF_GridType_Dipole            =  3, &! Displaced-pole dipole grid
      ESMF_GridType_Tripole           =  4, &! Tripolar grids
      ESMF_GridType_XY                =  5, &! Cartesian equally-space x-y grid
      ESMF_GridType_DataStream        =  6, &! Data stream
      ESMF_GridType_PhysFourier       =  7, &! Mixed Fourier Space/Phys Space grid
      ESMF_GridType_LatLonGauss       =  8, &! lat/lon grid with Gaussian latitudes
      ESMF_GridType_SphericalSpectral =  9, &! spectral space for spherical harmonics
      ESMF_GridType_Geodesic          = 10, &! spherical geodesic grid
      ESMF_GridType_CubedSphere       = 11  ! cubed sphere grid

   integer, parameter ::                   &! recognized staggering types
      ESMF_GridStagger_Unknown        =  0, &! unknown or undefined staggering
      ESMF_GridStagger_A              =  1, &! Arakawa A (centered velocity)
      ESMF_GridStagger_B              =  2, &! Arakawa B (velocities at grid corner)
      ESMF_GridStagger_C              =  3, &! Arakawa C (velocities at cell faces)
      ESMF_GridStagger_Z              =  4, &! C grid equiv for geodesic grid
      ESMF_GridStagger_VertCenter     =  5, &! vert velocity at vertical midpoints
      ESMF_GridStagger_VertFace       =  6  ! vert velocity/Pgrad at top(bottom)face

   integer, parameter ::                   &! recognized coordinate systems
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
      ESMF_CoordSystem_Lagrangian     = 14  ! Lagrangian coordinates
      ! I'm sure there are more - I'm not sure
      ! what the atmospheric ESMF models are using for vertical coords

!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.12 2002/12/06 16:23:40 nscollins Exp $'

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
!     This interface provides a single entry point for Grid create
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
      interface ESMF_GridSetCoordinate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetCoordinateFromArray
         module procedure ESMF_GridSetCoordinateFromBuffer
         module procedure ESMF_GridSetCoordinateCompute
         module procedure ESMF_GridSetCoordinateCopy

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
         module procedure ESMF_GridSetLMaskCompute
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
         module procedure ESMF_GridSetMMaskCompute
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
         module procedure ESMF_GridSetRegionIDCompute
         module procedure ESMF_GridSetRegionIDCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     region id's as part of a {\tt Grid}.
!
!EOP
      end interface
!
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
! !IROUTINE: ESMF_GridCreateEmpty - Create a new Grid with no data

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
!     internals.  Return a pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt Grid} name.
!     \item[[rc]]
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
      function ESMF_GridCreateInternal(name, gridtype, coord_system, &
                                       x_min, x_max, y_min, y_max, i_max, &
                                       j_max, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateInternal
!
! !ARGUMENTS:
      character (len=*), intent(in) :: name
      integer, intent(in), optional :: gridtype
      integer, intent(in), optional :: coord_system
      real, intent(in), optional :: x_min
      real, intent(in), optional :: x_max
      real, intent(in), optional :: y_min
      real, intent(in), optional :: y_max
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and internally generates the Grid.  Return a pointer to
!     the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt Grid} name.
!     \item[[gridtype]]
!          Integer specifier to denote gridtype:
!             gridtype=1   lat-lon
!             TODO:  fill out
!     \item[[coord\_system]]
!          Integer specifier to denote coordinate system:
!             coord\_system=1   spherical
!             coord\_system=2   Cartesian
!             coord\_system=3   cylindrical
!     \item[[x\_min]]
!          Minimum physical coordinate in the x-direction.
!     \item[[x\_max]]
!          Maximum physical coordinate in the x-direction.
!     \item[[y\_min]]
!          Minimum physical coordinate in the y-direction.
!     \item[[y\_max]]
!          Maximum physical coordinate in the y-direction.
!     \item[[i\_max]]
!          Number of even-spaced grid increments in the i-direction.
!     \item[[j\_max]]
!          Number of even-spaced grid increments in the j-direction.
!     \item[[rc]]
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
      call ESMF_GridConstructNew(grid, name, status)
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
      function ESMF_GridCreateRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateRead
!
! !ARGUMENTS:
      character (len=*), intent(in) :: name
      type(ESMF_IOSpec), intent(in) :: iospec   ! file specs
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and reads a {\tt Grid} in from a file.  Return a pointer to
!     the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt Grid} name.
!     \item[[iospec]]
!          File I/O specification.
!     \item[[rc]]
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
      function ESMF_GridCreateCopy(name, grid_in, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateCopy
!
! !ARGUMENTS:
      character (len=*), intent(in) :: name
      type(ESMF_Grid), intent(in) :: grid_in
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and copies attributes from another {\tt Grid}.  Return a
!     pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt Grid} name.
!     \item[[grid\_in]]
!          {\tt Grid} to be copied.
!     \item[[rc]]
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
      function ESMF_GridCreateCutout(name, grid_in, i_min, i_max, j_min, &
                                     j_max, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateCutout
!
! !ARGUMENTS:
      character (len=*), intent(in) :: name
      type(ESMF_Grid), intent(in) :: grid_in
      integer, intent(in) :: i_min
      integer, intent(in) :: i_max
      integer, intent(in) :: j_min
      integer, intent(in) :: j_max
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and copies a region from an existing {\tt Grid}.
!     Return a pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt Grid} name.
!     \item[[grid\_in]]
!          {\tt Grid} to be partially copied.
!     \item[[i\_min]]
!          Minimum global i-index for the region of the grid to be cutout.
!     \item[[i\_max]]
!          Maximum global i-index for the region of the grid to be cutout.
!     \item[[j\_min]]
!          Minimum global j-index for the region of the grid to be cutout.
!     \item[[j\_max]]
!          Maximum global j-index for the region of the grid to be cutout.
!     \item[[rc]]
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
      function ESMF_GridCreateChangeResolution(name, grid_in, i_resolution, &
                                               j_resolution, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateChangeResolution
!
! !ARGUMENTS:
      character (len=*), intent(in) :: name
      type(ESMF_Grid), intent(in) :: grid_in
      integer, intent(in) :: i_resolution
      integer, intent(in) :: j_resolution
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and creates a {\tt Grid} by either coarsening or refining an
!     existing {\tt Grid}.  Return a pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt Grid} name.
!     \item[[grid\_in]]
!          Source {\tt Grid} to be coarsened or refined.
!     \item[[i\_resolution]]
!          Integer resolution factor in the i-direction.
!     \item[[j\_resolution]]
!          Integer resolution factor in the j-direction.
!          Note:  The above arguments assume refinement by factor if positive
!          and coarsening by absolute value of the factor if negative.  For
!          example, i\_resolution=4 indicates the new {\tt Grid} will be four
!          times as resolved in the i-direction as the source {\tt Grid},
!          whereas j\_resolution=-3 means the new {\tt Grid} will sample every
!          third point in the j-direction.
!     \item[[rc]]
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
      function ESMF_GridCreateExchange(name, grid_in1, grid_in2, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateExchange
!
! !ARGUMENTS:
      character (len=*), intent(in) :: name
      type(ESMF_Grid), intent(in) :: grid_in1
      type(ESMF_Grid), intent(in) :: grid_in2
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Grid} object, constructs its
!     internals, and creates a new {\tt Grid} from the intersection of two
!     existing {\tt Grids}.  Return a pointer to the new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          New {\tt Grid} name.
!     \item[[grid\_in1]]
!          First source {\tt Grid}.
!     \item[[grid\_in2]]
!          Second source {\tt Grid}.
!     \item[[rc]]
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
      type(ESMF_Grid), intent(in) :: grid
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
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridDestroy

!------------------------------------------------------------------------------
!BOP
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
!     \item[arg1]
!          Argument 1.
!     \item[arg2]
!          Argument 2.
!     \item[[name]]
!          {\tt Grid} name.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOP

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructNew: Grid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridDestruct - Free any Grid memory allocated internally

! !INTERFACE:
      subroutine ESMF_GridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
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
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridDestruct

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
!     \item[[rc]]
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
!     \item[[rc]]
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
!     \item[[rc]]
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
! !IROUTINE: ESMF_GridSetCoordinateFromArray - Set the coordinates of a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetCoordinateFromArray(Grid, array, id, rc)
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
!     \item[[id]]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordinateFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordinateFromBuffer - Set the coordinates of a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetCoordinateFromBuffer(Grid, buffer, id, rc)
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
!     \item[[id]]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordinateFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordinateCompute - Compute coordinates for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetCoordinateCompute(Grid, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: id
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
!     \item[[id]]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!TODO: figure out the argument list necessary to completely describe the
!      internal calculation of the coordinates of a simple grid.
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordinateCompute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordinateCopy - Copies coordinates from one grid to another

! !INTERFACE:
      subroutine ESMF_GridSetCoordinateCopy(Grid, Grid_in, id, rc)
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
!     \item[[id]]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordinateCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetLMaskFromArray - Set a logical mask in a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetLMaskFromArray(Grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Array), intent(in) :: array
      character (len=*), intent(in) :: name  ! TODO: optional?
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
!     \item [[name]]
!           {\tt LMask} name.
!     \item[[rc]]
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
      character (len=*), intent(in) :: name  ! TODO: optional?
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
!     \item [[name]]
!           {\tt LMask} name.
!     \item[[rc]]
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
! !IROUTINE: ESMF_GridSetLMaskCompute - Compute a logical mask for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetLMaskCompute(Grid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes a logical mask for a Grid via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item [[name]]
!           {\tt LMask} name.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!TODO: figure out the argument list necessary to completely describe the
!      internal calculation of a logical mask for a simple grid.
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetLMaskCompute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetLMaskCopy - Copies a logical mask from one grid to another.

! !INTERFACE:
      subroutine ESMF_GridSetLMaskCopy(Grid, name, Grid_in, name_in, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      type(ESMF_Grid), intent(in) :: grid_in
      character (len=*), intent(in) :: name_in  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a logical mask for a Grid from another Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item [[name]]
!           {\tt LMask} name to be set.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
!     \item [[name\_in]]
!           {\tt LMask} name to be copied.
!     \item[[rc]]
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
      character (len=*), intent(in) :: name  ! TODO: optional?
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
!     \item [[name]]
!           {\tt MMask} name.
!     \item[[rc]]
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
!     \item [[name]]
!           {\tt MMask} name.
!     \item[[rc]]
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
! !IROUTINE: ESMF_GridSetMMaskCompute - Compute a multiplicative mask for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetMMaskCompute(Grid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes a multiplicative mask for a
!     Grid via a prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item [[name]]
!           {\tt MMask} name.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!TODO: figure out the argument list necessary to completely describe the
!      internal calculation of a multiplicative mask for a simple grid.
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskCompute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskCopy - Copies a multiplicative mask from one grid to another.

! !INTERFACE:
      subroutine ESMF_GridSetMMaskCopy(Grid, name, Grid_in, name_in, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      type(ESMF_Grid), intent(in) :: grid_in
      character (len=*), intent(in) :: name_in  ! TODO: optional?
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
!     \item [[name]]
!           {\tt MMask} name to be set.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
!     \item [[name\_in]]
!           {\tt MMask} name to be copied.
!     \item[[rc]]
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
      character (len=*), intent(in) :: name  ! TODO: optional?
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
!     \item [[name]]
!           {\tt Metric} name.
!     \item[[rc]]
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
      character (len=*), intent(in) :: name  ! TODO: optional?
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
!     \item [[name]]
!           {\tt Metric} name.
!     \item[[rc]]
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
      character (len=*), intent(in) :: name  ! TODO: optional?
      integer, intent(in) :: id
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
!     \item [[name]]
!           {\tt Metric} name.
!     \item[[id]]
!          Identifier for predescribed metrics.  TODO: make list
!     \item[[rc]]
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
!     \item [[name]]
!           {\tt Metric} name to be set.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
!     \item [[name\_in]]
!           {\tt Metric} name to be copied.
!     \item[[rc]]
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
!     \item [[name]]
!           {\tt RegionID} name.
!     \item[[rc]]
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
!     \item [[name]]
!           {\tt RegionID} name.
!     \item[[rc]]
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
! !IROUTINE: ESMF_GridSetRegionIDCompute - Compute a region identifier for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetRegionIDCompute(Grid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes a region identifier for a
!     Grid via a prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt Grid} to be modified.
!     \item [[name]]
!           {\tt RegionID} name.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!TODO: figure out the argument list necessary to completely describe the
!      internal calculation of a region identifier for a simple grid.
!EOP
! !REQUIREMENTS:

!
!  code goes here
!
      end subroutine ESMF_GridSetRegionIDCompute

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
!     \item [[name]]
!           {\tt RegionID} name to be set.
!     \item[grid\_in]
!          Pointer to a {\tt Grid} whose coordinates are to be copied.
!     \item [[name\_in]]
!           {\tt RegionID} name to be copied.
!     \item[[rc]]
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
!     \item[[opt]]
!          Validation options.
!     \item[[rc]]
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
!     \item[[opt]]
!          Print options that control the type of information and level of
!          detail.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  code goes here
!
      end subroutine ESMF_GridPrint

!------------------------------------------------------------------------------

      end module ESMF_GridMod
