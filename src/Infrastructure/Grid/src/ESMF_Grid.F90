! $Id: ESMF_Grid.F90,v 1.76 2003/08/05 23:06:18 jwolfe Exp $
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
! The code in this file implements the {\tt ESMF\_Grid} class.  This class
! provides a unified interface for both {\tt ESMF\_PhysGrid} and {\tt ESMF\_DistGrid}
! information for model grids.  Functions for defining and computing {\tt ESMF\_Grid}
! information are available through this class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_IOMod          ! ESMF I/O class
      use ESMF_DataMapMod     ! ESMF data map class
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
        type (ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
                                            ! logical identifier to indicate
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
        type (ESMF_GridType), pointer :: ptr => NULL()
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
    public ESMF_GridGetPhysGrid
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
      '$Id: ESMF_Grid.F90,v 1.76 2003/08/05 23:06:18 jwolfe Exp $'

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
         module procedure ESMF_GridCreateInternalSpecd
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
         module procedure ESMF_GridConstructInternal
         module procedure ESMF_GridConstructInternalSpecd

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
         module procedure ESMF_GridSetCoordCompute
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
          module procedure ESMF_GridAddPhysGrid
          module procedure ESMF_GridAddPhysGridSpecd
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     search a {\tt ESMF\_Grid} for point(s).
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
      function ESMF_GridCreateInternal(counts, x_min, x_max, &
                                       y_min, y_max, layout, &
                                       horz_gridtype, vert_gridtype, &
                                       horz_stagger, vert_stagger, &
                                       horz_coord_system, vert_coord_system, &
                                       periodic, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateInternal
!
! !ARGUMENTS:
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: counts
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
!     \item[{[counts]}]
!          Array of number of grid increments in each direction.
!     \item[{[x\_min]}]
!          Minimum physical coordinate in the x-direction.
!     \item[{[x\_max]}]
!          Maximum physical coordinate in the x-direction.
!     \item[{[y\_min]}]
!          Minimum physical coordinate in the y-direction.
!     \item[{[y\_max]}]
!          Maximum physical coordinate in the y-direction.
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
      call ESMF_GridConstruct(grid, counts, x_min, x_max, y_min, y_max, &
                              layout, &
                              horz_gridtype, vert_gridtype, &
                              horz_stagger, vert_stagger, &
                              horz_coord_system, vert_coord_system, &
                              periodic, name, status)
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
! !IROUTINE: ESMF_GridCreateInternalSpecd - Create a new Grid internally

! !INTERFACE:
      function ESMF_GridCreateInternalSpecd(countsPerDE1, countsPerDE2, &
                                            min, delta1, delta2, layout, &
                                            horz_gridtype, vert_gridtype, &
                                            horz_stagger, vert_stagger, &
                                            horz_coord_system, vert_coord_system, &
                                            name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateInternalSpecd
!
! !ARGUMENTS:
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
      real, dimension(ESMF_MAXGRIDDIM), intent(in) :: min
      real, dimension(:), intent(in) :: delta1
      real, dimension(:), intent(in) :: delta2
      type (ESMF_DELayout), intent(in) :: layout
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      integer, intent(in), optional :: horz_coord_system
      integer, intent(in), optional :: vert_coord_system
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
!     \item[{[countsPerDE1]}]
!          Array of number of grid increments per DE in the first direction.
!     \item[{[countsPerDE2]}]
!          Array of number of grid increments per DE in the second direction.
!     \item[{[min]}]
!          Array of minimum physical coordinate in each direction.
!     \item[{[delta1]}]
!          Array of physical increments between nodes in the first direction.
!     \item[{[delta2]}]
!          Array of physical increments between nodes in the second direction.
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
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
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
      nullify(ESMF_GridCreateInternalSpecd%ptr)

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
      call ESMF_GridConstruct(grid, countsPerDE1, countsPerDE2, &
                              min, delta1, delta2, layout, &
                              horz_gridtype, vert_gridtype, &
                              horz_stagger, vert_stagger, &
                              horz_coord_system, vert_coord_system, &
                              name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateInternal: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateInternalSpecd%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateInternalSpecd

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
      subroutine ESMF_GridConstructInternal(grid, counts, x_min, x_max, &
                                            y_min, y_max, layout, &
                                            horz_gridtype, vert_gridtype, &
                                            horz_stagger, vert_stagger, &
                                            horz_coord_system, vert_coord_system, &
                                            periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: counts
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
!     \item[{[counts]}]
!          Array of number of grid increments in each direction.
!     \item[{[x\_min]}]
!          Minimum physical coordinate in the x-direction.
!     \item[{[x\_max]}]
!          Maximum physical coordinate in the x-direction.
!     \item[{[y\_min]}]
!          Minimum physical coordinate in the y-direction.
!     \item[{[y\_max]}]
!          Maximum physical coordinate in the y-direction.
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
                        horz_coord_system, vert_coord_system, &
                        periodic=periodic, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternal: Grid set"
        return
      endif

!     Create the DistGrid
      grid%distgrid = ESMF_DistGridCreate(counts=counts, layout=layout, &
                                          rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternal: Distgrid create"
        return
      endif

!     Create main physgrid
      physgrid_name = 'base'
      call ESMF_GridAddPhysGrid(grid, counts, physgrid_id, &
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
! !IROUTINE: ESMF_GridConstructInternalSpecd - Construct the internals of an allocated Grid

! !INTERFACE:
      subroutine ESMF_GridConstructInternalSpecd(grid, countsPerDE1, &
                                                 countsPerDE2, min, delta1, &
                                                 delta2, layout, &
                                                 horz_gridtype, vert_gridtype, &
                                                 horz_stagger, vert_stagger, &
                                                 horz_coord_system, &
                                                 vert_coord_system, &
                                                 name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
      real, dimension(ESMF_MAXGRIDDIM), intent(in) :: min
      real, dimension(:), intent(in) :: delta1
      real, dimension(:), intent(in) :: delta2
      type (ESMF_DELayout), intent(in) :: layout
      integer, intent(in), optional :: horz_gridtype
      integer, intent(in), optional :: vert_gridtype
      integer, intent(in), optional :: horz_stagger
      integer, intent(in), optional :: vert_stagger
      integer, intent(in), optional :: horz_coord_system
      integer, intent(in), optional :: vert_coord_system
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
!     \item[{[countsPerDE1]}]
!          Array of number of grid increments per DE in the x-direction.
!     \item[{[countsPerDE2]}]
!          Array of number of grid increments per DE in the y-direction.
!     \item[{[min]}]
!          Array of minimum physical coordinate in each direction.
!     \item[{[delta1]}]
!          Array of physical increments between nodes in the first direction.
!     \item[{[delta2]}]
!          Array of physical increments between nodes in the second direction.
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      character(len=4) :: physgrid_name       !
      integer :: physgrid_id                  ! integer identifier for physgrid
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

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
        print *, "ERROR in ESMF_GridConstructInternalSpecd: Grid construct"
        return
      endif

!     Fill in grid derived type with subroutine arguments
      call ESMF_GridSet(grid, horz_gridtype, vert_gridtype, &
                        horz_stagger, vert_stagger, &
                        horz_coord_system, vert_coord_system, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternalSpecd: Grid set"
        return
      endif

!     Create the DistGrid
      grid%distgrid = ESMF_DistGridCreate(countsPerDE1=countsPerDE1, &
                                          countsPerDE2=countsPerDE2, &
                                          layout=layout, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternalSpecd: Distgrid create"
        return
      endif

!     Create main physgrid
      physgrid_name = 'base'
      call ESMF_GridAddPhysGrid(grid, physgrid_id, min, delta1, delta2, &
                                countsPerDE1, countsPerDE2, physgrid_name, &
                                status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternalSpecd: Add physgrid"
        return
      endif

!     Call SetCoord to create physical coordinates of subgrid
      call ESMF_GridSetCoord(grid, physgrid_id, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternalSpecd: Grid set coord compute"
        return
      endif

!     Create any necessary physgrids to support staggering  TODO

!     Create vertical physgrid if requested

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridConstructInternalSpecd: Grid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridConstructInternalSpecd

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

      integer :: status=ESMF_SUCCESS               ! Error status
      logical :: rcpresent=.FALSE.                 ! Return code present
      integer :: i
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
      do i=1,ESMF_MAXGRIDDIM
        grid%periodic(i) = ESMF_TF_FALSE
      enddo
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
! !IROUTINE: ESMF_GridAddPhysGrid - Add a PhysGrid to a Grid

! !INTERFACE:
      subroutine ESMF_GridAddPhysGrid(grid, counts, physgrid_id, &
                                      x_min, x_max, y_min, y_max, &
                                      physgrid_name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: counts
      integer, intent(out) :: physgrid_id
      real, intent(in), optional :: x_min
      real, intent(in), optional :: x_max
      real, intent(in), optional :: y_min
      real, intent(in), optional :: y_max
      character (len=*), intent(in), optional :: physgrid_name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysGrid} to a {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[counts]}]
!          Array of number of grid increments in each direction.
!     \item [{[physgrid\_id]}]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[{[x\_min]}]
!          Minimum physical coordinate in the x-direction.
!     \item[{[x\_max]}]
!          Maximum physical coordinate in the x-direction.
!     \item[{[y\_min]}]
!          Minimum physical coordinate in the y-direction.
!     \item[{[y\_max]}]
!          Maximum physical coordinate in the y-direction.
!     \item [{[physgrid\_name]}]
!          {\tt ESMF\_PhysGrid} name.
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
      type(ESMF_PhysGrid), dimension(:), pointer :: temp_pgrids
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

      if (real(counts(1)).ne.0.0) then
        delta(1) = (x_max - x_min) / real(counts(1))
      else
        print *, "ERROR in ESMF_GridAddPhysGrid: counts(1)=0"
        return
      endif
      if (real(counts(2)).ne.0.0) then
        delta(2) = (y_max - y_min) / real(counts(2))
      else
        print *, "ERROR in ESMF_GridAddPhysGrid: counts(2)=0"
        return
      endif
      do i = 1,2
        local_min_coord(i) = delta(i) * &
                             real(grid%distgrid%ptr%MyDE%ai_global(i)%min - 1)
        local_max_coord(i) = delta(i) * &
                             real(grid%distgrid%ptr%MyDE%ai_global(i)%max)
        local_nmax(i) = grid%distgrid%ptr%MyDE%ai_global(i)%max &
                      - grid%distgrid%ptr%MyDE%ai_global(i)%min + 1
      enddo
      global_min_coord(1)=x_min
      global_max_coord(1)=x_max
      global_nmax(1)=counts(1)
      global_min_coord(2)=y_min
      global_max_coord(2)=y_max
      global_nmax(2)=counts(2)
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
! !IROUTINE: ESMF_GridAddPhysGridSpecd - Add a PhysGrid to a Grid

! !INTERFACE:
      subroutine ESMF_GridAddPhysGridSpecd(grid, physgrid_id, min, delta1, &
                                           delta2, countsPerDE1, countsPerDE2, &
                                           physgrid_name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(out) :: physgrid_id
      real, dimension(ESMF_MAXGRIDDIM), intent(in), optional :: min
      real, dimension(:), intent(in) :: delta1
      real, dimension(:), intent(in) :: delta2
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
      character (len=*), intent(in), optional :: physgrid_name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysGrid} to a {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[counts]}]
!          Array of number of grid increments in each direction.
!     \item [{[physgrid\_id]}]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[{[min]}]
!          Array of minimum physical coordinates in each direction.
!     \item[{[delta1]}]
!          Array of physical increments between nodes in the first direction.
!     \item[{[delta2]}]
!          Array of physical increments between nodes in the second direction.
!     \item[{[countsPerDE1]}]
!          Array of number of grid increments per DE in the x-direction.
!     \item[{[countsPerDE2]}]
!          Array of number of grid increments per DE in the y-direction.
!     \item [{[physgrid\_name]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: status=ESMF_SUCCESS          ! Error status
      logical :: rcpresent=.FALSE.            ! Return code present
      integer :: i, myDE(2), counts(2)
      real :: global_min(2)
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: gridp
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
!         print *, "ERROR in ESMF_GridAddPhysGridSpecd: physgrids allocate"
!         return
!       endif
        allocate(temp_pgrids(grid%num_physgrids), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridAddPhysGridSpecd: temp_pgrids allocate"
          return
        endif
      else
        allocate(temp_pgrids(grid%num_physgrids), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridAddPhysGridSpecd: temp_pgrids allocate"
          return
        endif
        do i = 1, grid%num_physgrids - 1
          temp_pgrids(i) = grid%physgrids(i)
        enddo
        deallocate(grid%physgrids, stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridAddPhysGridSpecd: physgrids deallocate"
          return
        endif
      endif
      physgrid_id = grid%num_physgrids 

      global_min(1)=min(1)
      global_min(2)=min(2)

      gridp%ptr = grid
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

      temp_pgrids(physgrid_id) = ESMF_PhysGridCreate(dim_num=2, delta1=delta1, &
                                 delta2=delta2, global_min=global_min, &
                                 counts=counts, name=physgrid_name, rc=status)
      grid%physgrids => temp_pgrids

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddPhysGridSpecd

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
      subroutine ESMF_GridGetDE(grid, MyDE, local_cell_count, global_start, &
                                ai_global, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(inout), optional :: MyDE
      integer, intent(inout), optional :: local_cell_count
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

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGetDE(grid%ptr%distgrid%ptr, MyDE, local_cell_count, &
                              global_start, ai_global, status)
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
      subroutine ESMF_GridGetAllAxisIndex(grid, global_ai, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: global_ai
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[global\_ai]}]
!          Global axis indices for all DE's.
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
      call ESMF_DistGridGetAllAxisIndex(grid%ptr%distgrid%ptr, global_ai, &
                                        status)
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
! !IROUTINE: ESMF_GridGetPhysGrid - Get PhysGrid for a given relative location

! !INTERFACE:
      subroutine ESMF_GridGetPhysGrid(grid, physgrid, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid 
      type(ESMF_PhysGrid) :: physgrid
      type(ESMF_RelLoc) :: relloc
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return the {\tt ESMF\_PhysGrid} associated with the given relative
!     location.  Return  error if the grid contains no physgrid at the
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

! This routine essentially defines the mapping between the physgrid
! array index and the logical CELL_NORTH, etc...  Perhaps it should
! be used in the construction routine when these are assigned, as well.

      integer :: idx      ! The index into physgrid array

      idx = -1

! These are the values that the Relloc currently stores, but
! this index transformation belongs close to the physgrid list,
! so spell these out here.  Otherwise someone might change the
! datamap file without realizing it will mess this ordering up.

      if (relloc .eq. ESMF_CELL_CENTER) then
         idx = 1
      endif

! For some (unknown) reason, these cases are coming up as unfound
! references.  Fix later, TODO
!      if (relloc .eq. ESMF_CELL_NORTH) then
!         idx = 2
!      endif
!      if (relloc .eq. ESMF_CELL_EAST) then
!         idx = 3
!      endif
!      if (relloc .eq. ESMF_CELL_NE) then
!         idx = 4
!      endif
!      if (relloc .eq. ESMF_CELL_CELL) then
!         idx = 5
!      endif
!      if (relloc .eq. ESMF_CELL_VERTEX) then
!         idx = 6
!      endif

      if (idx .gt. grid%ptr%num_physgrids) then
        rc = ESMF_FAILURE
      else
        physgrid = grid%ptr%physgrids(idx)
        rc = ESMF_SUCCESS
      endif

      end subroutine ESMF_GridGetPhysGrid


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
!     This version of set internally computes coordinates for a {\tt ESMF\_Grid} via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[{[physgrid\_id]}]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
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
      integer, dimension(ESMF_MAXGRIDDIM) :: global_cell_dim
      integer, dimension(ESMF_MAXGRIDDIM) :: gcell_start
      integer, dimension(ESMF_MAXGRIDDIM) :: gcell_end
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
                            global_cell_dim=global_cell_dim, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridSetCoordCompute: Distgrid get"
        return
      endif
      DE_id = grid%distgrid%ptr%MyDE%MyDE
      call ESMF_DistGridGetCounts(grid%distgrid%ptr, DE_id, &
                                  gcell_start=gcell_start, &
                                  gcell_end=gcell_end, &
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
      if (global_cell_dim(1).ne.0) then
        delta1 = (global_max_coords(1)-global_min_coords(1)) / real(global_cell_dim(1))
      else
        print *, "ERROR in ESMF_GridSetCoordCompute: global_cell_dim1=0"
        return
      endif
      if (global_cell_dim(2).ne.0) then
        delta2 = (global_max_coords(2)-global_min_coords(2)) / real(global_cell_dim(2))
      else
        print *, "ERROR in ESMF_GridSetCoordCompute: global_cell_dim2=0"
        return
      endif

!     For now, set coord_loc by hand.  TODO:  automate for the different grid types
      coord_loc(1) = 1
      coord_loc(2) = 2
      ncoord_locs = 2
      call ESMF_PhysGridSetCoord(grid%physgrids(physgrid_id)%ptr, &
                                 ncoord_locs, coord_loc, &
                                 gcell_start(1), gcell_end(1), &
                                 gcell_start(2), gcell_end(2), &
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
      integer, intent(out), optional :: horz_coord_system
      integer, intent(out), optional :: vert_coord_system
      integer, intent(out), optional :: coord_order
      real, intent(out), dimension(ESMF_MAXGRIDDIM), optional :: global_min_coords
      real, intent(out), dimension(ESMF_MAXGRIDDIM), optional :: global_max_coords
      integer, intent(out), dimension(ESMF_MAXGRIDDIM), optional :: global_cell_dim
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

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: physgrid_id
      integer :: i                                ! Loop index
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
        physgrid_id = gridp%num_physgrids  ! TODO: fix so passed in?
        call ESMF_PhysGridGet(gridp%physgrids(physgrid_id)%ptr, &
                              global_min=global_min_coords, &
                              global_max=global_max_coords, rc=status)
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
                              coord_order, periodic, rc)
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
      type (ESMF_Logical), intent(in), optional :: periodic(:)
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

      integer :: status=ESMF_SUCCESS              ! Error status
      integer :: i                                ! loop index
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
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            grid%periodic(i) = periodic(i)
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
