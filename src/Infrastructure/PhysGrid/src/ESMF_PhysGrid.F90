! $Id: ESMF_PhysGrid.F90,v 1.69 2004/03/22 21:57:32 cdeluca Exp $
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
!     ESMF PhysGrid Module
      module ESMF_PhysGridMod
!
!==============================================================================
!
! This file contains the PhysGrid class definition and all PhysGrid class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!!#include "ESMF_PhysGrid.h"   ! this seems unnecessary
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_PhysGridMod - Physical properties of Grid
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_PhysGrid} class and is 
! responsible for computing or initializing physical properties of grids.   
! {\tt ESMF\_PhysGrid} properties include coordinate information necessary 
! for describing grids, grid metric information and grid masks.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LocalArrayMod
      use ESMF_DataMapMod
      use ESMF_ArrayMod
      use ESMF_PhysCoordMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     !  ESMF_PhysLocation
!
!     !  Physical locations for a set of points defining the grid.

      type ESMF_PhysLocation
      sequence
!      private
        type (ESMF_Base)    :: base     ! ESMF Base object, including name

        ! One array per number of dimensions:

        type (ESMF_Array), dimension(:), pointer :: compLocations
                                        ! the coordinates for each point in the
                                        ! grid.  If the coordinates are aligned,
                                        ! then this array is a simple vector of
                                        ! values along the axis.  Otherwise the
                                        ! Array must have the coordinates for
                                        ! all points in the Grid.

        type (ESMF_Array), dimension(:), pointer :: totalLocations
                                        ! same as above, but with an additional
                                        ! boundary layer of coordinates.  Only
                                        ! used internally, for example during
                                        ! regridding to handle external boundary
                                        ! conditions and to avoid the need for
                                        ! inter-DE communication at the internal
                                        ! boundaries.


      end type

!------------------------------------------------------------------------------
!     ! ESMF_RegionType
!
!     ! Type to specify kind of region for defined PhysGrid regions.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.  (They include polygons and ellipses/ellipsii.)

      type ESMF_RegionType
      sequence
        integer :: regionType
      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysRegion
!
!     !  Physical locations for a set of points defining regions of the grid
!     !  (e.g. cell centers or domains of influence).

      type ESMF_PhysRegion
      sequence
!      private
        type (ESMF_Base) :: base  ! ESMF Base class object

        type (ESMF_RegionType) :: regionType
                                  ! what kind of region          

        integer :: numVertices    ! number of vertices for a polygonal region;
                                  ! if variable, set this to the largest number.
                                  ! Vertices can be degenerate.

        type (ESMF_Array), dimension(:,:), pointer :: vertices
                                  ! coordinates in each direction for each corner
                                  ! of each region.
                                  ! The 2 dimensions are: numDims
                                  !                       numVertices

        type (ESMF_Array), dimension(:,:), pointer :: bbox 
                                  ! bounding box for each region to aid search
                                  ! methods.
                                  ! The 2 dimensions are: numDims
                                  !                       2 (1=min, 2=max)

        type (ESMF_Array), dimension(2) :: ellipse
                                  ! parameters of ellipse describing region
                                  ! around each point.  Note that the values can
                                  ! be equal, describing a circle or sphere
      
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridMaskType
!
!     ! Type to specify kind of region for defined PhysGrid regions.

      type ESMF_GridMaskType
      sequence
        integer :: maskType
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridMask
!
!     ! Data type describing masks for a PhysGrid.  Masks are named and can
!     ! be of different types, including logical masks, multiplicative masks,
!     ! and integer region IDs.

      type ESMF_GridMask
      sequence
!      private
        type (ESMF_Base) :: base  ! ESMF Base class object
        type (ESMF_GridMaskType) :: maskType
                                  ! type of mask
        type (ESMF_Array) :: data ! mask data at each grid point
      end type

!------------------------------------------------------------------------------
!     ! ESMF_PhysGridOrientation
!
!     ! Type to specify orientation for defined PhysGrids.  Useful for
!     !  queries of grid directions.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.  (They include horizontal, vertical, 3D)

      type ESMF_PhysGridOrientation
      sequence
        integer :: orientation
      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysGridType
!
!     !  An ESMF_PhysGrid fully describes a set of physical coordinates and
!     !  how they are connected into cells, polygons, or regions.  If data
!     !  resides at more than one location relative to the cells there will
!     !  be multiple PhysGrids created, which may share common internal
!     !  data values, but each can be customized to accurately describe the
!     !  exact characteristics of the Coordinates.

      type ESMF_PhysGridType
      sequence
!      private

        type (ESMF_Base) :: base  ! ESMF Base class object

        type (ESMF_RelLoc) :: relloc
                                  ! If this PhysGrid describes staggered part of
                                  ! a grid, this is the Relative Location for 
                                  ! easy determination of PhysGrid associated
                                  ! with a staggered location.

        type (ESMF_CoordSystem) :: coordSystem
                                  ! Coordinate system (eg Cartesian, Spherical, ...etc)

        integer :: numDims        ! Number of physical dimensions

        type (ESMF_PhysGridOrientation) :: orientation
                                  ! Orientation (eg Horizontal, Vertical, Unknown)

        type (ESMF_PhysCoord), dimension(:), pointer :: coords
                                  ! Description of each physical coordinate axis,
                                  ! including extents for this grid.  
      
        type (ESMF_PhysLocation) :: locations
                                  ! Structure which holds the actual coordinates
                                  ! for the grid locations.
      
        type (ESMF_PhysRegion) :: regions
                                  ! Information about grid regions, which
                                  ! typically describe each grid cell, but can
                                  ! be either polygons or circles/spheres/ellipses

        integer :: numMasks
        type (ESMF_GridMask), dimension(:), pointer :: masks
                                  ! Grid-based masks.  Includes both logical 
                                  ! and multiplicative masks.  Default mask 
                                  ! (for query) is the first one if no name
                                  ! given.  Region IDs can be encoded as a mask
                                  ! as well.

        integer :: numMetrics
        type (ESMF_Array), dimension(:), pointer :: metrics
                                  ! A place to store metrics for the grid.  
                                  ! There is no support for the Framework to use
                                  ! these metrics, but they can be set and
                                  ! queried as a convenience to the user.  If
                                  ! there arises a need for internally computed
                                  ! metrics, they will also be set here.

      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysGrid
!
!     !  The PhysGrid data structure that is passed between languages.

      type ESMF_PhysGrid
      sequence
!     private
        type (ESMF_PhysGridType), pointer :: ptr   ! pointer to a physgrid type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

      ! These are public primarily for use by the Grid module and
      ! are not meant to be directly accessible to users.
      public ESMF_PhysLocation
      public ESMF_RegionType
      public ESMF_PhysRegion
      public ESMF_GridMaskType
      public ESMF_GridMask
      public ESMF_PhysGridOrientation
      public ESMF_PhysGrid
      public ESMF_PhysGridType

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_PhysGridCreate
      public ESMF_PhysGridDestroy

      public ESMF_PhysGridGet
      public ESMF_PhysGridSet
      public ESMF_PhysGridGetCoord
      public ESMF_PhysGridSetCoord
      public ESMF_PhysGridGetLocations
      public ESMF_PhysGridSetLocations
      public ESMF_PhysGridGetRegions
      public ESMF_PhysGridSetRegions
      public ESMF_PhysGridGetMask
      public ESMF_PhysGridSetMask
      public ESMF_PhysGridGetMetric
      public ESMF_PhysGridSetMetric

      public ESMF_PhysGridValidate
      public ESMF_PhysGridPrint

!   public ESMF_PhysGridSearchBboxSpherical
!   public ESMF_PhysGridSearchGeneralSpherical
!   public ESMF_PhysGridSearchBboxCartesian

      public ESMF_PhysGridPointInCell

      public ESMF_PhysGridCompDistSpherical
      public ESMF_PhysGridCompDistCartesian
 
      public operator(==), operator(/=) ! for overloading region, mask
                                        ! comparison functions

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

      ! Supported ESMF PhysGrid Orientation Types
      !   Unknown     = unknown or undefined orientation
      !   Horizontal  = PhysGrid is a horizontal grid
      !   Vertical    = PhysGrid is a vertical grid
      !   3D          = PhysGrid is a full 3-d description of grid space
      !   XZ          = PhysGrid is a XZ (or zonal     ) slice out of 3-d space 
      !   YZ          = PhysGrid is a YZ (or meridional) slice out of 3-d space

      type (ESMF_PhysGridOrientation), parameter, public :: &! grid direction
         ESMF_PhysGridOrient_Unknown     = ESMF_PhysGridOrientation( 0), &
         ESMF_PhysGridOrient_Horizontal  = ESMF_PhysGridOrientation( 1), &
         ESMF_PhysGridOrient_Vertical    = ESMF_PhysGridOrientation( 2), &
         ESMF_PhysGridOrient_3D          = ESMF_PhysGridOrientation( 3), &
         ESMF_PhysGridOrient_XZ          = ESMF_PhysGridOrientation( 4), &
         ESMF_PhysGridOrient_YZ          = ESMF_PhysGridOrientation( 5)

      ! Supported ESMF PhysGrid Region Types
      !   Unknown     = unknown or undefined region type
      !   Polygonal   = polygons defined by vertex coordinates
      !   Elliptical  = ellipse centered on grid point, defined by two params

      type (ESMF_RegionType), parameter, public :: &! types of PhysGrid regions
         ESMF_RegionType_Unknown      = ESMF_RegionType( 0), &
         ESMF_RegionType_Polygon      = ESMF_RegionType( 1), &
         ESMF_RegionType_Ellipse      = ESMF_RegionType( 2)

      ! Supported ESMF PhysGrid Mask Types
      !   Unknown   = unknown or undefined mask type
      !   Logical   = logical mask
      !   Mult      = multiplicative mask
      !   RegionID  = integer assigning unique ID to each point

      type (ESMF_GridMaskType), parameter, public :: &! types of grid masks
         ESMF_GridMaskType_Unknown        = ESMF_GridMaskType( 0), &
         ESMF_GridMaskType_Logical        = ESMF_GridMaskType( 1), &
         ESMF_GridMaskType_Mult           = ESMF_GridMaskType( 2), &
         ESMF_GridMaskType_RegionID       = ESMF_GridMaskType( 3)

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_PhysGrid.F90,v 1.69 2004/03/22 21:57:32 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_PhysGridCreate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_PhysGridCreateNew
!        module procedure ESMF_PhysGridCreateInternal
!        module procedure ESMF_PhysGridCreateStagger
!        module procedure ESMF_PhysGridCreateRead
!        module procedure ESMF_PhysGridCreateCopy
!        module procedure ESMF_PhysGridCreateCutout
!        module procedure ESMF_PhysGridCreateChangeResolution
!        module procedure ESMF_PhysGridCreateExchange
!
! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_PhysGrid} 
!     create methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!!BOP
!! !INTERFACE:
!      interface ESMF_PhysGridSearchBboxSpherical
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_PhysGridSearchBboxSphericalPoint
!         module procedure ESMF_PhysGridSearchBboxSphericalList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that 
!!     search a grid for point(s) using a simple bounding box search
!!     in spherical coordinates.
!!
!!EOP
!      end interface 
!------------------------------------------------------------------------------
!!BOP
!! !INTERFACE:
!      interface ESMF_PhysGridSearchGeneralSpherical
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_PhysGridSearchGeneralSphericalPoint
!         module procedure ESMF_PhysGridSearchGeneralSphericalList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that 
!!     search a grid for point(s) using a general (cross-product) search
!!     in spherical coordinates.
!!
!!EOP
!      end interface 
!------------------------------------------------------------------------------
!!BOP
!! !INTERFACE:
!      interface ESMF_PhysGridSearchBboxCartesian
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_PhysGridSearchBboxCartesianPoint
!         module procedure ESMF_PhysGridSearchBboxCartesianList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that 
!!     search a grid for point(s) using a simple bounding box search
!!     in Cartesian coordinates.
!!
!!EOP
!      end interface 
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridMaskTypeEqual
         module procedure ESMF_RegionTypeEqual
         module procedure ESMF_PhysGridOrientEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF region kind, mask kind and orientation data types.  It is 
!     provided for easy comparisons of these types with defined values.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridMaskTypeNotEqual
         module procedure ESMF_RegionTypeNotEqual
         module procedure ESMF_PhysGridOrientNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF region kind, mask kind and orientation data types.  It is 
!     provided for easy comparisons of these types with defined values.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!==============================================================================

      contains

!==============================================================================
!
! This section includes the PhysGrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridCreateNew - Create a new PhysGrid

! !INTERFACE:
      function ESMF_PhysGridCreateNew(numDims, relloc, name, coordSystem, &
                                      orientation, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: numDims
      type (ESMF_RelLoc), intent(in) :: relloc
      character (len = *), intent(in), optional :: name  
      type (ESMF_CoordSystem), intent(in), optional :: coordSystem
      type (ESMF_PhysGridOrientation), intent(in), optional :: orientation
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates new {\tt ESMF\_PhysGrid} object and initializes some of
!     its scalar internals.  Returns a pointer to a new {\tt ESMF\_PhysGrid}.  
!     Values not known at time of this call can be set later using
!     a variety of {\tt ESMF\_PhysGridSet} calls.  Also, most arrays
!     must be set with set calls specific to those arrays.
!
!     The arguments are:
!     \begin{description}
!     \item[numDims]
!          Number of physical dimensions.
!     \item[relloc]
!          Relative location in grid cell for which this PhysGrid
!          is being defined.  For example, in a staggered grid,
!          this PhysGrid could be defined for the grid associated
!          with center points or corner points or face points.
!     \item[{[name]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[coordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.).
!     \item[{[orientation]}]
!          {\tt ESMF\_PhysGridOrientation} which identifies an ESMF standard
!          orientation (e.g. horizontal, vertical, 3D, etc.).
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_PhysGridType), pointer :: physgrid   ! Pointer to new physgrid
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize pointers
      nullify(physgrid)
      nullify(ESMF_PhysGridCreateNew%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(physgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status /= ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateNew: Allocate"
        return
      endif

      ! initialize base object in each of the types which use it
      call ESMF_BaseCreate(physgrid%base, "PhysGrid", name, 0, status)
      if(status /= ESMF_SUCCESS) then
         print *, "ERROR in ESMF_PhysGridCreate: BaseCreate"
         return
      endif

!     Fill new physgrid with supplied variables.

      physgrid%numDims  = numDims
      physgrid%relloc   = relloc
      physgrid%numMasks = 0
      nullify(physgrid%masks)
      nullify(physgrid%coords)
      
      if (present(coordSystem)) then
         physgrid%coordSystem = coordSystem
      else
         physgrid%coordSystem = ESMF_CoordSystem_Unknown
      endif
      
      if (present(orientation)) then
         physgrid%orientation = orientation
      else
         physgrid%orientation = ESMF_PhysGridOrient_Unknown
      endif
      
      ! initialize other objects which contains a base
      call ESMF_BaseCreate(physgrid%locations%base, "PhysGridLocations", name, 0, status)
      if(status /= ESMF_SUCCESS) then
         print *, "ERROR in ESMF_PhysGridCreate: Locations BaseCreate"
         return
      endif
      nullify(physgrid%locations%totalLocations)
      nullify(physgrid%locations%compLocations)

      call ESMF_BaseCreate(physgrid%regions%base, "PhysGridRegions", name, 0, status)
      if(status /= ESMF_SUCCESS) then
         print *, "ERROR in ESMF_PhysGridCreate: Regions BaseCreate"
         return
      endif

      nullify(physgrid%metrics)
      physgrid%numMetrics = 0

!     Set return values.
      ESMF_PhysGridCreateNew%ptr => physgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridDestroy - Free resources associated with a PhysGrid 

! !INTERFACE:
      subroutine ESMF_PhysGridDestroy(physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid) :: physgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt ESMF\_PhysGrid} object previously allocated
!     via an {\tt ESMF\_PhysGridCreate} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          The {\tt ESMF\_PhysGrid} object to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) rcpresent = .TRUE.

      ! Destroy all components of physgrid

      nullify(physgrid%ptr)
      status = ESMF_SUCCESS
      if (rcpresent) rc = status

      end subroutine ESMF_PhysGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSet - Sets value of selected PhysGrid quantities.

! !INTERFACE:
      subroutine ESMF_PhysGridSet(physgrid, name, coordSystem, orientation, rc)
!
! !ARGUMENTS:
      type (ESMF_PhysGrid), intent(inout) :: physgrid
      character (len = *), intent(in), optional :: name  
      type (ESMF_CoordSystem), intent(in), optional :: coordSystem
      type (ESMF_PhysGridOrientation), intent(in), optional :: orientation
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Sets a few individual parts of an existing {\tt ESMF\_PhysGrid}. Can be
!     used for only name and coord system.  All other quantities have
!     specific set calls.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          Existing {\tt ESMF\_PhysGrid} for which quantity is to be set.
!     \item[{[name]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[coordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.).
!     \item[{[orientation]}]
!          {\tt ESMF\_PhysGridOrientation} which identifies an ESMF standard
!          orientation (e.g. horizontal, vertical, 3D).
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      if (present(coordSystem)) then
         physgrid%ptr%coordSystem = coordSystem
      endif
      
      if (present(orientation)) then
         physgrid%ptr%orientation = orientation
      endif
      
      if (present(name)) then
         call ESMF_SetName(physgrid%ptr%base, name, "PhysGrid", status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridSet: set name"
            return
         endif
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSet
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGet - Retrieves selected internal PhysGrid quantities.

! !INTERFACE:
      subroutine ESMF_PhysGridGet(physgrid, relloc, name, numDims, &
                                  coordSystem, orientation, rc)
!
! !ARGUMENTS:
      type (ESMF_PhysGrid), intent(in) :: physgrid
      type (ESMF_RelLoc), intent(out), optional :: relloc
      character (len = *), intent(out), optional :: name  
      integer, intent(out), optional :: numDims
      type (ESMF_CoordSystem), intent(out), optional :: coordSystem
      type (ESMF_PhysGridOrientation), intent(out), optional :: orientation
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Retrieves individual parts of an existing {\tt ESMF\_PhysGrid}. Can be
!     used for only a few parts; most array components have specific
!     get calls.
!
!     The arguments are:
!     \begin{description}
!     \item[name]
!          Existing {\tt ESMF\_PhysGrid} from which quantity is to be extracted.
!     \item[{[name]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[relloc]}]
!          {\tt ESMF\_RelLoc} referring to position in staggered grid
!          to which grid quantities refer.
!     \item[{[numDims]}]
!          Number of physical dimensions.
!     \item[{[coordSystem]}]
!          {\tt ESMF\_CoordSystem} identifying coordinate system 
!          (e.g. spherical, cartesian, pressure, etc.).
!     \item[{[orientation]}]
!          {\tt ESMF\_PhysGridOrientation} identifying physgrid orientation 
!          (e.g. horizontal, vertical, 3D, etc.).
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      if (present(name)) then
         call ESMF_GetName(physgrid%ptr%base, name, status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridGet: get name"
            return
         endif
      endif

      if (present(relloc     )) relloc      = physgrid%ptr%relloc
      if (present(numDims    )) numDims     = physgrid%ptr%numDims
      if (present(coordSystem)) coordSystem = physgrid%ptr%coordSystem
      if (present(orientation)) orientation = physgrid%ptr%orientation

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetCoord - Adds a physical coordinate to a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSetCoord(physgrid, physCoord, dimOrder, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type (ESMF_PhysCoord), intent(in) :: physCoord
      integer, intent(in), optional ::  dimOrder
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysCoord} to a {\tt ESMF\_PhysGrid}.  The  
!     {\tt ESMF\_PhysCoord} must be already defined by a call to the
!     {\tt ESMF\_PhysCoordCreate} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which coordinate is to be added.
!     \item[physCoord]
!          A previously-created {\tt ESMF\_PhysCoord} to add to the PhysGrid.
!     \item[{[dimOrder]}]
!          Dimension to which this coordinate will be assigned (e.g. x coord =1,
!          (y coord = 2).  If not supplied, dimension order will be the
!          order in which a coordinate is added.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: idim, n
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     if coordinate array not allocated yet, do so now
!
      if (.not. associated(physgrid%ptr%coords)) then
        allocate(physgrid%ptr%coords(physgrid%ptr%numDims), stat=status)
        if (status /= ESMF_SUCCESS) then
          print *,'ERROR in PhysGridSetCoord: error allocating coord'
          return
        endif
      endif
!
!     add coordinate
!
      if (present(dimOrder)) then
         physgrid%ptr%coords(dimOrder) = physCoord
      else
         order_loop: do idim = 1,physgrid%ptr%numDims
            if (.not.associated(physgrid%ptr%coords(idim)%ptr)) then
               physgrid%ptr%coords(idim) = physCoord
               exit order_loop
            endif
         enddo order_loop
         if (idim > physgrid%ptr%numDims) then
            print *,'ERROR in PhysGridSetCoord: too many coords defined'
            return
         endif
      endif
!
!     set return code
!
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetCoord

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetCoord - Gets a physical coordinate from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetCoord(physgrid, physCoord, &
                                       dimOrder, name, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(in) :: physgrid
      type (ESMF_PhysCoord), intent(out) :: physCoord
      integer, intent(in), optional :: dimOrder
      character (*), intent(in), optional :: name
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Gets a {\tt ESMF\_PhysCoord} from a {\tt ESMF\_PhysGrid}.  Either  
!     the dimension assigned to this coordinate (when attached to the
!     PhysGrid) or the name must be supplied.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} from which coordinate is to be retrieved.
!     \item[physCoord]
!          An {\tt ESMF\_PhysCoord} where the retrieved coord will be stored.
!     \item[{[dimOrder]}]
!          If known, the dimension index assigned to this coordinate when
!          it was added to PhysGrid.
!     \item[{[name]}]
!          If dim\_order not supplied, a name must be supplied to identify
!          the coordinate to be retrieved.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: idim, n
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      logical :: found                            ! flag for name search
      character(len=ESMF_MAXSTR) :: nameTmp
!
!     Initialize return code
!
      status = ESMF_SUCCESS
      rcpresent = .FALSE. 
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     if dimension supplied, just grab the coordinate
!
      if (present(dimOrder)) then
         physCoord = physgrid%ptr%coords(dimOrder)
!
!     if a name supplied, search for a coord with given name
!
      elseif (present(name)) then
         found = .false.
         name_srch: do n=1,physgrid%ptr%numDims
            call ESMF_PhysCoordGet(physgrid%ptr%coords(n), name=nameTmp, rc=status)
            if (name == trim(nameTmp)) then
               found = .true.
               idim = n
               exit name_srch
            endif
         end do name_srch
         
         if (found) then
            physCoord = physgrid%ptr%coords(idim)
         else
            print *,'ERROR in PhysGridGetCoord: no coordinate with that name'
            return
         endif
!
!     if neither supplied, return an error
!
      else
         print *,'ERROR in PhysGridGetCoord: name or dim must be supplied'
         return
      endif
!
!     set return code
!
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetCoord

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetLocations - Gets locations from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetLocations(physgrid, name, locationArray, &
                                                                   total, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(in) :: physgrid
      character(*), intent(out), optional :: name
      type(ESMF_Array), dimension(:), intent(inout), optional::locationArray 
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Retrieves information about defined locations of a {\tt ESMF\_PhysGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} holding locations to be queried.
!     \item[{[name]}]
!          Name assigned to the locations.
!     \item[{[locationArray]}]
!          Array of {\tt ESMF\_Array}s containing the coordinates in each
!          physical dimension for each location.
!          The array is assumed to be dimensioned (num\_dims)
!          while the {\tt ESMF\_Array} would typically be consistent with
!          other grid arrays.
!     \item[{[total]}]
!          Logical. If TRUE, return the total coordinates including internally
!          generated boundary cells. If FALSE return the
!          computational cells (which is what the user will be expecting.)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      logical :: found                               ! name search flag

!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     if name requested, get name
!
      if (present(name)) then
         call ESMF_GetName(physgrid%ptr%locations%base, name, status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridGetLocations: error getting name"
            return
         endif
      endif
!
!     if locations requested, get them
!
      if (present(locationArray)) then
         locationArray = physgrid%ptr%locations%compLocations
         if (present(total)) then
           if (total) locationArray = physgrid%ptr%locations%totalLocations
         endif
      endif
!
!     set return code
!
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetLocations

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetLocations - Sets grid regions from input array.

! !INTERFACE:
      subroutine ESMF_PhysGridSetLocations(physgrid, locationArray, name, &
                                                                  total, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type (ESMF_Array), dimension(:), pointer :: locationArray
      character(*), intent(in), optional :: name
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Defines a set of locations in a {\tt ESMF\_PhysGrid} using 
!     user-defined data sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which locations are to be added.
!     \item[{[locationArray]}]
!          Array of {\tt ESMF\_Array}s containing the coordinates in each
!          physical dimension for each location.
!          The array is assumed to be dimensioned (num\_dims)
!          while the {\tt ESMF\_Array} would typically be consistent with
!          other grid arrays.
!     \item[{[name]}]
!          Optional name to assign to the locations.
!     \item[{[total]}]
!          Logical. If TRUE, set the total coordinates including internally
!          generated boundary cells are to be returned.  If FALSE return the
!          computational cells, which is what the user expects to see.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      character (len=ESMF_MAXSTR) :: name_tmp        ! temp for name creation 
!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     if name specified, add name.  otherwise, make one up based on PhysGrid.
!
      if (present(name)) then
        call ESMF_SetName(physgrid%ptr%regions%base, name, &
                          "PhysGridLocations", status)
      else
        call ESMF_GetName(physgrid%ptr%base, name_tmp, status)
        name_tmp = trim(name_tmp) // 'Locations'
        call ESMF_SetName(physgrid%ptr%regions%base, name_tmp, &
                          "PhysGridLocations", status)
      endif
      if (status /= ESMF_SUCCESS) then
         print *, "ERROR in ESMF_PhysGridSetLocations: error setting name"
         return
      endif
!
!     set locations
!
      if (present(total)) then
          if (total) physgrid%ptr%locations%totalLocations => locationArray
          if (.not.total) physgrid%ptr%locations%compLocations => locationArray
      else
          physgrid%ptr%locations%compLocations => locationArray
      endif
!
!     set return code
!
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetLocations

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetRegions - Sets grid regions from input array.

! !INTERFACE:
      subroutine ESMF_PhysGridSetRegions(physgrid, regionType, name, &
                                         numVertices, vertexArray,   &
                                         ellipseArray, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(inout) :: physgrid

      type(ESMF_RegionType), intent(in) :: &
         regionType           ! type of region (polygonal, elliptical)

      character(*), intent(in), optional :: name ! name to assign to regions

      integer, intent(in), optional :: numVertices
                              ! max number of vertices for polygonal regions

      type(ESMF_Array), dimension(:,:), pointer, optional :: &
         vertexArray          ! array of ESMF_arrays containing vertex
                              ! coordinates for each vertex of each polygonal
                              ! region in each physical dimension
                              ! dimensions are assumed num_vertices, numDims

      type(ESMF_Array), dimension(2), intent(in), optional :: &
         ellipseArray         ! array of ESMF_arrays containing ellipse
                              !  parameters describing elliptical region at
                              !  each grid point

      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Defines a set of regions or cells in a {\tt ESMF\_PhysGrid} using 
!     user-defined data sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which regions are to be added.
!     \item[regionType]
!          {\tt ESMF\_RegionType} denoting the type of region (e.g. polygonal,
!          elliptical)
!     \item[{[name]}]
!          Optional name to assign to the regions.
!     \item[{[numVertices]}]
!          If regions are polygonal, the number of vertices needed to describe
!          the regions.  Note that if the number of vertices varies, this 
!          should be set to the maximum number - regions with fewer vertices
!          are defined with degenerate vertex points.
!     \item[{[vertexArray]}]
!          Array of {\tt ESMF\_Array}s containing the coordinates in each
!          physical dimension for each vertex point at each logical grid point. 
!          The array is assumed to be dimensioned (num\_dims,num\_vertices)
!          while the {\tt ESMF\_Array} would typically be consistent with
!          other grid arrays. The order of dimensions is assumed to be the
!          same order defined for the PhysGrid coordinates.
!     \item[{[ellipseArray]}]
!          Array of {\tt ESMF\_Array}s containing the two parameters
!          necessary for describing the elliptical region at each grid point.
!          The array is dimensioned (2) while the {\tt ESMF\_Array} would 
!          typically be consistent with other grid arrays.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      character (len=ESMF_MAXSTR) :: name_tmp        ! temp for name creation 
!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     if name specified, add name.  otherwise, make one up based on PhysGrid.
!
      if (present(name)) then
        call ESMF_SetName(physgrid%ptr%regions%base, name, &
                          "PhysGridRegions", status)
      else
        call ESMF_GetName(physgrid%ptr%base, name_tmp, status)
        name_tmp = trim(name_tmp) // 'Region'
        call ESMF_SetName(physgrid%ptr%regions%base, name_tmp, &
                          "PhysGridRegions", status)
      endif
      if (status /= ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridSetRegions: error setting name"
        return
      endif
!
!     set region type
!
      physgrid%ptr%regions%regionType = regionType
!
!     set region arrays depending on type of region
!
      if (regionType == ESMF_RegionType_Polygon) then
        physgrid%ptr%regions%numVertices = numVertices
        physgrid%ptr%regions%vertices => vertexArray

      else if (regionType == ESMF_RegionType_Ellipse) then
        physgrid%ptr%regions%ellipse = ellipseArray

      else
        print *, "ERROR in ESMF_PhysGridSetRegions: unknown region type"
        return

      endif
!
!     define bounding box for each region to aid in future searches
!
      if (regionType == ESMF_RegionType_Polygon) then
         !TODO: create bbox array for polygons

      else if (regionType == ESMF_RegionType_Ellipse) then
         !TODO: define bounding box for elliptical region

      else
         print *, "ERROR in ESMF_PhysGridSetRegions: unknown region type"
         return

      endif
!
!     set return code
!
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetRegions

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetRegions - Gets region information from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetRegions(physgrid, regionType, name, &
                                         numVertices, vertexArray,   &
                                         ellipseArray, bboxArray, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(in) :: physgrid

      type(ESMF_RegionType), intent(inout), optional :: &
         regionType           ! type of region (polygonal, elliptical)

      character(*), intent(inout), optional :: name ! name to assign to regions

      integer, intent(inout), optional :: numVertices
                              ! max number of vertices for polygonal regions

      type (ESMF_Array), dimension(:,:), intent(inout), optional :: &
         vertexArray          ! array of ESMF_arrays containing vertex
                              ! coordinates for each vertex of each polygonal
                              ! region in each physical dimension
                              ! dimensions are assumed num_vertices, numDims

      type (ESMF_Array), dimension(2), intent(inout), optional :: &
         ellipseArray         ! array of ESMF_arrays containing ellipse
                              ! parameters describing elliptical region at
                              ! each grid point

      type (ESMF_Array), dimension(:,:), intent(inout), optional :: &
         bboxArray            ! array of ESMF_arrays containing bounding
                              ! boxes for each region
                              ! dimensions are assumed numDims,2 (1=min,2=max)

      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Retrieves information about defined regions of a {\tt ESMF\_PhysGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} holding regions to be queried.
!     \item[{[regionType]}]
!          {\tt ESMF\_RegionType} denoting the type of region (e.g. polygonal,
!          elliptical)
!     \item[{[name]}]
!          Name assigned to the regions.
!     \item[{[numVertices]}]
!          If regions are polygonal, the number of vertices needed to describe
!          the regions.  Note that if the number of vertices varies, this 
!          should be set to the maximum number - regions with fewer vertices
!          are defined with degenerate vertex points.
!     \item[{[vertexArray]}]
!          Array of {\tt ESMF\_Array}s containing the coordinates in each
!          physical dimension for each vertex point at each logical grid point. 
!          The array is assumed to be dimensioned (num\_dims,num\_vertices)
!          while the {\tt ESMF\_Array} would typically be consistent with
!          other grid arrays.
!     \item[{[ellipseArray]}]
!          Array of {\tt ESMF\_Array}s containing the two parameters
!          necessary for describing the elliptical region at each grid point.
!          The array is dimensioned (2) while the {\tt ESMF\_Array} would 
!          typically be consistent with other grid arrays.
!     \item[{[bboxArray]}]
!          Array of {\tt ESMF\_Array}s containing the bounding box
!          of each defined region.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      logical :: found                               ! name search flag

!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     if name requested, get name
!
      if (present(name)) then
         call ESMF_GetName(physgrid%ptr%regions%base, name, status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridGetRegions: error getting name"
            return
         endif
      endif
!
!     if number of vertices requested, get it
!
      if (present(numVertices)) then
         if (physgrid%ptr%regions%regionType == ESMF_RegionType_Polygon) then
            numVertices = physgrid%ptr%regions%numVertices
         else
            print *, "ERROR in ESMF_PhysGridGetRegions: invalid region type"
            return
         endif
      endif
!
!     if vertex coordinates requested, get them
!
      if (present(vertexArray)) then
         if (physgrid%ptr%regions%regionType == ESMF_RegionType_Polygon) then
            !TODO: consistency check for array sizes, shapes
            !TODO: return pointer or copy array?
            vertexArray = physgrid%ptr%regions%vertices
         else
            print *, "ERROR in ESMF_PhysGridGetRegions: invalid region type"
            return
         endif
      endif
!
!     if ellipses requested, get them
!
      if (present(ellipseArray)) then
         if (physgrid%ptr%regions%regionType == ESMF_RegionType_Ellipse) then
            !TODO: consistency check for array sizes, shapes
            !TODO: return pointer or copy array?
            ellipseArray = physgrid%ptr%regions%ellipse
         else
            print *, "ERROR in ESMF_PhysGridGetRegions: invalid region type"
            return
         endif
      endif
!
!     if bounding boxes requested, get them
!
      if (present(bboxArray)) then
         !TODO: consistency check for array sizes, shapes
         !TODO: return pointer or copy array?
         !bboxArray = physgrid%ptr%regions%bbox
      endif
!
!     set return code
!
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetRegions

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetMask - Sets grid mask from input array.

! !INTERFACE:
      subroutine ESMF_PhysGridSetMask(physgrid, maskArray, maskType, &
                                      name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(inout) :: physgrid

      ! for some reason the pgi compiler does not like intent(in)
      ! on the next line.  it says the types do not match.
      ! i will just take it out for now.   nsc 15oct03
      !type(ESMF_Array), intent(in) :: maskArray
      type(ESMF_Array) :: maskArray
                            ! array containing mask value for each cell

      type(ESMF_GridMaskType), intent(in) :: maskType
                           ! type of mask (logical, mult, regionID)

      character(*), intent(in) :: name ! name to assign to mask

      integer, intent(out), optional :: id  ! id assigned to mask

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Assigns a mask to a {\tt ESMF\_PhysGrid} using user-defined data 
!     sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which mask is to be added.
!     \item[maskArray]
!          Array containing mask values at for each grid cell.
!     \item[name]
!          Name to assign to mask.
!     \item[maskType]
!          {\tt ESMF\_GridMaskType} describing type of mask (e.g. logical,
!          multiplicative, region ID).
!     \item[{[id]}]
!          Integer id assigned to mask which allows faster access
!          when retrieving mask.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n, numMaskOld, numMaskNew
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      type(ESMF_GridMask), dimension(:), allocatable :: tempMask
                                                     ! temporary array of masks
!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     increment mask counter
!
      numMaskOld = physgrid%ptr%numMasks
      numMaskNew = numMaskOld + 1
!
!     if first mask, allocate mask array
!
      if (numMaskNew == 1) then
         allocate(physgrid%ptr%masks(1), stat=status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridSetMask: mask allocate"
            return
         endif
!
!     if not first mask, resize mask array to make room for new mask
!
      else
         allocate(tempMask(numMaskOld), stat=status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridSetMask: tempMask allocate"
            return
         endif

         ! store old values before resizing array
         do n = 1, numMaskOld
            tempMask(n) = physgrid%ptr%masks(n)
         enddo

         ! destroy old array to create new one
         deallocate(physgrid%ptr%masks, stat=status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridSetMask: mask deallocate"
            return
         endif

         ! allocate new mask array of correct size
         allocate(physgrid%ptr%masks(numMaskNew), stat=status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridSetMask: mask reallocate"
            return
         endif

         ! fill new array with old values
         do n = 1, numMaskOld
            physgrid%ptr%masks(n) = tempMask(n)
         enddo

         deallocate(tempMask, stat=status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridSetMask: tempMask deallocate"
            return
         endif

      endif
!
!     reset number of masks and add new mask
!
      if (present(id)) id = numMaskNew
      physgrid%ptr%numMasks = numMaskNew

      call ESMF_BaseCreate(physgrid%ptr%masks(numMaskNew)%base, &
                                            "PhysGridMask", name, 0, status)
      if(status /= ESMF_SUCCESS) then
         print *, "ERROR in ESMF_PhysGridCreate: Mask BaseCreate"
         return
      endif

      physgrid%ptr%masks(numMaskNew)%maskType = maskType
      physgrid%ptr%masks(numMaskNew)%data = maskArray

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetMask - Gets grid mask from PhysGrid object.

! !INTERFACE:
      subroutine ESMF_PhysGridGetMask(physgrid, maskArray, name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(in) :: physgrid

      type(ESMF_Array), intent(out) :: &
         maskArray         ! array containing mask value for each cell

      character(*), intent(in), optional :: name ! name of mask to get

      integer, intent(in), optional :: id  ! id for mask if known

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Retrieves a mask from a {\tt ESMF\_PhysGrid} based on either a name
!     or previously-assigned id.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} from which mask is to be retrieved.
!     \item[maskArray]
!          Array containing mask value at each grid cell.
!     \item[{[name]}]
!          If supplied, mask matching this name will be returned.
!     \item[{[id]}]
!          If supplied, mask matching this assigned id will be returned.
!          This is a faster method as the id refers to exact mask array
!          location.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      logical :: found                               ! name search flag
      character (len=ESMF_MAXSTR) :: name_tmp        ! temp for name check 

!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     if id supplied, check for valid id and return appropriate mask
!
      if (present(id)) then

         ! check for valid id
         if (id < 1 .or. id > physgrid%ptr%numMasks) then
            print *, "ERROR in ESMF_PhysGridGetMask: invalid mask id"
            return
         endif

         ! get mask associated with id
         maskArray = physgrid%ptr%masks(id)%data
!
!     if name supplied, check for valid id and return appropriate mask
!
      else if (present(name)) then

         found = .false.
         name_loop: do n=1,physgrid%ptr%numMasks
            
            call ESMF_GetName(physgrid%ptr%masks(n)%base, name_tmp, status)
            if(status /= ESMF_SUCCESS) then
               print *, "ERROR in ESMF_PhysGridGetMask: error retrieving name"
               return
            endif

            if (trim(name) == trim(name_tmp)) then
               maskArray = physgrid%ptr%masks(id)%data
               found = .true.
               exit name_loop
            endif
         end do name_loop

         if (.not. found) then
            print *, "ERROR in ESMF_PhysGridGetMask: no mask matches name"
            print *, "Requested name:",trim(name)
            return
         endif
!
!     if we enter this else branch, neither id nor mask has been 
!     supplied so return error
!
      else
         print *, "ERROR in ESMF_PhysGridGetMask: id or name must be supplied"
         return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetMetric - Sets grid metric from input array.

! !INTERFACE:
      subroutine ESMF_PhysGridSetMetric(physgrid, metricArray, name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(inout) :: physgrid

      type(ESMF_Array), intent(inout) :: metricArray
                                            ! array containing metric value for
                                            ! each cell

      character(*), intent(in) :: name      ! name to assign to metric

      integer, intent(out), optional :: id  ! id assigned to metric

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Assigns a metric to a {\tt ESMF\_PhysGrid} using user-defined data 
!     sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which metric is to be added.
!     \item[metricArray]
!          Array containing metric values at each grid cell.
!     \item[name]
!          Name to assign to metric.
!     \item[{[id]}]
!          Integer id assigned to metric which allows faster access
!          when retrieving metric.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: n, numMetricOld, numMetricNew
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      type(ESMF_Array), dimension(:), allocatable :: tempMetric
                                                    ! temporary array of metrics
!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     increment metric counter
!
      numMetricOld = physgrid%ptr%numMetrics
      numMetricNew = numMetricOld + 1
!
!     if first metric, allocate metric array
!
      if (numMetricNew == 1) then
        allocate(physgrid%ptr%metrics(1), stat=status)
        if (status /= ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetric: metric allocate"
          return
        endif
!
!     if not first metric, resize metric array to make room for new metric
!
      else
        allocate(tempMetric(numMetricOld), stat=status)
        if (status /= ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetric: tempMetric allocate"
          return
        endif

        ! store old values before resizing array
        do n = 1, numMetricOld
          tempMetric(n) = physgrid%ptr%metrics(n)
        enddo

        ! destroy old array to create new one
        deallocate(physgrid%ptr%metrics, stat=status)
        if (status /= ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetric: metric deallocate"
          return
        endif

        ! allocate new metric array of correct size
        allocate(physgrid%ptr%metrics(numMetricNew), stat=status)
        if (status /= ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetric: metric reallocate"
          return
        endif

        ! fill new array with old values
        do n = 1, numMetricOld
          physgrid%ptr%metrics(n) = tempMetric(n)
        enddo

        deallocate(tempMetric, stat=status)
        if (status /= ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetric: tempMetric deallocate"
          return
        endif
      endif
!
!     reset number of metrics and add new metric
!
      if (present(id)) id = numMetricNew
      physgrid%ptr%numMetrics = numMetricNew

      call ESMF_ArraySetName(physgrid%ptr%metrics(numMetricNew), name, status)
      if (status /= ESMF_SUCCESS) then
         print *, "ERROR in ESMF_PhysGridSetMetric: error setting metric name"
         return
      endif

      physgrid%ptr%metrics(numMetricNew) = metricArray

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetMetric

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetMetric - Gets grid metric from PhysGrid object.

! !INTERFACE:
      subroutine ESMF_PhysGridGetMetric(physgrid, metricArray, name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGrid), intent(in) :: physgrid

      type(ESMF_Array), intent(out) :: &
         metricArray         ! array containing metric value for each cell

      character(*), intent(in), optional :: name ! name of metric to get

      integer, intent(in), optional :: id  ! id for metric if known

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Assigns a metric to a {\tt ESMF\_PhysGrid} using user-defined data 
!     sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} from which metric is to be retrieved.
!     \item[metricArray]
!          Array containing metric values at for each grid cell.
!     \item[{[name]}]
!          If supplied, metric matching this name will be returned.
!     \item[{[id]}]
!          If supplied, metric matching this assigned id will be returned.
!          This is a faster method as the id refers to exact metric array
!          location.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      logical :: found                               ! name search flag
      character (len=ESMF_MAXSTR) :: name_tmp        ! temp for name check 

!
!     Initialize return code
!
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     if id supplied, check for valid id and return appropriate metric
!
      if (present(id)) then

        ! check for valid id
        if (id < 1 .or. id > physgrid%ptr%numMetrics) then
          print *, "ERROR in ESMF_PhysGridGetMetric: invalid metric id"
          return
        endif

        ! get metric associated with id
        metricArray = physgrid%ptr%metrics(id)
!
!     if name supplied, check for valid id and return appropriate metric
!
      else if (present(name)) then

        found = .false.
        name_loop: do n=1,physgrid%ptr%numMetrics
            
          call ESMF_ArrayGetName(physgrid%ptr%metrics(n), name_tmp, status)
          if(status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_PhysGridGetMetric: error retrieving name"
            return
          endif

          if (trim(name) == trim(name_tmp)) then
            metricArray = physgrid%ptr%metrics(id)
            found = .true.
            exit name_loop
          endif
        end do name_loop

        if (.not. found) then
          print *, "ERROR in ESMF_PhysGridGetMetric: no metric matches name"
          print *, "Requested name:",trim(name)
          return
        endif

!
!     if we enter this else branch, neither id nor name has been 
!     supplied so return error
!
      else
        print *, "ERROR in ESMF_PhysGridGetMetric: id or name must be supplied"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetMetric

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridValidate - Check internal consistency of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridValidate(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a {\tt ESMF\_PhysGrid} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
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
      end subroutine ESMF_PhysGridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridPrint - Print the contents of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridPrint(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a {\tt ESMF\_PhysGrid}.  
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Class to be queried.
!     \item[{[opt]}]
!          Print ptions that control the type of information and level of 
!          detail.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
      integer :: i

! This code will surely change, but for development purposes it
! is necessary to have some information available currently.

      print *, 'Physgrid:'
      print *, ' Dimensions:', physgrid%ptr%numDims

      print *, ' Coordinate Extents:'

! The global and local coordinate extents
      print *, '  Global Extents:'
      do i=1, physgrid%ptr%numDims
  !       print *, '   ', physgrid%ptr%global_min(i), &
  !       ',', physgrid%ptr%global_max(i)
      if (i .ne. physgrid%ptr%numDims) print *, '      x'
      enddo

      print *, '  Local Extents:'
      do i=1, physgrid%ptr%numDims
  !       print *, '   ', physgrid%ptr%local_min(i), &
  !       ',', physgrid%ptr%local_max(i)
      if (i .ne. physgrid%ptr%numDims) print *, '      x'
      enddo

      end subroutine ESMF_PhysGridPrint

!------------------------------------------------------------------------------
!!BOP
!! !IROUTINE: ESMF_PhysGridSearchBboxSphericalPoint - Search grid for a point
!
!! !INTERFACE:
!      subroutine ESMF_PhysGridSearchBboxSphericalPoint(dst_add, x, y, DEid, &
!                                  phys_grid, dist_grid, rc)
!
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) ::
!         dst_add      ! location in grid of grid cell containing search point
!
!      real(kind=ESMF_KIND_R8), intent(in) :: &
!         x,y          ! x,y coordinates of search point 
!
!      integer, intent(in) :: &
!         DEid         ! DE which owns the search point
!
!      type(ESMF_PhysGrid), intent(in) :: &
!         phys_grid  ! phys grid to search for location of point
!
!      type(ESMF_DistGrid), intent(in) :: &
!         dist_grid  ! dist grid associated with phys grid above
!
!      integer, intent(out), optional :: rc  ! return code
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the grid of a grid cell 
!!     containing the point given by the input x,y coordinates.  This 
!!     instantiation uses a simple bounding box check to search the
!!     grid and is therefore only applicable to grids where the logical
!!     and physical axes are aligned and logically-rectangular.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dst\_add]
!!          Address of grid cell containing the search point.
!!     \item[x,y]
!!          Coordinates of search point.
!!     \item[DEid]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[phys\_grid]
!!          {\tt ESMF\_PhysGrid} to search for location.
!!     \item[{[dis\_grid]
!!          {\tt ESMF\_DistGrid} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!!
!!     broadcast the point to all DEs
!!
!      if (my_DE == DEid) then
!         point(1) = x
!         point(2) = y
!      else
!         point = 0
!      endif
!
!      call ESMF_broadcast(point, DEid) 
!
!!
!!     initialize destination address to zero
!!
!      dst_add = 0
!
!!
!!     now search all the local DEs for this point
!!
!
!      search_loop: do iDE=1,nlocal_DEs
!
!!
!!        first check the bounding box for the entire DE
!!
!         
!         extract local_min_x, local_min_y, local_max_x, local_max_y from
!            phys_grid
!
!!
!!        make sure these are all in the same longitude range
!!        assuming degrees, but may need to check radians if
!!        axis units are in radians.  also assumes longitude
!!        is x axis.
!!
!         
!         if (local_min_x - point(1) >  270.) local_min_x = local_min_x - 360.
!         if (local_max_x - point(1) >  270.) local_max_x = local_max_x - 360.
!         if (local_min_x - point(1) > -270.) local_min_x = local_min_x + 360.
!         if (local_max_x - point(1) > -270.) local_max_x = local_max_x + 360.
!
!         if (point(1) < local_min_x .or. &
!             point(1) > local_max_x .or. &
!             point(2) < local_min_y .or. &
!             point(2) > local_max_y) exit search_loop ! point not in this DE
!
!!
!!        point may be somewhere in this DE, loop through the cells on the DE
!!
!
!         get jb,je,ib,ie, grid corners, grid centers
!
!         do j=jb,je     !jb,je correspond to exclusive domain on this DE
!         do i=ib,ie     !ib,ie ditto
!
!!
!!           check bounding box of local grid cell
!!
!            local_min_x = minval(corner_x(:,i,j))
!            local_max_x = maxval(corner_x(:,i,j))
!            local_min_y = minval(corner_y(:,i,j))
!            local_min_y = maxval(corner_y(:,i,j))
!
!            if (local_min_x - point(1) >  270.) local_min_x = local_min_x - 360.
!            if (local_max_x - point(1) >  270.) local_max_x = local_max_x - 360.
!            if (local_min_x - point(1) > -270.) local_min_x = local_min_x + 360.
!            if (local_max_x - point(1) > -270.) local_max_x = local_max_x + 360.
!
!            if (point(1) >= local_min_x .and. &
!                point(1) <= local_max_x .and. &
!                point(2) >= local_min_y .and. &
!                point(2) <= local_max_y) then ! point is in this cell
!               found = 1         ! found flag
!               owner_DE = my_DE  ! DE id for this DE
!               dst_i = i         ! local address of this cell
!               dst_j = j         ! local address of this cell
!               exit search_loop
!            endif
!
!         end do
!         end do
!      end do search_loop
!
!!
!!     now do a global sum of found flag to see if search returned
!!     a unique cell.  if not, return an error.
!!
!      ncells = global_sum(found)
!      if (ncells > 1) then
!         print *,'PhysGridSearch: more than one cell contains this point'
!         rc = ESMF_FAILURE
!      endif
!
!!
!!     if cell is found, use a global_maxval to get the address to all DEs
!!
!
!      if (ncells == 1) then
!         dst_add(1) = global_maxval(owner_DE)
!         dst_add(2) = global_maxval(dst_i)
!         dst_add(3) = global_maxval(dst_j)
!      endif
!            
!      set return code
!
!      end subroutine ESMF_PhysGridSearchBboxSphericalPoint
!
!------------------------------------------------------------------------------
!!BOP
!! !IROUTINE: ESMF_PhysGridSearchBboxCartesianPoint - Search grid for a point
!
!! !INTERFACE:
!      subroutine ESMF_PhysGridSearchBboxCartesianPoint(dst_add, x, y, DEid, &
!                                  phys_grid, dist_grid, rc)
!
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) ::
!         dst_add      ! location in grid of grid cell containing search point
!
!      real(kind=ESMF_KIND_R8), intent(in) :: &
!         x,y          ! x,y coordinates of search point 
!
!      integer, intent(in) :: &
!         DEid         ! DE which owns the search point
!
!      type(ESMF_PhysGrid), intent(in) :: &
!         phys_grid  ! phys grid to search for location of point
!
!      type(ESMF_DistGrid), intent(in) :: &
!         dist_grid  ! dist grid associated with phys grid above
!
!      integer, intent(out), optional :: rc  ! return code
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the grid of a grid cell 
!!     containing the point given by the input x,y coordinates.  This 
!!     instantiation uses a simple bounding box check to search the
!!     grid and is therefore only applicable to grids where the logical
!!     and physical axes are aligned and logically-rectangular.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dst\_add]
!!          Address of grid cell containing the search point.
!!     \item[x,y]
!!          Coordinates of search point.
!!     \item[DEid]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[phys\_grid]
!!          {\tt ESMF\_PhysGrid} to search for location.
!!     \item[{[dis\_grid]
!!          {\tt ESMF\_DistGrid} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!!
!!     broadcast the point to all DEs
!!
!      if (my_DE == DEid) then
!         point(1) = x
!         point(2) = y
!      else
!         point = 0
!      endif
!
!      call ESMF_broadcast(point, DEid) 
!
!!
!!     initialize destination address to zero
!!
!      dst_add = 0
!
!!
!!     now search all the local DEs for this point
!!
!
!      search_loop: do iDE=1,nlocal_DEs
!
!!
!!        first check the bounding box for the entire DE
!!
!         
!         extract local_min_x, local_min_y, local_max_x, local_max_y from
!            phys_grid
!
!         if (point(1) < local_min_x .or. &
!             point(1) > local_max_x .or. &
!             point(2) < local_min_y .or. &
!             point(2) > local_max_y) exit search_loop ! point not in this DE
!
!!
!!        point may be somewhere in this DE, loop through the cells on the DE
!!
!
!         get jb,je,ib,ie, grid corners, grid centers
!
!         do j=jb,je     !jb,je correspond to exclusive domain on this DE
!         do i=ib,ie     !ib,ie ditto
!
!!
!!           check bounding box of local grid cell
!!
!            local_min_x = minval(corner_x(:,i,j))
!            local_max_x = maxval(corner_x(:,i,j))
!            local_min_y = minval(corner_y(:,i,j))
!            local_min_y = maxval(corner_y(:,i,j))
!
!            if (point(1) >= local_min_x .and. &
!                point(1) <= local_max_x .and. &
!                point(2) >= local_min_y .and. &
!                point(2) <= local_max_y) then ! point is in this cell
!               found = 1         ! found flag
!               owner_DE = my_DE  ! DE id for this DE
!               dst_i = i         ! local address of this cell
!               dst_j = j         ! local address of this cell
!               exit search_loop
!            endif
!
!         end do
!         end do
!      end do search_loop
!
!!
!!     now do a global sum of found flag to see if search returned
!!     a unique cell.  if not, return an error.
!!
!      ncells = global_sum(found)
!      if (ncells > 1) then
!         print *,'PhysGridSearch: more than one cell contains this point'
!         rc = ESMF_FAILURE
!      endif
!
!!
!!     if cell is found, use a global_maxval to get the address to all DEs
!!
!
!      if (ncells == 1) then
!         dst_add(1) = global_maxval(owner_DE)
!         dst_add(2) = global_maxval(dst_i)
!         dst_add(3) = global_maxval(dst_j)
!      endif
!            
!      set return code
!
!      end subroutine ESMF_PhysGridSearchBboxCartesianPoint
!
!------------------------------------------------------------------------------
!!BOP
!! !IROUTINE: ESMF_PhysGridSearchGeneralSphericalPoint - Search grid for a point
!
!! !INTERFACE:
!      subroutine ESMF_PhysGridSearchGeneralSphericalPoint(dst_add, x, y, DEid, &
!                                  phys_grid, dist_grid, rc)
!
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) ::
!         dst_add      ! location in grid of grid cell containing search point
!
!      real(kind=ESMF_KIND_R8), intent(in) :: &
!         x,y          ! x,y coordinates of search point 
!
!      integer, intent(in) :: &
!         DEid         ! DE which owns the search point
!
!      type(ESMF_PhysGrid), intent(in) :: &
!         phys_grid  ! phys grid to search for location of point
!
!      type(ESMF_DistGrid), intent(in) :: &
!         dist_grid  ! dist grid associated with phys grid above
!
!      integer, intent(out), optional :: rc  ! return code
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the grid of a grid cell 
!!     containing the point given by the input x,y coordinates.  This 
!!     instantiation uses a general (cross-product) check to search the
!!     grid and works for all cells that are non-convex.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dst\_add]
!!          Address of grid cell containing the search point.
!!     \item[x,y]
!!          Coordinates of search point.
!!     \item[DEid]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[phys\_grid]
!!          {\tt ESMF\_PhysGrid} to search for location.
!!     \item[{[dis\_grid]
!!          {\tt ESMF\_DistGrid} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!!
!!     broadcast the point to all DEs
!!
!      if (my_DE == DEid) then
!         point(1) = x
!         point(2) = y
!      else
!         point = 0
!      endif
!
!      call ESMF_broadcast(point, DEid) 
!
!!
!!     initialize destination address to zero
!!
!      dst_add = 0
!
!!
!!     now search all the local DEs for this point
!!
!
!      search_loop: do iDE=1,nlocal_DEs
!
!!
!!        first check the bounding box for the entire DE
!!
!         
!         extract local_min_x, local_min_y, local_max_x, local_max_y from
!            phys_grid
!
!!
!!        make sure these are all in the same longitude range
!!        assuming degrees, but may need to check radians if
!!        axis units are in radians.  also assumes longitude
!!        is x axis.
!!
!         
!         if (local_min_x - point(1) >  270.) local_min_x = local_min_x - 360.
!         if (local_max_x - point(1) >  270.) local_max_x = local_max_x - 360.
!         if (local_min_x - point(1) > -270.) local_min_x = local_min_x + 360.
!         if (local_max_x - point(1) > -270.) local_max_x = local_max_x + 360.
!
!         if (point(1) < local_min_x .or. &
!             point(1) > local_max_x .or. &
!             point(2) < local_min_y .or. &
!             point(2) > local_max_y) exit search_loop ! point not in this DE
!
!!
!!        point may be somewhere in this DE, loop through the cells on the DE
!!
!
!         get jb,je,ib,ie, grid corners, grid centers
!
!         grid_loop_j: do j=jb,je   !ib,ie,jb,je correspond to exclusive 
!         do i=ib,ie                !domain on this DE
!
!!
!!           check bounding box of local grid cell
!!
!            local_min_x = minval(corner_x(:,i,j))
!            local_max_x = maxval(corner_x(:,i,j))
!            local_min_y = minval(corner_y(:,i,j))
!            local_min_y = maxval(corner_y(:,i,j))
!
!            if (local_min_x - point(1) >  270.) local_min_x = local_min_x - 360.
!            if (local_max_x - point(1) >  270.) local_max_x = local_max_x - 360.
!            if (local_min_x - point(1) > -270.) local_min_x = local_min_x + 360.
!            if (local_max_x - point(1) > -270.) local_max_x = local_max_x + 360.
!
!            if (point(1) >= local_min_x .and. &
!                point(1) <= local_max_x .and. &
!                point(2) >= local_min_y .and. &
!                point(2) <= local_max_y) then 
!!
!!              point may be in this cell - do more robust check
!!
!               cell_corner_x = corner_x(:,i,j)
!
!!              check for longitude range
!
!               where (cell_corner_x - point(1) >  270.) cell_corner_x = &
!                                                        cell_corner_x - 360.
!               where (cell_corner_x - point(1) > -270.) cell_corner_x = &
!                                                        cell_corner_x + 360.
!
!               found = ESMF_PhysGridCrossProductCheck(point, cell_corner_x, & 
!                                                             corner_y(:,i,j), rc)
!               if (found == 1) then ! found point in this cell
!                  owner_DE = my_DE  ! DE id for this DE
!                  dst_i = i         ! local address of this cell
!                  dst_j = j         ! local address of this cell
!                  exit search_loop
!               endif ! found check
!            endif ! bbox check
!
!         end do
!         end do
!      end do search_loop
!
!!
!!     now do a global sum of found flag to see if search returned
!!     a unique cell.  if not, return an error.
!!
!      ncells = global_sum(found)
!      if (ncells > 1) then
!         print *,'PhysGridSearch: more than one cell contains this point'
!         rc = ESMF_FAILURE
!      endif
!
!!
!!     if cell is found, use a global_maxval to get the address to all DEs
!!
!
!      if (ncells == 1) then
!         dst_add(1) = global_maxval(owner_DE)
!         dst_add(2) = global_maxval(dst_i)
!         dst_add(3) = global_maxval(dst_j)
!      endif
!            
!      set return code
!
!      end subroutine ESMF_PhysGridSearchGeneralSphericalPoint
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridPointInCell - Checks whether cell contains point
!
! !INTERFACE:
      function ESMF_PhysGridPointInCell(pointX, pointY, cornerX, cornerY, rc)

!
! !RETURN VALUE:
      logical :: ESMF_PhysGridPointInCell ! true if point located in cell
!
! !ARGUMENTS:
      real(kind=ESMF_KIND_R8), intent(in) ::  pointX   ! x coord of search point 
      real(kind=ESMF_KIND_R8), intent(in) ::  pointY   ! y coord of search point 

      real(kind=ESMF_KIND_R8), dimension(:), intent(in) :: &
         cornerX,     & ! x coordinates of cell corners
         cornerY        ! y coordinates of cell corners 

      integer, intent(out), optional :: rc  ! return code
!
! !DESCRIPTION:
!     This routine checks a cell defined by the corner coordinates
!     to see if it contains the input point.  It uses a cross product
!     test which is valid for all non-convex cells. For points in
!     spherical coordinates, this routine assumes the longitudes of
!     all coordinates are in the same range.
!
!     The arguments are:
!     \begin{description}
!     \item[pointX]
!          x-coordinate of search point.
!     \item[pointY]
!          y-coordinate of search point.
!     \item[cornerX]
!          x-coordinate of grid cell corners.
!     \item[cornerY]
!          y-coordinate of grid cell corners.
!     \item[ESMF\_PhysGridPointInCell]
!          return value = 1 if cell contains point.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!  local variables
!

   integer      :: ncorn, next_n,   &! corner index
                   num_corners       ! number of corners in each grid cell

   real(kind=ESMF_KIND_R8) :: &
      vec1X, vec1Y,    &! components of the cell side vector
      vec2X, vec2Y,    &! components of the vector from vertex to point
      cross_product,   &! cross product of two vectors
      test_product,    &! 
      ref_product,     &! the cross product for first non-zero value
      sign_test,       &! test to see if cross products are same sign
      zero, one
   real(kind=ESMF_KIND_R8) :: minX, maxX, minY, maxY
!
!     set default return value
!
      ESMF_PhysGridPointInCell = .false.
!
!     quick and dirty screen test first
!
      minX = minval(cornerX)
      maxX = maxval(cornerX)
      minY = minval(cornerY)
      maxY = maxval(cornerY)
      if (pointX.lt.minX .or. pointX.gt.maxX .or. &
          pointY.lt.minY .or. pointY.gt.maxY) then
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif
!
!     set constants
!
      zero = 0.0
      one  = 1.0
      ref_product = zero
!
!     perform the cross product for each cell side
!
      num_corners = size(cornerX)

      corner_loop: do ncorn=1,num_corners
         next_n = MOD(ncorn,num_corners) + 1
!
!        here we take the cross product of the vector making
!        up each cell side with the vector formed by the vertex
!        and search point.  if all the cross products are
!        the same sign, the point is contained in the cell.
!
         vec1X = cornerX(next_n) - cornerX(ncorn)
         vec1Y = cornerY(next_n) - cornerY(ncorn)
         vec2X = pointX - cornerX(ncorn)
         vec2Y = pointY - cornerY(ncorn)
!
!        if search point coincident with vertex
!        then cell contains the point
!
         if (vec2X == 0 .and. vec2Y == 0) then
            ESMF_PhysGridPointInCell = .true.
            exit corner_loop
         endif
!
!        if cell side has zero length (degenerate vertices)
!         then skip the side and move on to the next
!
         if (vec1X == 0 .and. vec1Y == 0) cycle corner_loop

!        compute cross product

         cross_product = vec1X*vec2Y - vec2X*vec1Y
!
!        if the cross product is zero, the point
!        lies exactly on the side and is contained in the cell
!        TODO:  talk to Phil - not exactly true since if either all
!               three x-points or all three y-points are colinear the
!               cross product will be zero but the point not necessarily inside
!
         if (cross_product == zero) then
            ESMF_PhysGridPointInCell = .true.
            exit corner_loop
         endif
!
!        if this is the first side, set a reference cross product
!        to the current value
!        otherwise, if this product is a different sign than
!        previous (reference) cross products, exit the loop
!
         if (ref_product == zero) then
            ref_product = cross_product
            test_product = one
         else
            test_product = cross_product*ref_product
         endif
         if (test_product < zero) exit corner_loop ! x-prod has different sign

      end do corner_loop
!
!     if cross products all same sign this location contains the pt
!
      if (test_product > zero)  ESMF_PhysGridPointInCell = .true.

      if (present(rc)) rc = ESMF_SUCCESS
      return

      end function ESMF_PhysGridPointInCell

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridCompDistSpherical - compute distance spherical coords
!
! !INTERFACE:
      function ESMF_PhysGridCompDistSpherical(lon1, lat1, lon2, lat2, radius,rc)

!
! !RETURN VALUE:
      real(kind=ESMF_KIND_R8) :: ESMF_PhysGridCompDistSpherical
!
! !ARGUMENTS:
      real(kind=ESMF_KIND_R8), intent(in) :: &
         lon1, lat1,   & ! longitude and latitude coordinates of points between
         lon2, lat2      !   which to compute distance

      real(kind=ESMF_KIND_R8), intent(in), optional :: &
         radius          ! if supplied, distance will be in length units on
                         !   a sphere of this radius.

      integer, intent(out), optional :: rc  ! return code

!
! !DESCRIPTION:
!     This routine computes the distance between two points defined by
!     the input coordinates.  This version computes the angular distance 
!     in radians between points in spherical coordinates given longitude, 
!     latitude in degrees.  If an optional radius is supplied, the distance
!     will be returned in length units on the surface of a sphere of the
!     specified radius.
!
!     The arguments are:
!     \begin{description}
!     \item[lon1,lat1,lon2,lat2]
!          Longitude and latitude coordinates (in degrees) of points between 
!          which distance is computed.
!     \item[{[radius]}]
!          If supplied, distance will be returned in length units (same units
!          as radius) on the surface of a sphere of the specified radius.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!     local variables
!

      real(kind=ESMF_KIND_R8) :: &
         rlon1, rlat1, rlon2, rlat2  ! lon/lat in radians
      real(kind=ESMF_KIND_R8) :: pi

!
!     initialize return code
!
      if (present(rc)) rc = ESMF_SUCCESS
!
!     set constants
!
      pi = 3.1416d0   ! TODO really set pi, just a bug fix for now
!
!     convert input coordinates to radians
!
      rlon1 = lon1*pi/180.d0
      rlat1 = lat1*pi/180.d0
      rlon2 = lon2*pi/180.d0
      rlat2 = lat2*pi/180.d0
!
!     compute angular distance
!
      ESMF_PhysGridCompDistSpherical = &
         acos( cos(rlat1)*cos(rlat2)*cos(rlon1)*cos(rlon2) + &
               cos(rlat1)*cos(rlat2)*sin(rlon1)*sin(rlon2) + &
               sin(rlat1)*sin(rlat2) )
!
!     if radius present, convert to linear distance
!
      if (present(radius)) then
         ESMF_PhysGridCompDistSpherical = radius*ESMF_PhysGridCompDistSpherical
   !  ???? JW
      endif

      end function ESMF_PhysGridCompDistSpherical

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridCompDistCartesian - Distance in Cartesian coords
!
! !INTERFACE:
      function ESMF_PhysGridCompDistCartesian(x1, y1, x2, y2, rc)

!
! !RETURN VALUE:
      real(kind=ESMF_KIND_R8) :: ESMF_PhysGridCompDistCartesian
!
! !ARGUMENTS:
      real(kind=ESMF_KIND_R8), intent(in) :: &
         x1, y1,        & ! x,y coordinates of points between which to 
         x2, y2           !   compute distance

      integer, intent(out), optional :: rc  ! return code
!
! !DESCRIPTION:
!     This routine computes the distance between two points defined by
!     the input coordinates.  This version computes the distance
!     in Cartesian coordinates.
!
!     The arguments are:
!     \begin{description}
!     \item[x1,y1,x2,y2]
!          Coordinates of points between which distance is computed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!     initialize return code
!
      if (present(rc)) rc = ESMF_SUCCESS
!
!     compute distance using the usual Cartesian formula
!
      ESMF_PhysGridCompDistCartesian = sqrt( (x2-x1)**2 + (y2-y1)**2 )

      end function ESMF_PhysGridCompDistCartesian

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridMaskTypeEqual - equality of PhysGrid mask types
!
! !INTERFACE:
      function ESMF_GridMaskTypeEqual(GridMaskType1, GridMaskType2)

! !RETURN VALUE:
      logical :: ESMF_GridMaskTypeEqual

! !ARGUMENTS:

      type (ESMF_GridMaskType), intent(in) :: &
         GridMaskType1,      &! Two region types to compare for
         GridMaskType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF PhysGrid mask types to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridMaskType1, GridMaskType2]
!          Two mask types to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridMaskTypeEqual = (GridMaskType1%maskType == &
                                GridMaskType2%maskType)

      end function ESMF_GridMaskTypeEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridMaskTypeNotEqual - non-equality of PhysGrid mask types
!
! !INTERFACE:
      function ESMF_GridMaskTypeNotEqual(GridMaskType1, GridMaskType2)

! !RETURN VALUE:
      logical :: ESMF_GridMaskTypeNotEqual

! !ARGUMENTS:

      type (ESMF_GridMaskType), intent(in) :: &
         GridMaskType1,      &! Two PhysGrid mask types to compare for
         GridMaskType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF PhysGrid mask types to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridMaskType1, GridMaskType2]
!          Two kinds of PhysGrid mask types to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridMaskTypeNotEqual = (GridMaskType1%maskType /= &
                                   GridMaskType2%maskType)

      end function ESMF_GridMaskTypeNotEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegionTypeEqual - equality of PhysGrid region kinds
!
! !INTERFACE:
      function ESMF_RegionTypeEqual(RegionType1, RegionType2)

! !RETURN VALUE:
      logical :: ESMF_RegionTypeEqual

! !ARGUMENTS:

      type (ESMF_RegionType), intent(in) :: &
         RegionType1,      &! Two region types to compare for
         RegionType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF PhysGrid region types to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[RegionType1, RegionType2]
!          Two region types to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegionTypeEqual = (RegionType1%regionType == &
                              RegionType2%regionType)

      end function ESMF_RegionTypeEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_RegionTypeNotEqual - non-equality of PhysGrid region kinds
!
! !INTERFACE:
      function ESMF_RegionTypeNotEqual(RegionType1, RegionType2)

! !RETURN VALUE:
      logical :: ESMF_RegionTypeNotEqual

! !ARGUMENTS:

      type (ESMF_RegionType), intent(in) :: &
         RegionType1,      &! Two PhysGrid region types to compare for
         RegionType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF PhysGrid region types to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[RegionType1, RegionType2]
!          Two kinds of PhysGrid regions to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegionTypeNotEqual = (RegionType1%regionType /= &
                                 RegionType2%regionType)

      end function ESMF_RegionTypeNotEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridOrientEqual - equality of PhysGrid orientation
!
! !INTERFACE:
      function ESMF_PhysGridOrientEqual(Orientation1, Orientation2)

! !RETURN VALUE:
      logical :: ESMF_PhysGridOrientEqual

! !ARGUMENTS:

      type (ESMF_PhysGridOrientation), intent(in) :: &
         Orientation1,      &! Two orientation types to compare for
         Orientation2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF PhysGridOrientation types to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[Orientation1, Orientation2]
!          Two orientation types to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_PhysGridOrientEqual = (Orientation1%orientation == &
                                  Orientation2%orientation)

      end function ESMF_PhysGridOrientEqual

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridOrientNotEqual - non-equality of PhysGrid orientations
!
! !INTERFACE:
      function ESMF_PhysGridOrientNotEqual(Orientation1, Orientation2)

! !RETURN VALUE:
      logical :: ESMF_PhysGridOrientNotEqual

! !ARGUMENTS:

      type (ESMF_PhysGridOrientation), intent(in) :: &
         Orientation1,      &! Two PhysGrid orientation types to compare for
         Orientation2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF PhysGridOrientation types to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[Orientation1, Orientation2]
!          Two PhysGrid orientations to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_PhysGridOrientNotEqual = (Orientation1%orientation /= &
                                     Orientation2%orientation)

      end function ESMF_PhysGridOrientNotEqual

!------------------------------------------------------------------------------

      end module ESMF_PhysGridMod

