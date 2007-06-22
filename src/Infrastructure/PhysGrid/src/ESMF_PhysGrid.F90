! $Id: ESMF_PhysGrid.F90,v 1.106 2007/06/22 23:21:38 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_PhysGrid.F90"
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
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_PhysGridMod - Physical properties of InternGrid
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_PhysGrid} class and is 
! responsible for computing or initializing physical properties of interngrids.   
! {\tt ESMF\_PhysGrid} properties include coordinate information necessary 
! for describing interngrids, interngrid metric information and interngrid masks.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_LocalArrayMod
      use ESMF_InternArrayDataMapMod
      use ESMF_InternArrayMod
      use ESMF_InternArrayCreateMod
      use ESMF_InternArrayGetMod
      use ESMF_PhysCoordMod
      use ESMF_InitMacrosMod

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     !  ESMF_PhysLocation
!
!     !  Physical locations for a set of points defining the interngrid.

      type ESMF_PhysLocation
      sequence
!      private
        type(ESMF_Base)    :: base      ! ESMF Base object, including name
        ! One array per number of dimensions:
        type(ESMF_InternArray), dimension(:), pointer :: compLocations
                                        ! the coordinates for each point in the
                                        ! interngrid.  If the coordinates are aligned,
                                        ! then this array is a simple vector of
                                        ! values along the axis.  Otherwise the
                                        ! Array must have the coordinates for
                                        ! all points in the InternGrid.
        type(ESMF_InternArray), dimension(:), pointer :: totalLocations
                                        ! same as above, but with an additional
                                        ! boundary layer of coordinates.  Only
                                        ! used internally, for example during
                                        ! regridding to handle external boundary
                                        ! conditions and to avoid the need for
                                        ! inter-DE communication at the internal
                                        ! boundaries.
        ESMF_INIT_DECLARE

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
!     !  Physical locations for a set of points defining regions of the interngrid
!     !  (e.g. cell centers or domains of influence).

      type ESMF_PhysRegion
      sequence
!      private
        type(ESMF_Base) :: base   ! ESMF Base class object
        type(ESMF_RegionType) :: regionType
                                  ! what kind of region          
        integer :: numVertices    ! number of vertices for a polygonal region;
                                  ! if variable, set this to the largest number.
                                  ! Vertices can be degenerate.
        ! One array per number of dimensions:
        type(ESMF_InternArray), dimension(:), pointer :: vertices
                                  ! coordinates in each direction for each corner
                                  ! of each region.
        type(ESMF_InternArray), dimension(:), pointer :: bbox 
                                  ! bounding box for each region to aid search
                                  ! methods.

        type(ESMF_InternArray), dimension(2) :: ellipse
                                  ! parameters of ellipse describing region
                                  ! around each point.  Note that the values can
                                  ! be equal, describing a circle or sphere
        ESMF_INIT_DECLARE
      
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridMaskType
!
!     ! Type to specify kind of region for defined PhysGrid regions.

      type ESMF_InternGridMaskType
      sequence
        integer :: maskType
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridMask
!
!     ! Data type describing masks for a PhysGrid.  Masks are named and can
!     ! be of different types, including logical masks, multiplicative masks,
!     ! and integer region IDs.

      type ESMF_InternGridMask
      sequence
!      private
        type(ESMF_Base) :: base   ! ESMF Base class object
        type(ESMF_InternGridMaskType) :: maskType
                                  ! type of mask
        type(ESMF_InternArray) :: data  ! mask data at each interngrid point
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_PhysGridOrientation
!
!     ! Type to specify orientation for defined PhysGrids.  Useful for
!     !  queries of interngrid directions.
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

        type(ESMF_Base) :: base   ! ESMF Base class object
        type(ESMF_RelLoc) :: relloc
                                  ! If this PhysGrid describes staggered part of
                                  ! a interngrid, this is the Relative Location for 
                                  ! easy determination of PhysGrid associated
                                  ! with a staggered location.
        type(ESMF_CoordSystem) :: coordSystem
                                  ! Coordinate system 
                                  ! (eg Cartesian, Spherical, ...etc)
        integer :: numDims        ! Number of physical dimensions
        type(ESMF_PhysGridOrientation) :: orientation
                                  ! Orientation
                                  ! (eg Horizontal, Vertical, Unknown)
        type(ESMF_PhysCoord), dimension(:), pointer :: coords
                                  ! Description of each physical coordinate axis,
                                  ! including extents for this interngrid.  
        type(ESMF_PhysLocation) :: locations
                                  ! Structure which holds the actual coordinates
                                  ! for the interngrid locations.
        type(ESMF_PhysRegion) :: regions
                                  ! Information about interngrid regions, which
                                  ! typically describe each interngrid cell, but can
                                  ! be either polygons or circles/spheres/ellipses
        integer :: numMasks
        type(ESMF_InternGridMask), dimension(:), pointer :: masks
                                  ! InternGrid-based masks.  Includes both logical 
                                  ! and multiplicative masks.  Default mask 
                                  ! (for query) is the first one if no name
                                  ! given.  Region IDs can be encoded as a mask
                                  ! as well.
        integer :: numMetrics
        type(ESMF_InternArray), dimension(:), pointer :: metrics
                                  ! A place to store metrics for the interngrid.  
                                  ! There is no support for the Framework to use
                                  ! these metrics, but they can be set and
                                  ! queried as a convenience to the user.  If
                                  ! there arises a need for internally computed
                                  ! metrics, they will also be set here.
        ESMF_INIT_DECLARE

      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysGrid
!
!     !  The PhysGrid data structure that is passed between languages.

      type ESMF_PhysGrid
      sequence
!     private
        type(ESMF_PhysGridType), pointer :: ptr   ! pointer to a physgrid type
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:

      ! These are public primarily for use by the InternGrid module and
      ! are not meant to be directly accessible to users.
      public ESMF_PhysLocation
      public ESMF_RegionType
      public ESMF_PhysRegion
      public ESMF_InternGridMaskType
      public ESMF_InternGridMask
      public ESMF_PhysGridOrientation
      public ESMF_PhysGrid
      public ESMF_PhysGridType

      public ESMF_PhysLocationInit
      public ESMF_PhysLocationValidate
      public ESMF_PhysLocationGetInit

      public ESMF_InternGridMaskInit
      public ESMF_InternGridMaskValidate
      public ESMF_InternGridMaskGetInit

      public ESMF_PhysGridTypeInit
      public ESMF_PhysGridTypeValidate
      public ESMF_PhysGridTypeGetInit

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
      public ESMF_PhysGridGetInit

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
      public ESMF_PhysGridSearchMyDERowCol

      public ESMF_PhysGridPointInCell

      public ESMF_PhysGridCompDistSpherical
      public ESMF_PhysGridCompDistCartesian
 
      public operator(==), operator(/=) ! for overloading region, mask
                                        ! comparison functions

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

      ! Supported ESMF PhysGrid Orientation Types
      !   UNKNOWN     = unknown or undefined orientation
      !   HORIZONTAL  = PhysGrid is a horizontal interngrid
      !   VERTICAL    = PhysGrid is a vertical interngrid
      !   3D          = PhysGrid is a full 3-d description of interngrid space
      !   XZ          = PhysGrid is a XZ (or zonal     ) slice out of 3-d space 
      !   YZ          = PhysGrid is a YZ (or meridional) slice out of 3-d space

      type(ESMF_PhysGridOrientation), parameter, public :: &! interngrid direction
         ESMF_PHYSGRID_ORIENT_UNKNOWN     = ESMF_PhysGridOrientation( 0), &
         ESMF_PHYSGRID_ORIENT_HORIZONTAL  = ESMF_PhysGridOrientation( 1), &
         ESMF_PHYSGRID_ORIENT_VERTICAL    = ESMF_PhysGridOrientation( 2), &
         ESMF_PHYSGRID_ORIENT_3D          = ESMF_PhysGridOrientation( 3), &
         ESMF_PHYSGRID_ORIENT_XZ          = ESMF_PhysGridOrientation( 4), &
         ESMF_PHYSGRID_ORIENT_YZ          = ESMF_PhysGridOrientation( 5)

      ! Supported ESMF PhysGrid Region Types
      !   UNKNOWN   = unknown or undefined region type
      !   POLYGON   = polygons defined by vertex coordinates
      !   ELLIPSE   = ellipse centered on interngrid point, defined by two params

      type(ESMF_RegionType), parameter, public :: &! types of PhysGrid regions
         ESMF_REGION_TYPE_UNKNOWN      = ESMF_RegionType( 0), &
         ESMF_REGION_TYPE_POLYGON      = ESMF_RegionType( 1), &
         ESMF_REGION_TYPE_ELLIPSE      = ESMF_RegionType( 2)

      ! Supported ESMF PhysGrid Mask Types
      !   UNKNOWN   = unknown or undefined mask type
      !   LOGICAL   = logical mask
      !   MULT      = multiplicative mask
      !   REGION_ID = integer assigning unique ID to each point

      type(ESMF_InternGridMaskType), parameter, public :: &! types of interngrid masks
         ESMF_GRID_MASKTYPE_UNKNOWN        = ESMF_InternGridMaskType( 0), &
         ESMF_GRID_MASKTYPE_LOGICAL        = ESMF_InternGridMaskType( 1), &
         ESMF_GRID_MASKTYPE_MULT           = ESMF_InternGridMaskType( 2), &
         ESMF_GRID_MASKTYPE_REGION_ID      = ESMF_InternGridMaskType( 3)

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_PhysGrid.F90,v 1.106 2007/06/22 23:21:38 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
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
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_PhysGridSearchBboxSpherical
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_PhysGridSearchBboxSphericalPoint
!         module procedure ESMF_PhysGridSearchBboxSphericalList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that 
!!     search a interngrid for point(s) using a simple bounding box search
!!     in spherical coordinates.
!!
!!EOPI
!      end interface 
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_PhysGridSearchGeneralSpherical
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_PhysGridSearchGeneralSphericalPoint
!         module procedure ESMF_PhysGridSearchGeneralSphericalList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that 
!!     search a interngrid for point(s) using a general (cross-product) search
!!     in spherical coordinates.
!!
!!EOPI
!      end interface 
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_PhysGridSearchBboxCartesian
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_PhysGridSearchBboxCartesianPoint
!         module procedure ESMF_PhysGridSearchBboxCartesianList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that 
!!     search a interngrid for point(s) using a simple bounding box search
!!     in Cartesian coordinates.
!!
!!EOPI
!      end interface 
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_InternGridMaskTypeEqual
         module procedure ESMF_RegionTypeEqual
         module procedure ESMF_PhysGridOrientEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF region kind, mask kind and orientation data types.  It is 
!     provided for easy comparisons of these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_InternGridMaskTypeNotEqual
         module procedure ESMF_RegionTypeNotEqual
         module procedure ESMF_PhysGridOrientNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF region kind, mask kind and orientation data types.  It is 
!     provided for easy comparisons of these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysLocationGetInit"
!BOPI
! !IROUTINE:  ESMF_PhysLocationGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_PhysLocationGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_PhysLocation), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_PhysLocationGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt physlocation}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysLocation} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_PhysLocationGetInit = ESMF_INIT_GET(s)
       else
         ESMF_PhysLocationGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_PhysLocationGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysLocationInit"
!BOPI
! !IROUTINE:  ESMF_PhysLocationInit - Initialize PhysLocation

! !INTERFACE:
    subroutine ESMF_PhysLocationInit(s)
!
! !ARGUMENTS:
       type(ESMF_PhysLocation) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt physlocation}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysLocation} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_PhysLocationInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysLocationValidate"
!BOPI
! !IROUTINE:  ESMF_PhysLocationValidate - Check validity of a PhysLocation

! !INTERFACE:
    subroutine ESMF_PhysLocationValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_PhysLocation), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt PhysLocation} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysLocation} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt physlocation}
!           is valid.
!     \end{description}
!
!EOPI
     ESMF_INIT_CHECK_SHALLOW(ESMF_PhysLocationGetInit,ESMF_PhysLocationInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_PhysLocationValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysRegionGetInit"
!BOPI
! !IROUTINE:  ESMF_PhysRegionGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_PhysRegionGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_PhysRegion), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_PhysRegionGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt physregion}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysRegion} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_PhysRegionGetInit = ESMF_INIT_GET(s)
       else
         ESMF_PhysRegionGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_PhysRegionGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysRegionInit"
!BOPI
! !IROUTINE:  ESMF_PhysRegionInit - Initialize PhysRegion

! !INTERFACE:
    subroutine ESMF_PhysRegionInit(s)
!
! !ARGUMENTS:
       type(ESMF_PhysRegion) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt physregion}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysRegion} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_PhysRegionInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysRegionValidate"
!BOPI
! !IROUTINE:  ESMF_PhysRegionValidate - Check validity of a PhysRegion

! !INTERFACE:
    subroutine ESMF_PhysRegionValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_PhysRegion), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt PhysRegion} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysRegion} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt physregion}
!           is valid.
!     \end{description}
!
!EOPI
     ESMF_INIT_CHECK_SHALLOW(ESMF_PhysRegionGetInit,ESMF_PhysRegionInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_PhysRegionValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridMaskGetInit"
!BOPI
! !IROUTINE:  ESMF_InternGridMaskGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_InternGridMaskGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_InternGridMask), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_InternGridMaskGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt interngridmask}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_InternGridMask} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_InternGridMaskGetInit = ESMF_INIT_GET(s)
       else
         ESMF_InternGridMaskGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_InternGridMaskGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridMaskInit"
!BOPI
! !IROUTINE:  ESMF_InternGridMaskInit - Initialize InternGridMask

! !INTERFACE:
    subroutine ESMF_InternGridMaskInit(s)
!
! !ARGUMENTS:
       type(ESMF_InternGridMask) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt interngridmask}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_InternGridMask} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_InternGridMaskInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridMaskValidate"
!BOPI
! !IROUTINE:  ESMF_InternGridMaskValidate - Check validity of a InternGridMask

! !INTERFACE:
    subroutine ESMF_InternGridMaskValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_InternGridMask), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt InternGridMask} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_InternGridMask} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt interngridmask}
!           is valid.
!     \end{description}
!
!EOPI
     ESMF_INIT_CHECK_SHALLOW(ESMF_InternGridMaskGetInit,ESMF_InternGridMaskInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_InternGridMaskValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridTypeGetInit"
!BOPI
! !IROUTINE:  ESMF_PhysGridTypeGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_PhysGridTypeGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_PhysGridType), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_PhysGridTypeGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt physgridtype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysGridType} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_PhysGridTypeGetInit = ESMF_INIT_GET(s)
       else
         ESMF_PhysGridTypeGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_PhysGridTypeGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridTypeInit"
!BOPI
! !IROUTINE:  ESMF_PhysGridTypeInit - Initialize PhysGridType

! !INTERFACE:
    subroutine ESMF_PhysGridTypeInit(s)
!
! !ARGUMENTS:
       type(ESMF_PhysGridType) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt physgridtype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysGridType} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_PhysGridTypeInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridTypeValidate"
!BOPI
! !IROUTINE:  ESMF_PhysGridTypeValidate - Check validity of a PhysGridType

! !INTERFACE:
    subroutine ESMF_PhysGridTypeValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_PhysGridType), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt PhysGridType} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysGridType} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt physgridtype}
!           is valid.
!     \end{description}
!
!EOPI
     ESMF_INIT_CHECK_SHALLOW(ESMF_PhysGridTypeGetInit,ESMF_PhysGridTypeInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_PhysGridTypeValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridGetInit"
!BOPI
! !IROUTINE:  ESMF_PhysGridGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_PhysGridGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_PhysGrid), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_PhysGridGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt physgrid}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_PhysGrid} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_PhysGridGetInit = ESMF_INIT_GET(d)
       else
         ESMF_PhysGridGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_PhysGridGetInit


!------------------------------------------------------------------------------
!
! This section includes the PhysGrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridCreateNew"
!BOPI
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
      type(ESMF_RelLoc), intent(in) :: relloc
      character (len = *), intent(in), optional :: name  
      type(ESMF_CoordSystem), intent(in), optional :: coordSystem
      type(ESMF_PhysGridOrientation), intent(in), optional :: orientation
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
!          Relative location in interngrid cell for which this PhysGrid
!          is being defined.  For example, in a staggered interngrid,
!          this PhysGrid could be defined for the interngrid associated
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
!EOPI
! !REQUIREMENTS:  TODO


      ! local variables
      type(ESMF_PhysGridType), pointer :: physgrid   ! Pointer to new physgrid
      integer :: localrc                             ! Error status

      ! Initialize pointers
      nullify(physgrid)
      nullify(ESMF_PhysGridCreateNew%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      allocate(physgrid, stat=localrc)
      ! If error write message and return.
      if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid type", &
                                     ESMF_CONTEXT, rc)) return

      ! initialize base object in each of the types which use it
      call ESMF_BaseCreate(physgrid%base, "PhysGrid", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Fill new physgrid with supplied variables.
      physgrid%numDims  = numDims
      physgrid%relloc   = relloc
      physgrid%numMasks = 0
      nullify(physgrid%masks)
      nullify(physgrid%coords)
      
      if (present(coordSystem)) then
         physgrid%coordSystem = coordSystem
      else
         physgrid%coordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      endif
      
      if (present(orientation)) then
         physgrid%orientation = orientation
      else
         physgrid%orientation = ESMF_PHYSGRID_ORIENT_UNKNOWN
      endif
      
      ! initialize other objects which contains a base
      call ESMF_BaseCreate(physgrid%locations%base, "PhysGridLocations", name, &
                           0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      nullify(physgrid%locations%totalLocations)
      nullify(physgrid%locations%compLocations)

      call ESMF_BaseCreate(physgrid%regions%base, "PhysGridRegions", name, &
                           0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      nullify(physgrid%regions%bbox)
      nullify(physgrid%regions%vertices)
      nullify(physgrid%metrics)
      physgrid%numMetrics = 0

      ! Set return values.
      ESMF_PhysGridCreateNew%ptr => physgrid
      ESMF_INIT_SET_CREATED(ESMF_PhysGridCreateNew)
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCreateNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridDestroy"
!BOPI
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
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: localrc                             ! Error status
      integer :: i

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! if already destroyed, fine.
      if (.not.associated(physgrid%ptr)) then 
          if (present(rc)) rc = ESMF_SUCCESS
          return
      endif


      ! release coordinate information
      if (associated(physgrid%ptr%coords)) then
          do i = 1, physgrid%ptr%numDims
              if (associated(physgrid%ptr%coords(i)%ptr)) then
                  call ESMF_PhysCoordDestroy(physgrid%ptr%coords(i), rc=localrc)
                  if (ESMF_LogMsgFoundError(localrc, &
                                            ESMF_ERR_PASSTHRU, &
                                            ESMF_CONTEXT, rc)) return
              endif
          enddo

          deallocate(physgrid%ptr%coords, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid coords", &
                                         ESMF_CONTEXT, rc)) return
      endif

      ! release all storage associated with masks
      if (physgrid%ptr%numMasks .gt. 0) then
          do i=1, physgrid%ptr%numMasks

              call ESMF_BaseDestroy(physgrid%ptr%masks(i)%base, rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
        
              call ESMF_InternArrayDestroy(physgrid%ptr%masks(i)%data, rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
        
          enddo
     
          deallocate(physgrid%ptr%masks, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid masks", &
                                         ESMF_CONTEXT, rc)) return
      endif

      ! release all storage associated with metrics
      if (physgrid%ptr%numMetrics .gt. 0) then
          do i=1, physgrid%ptr%numMetrics

              call ESMF_InternArrayDestroy(physgrid%ptr%metrics(i), rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
          enddo
    
          if (associated(physgrid%ptr%metrics)) then
              deallocate(physgrid%ptr%metrics, stat=localrc)
              if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid metrics", &
                                             ESMF_CONTEXT, rc)) return
          endif
      endif


      ! clean up location related storage
      if (associated(physgrid%ptr%locations%compLocations)) then
          do i=1, size(physgrid%ptr%locations%compLocations)
              call ESMF_InternArrayDestroy(physgrid%ptr%locations%compLocations(i), rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
          enddo

          deallocate(physgrid%ptr%locations%compLocations, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid compLocations", &
                                         ESMF_CONTEXT, rc)) return
      endif

      if (associated(physgrid%ptr%locations%totalLocations)) then
          do i=1, size(physgrid%ptr%locations%totalLocations)
              call ESMF_InternArrayDestroy(physgrid%ptr%locations%totalLocations(i), rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
          enddo

          deallocate(physgrid%ptr%locations%totalLocations, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid totalLocations", &
                                         ESMF_CONTEXT, rc)) return
      endif

      call ESMF_BaseDestroy(physgrid%ptr%locations%base, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! clean up region related storage
      if (associated(physgrid%ptr%regions%vertices)) then
          do i=1, size(physgrid%ptr%regions%vertices)
              call ESMF_InternArrayDestroy(physgrid%ptr%regions%vertices(i), rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
          enddo

          deallocate(physgrid%ptr%regions%vertices, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid vertices", &
                                         ESMF_CONTEXT, rc)) return
      endif

      if (associated(physgrid%ptr%regions%bbox)) then
          do i=1, size(physgrid%ptr%regions%bbox)
              call ESMF_InternArrayDestroy(physgrid%ptr%regions%bbox(i), rc=localrc)
              if (ESMF_LogMsgFoundError(localrc, &
                                        ESMF_ERR_PASSTHRU, &
                                        ESMF_CONTEXT, rc)) return
          enddo

          deallocate(physgrid%ptr%regions%bbox, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid bbox", &
                                         ESMF_CONTEXT, rc)) return
      endif

      ! TODO: delete 2 elipse arrays if defined

      call ESMF_BaseDestroy(physgrid%ptr%regions%base, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return


      ! free main base object
      call ESMF_BaseDestroy(physgrid%ptr%base, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! and finally, get rid of physgrid derived type itself and
      ! mark the pointer as null
      deallocate(physgrid%ptr, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid type", &
                                     ESMF_CONTEXT, rc)) return

      nullify(physgrid%ptr)

      ESMF_INIT_SET_DELETED(physgrid)
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSet"
!BOPI
! !IROUTINE: ESMF_PhysGridSet - Sets value of selected PhysGrid quantities.

! !INTERFACE:
      subroutine ESMF_PhysGridSet(physgrid, name, coordSystem, orientation, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      character (len = *), intent(in), optional :: name  
      type(ESMF_CoordSystem), intent(in), optional :: coordSystem
      type(ESMF_PhysGridOrientation), intent(in), optional :: orientation
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
!EOPI
! !REQUIREMENTS:  TODO

      ! local variables
      integer :: localrc                             ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (present(coordSystem)) then
        physgrid%ptr%coordSystem = coordSystem
      endif
      
      if (present(orientation)) then
        physgrid%ptr%orientation = orientation
      endif
      
      if (present(name)) then
        call ESMF_SetName(physgrid%ptr%base, name, "PhysGrid", localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridGet"
!BOPI
! !IROUTINE: ESMF_PhysGridGet - Retrieves selected internal PhysGrid quantities.

! !INTERFACE:
      subroutine ESMF_PhysGridGet(physgrid, relloc, name, numDims, &
                                  coordSystem, orientation, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_RelLoc), intent(out), optional :: relloc
      character (len = *), intent(out), optional :: name  
      integer, intent(out), optional :: numDims
      type(ESMF_CoordSystem), intent(out), optional :: coordSystem
      type(ESMF_PhysGridOrientation), intent(out), optional :: orientation
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Retrieves individual parts of an existing {\tt ESMF\_PhysGrid}. Can be
!     used for only a few parts; most array components have specific
!     get calls.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          Existing {\tt ESMF\_PhysGrid} from which quantity is to be extracted.
!     \item[{[name]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[relloc]}]
!          {\tt ESMF\_RelLoc} referring to position in staggered interngrid
!          to which interngrid quantities refer.
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
!EOPI
! !REQUIREMENTS:  TODO

      ! local variables
      integer :: localrc                             ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      if (present(name)) then
        call ESMF_GetName(physgrid%ptr%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(relloc     )) relloc      = physgrid%ptr%relloc
      if (present(numDims    )) numDims     = physgrid%ptr%numDims
      if (present(coordSystem)) coordSystem = physgrid%ptr%coordSystem
      if (present(orientation)) orientation = physgrid%ptr%orientation

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSetCoord"
!BOPI
! !IROUTINE: ESMF_PhysGridSetCoord - Adds a physical coordinate to a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSetCoord(physgrid, physCoord, dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_PhysCoord), intent(inout) :: physCoord
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
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: idim
      integer :: localrc                             ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_PhysCoordGetInit,physCoord,rc)

      ! if coordinate array not allocated yet, do so now
      if (.not. associated(physgrid%ptr%coords)) then
        allocate(physgrid%ptr%coords(physgrid%ptr%numDims), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid Coords", &
                                       ESMF_CONTEXT, rc)) return
      endif

      ! add coordinate
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
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "too many coords defined", &
                                      ESMF_CONTEXT, rc)
          return
        endif
      endif

      ! set return code
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetCoord

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridGetCoord"
!BOPI
! !IROUTINE: ESMF_PhysGridGetCoord - Gets a physical coordinate from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetCoord(physgrid, physCoord, dimOrder, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_PhysCoord), intent(out) :: physCoord
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
!          If dimOrder not supplied, a name must be supplied to identify
!          the coordinate to be retrieved.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: localrc                             ! Error status
      integer :: idim, n
      character(len=ESMF_MAXSTR) :: nameTmp
      logical :: found                               ! flag for name search

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! if dimension supplied, just grab the coordinate
      if (present(dimOrder)) then
        physCoord = physgrid%ptr%coords(dimOrder)

      ! if a name supplied, search for a coord with given name
      elseif (present(name)) then
        found = .false.
        name_srch: do n=1,physgrid%ptr%numDims
          call ESMF_PhysCoordGet(physgrid%ptr%coords(n), name=nameTmp, &
                                 rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          if (name == trim(nameTmp)) then
            found = .true.
            idim = n
            exit name_srch
          endif
        end do name_srch
         
        if (found) then
          physCoord = physgrid%ptr%coords(idim)
        else
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "no coordinate with that name", &
                                      ESMF_CONTEXT, rc)
          return
        endif

      ! if neither supplied, return an error
      else
        call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                 "name or dim must be supplied", &
                                    ESMF_CONTEXT, rc)
        return
      endif

      ! set return code
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetCoord

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridGetLocations"
!BOPI
! !IROUTINE: ESMF_PhysGridGetLocations - Gets locations from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetLocations(physgrid, name, locationArray, &
                                           total, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      character(*), intent(out), optional :: name
      type(ESMF_InternArray), dimension(:), intent(inout), optional :: locationArray 
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
!          other interngrid arrays.
!     \item[{[total]}]
!          Logical. If TRUE, return the total coordinates including internally
!          generated boundary cells. If FALSE return the
!          computational cells (which is what the user will be expecting.)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: localrc                             ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! if name requested, get name
      if (present(name)) then
        call ESMF_GetName(physgrid%ptr%locations%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! if locations requested, get them
      if (present(locationArray)) then
        locationArray = physgrid%ptr%locations%compLocations
        if (present(total)) then
          if (total) locationArray = physgrid%ptr%locations%totalLocations
        endif
      endif

      ! set return code
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetLocations

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSetLocations"
!BOPI
! !IROUTINE: ESMF_PhysGridSetLocations - Sets interngrid locations from input array.

! !INTERFACE:
      subroutine ESMF_PhysGridSetLocations(physgrid, locationArray, name, &
                                           total, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_InternArray), dimension(:), pointer :: locationArray
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
!          other interngrid arrays.
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
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: localrc                             ! Error status
      character (len=ESMF_MAXSTR) :: name_tmp        ! temp for name creation 

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! if name specified, add name.  otherwise, make one up based on PhysGrid.
      if (present(name)) then
        call ESMF_SetName(physgrid%ptr%regions%base, name, &
                          "PhysGridLocations", localrc)
      else
        call ESMF_GetName(physgrid%ptr%base, name_tmp, localrc)
        name_tmp = trim(name_tmp) // 'Locations'
        call ESMF_SetName(physgrid%ptr%regions%base, name_tmp, &
                          "PhysGridLocations", localrc)
      endif
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set locations
      if (present(total)) then
        if (total) physgrid%ptr%locations%totalLocations => locationArray
        if (.not.total) physgrid%ptr%locations%compLocations => locationArray
      else
        physgrid%ptr%locations%compLocations => locationArray
      endif

      ! set return code
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetLocations

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSetRegions"
!BOPI
! !IROUTINE: ESMF_PhysGridSetRegions - Sets interngrid regions from input array.

! !INTERFACE:
      subroutine ESMF_PhysGridSetRegions(physgrid, regionType, name, &
                                         numVertices, vertexArray,   &
                                         ellipseArray, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_RegionType), intent(in) :: regionType
      character(*), intent(in), optional :: name
      integer, intent(in), optional :: numVertices
      type(ESMF_InternArray), dimension(:), pointer, optional :: vertexArray
      type(ESMF_InternArray), dimension(2), intent(in), optional :: ellipseArray
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
!          physical dimension for each vertex point at each logical interngrid point. 
!          The array is assumed to be dimensioned (num\_dims,num\_vertices)
!          while the {\tt ESMF\_Array} would typically be consistent with
!          other interngrid arrays. The order of dimensions is assumed to be the
!          same order defined for the PhysGrid coordinates.
!     \item[{[ellipseArray]}]
!          Array of {\tt ESMF\_Array}s containing the two parameters
!          necessary for describing the elliptical region at each interngrid point.
!          The array is dimensioned (2) while the {\tt ESMF\_Array} would 
!          typically be consistent with other interngrid arrays.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: localrc                             ! Error status
      character(len=ESMF_MAXSTR) :: name_tmp         ! temp for name creation 

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! if name specified, add name.  otherwise, make one up based on PhysGrid.
      if (present(name)) then
        call ESMF_SetName(physgrid%ptr%regions%base, name, &
                          "PhysGridRegions", localrc)
      else
        call ESMF_GetName(physgrid%ptr%base, name_tmp, localrc)
        name_tmp = trim(name_tmp) // 'Region'
        call ESMF_SetName(physgrid%ptr%regions%base, name_tmp, &
                          "PhysGridRegions", localrc)
      endif
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set region type
      physgrid%ptr%regions%regionType = regionType

      ! set region arrays depending on type of region
      if (regionType == ESMF_REGION_TYPE_POLYGON) then
        physgrid%ptr%regions%numVertices = numVertices
        physgrid%ptr%regions%vertices => vertexArray

      else if (regionType == ESMF_REGION_TYPE_ELLIPSE) then
        physgrid%ptr%regions%ellipse = ellipseArray

      else
        call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                 "unknown region type", &
                                    ESMF_CONTEXT, rc)
        return

      endif

      ! define bounding box for each region to aid in future searches
      if (regionType == ESMF_REGION_TYPE_POLYGON) then
        !TODO: create bbox array for polygons

      else if (regionType == ESMF_REGION_TYPE_ELLIPSE) then
        !TODO: define bounding box for elliptical region

      else
        call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                 "unknown region type", &
                                    ESMF_CONTEXT, rc)
        return

      endif

      ! set return code
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetRegions

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridGetRegions"
!BOPI
! !IROUTINE: ESMF_PhysGridGetRegions - Gets region information from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetRegions(physgrid, regionType, name, &
                                         numVertices, vertexArray,   &
                                         ellipseArray, bboxArray, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_RegionType), intent(inout), optional :: regionType
      character(*), intent(inout), optional :: name
      integer, intent(inout), optional :: numVertices
      type(ESMF_InternArray), dimension(:), intent(inout), optional :: vertexArray
      type(ESMF_InternArray), dimension(2), intent(inout), optional :: ellipseArray
      type(ESMF_InternArray), dimension(:), intent(inout), optional :: bboxArray
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
!          physical dimension for each vertex point at each logical interngrid point. 
!          The array is assumed to be dimensioned (num\_dims,num\_vertices)
!          while the {\tt ESMF\_Array} would typically be consistent with
!          other interngrid arrays.
!     \item[{[ellipseArray]}]
!          Array of {\tt ESMF\_Array}s containing the two parameters
!          necessary for describing the elliptical region at each interngrid point.
!          The array is dimensioned (2) while the {\tt ESMF\_Array} would 
!          typically be consistent with other interngrid arrays.
!     \item[{[bboxArray]}]
!          Array of {\tt ESMF\_Array}s containing the bounding box
!          of each defined region.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: localrc                             ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! if name requested, get name
      if (present(name)) then
        call ESMF_GetName(physgrid%ptr%regions%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! if number of vertices requested, get it
      if (present(numVertices)) then
        if (physgrid%ptr%regions%regionType == ESMF_REGION_TYPE_POLYGON) then
          numVertices = physgrid%ptr%regions%numVertices
        else
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "invalid region type", &
                                      ESMF_CONTEXT, rc)
          return
        endif
      endif

      ! if vertex coordinates requested, get them
      if (present(vertexArray)) then
        if (physgrid%ptr%regions%regionType == ESMF_REGION_TYPE_POLYGON) then
          !TODO: consistency check for array sizes, shapes
          !TODO: return pointer or copy array?
          vertexArray = physgrid%ptr%regions%vertices
        else
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "invalid region type", &
                                      ESMF_CONTEXT, rc)
          return
        endif
      endif

      ! if ellipses requested, get them
      if (present(ellipseArray)) then
        if (physgrid%ptr%regions%regionType == ESMF_REGION_TYPE_ELLIPSE) then
          !TODO: consistency check for array sizes, shapes
          !TODO: return pointer or copy array?
          ellipseArray = physgrid%ptr%regions%ellipse
        else
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "invalid region type", &
                                      ESMF_CONTEXT, rc)
          return
        endif
      endif

      ! if bounding boxes requested, get them
      if (present(bboxArray)) then
        !TODO: consistency check for array sizes, shapes
        !TODO: return pointer or copy array?
        !bboxArray = physgrid%ptr%regions%bbox
      endif

      ! set return code
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetRegions

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSetMask"
!BOPI
! !IROUTINE: ESMF_PhysGridSetMask - Sets interngrid mask from input array.

! !INTERFACE:
      subroutine ESMF_PhysGridSetMask(physgrid, maskArray, maskType, &
                                      name, id, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      ! for some reason the pgi compiler does not like intent(in)
      ! on the next line.  it says the types do not match.
      ! i will just take it out for now.   nsc 15oct03
      !type(ESMF_InternArray), intent(in) :: maskArray
      type(ESMF_InternArray) :: maskArray
      type(ESMF_InternGridMaskType), intent(in) :: maskType
      character(*), intent(in), optional :: name
      integer, intent(out), optional :: id
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
!          Array containing mask values for each interngrid cell.
!     \item[maskType]
!          {\tt ESMF\_InternGridMaskType} describing type of mask (e.g. logical,
!          multiplicative, region ID).
!     \item[{[name]}]
!          Name to assign to mask.
!     \item[{[id]}]
!          Integer id assigned to mask which allows faster access
!          when retrieving mask.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: localrc                             ! Error status
      integer :: n, numMaskOld, numMaskNew
      type(ESMF_InternGridMask), dimension(:), allocatable :: tempMask
                                                     ! temporary array of masks

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! increment mask counter
      numMaskOld = physgrid%ptr%numMasks
      numMaskNew = numMaskOld + 1

      ! if first mask, allocate mask array
      if (numMaskNew == 1) then
        allocate(physgrid%ptr%masks(1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "new PhysGrid masks", &
                                       ESMF_CONTEXT, rc)) return

      ! if not first mask, resize mask array to make room for new mask
      else
        allocate(tempMask(numMaskOld), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "tempMask array", &
                                       ESMF_CONTEXT, rc)) return

        ! store old values before resizing array
        do n = 1, numMaskOld
          tempMask(n) = physgrid%ptr%masks(n)
        enddo

        ! destroy old array to create new one
        deallocate(physgrid%ptr%masks, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate PhysGrid masks", &
                                       ESMF_CONTEXT, rc)) return

        ! allocate new mask array of correct size
        allocate(physgrid%ptr%masks(numMaskNew), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid masks", &
                                       ESMF_CONTEXT, rc)) return

        ! fill new array with old values
        do n = 1, numMaskOld
          physgrid%ptr%masks(n) = tempMask(n)
        enddo

        deallocate(tempMask, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate tempMask", &
                                       ESMF_CONTEXT, rc)) return

      endif

      ! reset number of masks and add new mask
      if (present(id)) id = numMaskNew
      physgrid%ptr%numMasks = numMaskNew

      call ESMF_BaseCreate(physgrid%ptr%masks(numMaskNew)%base, &
                           "PhysGridMask", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      physgrid%ptr%masks(numMaskNew)%maskType = maskType
      physgrid%ptr%masks(numMaskNew)%data = maskArray

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetMask

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridGetMask"
!BOPI
! !IROUTINE: ESMF_PhysGridGetMask - Gets interngrid mask from PhysGrid object.

! !INTERFACE:
      subroutine ESMF_PhysGridGetMask(physgrid, maskArray, name, id, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_InternArray), intent(out) :: maskArray
      character(*), intent(in), optional :: name
      integer, intent(in), optional :: id
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
!          Array containing mask value at each interngrid cell.
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
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: n
      integer :: localrc                             ! Error status
      character(len=ESMF_MAXSTR) :: name_tmp         ! temp for name check 
      character(len=ESMF_MAXSTR) :: logMsg
      logical :: found                               ! name search flag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! if id supplied, check for valid id and return appropriate mask
      if (present(id)) then

        ! check for valid id
        if (id < 1 .or. id > physgrid%ptr%numMasks) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "invalid mask id", &
                                      ESMF_CONTEXT, rc)
          return
        endif

        ! get mask associated with id
        maskArray = physgrid%ptr%masks(id)%data

      ! if name supplied, check for valid id and return appropriate mask
      else if (present(name)) then

        found = .false.
        name_loop: do n=1,physgrid%ptr%numMasks
            
          call ESMF_GetName(physgrid%ptr%masks(n)%base, name_tmp, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          if (trim(name) == trim(name_tmp)) then
            maskArray = physgrid%ptr%masks(n)%data
            found = .true.
            exit name_loop
          endif
        end do name_loop

        if (.not. found) then
          write(logMsg, *) "no mask matches name.  Requested name: ", trim(name)
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, logMsg, &
                                      ESMF_CONTEXT, rc)
          return
        endif

      ! if we enter this else branch, neither id nor mask has been 
      ! supplied so return error
      else
        call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                 "id or name must be supplied", &
                                    ESMF_CONTEXT, rc)
        return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetMask

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSetMetric"
!BOPI
! !IROUTINE: ESMF_PhysGridSetMetric - Sets interngrid metric from input array.

! !INTERFACE:
      subroutine ESMF_PhysGridSetMetric(physgrid, metricArray, name, id, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_InternArray), intent(inout) :: metricArray
      character(*), intent(in) :: name
      integer, intent(out), optional :: id
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
!          Array containing metric values at each interngrid cell.
!     \item[name]
!          Name to assign to metric.
!     \item[{[id]}]
!          Integer id assigned to metric which allows faster access
!          when retrieving metric.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: n, numMetricOld, numMetricNew
      integer :: localrc                             ! Error status
      type(ESMF_InternArray), dimension(:), allocatable :: tempMetric
                                                     ! temporary array of metrics

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! increment metric counter
      numMetricOld = physgrid%ptr%numMetrics
      numMetricNew = numMetricOld + 1

      ! if first metric, allocate metric array
      if (numMetricNew == 1) then
        allocate(physgrid%ptr%metrics(1), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "new PhysGrid metrics", &
                                       ESMF_CONTEXT, rc)) return

      ! if not first metric, resize metric array to make room for new metric
      else
        allocate(tempMetric(numMetricOld), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "tempMetric", &
                                       ESMF_CONTEXT, rc)) return

        ! store old values before resizing array
        do n = 1, numMetricOld
          tempMetric(n) = physgrid%ptr%metrics(n)
        enddo

        ! destroy old array to create new one
        deallocate(physgrid%ptr%metrics, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate PhysGrid metrics", &
                                       ESMF_CONTEXT, rc)) return

        ! allocate new metric array of correct size
        allocate(physgrid%ptr%metrics(numMetricNew), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "PhysGrid metrics", &
                                       ESMF_CONTEXT, rc)) return

        ! fill new array with old values
        do n = 1, numMetricOld
          physgrid%ptr%metrics(n) = tempMetric(n)
        enddo

        deallocate(tempMetric, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate tempMetric", &
                                       ESMF_CONTEXT, rc)) return
      endif

      ! reset number of metrics and add new metric
      if (present(id)) id = numMetricNew
      physgrid%ptr%numMetrics = numMetricNew

      call ESMF_InternArraySet(physgrid%ptr%metrics(numMetricNew), name=name, &
                         rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      physgrid%ptr%metrics(numMetricNew) = metricArray

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetMetric

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridGetMetric"
!BOPI
! !IROUTINE: ESMF_PhysGridGetMetric - Gets interngrid metric from PhysGrid object.

! !INTERFACE:
      subroutine ESMF_PhysGridGetMetric(physgrid, metricArray, name, id, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      type(ESMF_InternArray), intent(out) :: metricArray
      character(*), intent(in), optional :: name
      integer, intent(in), optional :: id
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
!          Array containing metric values at for each interngrid cell.
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
!EOPI
! !REQUIREMENTS: 

      ! local variables
      integer :: localrc                             ! Error status
      integer :: n
      character(len=ESMF_MAXSTR) :: name_tmp        ! temp for name check 
      character(len=ESMF_MAXSTR) :: logMsg
      logical :: found                               ! name search flag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! if id supplied, check for valid id and return appropriate metric
      if (present(id)) then

        ! check for valid id
        if (id < 1 .or. id > physgrid%ptr%numMetrics) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "invalid metric id", &
                                      ESMF_CONTEXT, rc)
          return
        endif

        ! get metric associated with id
        metricArray = physgrid%ptr%metrics(id)

      ! if name supplied, check for valid id and return appropriate metric
      else if (present(name)) then

        found = .false.
        name_loop: do n=1,physgrid%ptr%numMetrics
            
          call ESMF_InternArrayGet(physgrid%ptr%metrics(n), name=name_tmp, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          if (trim(name) == trim(name_tmp)) then
            metricArray = physgrid%ptr%metrics(id)
            found = .true.
            exit name_loop
          endif
        end do name_loop

        if (.not. found) then
          write(logMsg, *) "No metric matches name.  Requested name: ", trim(name)
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, logMsg, &
                                      ESMF_CONTEXT, rc)
          return
        endif

      ! if we enter this else branch, neither id nor name has been 
      ! supplied so return error
      else
        call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                 "id or name must be supplied", &
                                    ESMF_CONTEXT, rc)
        return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetMetric

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridValidate"
!BOPI
! !IROUTINE: ESMF_PhysGridValidate - Check internal consistency of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridValidate(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid       
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
!EOPI
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_PhysGridValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridPrint"
!BOPI
! !IROUTINE: ESMF_PhysGridPrint - Print the contents of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridPrint(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid      
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ! local variables
      !integer :: localrc                             ! Error status
      integer :: i
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! This code will surely change, but for development purposes it
      ! is necessary to have some information available currently.

      print *, 'Physinterngrid:'
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSearchBboxSphericalPoint"
!!BOPI
!! !IROUTINE: ESMF_PhysGridSearchBboxSphericalPoint - Search interngrid for a point
!
!! !INTERFACE:
!      subroutine ESMF_PhysGridSearchBboxSphericalPoint(dst_add, x, y, DEid, &
!                                  physgrid, interndg, rc)
!
!!
!! !ARGUMENTS:
!      integer, dimension(?) :: dst_add
!      real(kind=ESMF_KIND_R8), intent(in) :: x
!      real(kind=ESMF_KIND_R8), intent(in) :: y
!      integer, intent(in) :: DEid
!      type(ESMF_PhysGrid), intent(inout) :: physgrid
!      type(ESMF_InternDG), intent(in) :: interndg
!      integer, intent(out), optional :: rc
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the interngrid of a interngrid cell 
!!     containing the point given by the input x,y coordinates.  This 
!!     instantiation uses a simple bounding box check to search the
!!     interngrid and is therefore only applicable to interngrids where the logical
!!     and physical axes are aligned and logically-rectangular.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dst\_add]
!!          Address of interngrid cell containing the search point.
!!     \item[x,y]
!!          Coordinates of search point.
!!     \item[DEid]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[physgrid]
!!          {\tt ESMF\_PhysGrid} to search for location.
!!     \item[interndg]
!!          {\tt ESMF\_InternDG} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

!      character(len=ESMF_MAXSTR) :: logMsg
!!
!      ! check variables
!      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)
!
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
!            phys_interngrid
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
!         get jb,je,ib,ie, interngrid corners, interngrid centers
!
!         do j=jb,je     !jb,je correspond to exclusive domain on this DE
!         do i=ib,ie     !ib,ie ditto
!
!!
!!           check bounding box of local interngrid cell
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
!        call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
!                                 "more than one cell contains this point", &
!                                  ESMF_CONTEXT, rc)
!        return
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSearchMyDECartPt"
!BOPI
! !IROUTINE: ESMF_PhysGridSrchMyDECartPt - Search interngrid on this DE for a point

! !INTERFACE:
      subroutine ESMF_PhysGridSearchMyDECartPt(physgrid, dstAdd, point, rc)

!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      integer, dimension(2) :: dstAdd
      real(kind=ESMF_KIND_R8), dimension(2), intent(in) :: point
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     This routine searches for the location in the interngrid of a interngrid cell 
!     containing the point given by the input x,y coordinates.  This 
!     instantiation uses a simple bounding box check to search the
!     interngrid and is therefore only applicable to interngrids where the logical
!     and physical axes are aligned and logically-rectangular.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} to search for location.
!     \item[dstAdd]
!          Address of interngrid cell containing the search point.
!     \item[point]
!          Coordinates of search point.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!REQUIREMENTS:  SSSn.n, GGGn.n

      ! local variables
      integer :: i, ib, ie, j, jb, je
      real(ESMF_KIND_R8) ::  cellMinX,  cellMaxX,  cellMinY,  cellMaxY
      real(ESMF_KIND_R8) :: localMinX, localMaxX, localMinY, localMaxY
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: cornerX, cornerY

      ! Initialize return code - assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! initialize destination address to zero
      dstAdd = 0

      call ESMF_InternArrayGetData(physgrid%ptr%regions%vertices(1), cornerX, &
                             ESMF_DATA_REF, rc=rc)
      call ESMF_InternArrayGetData(physgrid%ptr%regions%vertices(2), cornerY, &
                             ESMF_DATA_REF, rc=rc)

      ! extract local minima and maxima from the vertex arrays
      localMinX = minval(cornerX)
      localMaxX = maxval(cornerX)
      localMinY = minval(cornerY)
      localMaxY = maxval(cornerY)

      ! first check the bounding box for myDE
      if (point(1) < localMinX .OR. &
          point(1) > localMaxX .OR. &
          point(2) < localMinY .OR. &
          point(2) > localMaxY) return ! point not in this DE

      ! point may be somewhere in this DE, loop through the cells on the DE

      ! get jb,je,ib,ie for the interngrid corners
      ib = 2
      ie = size(cornerX,2) - 1
      jb = 2
      je = size(cornerY,3) - 1

      do j   = jb,je     !jb,je correspond to exclusive domain on this DE
        do i = ib,ie     !ib,ie ditto

          ! check bounding box of local interngrid cell
          cellMinX = minval(cornerX(:,i,j))
          cellMaxX = maxval(cornerX(:,i,j))
          cellMinY = minval(cornerY(:,i,j))
          cellMaxY = maxval(cornerY(:,i,j))

          if (point(1).gt.cellMinX .AND. point(1).le.cellMaxX .AND. &
              point(2).gt.cellMinY .AND. point(2).le.cellMaxY) then ! point is in this cell
            dstAdd(1) = i         ! local address of this cell
            dstAdd(2) = j         ! local address of this cell
            if (present(rc)) rc = ESMF_SUCCESS
            return
          endif

        enddo
      enddo

      ! set return code
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSearchMyDECartPt

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSearchMyDERowCol"
!BOPI
! !IROUTINE: ESMF_PhysGridSrchMyDERowCol - Search interngrid on this DE for a row or column

! !INTERFACE:
      subroutine ESMF_PhysGridSearchMyDERowCol(physgrid, dstAdd, point, option, &
                                               total, rc)

!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(inout) :: physgrid
      integer, dimension(2) :: dstAdd
      real(kind=ESMF_KIND_R8), dimension(2), intent(in) :: point
      character(3), intent(in) :: option
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     This routine searches for the location in the interngrid of a row and/or column
!     containing the point given by the input x,y coordinates.  This 
!     instantiation uses a simple bounding box check to search the
!     interngrid and is therefore only applicable to interngrids where the logical
!     and physical axes are aligned and logically-rectangular.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} to search for location.
!     \item[dstAdd]
!          Address pair of the corresponding row and/or column bounding
!          the point.  In the case that the point is coincident with a interngrid
!          line, the min/max flag is used to determine which value to return.
!     \item[point]
!          Coordinates of search point.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!REQUIREMENTS:  SSSn.n, GGGn.n

      ! local variables
      integer :: i, ib, ie, j, jb, je
      logical :: totalUse
      real(ESMF_KIND_R8) ::  cellMinX,  cellMaxX,  cellMinY,  cellMaxY
      real(ESMF_KIND_R8) :: localMinX, localMaxX, localMinY, localMaxY
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: cornerX, cornerY

      ! Initialize return code - assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! check validity of "option" argument
      if (option.ne.'min' .AND. option.ne.'max') then
      endif
 
      ! set default values for optional arguments
      totalUse = .false.
      if (present(total)) totalUse = total

      ! initialize destination address to zero for max option and something large
      ! for min
      dstAdd = 0
      if (option.eq.'min') dstAdd = 12345678

      call ESMF_InternArrayGetData(physgrid%ptr%regions%vertices(1), cornerX, &
                             ESMF_DATA_REF, rc=rc)
      call ESMF_InternArrayGetData(physgrid%ptr%regions%vertices(2), cornerY, &
                             ESMF_DATA_REF, rc=rc)

      ! extract local minima and maxima from the vertex arrays
      localMinX = minval(cornerX)
      localMaxX = maxval(cornerX)
      localMinY = minval(cornerY)
      localMaxY = maxval(cornerY)

      ! get jb,je,ib,ie for the interngrid corners
      ib = 1
      ie = size(cornerX,2)
      jb = 1
      je = size(cornerY,3)

      if (option.eq.'min') then
        if (point(1).lt.localMinX .AND. point(1).lt.localMaxX) dstAdd(1) = ib
        if (point(2).lt.localMinY .AND. point(2).lt.localMaxY) dstAdd(2) = jb
      endif
      if (option.eq.'max') then
        if (point(1).gt.localMaxX .AND. point(1).gt.localMinX) dstAdd(1) = ie
        if (point(2).gt.localMaxY .AND. point(2).gt.localMinY) dstAdd(2) = je
      endif

      ! first check the bounding box for myDE to see if both values are
      ! out of bounds
      if ((point(1) < localMinX  .OR. &
           point(1) > localMaxX) .AND. &
          (point(2) < localMinY .OR. &
           point(2) > localMaxY)) then ! neither of the values is here
        continue
      else
        ! point may be somewhere in this DE, loop through the cells on the DE
        do j   = jb,je     !jb,je correspond to exclusive domain on this DE
          do i = ib,ie     !ib,ie ditto

            ! check bounding box of local interngrid cell
            cellMinX = minval(cornerX(:,i,j))
            cellMaxX = maxval(cornerX(:,i,j))
            cellMinY = minval(cornerY(:,i,j))
            cellMaxY = maxval(cornerY(:,i,j))

            ! check against i-direction first
            if (point(1).ge.cellMinX .AND. point(1).le.cellMaxX) then
              if (option.eq.'min' .AND. i.lt.dstAdd(1)) dstAdd(1) = i
              if (option.eq.'max' .AND. i.gt.dstAdd(1)) dstAdd(1) = i
            endif
            ! and j-direction separately
            if (point(2).ge.cellMinY .AND. point(2).le.cellMaxY) then
              if (option.eq.'min' .AND. j.lt.dstAdd(2)) dstAdd(2) = j
              if (option.eq.'max' .AND. j.gt.dstAdd(2)) dstAdd(2) = j
            endif

          enddo
        enddo
      endif

      ! if total, modify the results slightly for internal interngrid halo width
      if (totalUse) then
        dstAdd(1) = dstAdd(1) + 1
        dstAdd(2) = dstAdd(2) + 1
      endif

      ! set return code
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSearchMyDERowCol

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSearchBboxCartesianPoint"
!!BOPI
!! !IROUTINE: ESMF_PhysGridSearchBboxCartesianPoint - Search interngrid for a point
!
!! !INTERFACE:
!      subroutine ESMF_PhysGridSearchBboxCartesianPoint(dst_add, x, y, DEid, &
!                                                       physgrid, interndg, rc)
!
!!
!! !ARGUMENTS:
!      integer, dimension(?) :: dst_add
!      real(kind=ESMF_KIND_R8), intent(in) :: x
!      real(kind=ESMF_KIND_R8), intent(in) :: y
!      integer, intent(in) :: DEid
!      type(ESMF_PhysGrid), intent(inout) :: physgrid
!      type(ESMF_InternDG), intent(in) :: interndg
!      integer, intent(out), optional :: rc
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the interngrid of a interngrid cell 
!!     containing the point given by the input x,y coordinates.  This 
!!     instantiation uses a simple bounding box check to search the
!!     interngrid and is therefore only applicable to interngrids where the logical
!!     and physical axes are aligned and logically-rectangular.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dst\_add]
!!          Address of interngrid cell containing the search point.
!!     \item[x,y]
!!          Coordinates of search point.
!!     \item[DEid]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[physgrid]
!!          {\tt ESMF\_PhysGrid} to search for location.
!!     \item[interndg]
!!          {\tt ESMF\_InternDG} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

!      character(len=ESMF_MAXSTR) :: logMsg
!
!      ! check variables
!      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)
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
!            phys_interngrid
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
!         get jb,je,ib,ie, interngrid corners, interngrid centers
!
!         do j=jb,je     !jb,je correspond to exclusive domain on this DE
!         do i=ib,ie     !ib,ie ditto
!
!!
!!           check bounding box of local interngrid cell
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
!      call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
!                               "more than one cell contains this point", &
!                                ESMF_CONTEXT, rc)
!        return
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridSearchGeneralSphericalPoint"
!!BOPI
!! !IROUTINE: ESMF_PhysGridSearchGeneralSphericalPoint - Search interngrid for a point
!
!! !INTERFACE:
!      subroutine ESMF_PhysGridSearchGeneralSphericalPoint(dst_add, x, y, DEid, &
!                                                          physgrid, interndg, rc)
!
!!
!! !ARGUMENTS:
!      integer, dimension(?) :: dst_addt
!      real(kind=ESMF_KIND_R8), intent(in) :: x
!      real(kind=ESMF_KIND_R8), intent(in) :: y
!      integer, intent(in) :: DEid
!      type(ESMF_PhysGrid), intent(inout) :: physgrid
!      type(ESMF_InternDG), intent(in) :: interndg
!      integer, intent(out), optional :: rc
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the interngrid of a interngrid cell 
!!     containing the point given by the input x,y coordinates.  This 
!!     instantiation uses a general (cross-product) check to search the
!!     interngrid and works for all cells that are non-convex.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dst\_add]
!!          Address of interngrid cell containing the search point.
!!     \item[x,y]
!!          Coordinates of search point.
!!     \item[DEid]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[physgrid]
!!          {\tt ESMF\_PhysGrid} to search for location.
!!     \item[interndg]
!!          {\tt ESMF\_InternDG} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

!      character(len=ESMF_MAXSTR) :: logMsg
!
!      ! check variables
!      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)
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
!            phys_interngrid
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
!         get jb,je,ib,ie, interngrid corners, interngrid centers
!
!         interngrid_loop_j: do j=jb,je   !ib,ie,jb,je correspond to exclusive 
!         do i=ib,ie                !domain on this DE
!
!!
!!           check bounding box of local interngrid cell
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
!        call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
!                                   "more than one cell contains this point", &
!                                    ESMF_CONTEXT, rc)
!        return
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridPointInCell"
!BOPI
! !IROUTINE: ESMF_PhysGridPointInCell - Checks whether cell contains point
!
! !INTERFACE:
      function ESMF_PhysGridPointInCell(pointX, pointY, cornerX, cornerY, rc)

!
! !RETURN VALUE:
      logical :: ESMF_PhysGridPointInCell ! true if point located in cell
!
! !ARGUMENTS:
      real(kind=ESMF_KIND_R8), intent(in) ::  pointX
      real(kind=ESMF_KIND_R8), intent(in) ::  pointY
      real(kind=ESMF_KIND_R8), dimension(:), intent(in) :: cornerX
      real(kind=ESMF_KIND_R8), dimension(:), intent(in) :: cornerY
      integer, intent(out), optional :: rc
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
!          x-coordinate of interngrid cell corners.
!     \item[cornerY]
!          y-coordinate of interngrid cell corners.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ! local variables
      !integer :: localrc                             ! Error status
      integer :: ncorn, next_n   ! corner index
      integer :: num_corners     ! number of corners in each interngrid cell

      real(kind=ESMF_KIND_R8) :: vec1X, vec1Y  ! components of the cell 
                                               ! side vector
      real(kind=ESMF_KIND_R8) :: vec2X, vec2Y  ! components of the vector
                                               !  from vertex to point
      real(kind=ESMF_KIND_R8) :: cross_product ! cross product of two vectors
      real(kind=ESMF_KIND_R8) :: test_product
      real(kind=ESMF_KIND_R8) :: ref_product   ! the cross product for first
                                               ! non-zero value
      !real(kind=ESMF_KIND_R8) :: sign_test     ! test to see if cross products
                                               ! are same sign
      real(kind=ESMF_KIND_R8) :: zero, one
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! set default return value
      ESMF_PhysGridPointInCell = .false.

      ! set constants
      zero = 0.0
      one  = 1.0
      ref_product = zero

      ! perform the cross product for each cell side
      num_corners = size(cornerX)

      corner_loop: do ncorn=1,num_corners
        next_n = MOD(ncorn,num_corners) + 1

        ! here we take the cross product of the vector making
        ! up each cell side with the vector formed by the vertex
        ! and search point.  if all the cross products are
        ! the same sign, the point is contained in the cell.
        vec1X = cornerX(next_n) - cornerX(ncorn)
        vec1Y = cornerY(next_n) - cornerY(ncorn)
        vec2X = pointX - cornerX(ncorn)
        vec2Y = pointY - cornerY(ncorn)

        ! if search point coincident with vertex then cell contains the point
        if (vec2X == 0 .and. vec2Y == 0) then
          ESMF_PhysGridPointInCell = .true.
          exit corner_loop
        endif

        ! if cell side has zero length (degenerate vertices)
        ! then skip the side and move on to the next
        if (vec1X == 0 .and. vec1Y == 0) cycle corner_loop

        ! compute cross product
        cross_product = vec1X*vec2Y - vec2X*vec1Y

        ! if the cross product is zero, the point
        ! lies exactly on the side and is contained in the cell
        ! TODO:  talk to Phil - not exactly true since if either all
        !        three x-points or all three y-points are colinear the
        !        cross product will be zero but the point not necessarily inside
        if (cross_product == zero) then
          ESMF_PhysGridPointInCell = .true.
          exit corner_loop
        endif

        ! if this is the first side, set a reference cross product
        ! to the current value
        ! otherwise, if this product is a different sign than
        ! previous (reference) cross products, exit the loop
        if (ref_product == zero) then
          ref_product = cross_product
          test_product = one
        else
          test_product = cross_product*ref_product
        endif
        if (test_product < zero) exit corner_loop ! x-prod has different sign

      end do corner_loop

      ! if cross products all same sign this location contains the pt
      if (test_product > zero)  ESMF_PhysGridPointInCell = .true.

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_PhysGridPointInCell

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridCompDistSpherical"
!BOPI
! !IROUTINE: ESMF_PhysGridCompDistSpherical - compute distance spherical coords
!
! !INTERFACE:
      function ESMF_PhysGridCompDistSpherical(lon1, lat1, lon2, lat2, radius,rc)

!
! !RETURN VALUE:
      real(kind=ESMF_KIND_R8) :: ESMF_PhysGridCompDistSpherical
!
! !ARGUMENTS:
      real(kind=ESMF_KIND_R8), intent(in) :: lon1
      real(kind=ESMF_KIND_R8), intent(in) :: lat1
      real(kind=ESMF_KIND_R8), intent(in) :: lon2
      real(kind=ESMF_KIND_R8), intent(in) :: lat2
      real(kind=ESMF_KIND_R8), intent(in), optional :: radius
      integer, intent(out), optional :: rc
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ! local variables
      real(kind=ESMF_KIND_R8) :: rlon1, rlat1, rlon2, rlat2  ! lon/lat in degrees
      real(kind=ESMF_KIND_R8) :: pi, innards
      real(kind=ESMF_KIND_R8) :: tiny = 1.0d-12              ! TODO: should be in base
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! set constants
      pi = 3.1416d0   ! TODO really set pi, just a bug fix for now

      ! convert input coordinates to radians
      rlon1 = lon1*pi/180.d0
      rlat1 = lat1*pi/180.d0
      rlon2 = lon2*pi/180.d0
      rlat2 = lat2*pi/180.d0

      ! compute angular distance
      innards = cos(rlat1)*cos(rlat2)*cos(rlon1)*cos(rlon2) + &
                cos(rlat1)*cos(rlat2)*sin(rlon1)*sin(rlon2) + &
                sin(rlat1)*sin(rlat2)
      if (innards.gt.1.0d0 .AND. (innards-tiny).le.1.0d0) innards = 1.0d0
      ESMF_PhysGridCompDistSpherical = acos(innards)

      ! if radius present, convert to linear distance
      if (present(radius)) then
         ESMF_PhysGridCompDistSpherical = radius*ESMF_PhysGridCompDistSpherical
   !  ???? JW
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCompDistSpherical

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridCompDistCartesian"
!BOPI
! !IROUTINE: ESMF_PhysGridCompDistCartesian - Distance in Cartesian coords
!
! !INTERFACE:
      function ESMF_PhysGridCompDistCartesian(x1, y1, x2, y2, rc)

!
! !RETURN VALUE:
      real(kind=ESMF_KIND_R8) :: ESMF_PhysGridCompDistCartesian
!
! !ARGUMENTS:
      real(kind=ESMF_KIND_R8), intent(in) :: x1
      real(kind=ESMF_KIND_R8), intent(in) :: y1
      real(kind=ESMF_KIND_R8), intent(in) :: x2
      real(kind=ESMF_KIND_R8), intent(in) :: y2
      integer, intent(out), optional :: rc
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ! local variables
      !integer :: localrc                             ! Error status
      
      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! compute distance using the usual Cartesian formula
      ESMF_PhysGridCompDistCartesian = sqrt( (x2-x1)**2 + (y2-y1)**2 )

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCompDistCartesian

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridMaskTypeEqual"
!BOPI
! !IROUTINE: ESMF_InternGridMaskTypeEqual - equality of PhysGrid mask types
!
! !INTERFACE:
      function ESMF_InternGridMaskTypeEqual(InternGridMaskType1, InternGridMaskType2)

! !RETURN VALUE:
      logical :: ESMF_InternGridMaskTypeEqual

! !ARGUMENTS:
      type(ESMF_InternGridMaskType), intent(in) :: InternGridMaskType1
      type(ESMF_InternGridMaskType), intent(in) :: InternGridMaskType2

! !DESCRIPTION:
!     This routine compares two ESMF PhysGrid mask types to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridMaskType1, InternGridMaskType2]
!          Two mask types to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridMaskTypeEqual = (InternGridMaskType1%maskType == &
                                InternGridMaskType2%maskType)

      end function ESMF_InternGridMaskTypeEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridMaskTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_InternGridMaskTypeNotEqual - non-equality of PhysGrid mask types
!
! !INTERFACE:
      function ESMF_InternGridMaskTypeNotEqual(InternGridMaskType1, InternGridMaskType2)

! !RETURN VALUE:
      logical :: ESMF_InternGridMaskTypeNotEqual

! !ARGUMENTS:

      type(ESMF_InternGridMaskType), intent(in) :: InternGridMaskType1
      type(ESMF_InternGridMaskType), intent(in) :: InternGridMaskType2

! !DESCRIPTION:
!     This routine compares two ESMF PhysGrid mask types to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridMaskType1, InternGridMaskType2]
!          Two kinds of PhysGrid mask types to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridMaskTypeNotEqual = (InternGridMaskType1%maskType /= &
                                   InternGridMaskType2%maskType)

      end function ESMF_InternGridMaskTypeNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegionTypeEqual"
!BOPI
! !IROUTINE: ESMF_RegionTypeEqual - equality of PhysGrid region kinds
!
! !INTERFACE:
      function ESMF_RegionTypeEqual(RegionType1, RegionType2)

! !RETURN VALUE:
      logical :: ESMF_RegionTypeEqual

! !ARGUMENTS:

      type(ESMF_RegionType), intent(in) :: RegionType1
      type(ESMF_RegionType), intent(in) :: RegionType2

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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegionTypeEqual = (RegionType1%regionType == &
                              RegionType2%regionType)

      end function ESMF_RegionTypeEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegionTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_RegionTypeNotEqual - non-equality of PhysGrid region kinds
!
! !INTERFACE:
      function ESMF_RegionTypeNotEqual(RegionType1, RegionType2)

! !RETURN VALUE:
      logical :: ESMF_RegionTypeNotEqual

! !ARGUMENTS:

      type(ESMF_RegionType), intent(in) :: RegionType1
      type(ESMF_RegionType), intent(in) :: RegionType2

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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegionTypeNotEqual = (RegionType1%regionType /= &
                                 RegionType2%regionType)

      end function ESMF_RegionTypeNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridOrientEqual"
!BOPI
! !IROUTINE: ESMF_PhysGridOrientEqual - equality of PhysGrid orientation
!
! !INTERFACE:
      function ESMF_PhysGridOrientEqual(Orientation1, Orientation2)

! !RETURN VALUE:
      logical :: ESMF_PhysGridOrientEqual

! !ARGUMENTS:

      type(ESMF_PhysGridOrientation), intent(in) :: Orientation1
      type(ESMF_PhysGridOrientation), intent(in) :: Orientation2

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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_PhysGridOrientEqual = (Orientation1%orientation == &
                                  Orientation2%orientation)

      end function ESMF_PhysGridOrientEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_PhysGridOrientNotEqual"
!BOPI
! !IROUTINE: ESMF_PhysGridOrientNotEqual - non-equality of PhysGrid orientations
!
! !INTERFACE:
      function ESMF_PhysGridOrientNotEqual(Orientation1, Orientation2)

! !RETURN VALUE:
      logical :: ESMF_PhysGridOrientNotEqual

! !ARGUMENTS:
      type(ESMF_PhysGridOrientation), intent(in) :: Orientation1
      type(ESMF_PhysGridOrientation), intent(in) :: Orientation2

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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_PhysGridOrientNotEqual = (Orientation1%orientation /= &
                                     Orientation2%orientation)

      end function ESMF_PhysGridOrientNotEqual

!------------------------------------------------------------------------------

      end module ESMF_PhysGridMod

