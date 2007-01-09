! $Id: ESMF_GridTypes.F90,v 1.52 2007/01/09 21:10:22 oehmke Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_GridTypes.F90"
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
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod    ! ESMF local array class
      use ESMF_InternArrayDataMapMod  ! ESMF data map class
      use ESMF_DELayoutMod      ! ESMF layout class
      use ESMF_InternDGMod      ! ESMF distributed grid class
      use ESMF_PhysCoordMod     ! ESMF physical coord class
      use ESMF_PhysGridMod      ! ESMF physical grid class
      use ESMF_InitMacrosMod  ! init macros stuff
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
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: deltaPerDim
        type(ESMF_LocalArray), dimension(:), pointer :: coords
        integer, dimension(ESMF_MAXGRIDDIM) :: countPerDim

         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridSpecific
!
!     ! Type to hold pointers to all available specific grid derived types

      type ESMF_GridSpecific
      sequence
        type (ESMF_LogRectGrid), pointer :: logRectGrid

        ESMF_INIT_DECLARE
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
!     ! ESMF_GridVertType
!
!     ! Type to specify kind of vertical grid for supported ESMF Grids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_GridVertType
      sequence
!      private
        integer :: gridVertType
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridHorzStagger
!
!     ! Type to specify type of horizontal grid staggering for supported
!     !  ESMF Grids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_GridHorzStagger
      sequence
!      private
        integer :: stagger
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridVertStagger
!
!     ! Type to specify type of vertical grid staggering for supported
!     !  ESMF Grids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_GridVertStagger
      sequence
!      private
        integer :: stagger
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridStorage
!
!     ! Type to specify type of grid storage schemes for supported
!     !  ESMF Grids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_GridStorage
      sequence
!      private
        integer :: storage
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

        type (ESMF_Base) :: base              ! base class object
        type (ESMF_GridStatus) :: gridStatus  ! uninitialized, init ok, etc

#ifdef ESMF_IS_32BIT_MACHINE

        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: minGlobalCoordPerDim
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: maxGlobalCoordPerDim

        integer :: dimCount                   ! number of dimensions
#else
        integer :: dimCount                   ! number of dimensions

        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: minGlobalCoordPerDim
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: maxGlobalCoordPerDim

#endif

        type (ESMF_Logical) :: hasLocalData
        type (ESMF_GridStructure) :: gridStructure
                                              ! enum for structure of grid
                                              ! i.e. logically rectangular, etc
        type (ESMF_GridType) :: horzGridType  ! enum for type of horizontal grid
        type (ESMF_GridVertType) :: vertGridType
                                              ! enum for type of vertical grid
        type (ESMF_GridHorzStagger) :: horzStagger
                                              ! enum for horizontal grid staggering
        type (ESMF_GridVertStagger) :: vertStagger
                                              ! enum for vertical grid staggering
        type (ESMF_GridStorage) :: gridStorage
                                              ! enum for grid storage scheme
        type (ESMF_CoordSystem) :: horzCoordSystem  
                                              ! identifier for horizontal
                                              ! physical coordinate system
        type (ESMF_CoordSystem) :: vertCoordSystem  
                                              ! identifier for vertical
                                              ! physical coordinate system
        type (ESMF_CoordOrder) :: coordOrder  ! enum for mapping of xyz to ijk
        type (ESMF_CoordIndex) :: coordIndex  ! enum for global, local indexing
        type (ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
                                              ! logical identifier to indicate
                                              ! periodic boundary conditions in
                                              ! each direction
        integer :: numPhysGrids               ! number of grid descriptors
                                              ! necessary to support
                                              ! staggering, vertical
                                              ! grids, background grids


        integer :: numPhysGridsAlloc          ! number of physgrids allocated

        type (ESMF_PhysGrid), dimension(:), pointer :: physgrids
                                              ! info for all grid descriptions
                                              ! necessary to define horizontal,
                                              ! staggered and vertical grids
        integer, dimension(:), pointer :: internDGIndex
                                              ! for each physgrid, the index of
                                              ! the corresponding InternDG


        type (ESMF_InternDG), dimension(:), pointer :: internDGs       
                                              ! decomposition and other
                                              ! logical space info for grid

        character(len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: dimNames
        character(len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: dimUnits

        type (ESMF_LocalArray) :: boundingBoxes
                                            ! array of bounding boxes on each DE
                                            ! used for search routines
        type (ESMF_GridSpecific) :: gridSpecific

        integer :: numInternDGsAlloc          ! number of InternDGs allocated

        integer :: numInternDGs               ! number of grid descriptors
                                              ! necessary to support
                                              ! staggering, vertical
                                              ! grids, background grids
!       type (???) :: searchStructure

         ESMF_INIT_DECLARE

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
       
        ESMF_INIT_DECLARE

      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

      public ESMF_LogRectGrid,   ESMF_GridSpecific,    ESMF_GridStatus
      public ESMF_GridStructure, ESMF_GridHorzStagger, ESMF_GridVertStagger
      public ESMF_GridClass,     ESMF_GridType,        ESMF_GridVertType
      public ESMF_CoordOrder,    ESMF_CoordIndex,      ESMF_GridStorage
      public ESMF_Grid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    ! These functions are generally accessed only by the main Grid module
    ! and are not meant to be accessed by the user.  Well...except for
    ! the overloaded operators.

    public ESMF_GridGetInit 
    public ESMF_GridClassValidate   
    public ESMF_GridClassGetInit 
    public ESMF_LogRectGridValidate   
    public ESMF_LogRectGridInit
    public ESMF_LogRectGridGetInit 
    public ESMF_GridSpecificValidate   
    public ESMF_GridSpecificInit
    public ESMF_GridSpecificGetInit 
    public ESMF_GridConstructNew
    public ESMF_GridGetDELayout
    public ESMF_GridAddInternDG
    public ESMF_GridMakeInternDGSpace
    public ESMF_GridAddPhysGrid
    public ESMF_GridMakePhysGridSpace
    public ESMF_GridGetPhysGrid
    public ESMF_GridGetPhysGridID
    public ESMF_GridGetInternDG
    public ESMF_GridGetBoundingBoxes

    public operator(==), operator(/=) ! for overloading 
                                      ! comparison functions

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:
  ! Supported ESMF structure kinds:
  !   ESMF_GRID_STATUS_UNKNOWN           ! unknown or undefined status
  !   ESMF_GRID_STATUS_UNINIT            ! uninitialized
  !   ESMF_GRID_STATUS_READY             ! completely ready for use
  !   ESMF_GRID_STATUS_UNALLOCATED       ! unallocated
  !   ESMF_GRID_STATUS_ALLOCATED         ! allocated
  !   ESMF_GRID_STATUS_INIT              ! initialized but not distributed
  !   ESMF_GRID_STATUS_INVALID           ! invalid

   type (ESMF_GridStatus), parameter, public ::              &
      ESMF_GRID_STATUS_UNKNOWN      =  ESMF_GridStatus(0), &
      ESMF_GRID_STATUS_UNINIT       =  ESMF_GridStatus(1), &
      ESMF_GRID_STATUS_READY        =  ESMF_GridStatus(2), &
      ESMF_GRID_STATUS_UNALLOCATED  =  ESMF_GridStatus(3), &
      ESMF_GRID_STATUS_ALLOCATED    =  ESMF_GridStatus(4), &
      ESMF_GRID_STATUS_INIT         =  ESMF_GridStatus(5), &
      ESMF_GRID_STATUS_INVALID      =  ESMF_GridStatus(6)

  ! Supported ESMF structure kinds:
  !   ESMF_GRID_STRUCTURE_UNKNOWN         ! unknown or undefined grid
  !   ESMF_GRID_STRUCTURE_LOGRECT         ! logically rectangular grid
  !   ESMF_GRID_STRUCTURE_LOGRECT_BLK     ! logically rectangular blocked grid
  !   ESMF_GRID_STRUCTURE_UNSTRUCT        ! unstructured grid
  !   ESMF_GRID_STRUCTURE_USER            ! user-defined grid

   type (ESMF_GridStructure), parameter, public ::              &
      ESMF_GRID_STRUCTURE_UNKNOWN     =  ESMF_GridStructure(0), &
      ESMF_GRID_STRUCTURE_LOGRECT     =  ESMF_GridStructure(1), &
      ESMF_GRID_STRUCTURE_LOGRECT_BLK =  ESMF_GridStructure(2), &
      ESMF_GRID_STRUCTURE_UNSTRUCT    =  ESMF_GridStructure(3), &
      ESMF_GRID_STRUCTURE_USER        =  ESMF_GridStructure(4)

  ! Supported ESMF grid kinds:
  !   ESMF_GRID_TYPE_UNKNOWN           ! unknown or undefined grid
  !   ESMF_GRID_TYPE_LATLON            ! aligned with longitude,latitude
  !   ESMF_GRID_TYPE_LATLON_UNI        ! LatLon grid with uniform spacing
  !   ESMF_GRID_TYPE_LATLON_GAUSS      ! LatLon with gaussian-spaced latitudes
  !   ESMF_GRID_TYPE_LATLON_MERC       ! LatLon with Mercator-spaced latitudes
  !   ESMF_GRID_TYPE_REDUCED           ! LatLon with num lon pts a fcn of lat
  !   ESMF_GRID_TYPE_DIPOLE            ! Displaced-pole dipole grid
  !   ESMF_GRID_TYPE_TRIPOLE           ! Tripolar grids
  !   ESMF_GRID_TYPE_XY                ! aligned with Cartesian x-y coords
  !   ESMF_GRID_TYPE_XY_UNI            ! XY grid with uniform spacing
  !   ESMF_GRID_TYPE_DATASTREAM        ! Data stream - set of locations
  !   ESMF_GRID_TYPE_PHYSFOURIER       ! Mixed Fourier/Phys Space grid
  !   ESMF_GRID_TYPE_SPHER_SPECT       ! spectral space:spherical harmonics
  !   ESMF_GRID_TYPE_CART_SPECT        ! spectral space:Cartesian coords
  !   ESMF_GRID_TYPE_GEODESIC          ! spherical geodesic grid
  !   ESMF_GRID_TYPE_CUBEDSPHERE       ! cubed sphere grid
  !   ESMF_GRID_TYPE_EXCHANGE          ! intersection of two grids

   type (ESMF_GridType), parameter, public ::              &
      ESMF_GRID_TYPE_UNKNOWN           = ESMF_GridType( 0), &
      ESMF_GRID_TYPE_LATLON            = ESMF_GridType( 1), &
      ESMF_GRID_TYPE_LATLON_UNI        = ESMF_GridType( 2), &
      ESMF_GRID_TYPE_LATLON_GAUSS      = ESMF_GridType( 3), &
      ESMF_GRID_TYPE_LATLON_MERC       = ESMF_GridType( 4), &
      ESMF_GRID_TYPE_REDUCED           = ESMF_GridType( 5), &
      ESMF_GRID_TYPE_DIPOLE            = ESMF_GridType( 6), &
      ESMF_GRID_TYPE_TRIPOLE           = ESMF_GridType( 7), &
      ESMF_GRID_TYPE_XY                = ESMF_GridType( 8), &
      ESMF_GRID_TYPE_XY_UNI            = ESMF_GridType( 9), &
      ESMF_GRID_TYPE_DATASTREAM        = ESMF_GridType(10), &
      ESMF_GRID_TYPE_PHYSFOURIER       = ESMF_GridType(11), &
      ESMF_GRID_TYPE_SPHER_SPECT       = ESMF_GridType(12), &
      ESMF_GRID_TYPE_CART_SPECT        = ESMF_GridType(13), &
      ESMF_GRID_TYPE_GEODESIC          = ESMF_GridType(14), &
      ESMF_GRID_TYPE_CUBEDSPHERE       = ESMF_GridType(15), &
      ESMF_GRID_TYPE_EXCHANGE          = ESMF_GridType(16)

  ! Supported ESMF vertical grid kinds:
  !   ESMF_GRID_TYPE_VERT_UNKNOWN        ! unknown or undefined vertical grid
  !   ESMF_GRID_TYPE_VERT_USER           ! user-defined vertical grid
  !   ESMF_GRID_TYPE_VERT_DEPTH          ! vertical grid with 0 at top surface
  !   ESMF_GRID_TYPE_VERT_HEIGHT         ! vertical grid with 0 at bottom
  !   ESMF_GRID_TYPE_VERT_PRESSURE       ! vertical grid with pressure coordinates
  !   ESMF_GRID_TYPE_VERT_SIGMA          ! vertical grid with sigma coordinates
  !   ESMF_GRID_TYPE_VERT_THETA          ! vertical grid with theta coordinates
  !   ESMF_GRID_TYPE_VERT_ETA            ! vertical grid with eta coordinates
  !   ESMF_GRID_TYPE_VERT_ISOPYCNAL      ! vertical grid with density coordinates
  !   ESMF_GRID_TYPE_VERT_HYBRID         ! vertical grid with hybrid coordinates
  !   ESMF_GRID_TYPE_VERT_LAGRANGIAN     ! vertical grid with lagrangian coordinates

   type (ESMF_GridVertType), parameter, public ::              &
      ESMF_GRID_VERT_TYPE_UNKNOWN      = ESMF_GridVertType( 0), &
      ESMF_GRID_VERT_TYPE_USER         = ESMF_GridVertType( 1), &
      ESMF_GRID_VERT_TYPE_DEPTH        = ESMF_GridVertType( 2), &
      ESMF_GRID_VERT_TYPE_HEIGHT       = ESMF_GridVertType( 3), &
      ESMF_GRID_VERT_TYPE_PRESSURE     = ESMF_GridVertType( 4), &
      ESMF_GRID_VERT_TYPE_SIGMA        = ESMF_GridVertType( 5), &
      ESMF_GRID_VERT_TYPE_THETA        = ESMF_GridVertType( 6), &
      ESMF_GRID_VERT_TYPE_ETA          = ESMF_GridVertType( 7), &
      ESMF_GRID_VERT_TYPE_ISOPYCNAL    = ESMF_GridVertType( 8), &
      ESMF_GRID_VERT_TYPE_HYBRID       = ESMF_GridVertType( 9), &
      ESMF_GRID_VERT_TYPE_LAGRANGIAN   = ESMF_GridVertType(10)

  ! Recognized ESMF horizontal staggering types
  !   ESMF_GRID_HORZ_STAGGER_UNKNOWN  ! unknown or undefined staggering
  !   ESMF_GRID_HORZ_STAGGER_A        ! Arakawa A (centered velocity)
  !   ESMF_GRID_HORZ_STAGGER_B_NE     ! Arakawa B (U,V at NE corner)
  !   ESMF_GRID_HORZ_STAGGER_B_SW     ! Arakawa B (U,V at SW corner)
  !   ESMF_GRID_HORZ_STAGGER_B_SE     ! Arakawa B (U,V at SE corner)
  !   ESMF_GRID_HORZ_STAGGER_B_NW     ! Arakawa B (U,V at NW corner)
  !   ESMF_GRID_HORZ_STAGGER_C_NE     ! Arakawa C (U at E face, V at N face)
  !   ESMF_GRID_HORZ_STAGGER_C_SW     ! Arakawa C (U at W face, V at S face)
  !   ESMF_GRID_HORZ_STAGGER_C_SE     ! Arakawa C (U at E face, V at S face)
  !   ESMF_GRID_HORZ_STAGGER_C_NW     ! Arakawa C (U at W face, V at N face)
  !   ESMF_GRID_HORZ_STAGGER_D_NE     ! Arakawa D (V at E face, U at N face)
  !   ESMF_GRID_HORZ_STAGGER_D_SW     ! Arakawa D (V at W face, U at S face)
  !   ESMF_GRID_HORZ_STAGGER_D_SE     ! Arakawa D (V at E face, U at S face)
  !   ESMF_GRID_HORZ_STAGGER_D_NW     ! Arakawa D (V at W face, U at N face)
  !   ESMF_GRID_HORZ_STAGGER_E        ! Arakawa E
  !   ESMF_GRID_HORZ_STAGGER_Z        ! C grid equiv for geodesic grid

   type (ESMF_GridHorzStagger), parameter, public ::              &
      ESMF_GRID_HORZ_STAGGER_UNKNOWN  = ESMF_GridHorzStagger( 0), &
      ESMF_GRID_HORZ_STAGGER_A        = ESMF_GridHorzStagger( 1), &
      ESMF_GRID_HORZ_STAGGER_B_NE     = ESMF_GridHorzStagger( 2), &
      ESMF_GRID_HORZ_STAGGER_B_SW     = ESMF_GridHorzStagger( 3), &
      ESMF_GRID_HORZ_STAGGER_B_SE     = ESMF_GridHorzStagger( 4), &
      ESMF_GRID_HORZ_STAGGER_B_NW     = ESMF_GridHorzStagger( 5), &
      ESMF_GRID_HORZ_STAGGER_C_NE     = ESMF_GridHorzStagger( 6), &
      ESMF_GRID_HORZ_STAGGER_C_SW     = ESMF_GridHorzStagger( 7), &
      ESMF_GRID_HORZ_STAGGER_C_SE     = ESMF_GridHorzStagger( 8), &
      ESMF_GRID_HORZ_STAGGER_C_NW     = ESMF_GridHorzStagger( 9), &
      ESMF_GRID_HORZ_STAGGER_D_NE     = ESMF_GridHorzStagger(10), &
      ESMF_GRID_HORZ_STAGGER_D_SW     = ESMF_GridHorzStagger(11), &
      ESMF_GRID_HORZ_STAGGER_D_SE     = ESMF_GridHorzStagger(12), &
      ESMF_GRID_HORZ_STAGGER_D_NW     = ESMF_GridHorzStagger(13), &
      ESMF_GRID_HORZ_STAGGER_E        = ESMF_GridHorzStagger(14), &
      ESMF_GRID_HORZ_STAGGER_Z        = ESMF_GridHorzStagger(15)

  ! Recognized ESMF vertical staggering types
  !   ESMF_GRID_VERT_STAGGER_UNKNOWN  ! unknown or undefined staggering
  !   ESMF_GRID_VERT_STAGGER_CENTER   ! vertical midpoints
  !   ESMF_GRID_VERT_STAGGER_TOP      ! at top    face of vertical grid
  !   ESMF_GRID_VERT_STAGGER_BOTTOM   ! at bottom face of vertical grid

   type (ESMF_GridVertStagger), parameter, public ::              &
      ESMF_GRID_VERT_STAGGER_UNKNOWN  = ESMF_GridVertStagger( 0), &
      ESMF_GRID_VERT_STAGGER_CENTER   = ESMF_GridVertStagger( 1), &
      ESMF_GRID_VERT_STAGGER_TOP      = ESMF_GridVertStagger( 2), &
      ESMF_GRID_VERT_STAGGER_BOTTOM   = ESMF_GridVertStagger( 3)

  ! Recognized grid storage schemes
  !   ESMF_GRID_STORAGE_UNKNOWN   ! unknown or undefined grid storage
  !   ESMF_GRID_STORAGE_LOGRECT   ! uses logically rectangular storage, one
  !                               ! block per DE
  !   ESMF_GRID_STORAGE_BLOCK     ! uses logically rectangular storage, multiple
  !                               ! blocks per DE
  !   ESMF_GRID_STORAGE_ARBITRARY ! uses arbitrary storage, which infers a
  !                               ! scattering of grid cell locations and limits
  !                               ! available communication and query functions

   type (ESMF_GridStorage), parameter, public ::         &
      ESMF_GRID_STORAGE_UNKNOWN   = ESMF_GridStorage(0), &
      ESMF_GRID_STORAGE_LOGRECT   = ESMF_GridStorage(1), &
      ESMF_GRID_STORAGE_BLOCK     = ESMF_GridStorage(2), &
      ESMF_GRID_STORAGE_ARBITRARY = ESMF_GridStorage(3)

  ! Recognized coordinate orderings
  !   ESMF_COORD_ORDER_UNKNOWN  ! unknown or undefined coord ordering
  !   ESMF_COORD_ORDER_XYZ      ! IJK maps to XYZ
  !   ESMF_COORD_ORDER_XZY      ! IJK maps to XZY
  !   ESMF_COORD_ORDER_YXZ      ! IJK maps to YXZ
  !   ESMF_COORD_ORDER_YZX      ! IJK maps to YZX
  !   ESMF_COORD_ORDER_ZXY      ! IJK maps to ZXY
  !   ESMF_COORD_ORDER_ZYX      ! IJK maps to ZYX

   type (ESMF_CoordOrder), parameter, public ::       &
      ESMF_COORD_ORDER_UNKNOWN = ESMF_CoordOrder( 0), &
      ESMF_COORD_ORDER_XYZ     = ESMF_CoordOrder( 1), &
      ESMF_COORD_ORDER_XZY     = ESMF_CoordOrder( 2), &
      ESMF_COORD_ORDER_YXZ     = ESMF_CoordOrder( 3), &
      ESMF_COORD_ORDER_YZX     = ESMF_CoordOrder( 4), &
      ESMF_COORD_ORDER_ZXY     = ESMF_CoordOrder( 5), &
      ESMF_COORD_ORDER_ZYX     = ESMF_CoordOrder( 6)

  ! Recognized coordinate indexings
  !   ESMF_COORD_INDEX_UNKNOWN  ! unknown or undefined coord indexing
  !   ESMF_COORD_INDEX_LOCAL    ! uses local indexing
  !   ESMF_COORD_INDEX_GLOBAL   ! uses global indexing

   type (ESMF_CoordIndex), parameter, public ::       &
      ESMF_COORD_INDEX_UNKNOWN = ESMF_CoordIndex( 0), &
      ESMF_COORD_INDEX_LOCAL   = ESMF_CoordIndex( 1), &
      ESMF_COORD_INDEX_GLOBAL  = ESMF_CoordIndex( 2)

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
      '$Id: ESMF_GridTypes.F90,v 1.52 2007/01/09 21:10:22 oehmke Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridPointerEqual
         module procedure ESMF_GridStatusEqual
         module procedure ESMF_GridStructureEqual
         module procedure ESMF_GridTypeEqual
         module procedure ESMF_GridVertTypeEqual
         module procedure ESMF_GridHorzStaggerEqual
         module procedure ESMF_GridVertStaggerEqual
         module procedure ESMF_GridStorageEqual
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
         module procedure ESMF_GridPointerNotEqual
         module procedure ESMF_GridStatusNotEqual
         module procedure ESMF_GridStructureNotEqual
         module procedure ESMF_GridTypeNotEqual
         module procedure ESMF_GridVertTypeNotEqual
         module procedure ESMF_GridHorzStaggerNotEqual
         module procedure ESMF_GridVertStaggerNotEqual
         module procedure ESMF_GridStorageNotEqual
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridConstructNew"
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

      integer :: localrc                          ! Error status
      integer :: i
      !character (len = ESMF_MAXSTR) :: defaultname ! default grid name

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Set the Grid name if present, otherwise construct a default one
      call ESMF_BaseCreate(grid%base, "Grid", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Initialize grid contents
      grid%gridStatus      = ESMF_GRID_STATUS_READY
      grid%gridStructure   = ESMF_GRID_STRUCTURE_UNKNOWN
      grid%horzGridType    = ESMF_GRID_TYPE_UNKNOWN
      grid%vertGridType    = ESMF_GRID_VERT_TYPE_UNKNOWN
      grid%horzStagger     = ESMF_GRID_HORZ_STAGGER_UNKNOWN
      grid%vertStagger     = ESMF_GRID_VERT_STAGGER_UNKNOWN
      grid%gridStorage     = ESMF_GRID_STORAGE_UNKNOWN
      grid%horzCoordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      grid%vertCoordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      grid%coordOrder      = ESMF_COORD_ORDER_XYZ
      do i=1,ESMF_MAXGRIDDIM
        grid%periodic(i)     = ESMF_FALSE
      !   grid%coversDomain(i) = ESMF_TRUE
      enddo
      grid%numInternDGs = 0
      grid%numInternDGsAlloc = 0
      nullify(grid%internDGs)
      grid%numPhysGrids = 0
      grid%numPhysGridsAlloc = 0
      nullify(grid%physGrids)
      nullify(grid%internDGIndex)
      ! nullify(grid%gridSpecific)

      ESMF_INIT_SET_CREATED(grid)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridConstructNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetDELayout"
!BOP
! !IROUTINE: ESMF_GridGetDELayout - Get pointer to a DELayout from a Grid

! !INTERFACE:
      subroutine ESMF_GridGetDELayout(grid, delayout, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_DELayout),intent(out) :: delayout
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_InternDG} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[delayout]
!          Pointer to the {\tt ESMF\_DELayout} corresponding to the
!          {\tt ESMF\_Grid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level -- does not matter which one since they all share
      ! the same layout    !TODO: move layout to Grid class?
      call ESMF_InternDGGetDELayout(grid%ptr%internDGs(1)%ptr, delayout, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDELayout

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddInternDG"
!BOPI
! !IROUTINE: ESMF_GridAddInternDG - adds a complete InternDG to Grid type

! !INTERFACE:
      subroutine ESMF_GridAddInternDG(grid, interndg, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(inout) :: grid
      type(ESMF_InternDG), intent(in)    :: interndg
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine attaches an {\tt ESMF\_InternDG} object to an
!     {\tt ESMF\_Grid} object.  It is only meant to be called by
!     grid creation routines in the processes of building a grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Grid structure to which new InternDG is to be added.
!     \item[interndg]
!          Complete {\tt ESMF\_InternDG} to be added.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridClassGetInit,grid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_InternDGGetInit,interndg,rc)

      ! Update the InternDGAlloc counter and check to see if InternDG
      ! array needs to be resized to add the new interndg
      call ESMF_GridMakeInternDGSpace(grid, grid%numInternDGs+1, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      grid%numInternDGs = grid%numInternDGs + 1

      ! Add the InternDG
      grid%internDGs(grid%numInternDGs) = interndg

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddInternDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridMakeInternDGSpace"
!BOPI
! !IROUTINE: ESMF_GridMakeInternDGSpace - Allocate or extend InternDG array

! !INTERFACE:
      subroutine ESMF_GridMakeInternDGSpace(gridp, newcount, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: gridp
      integer, intent(in) :: newcount
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Internal routine which verifies there is enough array space to
!     hold the specified number of InternDGs.  Allocates more space and
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
      ! this routine. it's going to be pointed to by the InternDGs pointer
      ! in the grid structure, but that might not be obvious to the compiler.
      type(ESMF_InternDG), dimension(:), pointer, save :: temp_dgrids
      integer :: localrc                          ! Error status
      integer :: i, oldcount, alloccount

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridClassGetInit,gridp,rc)

      ! number of currently used and available entries
      oldcount = gridp%numInternDGs
      alloccount = gridp%numInternDGsAlloc

      ! if there are already enough, we are done.
      if (alloccount .ge. newcount) then
         rc = ESMF_SUCCESS
         return
      endif

#define CHUNK 4

      ! if none are allocated yet, allocate and return.
      ! the chunksize is 4 because it is a round number in base 2.
      if (alloccount .eq. 0) then
        allocate(gridp%internDGs(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial InternDGs", &
                                       ESMF_CONTEXT, rc)) return
        gridp%numInternDGsAlloc = CHUNK
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
     allocate(temp_dgrids(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Extending internal Grid list", &
                                    ESMF_CONTEXT, rc)) return

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_dgrids(i) = gridp%internDGs(i)
     enddo

     ! deallocate old array
     deallocate(gridp%internDGs, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating InternDGs", &
                                    ESMF_CONTEXT, rc)) return

     ! and set original pointer to the new space
     gridp%internDGs => temp_dgrids

     ! update count of how many items are currently allocated
     gridp%numInternDGsAlloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_GridMakeInternDGSpace

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridAddPhysGrid"
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

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridClassGetInit,grid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! Update the PhysGridAlloc counter and check to see if PhysGrid
      ! array needs to be resized to add the new physgrid
      call ESMF_GridMakePhysGridSpace(grid, grid%numPhysGrids+1, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      grid%numPhysGrids = grid%numPhysGrids + 1

      ! Add the PhysGrid
      grid%physgrids(grid%numPhysGrids) = physgrid

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddPhysGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridMakePhysGridSpace"
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
      integer :: localrc                          ! Error status
      integer :: i, oldcount, alloccount

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridClassGetInit,gridp,rc)

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
        allocate(gridp%physGrids(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial physGrids", &
                                       ESMF_CONTEXT, rc)) return

        allocate(gridp%internDGIndex(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial InternDGIndex", &
                                       ESMF_CONTEXT, rc)) return

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
     allocate(temp_pgrids(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Allocating temp_pgrids", &
                                    ESMF_CONTEXT, rc)) return

     allocate(temp_dgIndex(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Allocating temp_dgIndex", &
                                    ESMF_CONTEXT, rc)) return

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_pgrids(i)  = gridp%physGrids(i)
       temp_dgIndex(i) = gridp%internDGIndex(i)
     enddo

     ! deallocate old arrays
     deallocate(gridp%physGrids, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating physGrids", &
                                    ESMF_CONTEXT, rc)) return

     deallocate(gridp%internDGIndex, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating InternDGIndex", &
                                    ESMF_CONTEXT, rc)) return

     ! and set original pointers to the new space
     gridp%physGrids => temp_pgrids
     gridp%internDGIndex => temp_dgIndex

     ! update count of how many items are currently allocated
     gridp%numPhysGridsAlloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_GridMakePhysGridSpace

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetPhysGrid"
!BOPI
! !IROUTINE: ESMF_GridGetPhysGrid - retrieves complete PhysGrid from Grid type

! !INTERFACE:
      subroutine ESMF_GridGetPhysGrid(physgrid, grid, relloc, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(out) :: physgrid
      type(ESMF_GridClass), intent(inout)  :: grid
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

      integer :: n                              ! search loop index
      integer :: localrc                        ! Error status
      logical :: found                          ! found flag for searches
      logical :: dummy
      character (len=ESMF_MAXSTR) :: nameTmp    ! temporary name variable
      type(ESMF_RelLoc) :: rellocTmp

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridClassGetInit,grid,rc)

      ! If name supplied, search by name and return selected PhysGrid
      if (present(name)) then
        found = .false.
        name_search: do n = 1,grid%numPhysGridsAlloc
          call ESMF_PhysGridGet(grid%physgrids(n), name=nameTmp, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (name == nameTmp) then
            physgrid = grid%physgrids(n)
            found = .true.
            exit name_search
          endif
        enddo name_search
        if (.not. found) then
          dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                        "unknown name", &
                                        ESMF_CONTEXT, rc)
          return
        endif

      ! If relloc supplied, search by relloc and return selected PhysGrid
      else if (present(relloc)) then
        found = .false.
        relloc_search: do n = 1,grid%numPhysGridsAlloc
          call ESMF_PhysGridGet(grid%physgrids(n), relloc=rellocTmp, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (relloc == rellocTmp) then
            physgrid = grid%physgrids(n)
            found = .true.
            exit relloc_search
          endif
        enddo relloc_search
        if (.not. found) then
          dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                        "unknown relative location", &
                                        ESMF_CONTEXT, rc)
          return
        endif
      
      ! If neither supplied, return error.
      else
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                      "must supply a name or relloc", &
                                      ESMF_CONTEXT, rc)
        return
      endif

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetPhysGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetPhysGridID"
!BOPI
! !IROUTINE: ESMF_GridGetPhysGridID - Get PhysGrid Id for a given relative location

! !INTERFACE:
      subroutine ESMF_GridGetPhysGridID(grid, relloc, physGridId, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(inout) :: grid
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

      integer :: localrc                          ! Error status
      type(ESMF_RelLoc) :: thisRelloc
      integer :: i

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridClassGetInit,grid,rc)

      physGridId = -1

      ! Loop through physGrids comparing rellocs  TODO: make part of the Grid obj?
      do i = 1,grid%numPhysGrids
        call ESMF_PhysGridGet(grid%physGrids(i), relloc=thisRelloc, &
                              rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        if (relloc.eq.thisRelloc) then
          physGridId = i
          localrc = ESMF_SUCCESS
          exit
        endif
      enddo

      ! print error if the relloc is not found
      if (physGridId.eq.-1) then
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                   "relloc not valid", &
                                   ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = localrc

      end subroutine ESMF_GridGetPhysGridID

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetInternDG"
!BOPI
! !IROUTINE: ESMF_GridGetInternDG - retrieves complete InternDG from Grid type

! !INTERFACE:
      subroutine ESMF_GridGetInternDG(interndg, grid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_InternDG), intent(out) :: interndg
      type(ESMF_GridClass), intent(in)  :: grid
      character(*), intent(in) :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine retrieves an {\tt ESMF\_InternDG} object from an
!     {\tt ESMF\_Grid} object, given the name of the interndg.
!
!     The arguments are:
!     \begin{description}
!     \item[interndg]
!          {\tt ESMF\_InternDG} to be retrieved.
!     \item[grid]
!          Grid structure from which interndg is to be extracted.
!     \item[name]
!          Name to identify which InternDG to retrieve.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: n                              ! search loop index
      integer :: localrc                        ! Error status
      logical :: found                          ! found flag for searches
      character (len=ESMF_MAXSTR) :: nameTmp    ! temporary name variable

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridClassGetInit,grid,rc)

      ! Search by name and return selected InternDG
      found = .false.
      name_search: do n=1,grid%numInternDGsAlloc
         call ESMF_InternDGGet(grid%internDGs(n), name=nameTmp, rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rc)) return
         if (name == nameTmp) then
            interndg = grid%internDGs(n)
            found = .true.
            exit name_search
         endif
      end do name_search
      if (.not. found) then
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "unknown name", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetInternDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetBoundingBoxes"
!BOPI
! !IROUTINE: ESMF_GridGetBoundingBoxes - Get the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_GridGetBoundingBoxes(grid, array, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(in) :: grid
      type(ESMF_LocalArray), intent(out) :: array !BOB changed to just out
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

      !integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_GridClassGetInit,grid,rc)

      array = grid%boundingBoxes

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetBoundingBoxes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridPointerEqual"
!BOPI
! !IROUTINE: ESMF_GridPointerEqual - equality of Grids
!
! !INTERFACE:
      function ESMF_GridPointerEqual(Grid1, Grid2)

! !RETURN VALUE:
      logical :: ESMF_GridPointerEqual

! !ARGUMENTS:

      type (ESMF_Grid), intent(in) :: &
         Grid1,      &! Two grids to compare for
         Grid2        ! equality (identity)

! !DESCRIPTION:
!     This routine compares two ESMF Grids to see if
!     they have equivalent pointers to the same internal ESMF_GridClass.
!     This will return false if the pointers are different, even if
!     the grids describe exactly the same physical dimensions.  It is a
!     quick and dirty check.
!
!     The arguments are:
!     \begin{description}
!     \item[Grid1, Grid2]
!          Two grids to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridPointerEqual = Associated(Grid1%ptr, Grid2%ptr)

      end function ESMF_GridPointerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStatusEqual"
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStructureEqual"
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStructureEqual = (GridStructure1%gridStructure == &
                                 GridStructure2%gridStructure)

      end function ESMF_GridStructureEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridTypeEqual"
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridTypeEqual = (GridType1%gridType == &
                            GridType2%gridType)

      end function ESMF_GridTypeEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridVertTypeEqual"
!BOPI
! !IROUTINE: ESMF_GridVertTypeEqual - equality of vertical Grid types
!
! !INTERFACE:
      function ESMF_GridVertTypeEqual(GridVertType1, GridVertType2)

! !RETURN VALUE:
      logical :: ESMF_GridVertTypeEqual

! !ARGUMENTS:

      type (ESMF_GridVertType), intent(in) :: &
         GridVertType1,      &! Two vertical grid kinds to compare for
         GridVertType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF vertical Grid kinds to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridVertType1, GridVertType2]
!          Two vertical grid types to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridVertTypeEqual = (GridVertType1%gridVertType == &
                            GridVertType2%gridVertType)

      end function ESMF_GridVertTypeEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridHorzStaggerEqual"
!BOPI
! !IROUTINE: ESMF_GridHorzStaggerEqual - equality of Grid horizontal staggerings
!
! !INTERFACE:
      function ESMF_GridHorzStaggerEqual(GridHorzStagger1, GridHorzStagger2)

! !RETURN VALUE:
      logical :: ESMF_GridHorzStaggerEqual

! !ARGUMENTS:

      type (ESMF_GridHorzStagger), intent(in) :: &
         GridHorzStagger1,      &! Two grid horz staggerings to compare for
         GridHorzStagger2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid horizontal staggerings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridHorzStagger1, GridHorzStagger2]
!          Two grid horizontal staggerings to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridHorzStaggerEqual = (GridHorzStagger1%stagger == &
                                   GridHorzStagger2%stagger)

      end function ESMF_GridHorzStaggerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridVertStaggerEqual"
!BOPI
! !IROUTINE: ESMF_GridVertStaggerEqual - equality of Grid vertical staggerings
!
! !INTERFACE:
      function ESMF_GridVertStaggerEqual(GridVertStagger1, GridVertStagger2)

! !RETURN VALUE:
      logical :: ESMF_GridVertStaggerEqual

! !ARGUMENTS:

      type (ESMF_GridVertStagger), intent(in) :: &
         GridVertStagger1,      &! Two grid vert staggerings to compare for
         GridVertStagger2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid vertical staggerings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridVertStagger1, GridVertStagger2]
!          Two grid vertical staggerings to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridVertStaggerEqual = (GridVertStagger1%stagger == &
                                   GridVertStagger2%stagger)

      end function ESMF_GridVertStaggerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStorageEqual"
!BOPI
! !IROUTINE: ESMF_GridStorageEqual - equality of Grid storage schemes
!
! !INTERFACE:
      function ESMF_GridStorageEqual(GridStorage1, GridStorage2)

! !RETURN VALUE:
      logical :: ESMF_GridStorageEqual

! !ARGUMENTS:

      type (ESMF_GridStorage), intent(in) :: &
         GridStorage1,      &! Two grid storage schemes to compare for
         GridStorage2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid storage schemes to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStorage1, GridStorage2]
!          Two grid storage schemes to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStorageEqual = (GridStorage1%storage == &
                               GridStorage2%storage)

      end function ESMF_GridStorageEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordOrderEqual"
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordOrderEqual = (CoordOrder1%order == &
                              CoordOrder2%order)

      end function ESMF_CoordOrderEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordIndexEqual"
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordIndexEqual = (CoordIndex1%index == &
                              CoordIndex2%index)

      end function ESMF_CoordIndexEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridPointerNotEqual"
!BOPI
! !IROUTINE: ESMF_GridPointerNotEqual - equality of Grids
!
! !INTERFACE:
      function ESMF_GridPointerNotEqual(Grid1, Grid2)

! !RETURN VALUE:
      logical :: ESMF_GridPointerNotEqual

! !ARGUMENTS:

      type (ESMF_Grid), intent(in) :: &
         Grid1,      &! Two grids to compare for
         Grid2        ! inequality (not identical)

! !DESCRIPTION:
!     This routine compares two ESMF Grids to see if
!     they have pointers to different internal ESMF_GridClasses.
!     This will return true if the pointers are different, even if
!     the grids describe exactly the same physical dimensions.  It is a
!     quick and dirty check.
!
!     The arguments are:
!     \begin{description}
!     \item[Grid1, Grid2]
!          Two grids to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridPointerNotEqual = .not.Associated(Grid1%ptr, Grid2%ptr)

      end function ESMF_GridPointerNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStatusNotEqual"
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStatusNotEqual = (GridStatus1%gridStatus /= &
                                 GridStatus2%gridStatus)

      end function ESMF_GridStatusNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStructureNotEqual"
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStructureNotEqual = (GridStructure1%gridStructure /= &
                                    GridStructure2%gridStructure)

      end function ESMF_GridStructureNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridTypeNotEqual"
!BOPI
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
!EOPI

      ESMF_GridTypeNotEqual = (GridType1%gridType /= &
                               GridType2%gridType)

      end function ESMF_GridTypeNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridVertTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_GridVertTypeNotEqual - non-equality of vertical Grid kinds
!
! !INTERFACE:
      function ESMF_GridVertTypeNotEqual(GridVertType1, GridVertType2)

! !RETURN VALUE:
      logical :: ESMF_GridVertTypeNotEqual

! !ARGUMENTS:

      type (ESMF_GridVertType), intent(in) :: &
         GridVertType1,      &! Two vertical Grid kinds to compare for
         GridVertType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF vertical Grid kinds to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridVertType1, GridVertType2]
!          Two kinds of vertical Grids to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_GridVertTypeNotEqual = (GridVertType1%gridVertType /= &
                                   GridVertType2%gridVertType)

      end function ESMF_GridVertTypeNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridHorzStaggerNotEqual"
!BOPI
! !IROUTINE: ESMF_GridHorzStaggerNotEqual - inequality of Grid horizontal staggerings
!
! !INTERFACE:
      function ESMF_GridHorzStaggerNotEqual(GridHorzStagger1, GridHorzStagger2)

! !RETURN VALUE:
      logical :: ESMF_GridHorzStaggerNotEqual

! !ARGUMENTS:

      type (ESMF_GridHorzStagger), intent(in) :: &
         GridHorzStagger1,      &! Two grid horizontal staggerings to compare for
         GridHorzStagger2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid horizontal staggerings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridHorzStagger1, GridHorzStagger2]
!          Two grid horizontal staggerings to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridHorzStaggerNotEqual = (GridHorzStagger1%stagger /= &
                                      GridHorzStagger2%stagger)

      end function ESMF_GridHorzStaggerNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridVertStaggerNotEqual"
!BOPI
! !IROUTINE: ESMF_GridVertStaggerNotEqual - inequality of Grid vertical staggerings
!
! !INTERFACE:
      function ESMF_GridVertStaggerNotEqual(GridVertStagger1, GridVertStagger2)

! !RETURN VALUE:
      logical :: ESMF_GridVertStaggerNotEqual

! !ARGUMENTS:

      type (ESMF_GridVertStagger), intent(in) :: &
         GridVertStagger1,      &! Two grid vertical staggerings to compare for
         GridVertStagger2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid vertical staggerings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridVertStagger1, GridVertStagger2]
!          Two grid vertical staggerings to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridVertStaggerNotEqual = (GridVertStagger1%stagger /= &
                                      GridVertStagger2%stagger)

      end function ESMF_GridVertStaggerNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridStorageNotEqual"
!BOPI
! !IROUTINE: ESMF_GridStorageNotEqual - inequality of Grid storage schemes
!
! !INTERFACE:
      function ESMF_GridStorageNotEqual(GridStorage1, GridStorage2)

! !RETURN VALUE:
      logical :: ESMF_GridStorageNotEqual

! !ARGUMENTS:

      type (ESMF_GridStorage), intent(in) :: &
         GridStorage1,      &! Two grid storage schemes to compare for
         GridStorage2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid storage schemes to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridStorage1, GridStorage2]
!          Two grid storage schemes to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridStorageNotEqual = (GridStorage1%storage /= &
                                  GridStorage2%storage)

      end function ESMF_GridStorageNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordOrderNotEqual"
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordOrderNotEqual = (CoordOrder1%order /= &
                                 CoordOrder2%order)

      end function ESMF_CoordOrderNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordIndexNotEqual"
!BOPI
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
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_CoordIndexNotEqual = (CoordIndex1%index /= &
                                 CoordIndex2%index)

      end function ESMF_CoordIndexNotEqual

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRectGridValidate()"
!BOP
! !IROUTINE: ESMF_LogRectGridValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_LogRectGridValidate(lrg, rc)
!
! !ARGUMENTS:
    type(ESMF_LogRectGrid), intent(in)              :: lrg
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt lrg} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[lrg] 
!          Specified {\tt ESMF\_DataHolder} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_LogRectGridGetInit, ESMF_LogRectGridInit,lrg)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_LogRectGridValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRectGridInit()"
!BOPI
! !IROUTINE: ESMF_LogRectGridInit - Init DataHolder internals

! !INTERFACE:
  subroutine ESMF_LogRectGridInit(lrg)
!
! !ARGUMENTS:
    type(ESMF_LogRectGrid), intent(inout)              :: lrg
!         
!
! !DESCRIPTION:
!      Initialize DataHolder internals.
!
!     The arguments are:
!     \begin{description}
!     \item[lrg] 
!          Specified {\tt ESMF\_DataHolder} object.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ESMF_INIT_SET_DEFINED(lrg)
  end subroutine ESMF_LogRectGridInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRectGridGetInit"
!BOPI
! !IROUTINE: ESMF_LogRectGridGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_LogRectGridGetInit(lrg) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_LogRectGridGetInit   
!
! !ARGUMENTS:
      type(ESMF_LogRectGrid), intent(in), optional :: lrg
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [lrg]
!           DataHolder object.
!     \end{description}
!
!EOPI

    if (present(lrg)) then
      ESMF_LogRectGridGetInit = ESMF_INIT_GET(lrg)
    else
      ESMF_LogRectGridGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_LogRectGridGetInit
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSpecificValidate()"
!BOP
! !IROUTINE: ESMF_GridSpecificValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_GridSpecificValidate(gs, rc)
!
! !ARGUMENTS:
    type(ESMF_GridSpecific), intent(in)              :: gs
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt gs} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[gs] 
!          Specified {\tt ESMF\_DataHolder} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_GridSpecificGetInit, ESMF_GridSpecificInit,gs)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_GridSpecificValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSpecificInit()"
!BOPI
! !IROUTINE: ESMF_GridSpecificInit - Init DataHolder internals

! !INTERFACE:
  subroutine ESMF_GridSpecificInit(gs)
!
! !ARGUMENTS:
    type(ESMF_GridSpecific), intent(inout)              :: gs
!         
!
! !DESCRIPTION:
!      Initialize DataHolder internals.
!
!     The arguments are:
!     \begin{description}
!     \item[gs] 
!          Specified {\tt ESMF\_DataHolder} object.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    ESMF_INIT_SET_DEFINED(gs)
  end subroutine ESMF_GridSpecificInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridSpecificGetInit"
!BOPI
! !IROUTINE: ESMF_GridSpecificGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_GridSpecificGetInit(gs) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_GridSpecificGetInit   
!
! !ARGUMENTS:
      type(ESMF_GridSpecific), intent(in), optional :: gs
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [gs]
!           DataHolder object.
!     \end{description}
!
!EOPI

    if (present(gs)) then
      ESMF_GridSpecificGetInit = ESMF_INIT_GET(gs)
    else
      ESMF_GridSpecificGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_GridSpecificGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridClassValidate()"
!BOP
! !IROUTINE: ESMF_GridClassValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_GridClassValidate(gc, rc)
!
! !ARGUMENTS:
    type(ESMF_GridClass), intent(in)              :: gc
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt gc} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[gc] 
!          Specified {\tt ESMF\_DataHolder} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_GridClassGetInit, ESMF_GridClassInit,gc)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_GridClassValidate
!------------------------------------------------------------------------------

! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridClassGetInit"
!BOPI
! !IROUTINE: ESMF_GridClassGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_GridClassGetInit(gc) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_GridClassGetInit   
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(in), optional :: gc
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [gc]
!           DataHolder object.
!     \end{description}
!
!EOPI

    if (present(gc)) then
      ESMF_GridClassGetInit = ESMF_INIT_GET(gc)
    else
      ESMF_GridClassGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_GridClassGetInit
!------------------------------------------------------------------------------

! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridGetInit"
!BOPI
! !IROUTINE: ESMF_GridGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_GridGetInit(g) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_GridGetInit   
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in), optional :: g
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [g]
!           DataHolder object.
!     \end{description}
!
!EOPI

    if (present(g)) then
      ESMF_GridGetInit = ESMF_INIT_GET(g)
    else
      ESMF_GridGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_GridGetInit
!------------------------------------------------------------------------------



      end module ESMF_GridTypesMod
