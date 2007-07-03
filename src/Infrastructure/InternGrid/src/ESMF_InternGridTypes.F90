! $Id: ESMF_InternGridTypes.F90,v 1.5 2007/07/03 23:21:29 oehmke Exp $
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
#define ESMF_FILENAME "ESMF_IGridTypes.F90"
!
!     ESMF IGrid Types Module
      module ESMF_IGridTypesMod
!
!==============================================================================
!
! This file contains the IGrid class definition and basic IGrid class
! methods.  These are used by the main IGrid module interfaces which
! provide the user interface for igrid functions. 
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_IGridTypesMod - IGrid class data types
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the 
! {\tt ESMF\_IGrid} class.  This class provides utilities for the IGrid
! class that are used by the main {\tt ESMF\_IGrid} module.
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
      use ESMF_InternDGMod      ! ESMF distributed igrid class
      use ESMF_PhysCoordMod     ! ESMF physical coord class
      use ESMF_PhysGridMod      ! ESMF physical igrid class
      use ESMF_InitMacrosMod  ! init macros stuff
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!  For now, add derived types for specific igrid structures here:
!  These derived types contain all the necessary information beyond the
!  IGridClass derived type.
!------------------------------------------------------------------------------
!     ! ESMF_LogRectIGrid
!
!     ! Type to contain extra information for Logically rectangular igrids.

      type ESMF_LogRectIGrid
      sequence
        real(ESMF_KIND_R8), dimension(ESMF_MAXIGRIDDIM) :: deltaPerDim
        type(ESMF_LocalArray), dimension(:), pointer :: coords
        integer, dimension(ESMF_MAXIGRIDDIM) :: countPerDim

         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_IGridSpecific
!
!     ! Type to hold pointers to all available specific igrid derived types

      type ESMF_IGridSpecific
      sequence
        type (ESMF_LogRectIGrid), pointer :: logRectIGrid

        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_IGridStatus
!
!     ! Type to specify overall status of igrid.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_IGridStatus
      sequence
        integer :: igridStatus
      end type

!------------------------------------------------------------------------------
!     ! ESMF_IGridStructure
!
!     ! Type to specify overall structure of igrid for supported ESMF IGrids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_IGridStructure
      sequence
!      private
        integer :: igridStructure
      end type

!------------------------------------------------------------------------------
!     ! ESMF_IGridType
!
!     ! Type to specify kind of igrid for supported ESMF IGrids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_IGridType
      sequence
!      private
        integer :: igridType
      end type

!------------------------------------------------------------------------------
!     ! ESMF_IGridVertType
!
!     ! Type to specify kind of vertical igrid for supported ESMF IGrids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_IGridVertType
      sequence
!      private
        integer :: igridVertType
      end type

!------------------------------------------------------------------------------
!     ! ESMF_IGridHorzStagger
!
!     ! Type to specify type of horizontal igrid staggering for supported
!     !  ESMF IGrids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_IGridHorzStagger
      sequence
!      private
        integer :: stagger
      end type

!------------------------------------------------------------------------------
!     ! ESMF_IGridVertStagger
!
!     ! Type to specify type of vertical igrid staggering for supported
!     !  ESMF IGrids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_IGridVertStagger
      sequence
!      private
        integer :: stagger
      end type

!------------------------------------------------------------------------------
!     ! ESMF_IGridStorage
!
!     ! Type to specify type of igrid storage schemes for supported
!     !  ESMF IGrids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_IGridStorage
      sequence
!      private
        integer :: storage
      end type

!------------------------------------------------------------------------------
!     ! ESMF_CoordOrder
!
!     ! Type to specify logical ordering of coordinate in ESMF IGrids.
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
!     ! Type to specify global or local indexing of ESMF IGrids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_CoordIndex
      sequence
!      private
        integer :: index
      end type

!------------------------------------------------------------------------------
!     !  ESMF_IGridClass
!
!     ! Definition for the IGrid class.

      type ESMF_IGridClass
      sequence
!      private

        type (ESMF_Base) :: base              ! base class object
        type (ESMF_IGridStatus) :: igridStatus  ! uninitialized, init ok, etc

#ifdef ESMF_IS_32BIT_MACHINE

        real(ESMF_KIND_R8), dimension(ESMF_MAXIGRIDDIM) :: minGlobalCoordPerDim
        real(ESMF_KIND_R8), dimension(ESMF_MAXIGRIDDIM) :: maxGlobalCoordPerDim

        integer :: dimCount                   ! number of dimensions
#else
        integer :: dimCount                   ! number of dimensions

        real(ESMF_KIND_R8), dimension(ESMF_MAXIGRIDDIM) :: minGlobalCoordPerDim
        real(ESMF_KIND_R8), dimension(ESMF_MAXIGRIDDIM) :: maxGlobalCoordPerDim

#endif

        type (ESMF_Logical) :: hasLocalData
        type (ESMF_IGridStructure) :: igridStructure
                                              ! enum for structure of igrid
                                              ! i.e. logically rectangular, etc
        type (ESMF_IGridType) :: horzIGridType  ! enum for type of horizontal igrid
        type (ESMF_IGridVertType) :: vertIGridType
                                              ! enum for type of vertical igrid
        type (ESMF_IGridHorzStagger) :: horzStagger
                                              ! enum for horizontal igrid staggering
        type (ESMF_IGridVertStagger) :: vertStagger
                                              ! enum for vertical igrid staggering
        type (ESMF_IGridStorage) :: igridStorage
                                              ! enum for igrid storage scheme
        type (ESMF_CoordSystem) :: horzCoordSystem  
                                              ! identifier for horizontal
                                              ! physical coordinate system
        type (ESMF_CoordSystem) :: vertCoordSystem  
                                              ! identifier for vertical
                                              ! physical coordinate system
        type (ESMF_CoordOrder) :: coordOrder  ! enum for mapping of xyz to ijk
        type (ESMF_CoordIndex) :: coordIndex  ! enum for global, local indexing
        type (ESMF_Logical), dimension(ESMF_MAXIGRIDDIM) :: periodic
                                              ! logical identifier to indicate
                                              ! periodic boundary conditions in
                                              ! each direction
        integer :: numPhysGrids               ! number of igrid descriptors
                                              ! necessary to support
                                              ! staggering, vertical
                                              ! igrids, background igrids


        integer :: numPhysGridsAlloc          ! number of physgrids allocated

        type (ESMF_PhysGrid), dimension(:), pointer :: physgrids
                                              ! info for all igrid descriptions
                                              ! necessary to define horizontal,
                                              ! staggered and vertical igrids
        integer, dimension(:), pointer :: internDGIndex
                                              ! for each physgrid, the index of
                                              ! the corresponding InternDG


        type (ESMF_InternDG), dimension(:), pointer :: internDGs       
                                              ! decomposition and other
                                              ! logical space info for igrid

        character(len=ESMF_MAXSTR), dimension(ESMF_MAXIGRIDDIM) :: dimNames
        character(len=ESMF_MAXSTR), dimension(ESMF_MAXIGRIDDIM) :: dimUnits

        type (ESMF_LocalArray) :: boundingBoxes
                                            ! array of bounding boxes on each DE
                                            ! used for search routines
        type (ESMF_IGridSpecific) :: igridSpecific

        integer :: numInternDGsAlloc          ! number of InternDGs allocated

        integer :: numInternDGs               ! number of igrid descriptors
                                              ! necessary to support
                                              ! staggering, vertical
                                              ! igrids, background igrids
!       type (???) :: searchStructure

         ESMF_INIT_DECLARE

      end type

!------------------------------------------------------------------------------
!     !  ESMF_IGrid
!
!     ! The IGrid data structure that is passed between languages.

      type ESMF_IGrid
      sequence
!      private
#ifndef ESMF_NO_INITIALIZERS
        type (ESMF_IGridClass), pointer :: ptr => NULL()
#else
        type (ESMF_IGridClass), pointer :: ptr
#endif
       
        ESMF_INIT_DECLARE

      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

      public ESMF_LogRectIGrid,   ESMF_IGridSpecific,    ESMF_IGridStatus
      public ESMF_IGridStructure, ESMF_IGridHorzStagger, ESMF_IGridVertStagger
      public ESMF_IGridClass,     ESMF_IGridType,        ESMF_IGridVertType
      public ESMF_CoordOrder,    ESMF_CoordIndex,      ESMF_IGridStorage
      public ESMF_IGrid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    ! These functions are generally accessed only by the main IGrid module
    ! and are not meant to be accessed by the user.  Well...except for
    ! the overloaded operators.

    public ESMF_IGridGetInit 
    public ESMF_IGridSetInitCreated
    public ESMF_IGridClassValidate   
    public ESMF_IGridClGetInit 
    public ESMF_LogRectIGridValidate   
    public ESMF_LogRectIGridInit
    public ESMF_LogRectIGridGetInit 
    public ESMF_IGridSpecificValidate   
    public ESMF_IGridSpecificInit
    public ESMF_IGridSpecificGetInit 
    public ESMF_IGridConstructNew
    public ESMF_IGridGetDELayout
    public ESMF_IGridAddInternDG
    public ESMF_IGridMakeDGSpace
    public ESMF_IGridAddPhysGrid
    public ESMF_IGridMakePGSpace
    public ESMF_IGridGetPhysGrid
    public ESMF_IGridGetPhysGridID
    public ESMF_IGridGetInternDG
    public ESMF_IGridGetBoundingBoxes

    public operator(==), operator(/=) ! for overloading 
                                      ! comparison functions

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:
  ! Supported ESMF structure kinds:
  !   ESMF_IGRID_STATUS_UNKNOWN           ! unknown or undefined status
  !   ESMF_IGRID_STATUS_UNINIT            ! uninitialized
  !   ESMF_IGRID_STATUS_READY             ! completely ready for use
  !   ESMF_IGRID_STATUS_UNALLOCATED       ! unallocated
  !   ESMF_IGRID_STATUS_ALLOCATED         ! allocated
  !   ESMF_IGRID_STATUS_INIT              ! initialized but not distributed
  !   ESMF_IGRID_STATUS_INVALID           ! invalid

   type (ESMF_IGridStatus), parameter, public ::              &
      ESMF_IGRID_STATUS_UNKNOWN      =  ESMF_IGridStatus(0), &
      ESMF_IGRID_STATUS_UNINIT       =  ESMF_IGridStatus(1), &
      ESMF_IGRID_STATUS_READY        =  ESMF_IGridStatus(2), &
      ESMF_IGRID_STATUS_UNALLOCATED  =  ESMF_IGridStatus(3), &
      ESMF_IGRID_STATUS_ALLOCATED    =  ESMF_IGridStatus(4), &
      ESMF_IGRID_STATUS_INIT         =  ESMF_IGridStatus(5), &
      ESMF_IGRID_STATUS_INVALID      =  ESMF_IGridStatus(6)

  ! Supported ESMF structure kinds:
  !   ESMF_IGRID_STRUCT_UNKNOWN         ! unknown or undefined igrid
  !   ESMF_IGRID_STRUCT_LOGRECT         ! logically rectangular igrid
  !   ESMF_IGRID_STRUCT_LOGRECT_BLK     ! logically rectangular blocked igrid
  !   ESMF_IGRID_STRUCT_UNSTRUCT        ! unstructured igrid
  !   ESMF_IGRID_STRUCT_USER            ! user-defined igrid

   type (ESMF_IGridStructure), parameter, public ::              &
      ESMF_IGRID_STRUCT_UNKNOWN     =  ESMF_IGridStructure(0), &
      ESMF_IGRID_STRUCT_LOGRECT     =  ESMF_IGridStructure(1), &
      ESMF_IGRID_STRUCT_LOGRECT_BLK =  ESMF_IGridStructure(2), &
      ESMF_IGRID_STRUCT_UNSTRUCT    =  ESMF_IGridStructure(3), &
      ESMF_IGRID_STRUCT_USER        =  ESMF_IGridStructure(4)

  ! Supported ESMF igrid kinds:
  !   ESMF_IGRID_TYPE_UNKNOWN           ! unknown or undefined igrid
  !   ESMF_IGRID_TYPE_LATLON            ! aligned with longitude,latitude
  !   ESMF_IGRID_TYPE_LATLON_UNI        ! LatLon igrid with uniform spacing
  !   ESMF_IGRID_TYPE_LATLON_GAUSS      ! LatLon with gaussian-spaced latitudes
  !   ESMF_IGRID_TYPE_LATLON_MERC       ! LatLon with Mercator-spaced latitudes
  !   ESMF_IGRID_TYPE_REDUCED           ! LatLon with num lon pts a fcn of lat
  !   ESMF_IGRID_TYPE_DIPOLE            ! Displaced-pole dipole igrid
  !   ESMF_IGRID_TYPE_TRIPOLE           ! Tripolar igrids
  !   ESMF_IGRID_TYPE_XY                ! aligned with Cartesian x-y coords
  !   ESMF_IGRID_TYPE_XY_UNI            ! XY igrid with uniform spacing
  !   ESMF_IGRID_TYPE_DATASTREAM        ! Data stream - set of locations
  !   ESMF_IGRID_TYPE_PHYSFOURIER       ! Mixed Fourier/Phys Space igrid
  !   ESMF_IGRID_TYPE_SPHER_SPECT       ! spectral space:spherical harmonics
  !   ESMF_IGRID_TYPE_CART_SPECT        ! spectral space:Cartesian coords
  !   ESMF_IGRID_TYPE_GEODESIC          ! spherical geodesic igrid
  !   ESMF_IGRID_TYPE_CUBEDSPHERE       ! cubed sphere igrid
  !   ESMF_IGRID_TYPE_EXCHANGE          ! intersection of two igrids

   type (ESMF_IGridType), parameter, public ::              &
      ESMF_IGRID_TYPE_UNKNOWN           = ESMF_IGridType( 0), &
      ESMF_IGRID_TYPE_LATLON            = ESMF_IGridType( 1), &
      ESMF_IGRID_TYPE_LATLON_UNI        = ESMF_IGridType( 2), &
      ESMF_IGRID_TYPE_LATLON_GAUSS      = ESMF_IGridType( 3), &
      ESMF_IGRID_TYPE_LATLON_MERC       = ESMF_IGridType( 4), &
      ESMF_IGRID_TYPE_REDUCED           = ESMF_IGridType( 5), &
      ESMF_IGRID_TYPE_DIPOLE            = ESMF_IGridType( 6), &
      ESMF_IGRID_TYPE_TRIPOLE           = ESMF_IGridType( 7), &
      ESMF_IGRID_TYPE_XY                = ESMF_IGridType( 8), &
      ESMF_IGRID_TYPE_XY_UNI            = ESMF_IGridType( 9), &
      ESMF_IGRID_TYPE_DATASTREAM        = ESMF_IGridType(10), &
      ESMF_IGRID_TYPE_PHYSFOURIER       = ESMF_IGridType(11), &
      ESMF_IGRID_TYPE_SPHER_SPECT       = ESMF_IGridType(12), &
      ESMF_IGRID_TYPE_CART_SPECT        = ESMF_IGridType(13), &
      ESMF_IGRID_TYPE_GEODESIC          = ESMF_IGridType(14), &
      ESMF_IGRID_TYPE_CUBEDSPHERE       = ESMF_IGridType(15), &
      ESMF_IGRID_TYPE_EXCHANGE          = ESMF_IGridType(16)

  ! Supported ESMF vertical igrid kinds:
  !   ESMF_IGRID_TYPE_VERT_UNKNOWN        ! unknown or undefined vertical igrid
  !   ESMF_IGRID_TYPE_VERT_USER           ! user-defined vertical igrid
  !   ESMF_IGRID_TYPE_VERT_DEPTH          ! vertical igrid with 0 at top surface
  !   ESMF_IGRID_TYPE_VERT_HEIGHT         ! vertical igrid with 0 at bottom
  !   ESMF_IGRID_TYPE_VERT_PRESSURE       ! vertical igrid with pressure coordinates
  !   ESMF_IGRID_TYPE_VERT_SIGMA          ! vertical igrid with sigma coordinates
  !   ESMF_IGRID_TYPE_VERT_THETA          ! vertical igrid with theta coordinates
  !   ESMF_IGRID_TYPE_VERT_ETA            ! vertical igrid with eta coordinates
  !   ESMF_IGRID_TYPE_VERT_ISOPYCNAL      ! vertical igrid with density coordinates
  !   ESMF_IGRID_TYPE_VERT_HYBRID         ! vertical igrid with hybrid coordinates
  !   ESMF_IGRID_TYPE_VERT_LAGRANGIAN     ! vertical igrid with lagrangian coordinates

   type (ESMF_IGridVertType), parameter, public ::              &
      ESMF_IGRID_VERT_TYPE_UNKNOWN      = ESMF_IGridVertType( 0), &
      ESMF_IGRID_VERT_TYPE_USER         = ESMF_IGridVertType( 1), &
      ESMF_IGRID_VERT_TYPE_DEPTH        = ESMF_IGridVertType( 2), &
      ESMF_IGRID_VERT_TYPE_HEIGHT       = ESMF_IGridVertType( 3), &
      ESMF_IGRID_VERT_TYPE_PRESSURE     = ESMF_IGridVertType( 4), &
      ESMF_IGRID_VERT_TYPE_SIGMA        = ESMF_IGridVertType( 5), &
      ESMF_IGRID_VERT_TYPE_THETA        = ESMF_IGridVertType( 6), &
      ESMF_IGRID_VERT_TYPE_ETA          = ESMF_IGridVertType( 7), &
      ESMF_IGRID_VERT_TYPE_ISOPYCNAL    = ESMF_IGridVertType( 8), &
      ESMF_IGRID_VERT_TYPE_HYBRID       = ESMF_IGridVertType( 9), &
      ESMF_IGRID_VERT_TYPE_LAGRANGIAN   = ESMF_IGridVertType(10)

  ! Recognized ESMF horizontal staggering types
  !   ESMF_IGRID_HORZ_STAGGER_UNKNOWN  ! unknown or undefined staggering
  !   ESMF_IGRID_HORZ_STAGGER_A        ! Arakawa A (centered velocity)
  !   ESMF_IGRID_HORZ_STAGGER_B_NE     ! Arakawa B (U,V at NE corner)
  !   ESMF_IGRID_HORZ_STAGGER_B_SW     ! Arakawa B (U,V at SW corner)
  !   ESMF_IGRID_HORZ_STAGGER_B_SE     ! Arakawa B (U,V at SE corner)
  !   ESMF_IGRID_HORZ_STAGGER_B_NW     ! Arakawa B (U,V at NW corner)
  !   ESMF_IGRID_HORZ_STAGGER_C_NE     ! Arakawa C (U at E face, V at N face)
  !   ESMF_IGRID_HORZ_STAGGER_C_SW     ! Arakawa C (U at W face, V at S face)
  !   ESMF_IGRID_HORZ_STAGGER_C_SE     ! Arakawa C (U at E face, V at S face)
  !   ESMF_IGRID_HORZ_STAGGER_C_NW     ! Arakawa C (U at W face, V at N face)
  !   ESMF_IGRID_HORZ_STAGGER_D_NE     ! Arakawa D (V at E face, U at N face)
  !   ESMF_IGRID_HORZ_STAGGER_D_SW     ! Arakawa D (V at W face, U at S face)
  !   ESMF_IGRID_HORZ_STAGGER_D_SE     ! Arakawa D (V at E face, U at S face)
  !   ESMF_IGRID_HORZ_STAGGER_D_NW     ! Arakawa D (V at W face, U at N face)
  !   ESMF_IGRID_HORZ_STAGGER_E        ! Arakawa E
  !   ESMF_IGRID_HORZ_STAGGER_Z        ! C igrid equiv for geodesic igrid

   type (ESMF_IGridHorzStagger), parameter, public ::              &
      ESMF_IGRID_HORZ_STAGGER_UNKNOWN  = ESMF_IGridHorzStagger( 0), &
      ESMF_IGRID_HORZ_STAGGER_A        = ESMF_IGridHorzStagger( 1), &
      ESMF_IGRID_HORZ_STAGGER_B_NE     = ESMF_IGridHorzStagger( 2), &
      ESMF_IGRID_HORZ_STAGGER_B_SW     = ESMF_IGridHorzStagger( 3), &
      ESMF_IGRID_HORZ_STAGGER_B_SE     = ESMF_IGridHorzStagger( 4), &
      ESMF_IGRID_HORZ_STAGGER_B_NW     = ESMF_IGridHorzStagger( 5), &
      ESMF_IGRID_HORZ_STAGGER_C_NE     = ESMF_IGridHorzStagger( 6), &
      ESMF_IGRID_HORZ_STAGGER_C_SW     = ESMF_IGridHorzStagger( 7), &
      ESMF_IGRID_HORZ_STAGGER_C_SE     = ESMF_IGridHorzStagger( 8), &
      ESMF_IGRID_HORZ_STAGGER_C_NW     = ESMF_IGridHorzStagger( 9), &
      ESMF_IGRID_HORZ_STAGGER_D_NE     = ESMF_IGridHorzStagger(10), &
      ESMF_IGRID_HORZ_STAGGER_D_SW     = ESMF_IGridHorzStagger(11), &
      ESMF_IGRID_HORZ_STAGGER_D_SE     = ESMF_IGridHorzStagger(12), &
      ESMF_IGRID_HORZ_STAGGER_D_NW     = ESMF_IGridHorzStagger(13), &
      ESMF_IGRID_HORZ_STAGGER_E        = ESMF_IGridHorzStagger(14), &
      ESMF_IGRID_HORZ_STAGGER_Z        = ESMF_IGridHorzStagger(15)

  ! Recognized ESMF vertical staggering types
  !   ESMF_IGRID_VERT_STAGGER_UNKNOWN  ! unknown or undefined staggering
  !   ESMF_IGRID_VERT_STAGGER_CENTER   ! vertical midpoints
  !   ESMF_IGRID_VERT_STAGGER_TOP      ! at top    face of vertical igrid
  !   ESMF_IGRID_VERT_STAGGER_BOTTOM   ! at bottom face of vertical igrid

   type (ESMF_IGridVertStagger), parameter, public ::              &
      ESMF_IGRID_VERT_STAGGER_UNKNOWN  = ESMF_IGridVertStagger( 0), &
      ESMF_IGRID_VERT_STAGGER_CENTER   = ESMF_IGridVertStagger( 1), &
      ESMF_IGRID_VERT_STAGGER_TOP      = ESMF_IGridVertStagger( 2), &
      ESMF_IGRID_VERT_STAGGER_BOTTOM   = ESMF_IGridVertStagger( 3)

  ! Recognized igrid storage schemes
  !   ESMF_IGRID_STORAGE_UNKNOWN   ! unknown or undefined igrid storage
  !   ESMF_IGRID_STORAGE_LOGRECT   ! uses logically rectangular storage, one
  !                               ! block per DE
  !   ESMF_IGRID_STORAGE_BLOCK     ! uses logically rectangular storage, multiple
  !                               ! blocks per DE
  !   ESMF_IGRID_STORAGE_ARBITRARY ! uses arbitrary storage, which infers a
  !                               ! scattering of igrid cell locations and limits
  !                               ! available communication and query functions

   type (ESMF_IGridStorage), parameter, public ::         &
      ESMF_IGRID_STORAGE_UNKNOWN   = ESMF_IGridStorage(0), &
      ESMF_IGRID_STORAGE_LOGRECT   = ESMF_IGridStorage(1), &
      ESMF_IGRID_STORAGE_BLOCK     = ESMF_IGridStorage(2), &
      ESMF_IGRID_STORAGE_ARBITRARY = ESMF_IGridStorage(3)

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
      igridOrder = reshape((/ 1, 1, 1, &
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
      '$Id: ESMF_InternGridTypes.F90,v 1.5 2007/07/03 23:21:29 oehmke Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_IGridPointerEqual
         module procedure ESMF_IGridStatusEqual
         module procedure ESMF_IGridStructureEqual
         module procedure ESMF_IGridTypeEqual
         module procedure ESMF_IGridVertTypeEqual
         module procedure ESMF_IGridHorzStaggerEqual
         module procedure ESMF_IGridVertStaggerEqual
         module procedure ESMF_IGridStorageEqual
         module procedure ESMF_CoordOrderEqual
         module procedure ESMF_CoordIndexEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF IGrid ids (enums).  It is provided for easy comparisons of 
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
         module procedure ESMF_IGridPointerNotEqual
         module procedure ESMF_IGridStatusNotEqual
         module procedure ESMF_IGridStructureNotEq
         module procedure ESMF_IGridTypeNotEqual
         module procedure ESMF_IGridVertTypeNotEqual
         module procedure ESMF_IGridHorzStagNotEq
         module procedure ESMF_IGridVertStagNotEq
         module procedure ESMF_IGridStorageNotEqual
         module procedure ESMF_CoordOrderNotEqual
         module procedure ESMF_CoordIndexNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF IGrid ids (enums).  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridConstructNew"
!BOPI
! !IROUTINE: ESMF_IGridConstructNew - Construct the internals of an allocated IGrid

! !INTERFACE:
      subroutine ESMF_IGridConstructNew(igrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: igrid
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
!     \item[igrid]
!          Pointer to a {\tt ESMF\_IGrid}
!     \item[{[name]}]
!          {\tt ESMF\_IGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                          ! Error status
      integer :: i
      !character (len = ESMF_MAXSTR) :: defaultname ! default igrid name

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Set the IGrid name if present, otherwise construct a default one
      call ESMF_BaseCreate(igrid%base, "IGrid", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Initialize igrid contents
      igrid%igridStatus      = ESMF_IGRID_STATUS_READY
      igrid%igridStructure   = ESMF_IGRID_STRUCT_UNKNOWN
      igrid%horzIGridType    = ESMF_IGRID_TYPE_UNKNOWN
      igrid%vertIGridType    = ESMF_IGRID_VERT_TYPE_UNKNOWN
      igrid%horzStagger     = ESMF_IGRID_HORZ_STAGGER_UNKNOWN
      igrid%vertStagger     = ESMF_IGRID_VERT_STAGGER_UNKNOWN
      igrid%igridStorage     = ESMF_IGRID_STORAGE_UNKNOWN
      igrid%horzCoordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      igrid%vertCoordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      igrid%coordOrder      = ESMF_COORD_ORDER_XYZ
      do i=1,ESMF_MAXIGRIDDIM
        igrid%periodic(i)     = ESMF_FALSE
      !   igrid%coversDomain(i) = ESMF_TRUE
      enddo
      igrid%numInternDGs = 0
      igrid%numInternDGsAlloc = 0
      nullify(igrid%internDGs)
      igrid%numPhysGrids = 0
      igrid%numPhysGridsAlloc = 0
      nullify(igrid%physgrids)
      nullify(igrid%internDGIndex)
      ! nullify(igrid%igridSpecific)

      ESMF_INIT_SET_CREATED(igrid)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridConstructNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetDELayout"
!BOP
! !IROUTINE: ESMF_IGridGetDELayout - Get pointer to a DELayout from a IGrid

! !INTERFACE:
      subroutine ESMF_IGridGetDELayout(igrid, delayout, rc)
!
! !ARGUMENTS:
      type(ESMF_IGrid) :: igrid
      type(ESMF_DELayout),intent(out) :: delayout
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_InternDG} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          Class to be queried.
!     \item[delayout]
!          Pointer to the {\tt ESMF\_DELayout} corresponding to the
!          {\tt ESMF\_IGrid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level -- does not matter which one since they all share
      ! the same layout    !TODO: move layout to IGrid class?
      call ESMF_InternDGGetDELayout(igrid%ptr%internDGs(1)%ptr, delayout, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetDELayout

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridAddInternDG"
!BOPI
! !IROUTINE: ESMF_IGridAddInternDG - adds a complete InternDG to IGrid type

! !INTERFACE:
      subroutine ESMF_IGridAddInternDG(igrid, interndg, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), intent(inout) :: igrid
      type(ESMF_InternDG), intent(in)    :: interndg
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine attaches an {\tt ESMF\_InternDG} object to an
!     {\tt ESMF\_IGrid} object.  It is only meant to be called by
!     igrid creation routines in the processes of building a igrid.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          IGrid structure to which new InternDG is to be added.
!     \item[interndg]
!          Complete {\tt ESMF\_InternDG} to be added.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,igrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_InternDGGetInit,interndg,rc)

      ! Update the InternDGAlloc counter and check to see if InternDG
      ! array needs to be resized to add the new interndg
      call ESMF_IGridMakeDGSpace(igrid, igrid%numInternDGs+1, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      igrid%numInternDGs = igrid%numInternDGs + 1

      ! Add the InternDG
      igrid%internDGs(igrid%numInternDGs) = interndg

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridAddInternDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridMakeDGSpace"
!BOPI
! !IROUTINE: ESMF_IGridMakeDGSpace - Allocate or extend InternDG array

! !INTERFACE:
      subroutine ESMF_IGridMakeDGSpace(igridp, newcount, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: igridp
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
!     \item[igridp]
!          Pointer to an {\tt ESMF\_IGridClass}, the internal structure
!          which holds the {\tt IGrid} information.
!     \item[newcount]
!          Make sure there are enough space in the array to hold
!          {\tt newcount} items.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
 
       ! note: this is an internal routine.  rc isn't optional - so we
      ! don't have to fool with rcpresent and status here.

      ! the save attribute is to be sure that the temp space isn't
      ! deallocated automatically by the compiler at the return of
      ! this routine. it's going to be pointed to by the InternDGs pointer
      ! in the igrid structure, but that might not be obvious to the compiler.
      type(ESMF_InternDG), dimension(:), pointer, save :: temp_digrids
      integer :: localrc                          ! Error status
      integer :: i, oldcount, alloccount

      ! Initialize return code; assume routine not implemented
      rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,igridp,rc)

      ! number of currently used and available entries
      oldcount = igridp%numInternDGs
      alloccount = igridp%numInternDGsAlloc

      ! if there are already enough, we are done.
      if (alloccount .ge. newcount) then
         rc = ESMF_SUCCESS
         return
      endif

#define CHUNK 4

      ! if none are allocated yet, allocate and return.
      ! the chunksize is 4 because it is a round number in base 2.
      if (alloccount .eq. 0) then
        allocate(igridp%internDGs(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial InternDGs", &
                                       ESMF_CONTEXT, rc)) return
        igridp%numInternDGsAlloc = CHUNK
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
     allocate(temp_digrids(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Extending internal IGrid list", &
                                    ESMF_CONTEXT, rc)) return

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_digrids(i) = igridp%internDGs(i)
     enddo

     ! deallocate old array
     deallocate(igridp%internDGs, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating InternDGs", &
                                    ESMF_CONTEXT, rc)) return

     ! and set original pointer to the new space
     igridp%internDGs => temp_digrids

     ! update count of how many items are currently allocated
     igridp%numInternDGsAlloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_IGridMakeDGSpace

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridAddPhysGrid"
!BOPI
! !IROUTINE: ESMF_IGridAddPhysGrid - adds a complete PhysGrid to IGrid type

! !INTERFACE:
      subroutine ESMF_IGridAddPhysGrid(igrid, physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), intent(inout) :: igrid
      type(ESMF_PhysGrid), intent(in)    :: physgrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine attaches an {\tt ESMF\_PhysGrid} object to an
!     {\tt ESMF\_IGrid} object.  It is only meant to be called by
!     igrid creation routines in the processes of building a igrid.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          IGrid structure to which new PhysGrid is to be added.
!     \item[physgrid]
!          Complete {\tt ESMF\_PhysGrid} to be added.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,igrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! Update the PhysGridAlloc counter and check to see if PhysGrid
      ! array needs to be resized to add the new physgrid
      call ESMF_IGridMakePGSpace(igrid, igrid%numPhysGrids+1, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      igrid%numPhysGrids = igrid%numPhysGrids + 1

      ! Add the PhysGrid
      igrid%physgrids(igrid%numPhysGrids) = physgrid

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridAddPhysGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridMakePGSpace"
!BOPI
! !IROUTINE: ESMF_IGridMakePGSpace - Allocate or extend PhysGrid array

! !INTERFACE:
      subroutine ESMF_IGridMakePGSpace(igridp, newcount, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass) :: igridp
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
!     \item[igridp]
!          Pointer to an {\tt ESMF\_IGridClass}, the internal structure
!          which holds the {\tt IGrid} information.
!     \item[newcount]
!          Make sure there are enough space in the array to hold
!          {\tt newcount} items.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      ! note: this is an internal routine.  rc isn't optional - so we
      ! don't have to fool with rcpresent and status here.

      ! the save attribute is to be sure that the temp space isn't
      ! deallocated automatically by the compiler at the return of
      ! this routine. it's going to be pointed to by the PhysGrids pointer
      ! in the igrid structure, but that might not be obvious to the compiler.
      type(ESMF_PhysGrid), dimension(:), pointer, save :: temp_pigrids
      integer, dimension(:), pointer, save :: temp_dgIndex
      integer :: localrc                          ! Error status
      integer :: i, oldcount, alloccount

      ! Initialize return code; assume routine not implemented
      rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,igridp,rc)

      ! number of currently used and available entries
      oldcount = igridp%numPhysGrids
      alloccount = igridp%numPhysGridsAlloc

      ! if there are already enough, we are done.
      if (alloccount .ge. newcount) then
         rc = ESMF_SUCCESS
         return
      endif

#define CHUNK 4

      ! if none are allocated yet, allocate and return.
      ! the chunksize is 4 because it is a round number in base 2.
      if (alloccount .eq. 0) then
        allocate(igridp%physgrids(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial physgrids", &
                                       ESMF_CONTEXT, rc)) return

        allocate(igridp%internDGIndex(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial InternDGIndex", &
                                       ESMF_CONTEXT, rc)) return

        igridp%numPhysGridsAlloc = CHUNK
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
     allocate(temp_pigrids(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Allocating temp_pigrids", &
                                    ESMF_CONTEXT, rc)) return

     allocate(temp_dgIndex(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Allocating temp_dgIndex", &
                                    ESMF_CONTEXT, rc)) return

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_pigrids(i)  = igridp%physgrids(i)
       temp_dgIndex(i) = igridp%internDGIndex(i)
     enddo

     ! deallocate old arrays
     deallocate(igridp%physgrids, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating physgrids", &
                                    ESMF_CONTEXT, rc)) return

     deallocate(igridp%internDGIndex, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating InternDGIndex", &
                                    ESMF_CONTEXT, rc)) return

     ! and set original pointers to the new space
     igridp%physgrids => temp_pigrids
     igridp%internDGIndex => temp_dgIndex

     ! update count of how many items are currently allocated
     igridp%numPhysGridsAlloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_IGridMakePGSpace

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetPhysGrid"
!BOPI
! !IROUTINE: ESMF_IGridGetPhysGrid - retrieves complete PhysGrid from IGrid type

! !INTERFACE:
      subroutine ESMF_IGridGetPhysGrid(physgrid, igrid, relloc, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(out) :: physgrid
      type(ESMF_IGridClass), intent(inout)  :: igrid
      type(ESMF_RelLoc), intent(in), optional :: relloc
      character(*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine retrieves an {\tt ESMF\_PhysGrid} object from an
!     {\tt ESMF\_IGrid} object.  A PhysGrid can be retrieved either
!     by name or by relative location.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} to be retrieved.
!     \item[igrid]
!          IGrid structure from which PhysGrid is to be extracted.
!     \item[{[relloc]}]
!          Relative location ({\tt ESMF_RelLoc}) to identify which
!          PhysGrid to retrieve.
!     \item[{[name]}]
!          Optional name to identify which PhysGrid to retrieve.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: n                              ! search loop index
      integer :: localrc                        ! Error status
      logical :: found                          ! found flag for searches
      logical :: dummy
      character (len=ESMF_MAXSTR) :: nameTmp    ! temporary name variable
      type(ESMF_RelLoc) :: rellocTmp

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,igrid,rc)

      ! If name supplied, search by name and return selected PhysGrid
      if (present(name)) then
        found = .false.
        name_search: do n = 1,igrid%numPhysGridsAlloc
          call ESMF_PhysGridGet(igrid%physgrids(n), name=nameTmp, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (name == nameTmp) then
            physgrid = igrid%physgrids(n)
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
        relloc_search: do n = 1,igrid%numPhysGridsAlloc
          call ESMF_PhysGridGet(igrid%physgrids(n), relloc=rellocTmp, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (relloc == rellocTmp) then
            physgrid = igrid%physgrids(n)
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

      end subroutine ESMF_IGridGetPhysGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetPhysGridID"
!BOPI
! !IROUTINE: ESMF_IGridGetPhysGridID - Get PhysGrid Id for a given relative location

! !INTERFACE:
      subroutine ESMF_IGridGetPhysGridID(igrid, relloc, physIGridId, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), intent(inout) :: igrid
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(out) :: physIGridId
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return the {\tt ESMF\_PhysGridId} associated with the given relative
!     location.  Return error if the igrid contains no PhysGrid at the
!     specified location.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          Class to be queried.
!     \item[relloc]
!          Relative location to query
!     \item[physIGridId]
!          Returned physIGrid identifier.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                          ! Error status
      type(ESMF_RelLoc) :: thisRelloc
      integer :: i

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,igrid,rc)

      physIGridId = -1

      ! Loop through physgrids comparing rellocs  TODO: make part of the IGrid obj?
      do i = 1,igrid%numPhysGrids
        call ESMF_PhysGridGet(igrid%physgrids(i), relloc=thisRelloc, &
                              rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        if (relloc.eq.thisRelloc) then
          physIGridId = i
          localrc = ESMF_SUCCESS
          exit
        endif
      enddo

      ! print error if the relloc is not found
      if (physIGridId.eq.-1) then
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                   "relloc not valid", &
                                   ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = localrc

      end subroutine ESMF_IGridGetPhysGridID

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetInternDG"
!BOPI
! !IROUTINE: ESMF_IGridGetInternDG - retrieves complete InternDG from IGrid type

! !INTERFACE:
      subroutine ESMF_IGridGetInternDG(interndg, igrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_InternDG), intent(out) :: interndg
      type(ESMF_IGridClass), intent(in)  :: igrid
      character(*), intent(in) :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine retrieves an {\tt ESMF\_InternDG} object from an
!     {\tt ESMF\_IGrid} object, given the name of the interndg.
!
!     The arguments are:
!     \begin{description}
!     \item[interndg]
!          {\tt ESMF\_InternDG} to be retrieved.
!     \item[igrid]
!          IGrid structure from which interndg is to be extracted.
!     \item[name]
!          Name to identify which InternDG to retrieve.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

      integer :: n                              ! search loop index
      integer :: localrc                        ! Error status
      logical :: found                          ! found flag for searches
      character (len=ESMF_MAXSTR) :: nameTmp    ! temporary name variable

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,igrid,rc)

      ! Search by name and return selected InternDG
      found = .false.
      name_search: do n=1,igrid%numInternDGsAlloc
         call ESMF_InternDGGet(igrid%internDGs(n), name=nameTmp, rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rc)) return
         if (name == nameTmp) then
            interndg = igrid%internDGs(n)
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

      end subroutine ESMF_IGridGetInternDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetBoundingBoxes"
!BOPI
! !IROUTINE: ESMF_IGridGetBoundingBoxes - Get the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_IGridGetBoundingBoxes(igrid, array, rc)
!
! !ARGUMENTS:
      type(ESMF_IGridClass), intent(in) :: igrid
      type(ESMF_LocalArray), intent(out) :: array !BOB changed to just out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the region identifier data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[igrid]
!          Pointer to a {\tt ESMF\_IGrid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      !integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,igrid,rc)

      array = igrid%boundingBoxes

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_IGridGetBoundingBoxes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridPointerEqual"
!BOPI
! !IROUTINE: ESMF_IGridPointerEqual - equality of IGrids
!
! !INTERFACE:
      function ESMF_IGridPointerEqual(IGrid1, IGrid2)

! !RETURN VALUE:
      logical :: ESMF_IGridPointerEqual

! !ARGUMENTS:

      type (ESMF_IGrid), intent(in) :: &
         IGrid1,      &! Two igrids to compare for
         IGrid2        ! equality (identity)

! !DESCRIPTION:
!     This routine compares two ESMF IGrids to see if
!     they have equivalent pointers to the same internal ESMF_IGridClass.
!     This will return false if the pointers are different, even if
!     the igrids describe exactly the same physical dimensions.  It is a
!     quick and dirty check.
!
!     The arguments are:
!     \begin{description}
!     \item[IGrid1, IGrid2]
!          Two igrids to compare for equality
!     \end{description}
!
!EOPI

      ESMF_IGridPointerEqual = Associated(IGrid1%ptr, IGrid2%ptr)

      end function ESMF_IGridPointerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridStatusEqual"
!BOPI
! !IROUTINE: ESMF_IGridStatusEqual - equality of IGrid statuses
!
! !INTERFACE:
      function ESMF_IGridStatusEqual(IGridStatus1, IGridStatus2)

! !RETURN VALUE:
      logical :: ESMF_IGridStatusEqual

! !ARGUMENTS:

      type (ESMF_IGridStatus), intent(in) :: &
         IGridStatus1,      &! Two igrid statuses to compare for
         IGridStatus2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridStatus1, IGridStatus2]
!          Two igrid statuses to compare for equality
!     \end{description}
!
!EOPI

      ESMF_IGridStatusEqual = (IGridStatus1%igridStatus == &
                              IGridStatus2%igridStatus)

      end function ESMF_IGridStatusEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridStructureEqual"
!BOPI
! !IROUTINE: ESMF_IGridStructureEqual - equality of IGrid structures
!
! !INTERFACE:
      function ESMF_IGridStructureEqual(IGridStructure1, IGridStructure2)

! !RETURN VALUE:
      logical :: ESMF_IGridStructureEqual

! !ARGUMENTS:

      type (ESMF_IGridStructure), intent(in) :: &
         IGridStructure1,      &! Two igrid structures to compare for
         IGridStructure2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid structures to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridStructure1, IGridStructure2]
!          Two region types to compare for equality
!     \end{description}
!
!EOPI

      ESMF_IGridStructureEqual = (IGridStructure1%igridStructure == &
                                 IGridStructure2%igridStructure)

      end function ESMF_IGridStructureEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridTypeEqual"
!BOPI
! !IROUTINE: ESMF_IGridTypeEqual - equality of IGrid types
!
! !INTERFACE:
      function ESMF_IGridTypeEqual(IGridType1, IGridType2)

! !RETURN VALUE:
      logical :: ESMF_IGridTypeEqual

! !ARGUMENTS:

      type (ESMF_IGridType), intent(in) :: &
         IGridType1,      &! Two igrid kinds to compare for
         IGridType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid kinds to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridType1, IGridType2]
!          Two region types to compare for equality
!     \end{description}
!
!EOPI

      ESMF_IGridTypeEqual = (IGridType1%igridType == &
                            IGridType2%igridType)

      end function ESMF_IGridTypeEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridVertTypeEqual"
!BOPI
! !IROUTINE: ESMF_IGridVertTypeEqual - equality of vertical IGrid types
!
! !INTERFACE:
      function ESMF_IGridVertTypeEqual(IGridVertType1, IGridVertType2)

! !RETURN VALUE:
      logical :: ESMF_IGridVertTypeEqual

! !ARGUMENTS:

      type (ESMF_IGridVertType), intent(in) :: &
         IGridVertType1,      &! Two vertical igrid kinds to compare for
         IGridVertType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF vertical IGrid kinds to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridVertType1, IGridVertType2]
!          Two vertical igrid types to compare for equality
!     \end{description}
!
!EOPI

      ESMF_IGridVertTypeEqual = (IGridVertType1%igridVertType == &
                            IGridVertType2%igridVertType)

      end function ESMF_IGridVertTypeEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridHorzStaggerEqual"
!BOPI
! !IROUTINE: ESMF_IGridHorzStaggerEqual - equality of IGrid horizontal staggerings
!
! !INTERFACE:
      function ESMF_IGridHorzStaggerEqual(IGridHorzStagger1, IGridHorzStagger2)

! !RETURN VALUE:
      logical :: ESMF_IGridHorzStaggerEqual

! !ARGUMENTS:

      type (ESMF_IGridHorzStagger), intent(in) :: &
         IGridHorzStagger1,      &! Two igrid horz staggerings to compare for
         IGridHorzStagger2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid horizontal staggerings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridHorzStagger1, IGridHorzStagger2]
!          Two igrid horizontal staggerings to compare for equality
!     \end{description}
!
!EOPI

      ESMF_IGridHorzStaggerEqual = (IGridHorzStagger1%stagger == &
                                   IGridHorzStagger2%stagger)

      end function ESMF_IGridHorzStaggerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridVertStaggerEqual"
!BOPI
! !IROUTINE: ESMF_IGridVertStaggerEqual - equality of IGrid vertical staggerings
!
! !INTERFACE:
      function ESMF_IGridVertStaggerEqual(IGridVertStagger1, IGridVertStagger2)

! !RETURN VALUE:
      logical :: ESMF_IGridVertStaggerEqual

! !ARGUMENTS:

      type (ESMF_IGridVertStagger), intent(in) :: &
         IGridVertStagger1,      &! Two igrid vert staggerings to compare for
         IGridVertStagger2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid vertical staggerings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridVertStagger1, IGridVertStagger2]
!          Two igrid vertical staggerings to compare for equality
!     \end{description}
!
!EOPI

      ESMF_IGridVertStaggerEqual = (IGridVertStagger1%stagger == &
                                   IGridVertStagger2%stagger)

      end function ESMF_IGridVertStaggerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridStorageEqual"
!BOPI
! !IROUTINE: ESMF_IGridStorageEqual - equality of IGrid storage schemes
!
! !INTERFACE:
      function ESMF_IGridStorageEqual(IGridStorage1, IGridStorage2)

! !RETURN VALUE:
      logical :: ESMF_IGridStorageEqual

! !ARGUMENTS:

      type (ESMF_IGridStorage), intent(in) :: &
         IGridStorage1,      &! Two igrid storage schemes to compare for
         IGridStorage2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid storage schemes to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridStorage1, IGridStorage2]
!          Two igrid storage schemes to compare for equality
!     \end{description}
!
!EOPI

      ESMF_IGridStorageEqual = (IGridStorage1%storage == &
                               IGridStorage2%storage)

      end function ESMF_IGridStorageEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordOrderEqual"
!BOPI
! !IROUTINE: ESMF_CoordOrderEqual - equality of IGrid coordinate orders
!
! !INTERFACE:
      function ESMF_CoordOrderEqual(CoordOrder1, CoordOrder2)

! !RETURN VALUE:
      logical :: ESMF_CoordOrderEqual

! !ARGUMENTS:

      type (ESMF_CoordOrder), intent(in) :: &
         CoordOrder1,      &! Two igrid coordinate orders to compare for
         CoordOrder2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid coordinate orderings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordOrder1, CoordOrder2]
!          Two coordinate orders to compare for equality
!     \end{description}
!
!EOPI

      ESMF_CoordOrderEqual = (CoordOrder1%order == &
                              CoordOrder2%order)

      end function ESMF_CoordOrderEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordIndexEqual"
!BOPI
! !IROUTINE: ESMF_CoordIndexEqual - equality of IGrid coordinate indexing
!
! !INTERFACE:
      function ESMF_CoordIndexEqual(CoordIndex1, CoordIndex2)

! !RETURN VALUE:
      logical :: ESMF_CoordIndexEqual

! !ARGUMENTS:

      type (ESMF_CoordIndex), intent(in) :: &
         CoordIndex1,      &! Two igrid coordinate indexings to compare
         CoordIndex2        ! for equality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid coordinate indexings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordIndex1, CoordIndex2]
!          Two coordinate indexings to compare for equality
!     \end{description}
!
!EOPI

      ESMF_CoordIndexEqual = (CoordIndex1%index == &
                              CoordIndex2%index)

      end function ESMF_CoordIndexEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridPointerNotEqual"
!BOPI
! !IROUTINE: ESMF_IGridPointerNotEqual - equality of IGrids
!
! !INTERFACE:
      function ESMF_IGridPointerNotEqual(IGrid1, IGrid2)

! !RETURN VALUE:
      logical :: ESMF_IGridPointerNotEqual

! !ARGUMENTS:

      type (ESMF_IGrid), intent(in) :: &
         IGrid1,      &! Two igrids to compare for
         IGrid2        ! inequality (not identical)

! !DESCRIPTION:
!     This routine compares two ESMF IGrids to see if
!     they have pointers to different internal ESMF_IGridClasses.
!     This will return true if the pointers are different, even if
!     the igrids describe exactly the same physical dimensions.  It is a
!     quick and dirty check.
!
!     The arguments are:
!     \begin{description}
!     \item[IGrid1, IGrid2]
!          Two igrids to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_IGridPointerNotEqual = .not.Associated(IGrid1%ptr, IGrid2%ptr)

      end function ESMF_IGridPointerNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridStatusNotEqual"
!BOPI
! !IROUTINE: ESMF_IGridStatusNotEqual - non-equality of IGrid statuses
!
! !INTERFACE:
      function ESMF_IGridStatusNotEqual(IGridStatus1, IGridStatus2)

! !RETURN VALUE:
      logical :: ESMF_IGridStatusNotEqual

! !ARGUMENTS:

      type (ESMF_IGridStatus), intent(in) :: &
         IGridStatus1,      &! Two IGrid Statuses to compare for
         IGridStatus2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridStatus1, IGridStatus2]
!          Two statuses of IGrids to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_IGridStatusNotEqual = (IGridStatus1%igridStatus /= &
                                 IGridStatus2%igridStatus)

      end function ESMF_IGridStatusNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridStructureNotEq"
!BOPI
! !IROUTINE: ESMF_IGridStructureNotEq - non-equality of IGrid structures
!
! !INTERFACE:
      function ESMF_IGridStructureNotEq(IGridStructure1, IGridStructure2)

! !RETURN VALUE:
      logical :: ESMF_IGridStructureNotEq

! !ARGUMENTS:

      type (ESMF_IGridStructure), intent(in) :: &
         IGridStructure1,      &! Two IGrid Structures to compare for
         IGridStructure2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid structures to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridStructure1, IGridStructure2]
!          Two structures of IGrids to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_IGridStructureNotEq = (IGridStructure1%igridStructure /= &
                                    IGridStructure2%igridStructure)

      end function ESMF_IGridStructureNotEq

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_IGridTypeNotEqual - non-equality of IGrid kinds
!
! !INTERFACE:
      function ESMF_IGridTypeNotEqual(IGridType1, IGridType2)

! !RETURN VALUE:
      logical :: ESMF_IGridTypeNotEqual

! !ARGUMENTS:

      type (ESMF_IGridType), intent(in) :: &
         IGridType1,      &! Two IGrid kinds to compare for
         IGridType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid kinds to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridType1, IGridType2]
!          Two kinds of IGrids to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_IGridTypeNotEqual = (IGridType1%igridType /= &
                               IGridType2%igridType)

      end function ESMF_IGridTypeNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridVertTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_IGridVertTypeNotEqual - non-equality of vertical IGrid kinds
!
! !INTERFACE:
      function ESMF_IGridVertTypeNotEqual(IGridVertType1, IGridVertType2)

! !RETURN VALUE:
      logical :: ESMF_IGridVertTypeNotEqual

! !ARGUMENTS:

      type (ESMF_IGridVertType), intent(in) :: &
         IGridVertType1,      &! Two vertical IGrid kinds to compare for
         IGridVertType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF vertical IGrid kinds to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridVertType1, IGridVertType2]
!          Two kinds of vertical IGrids to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_IGridVertTypeNotEqual = (IGridVertType1%igridVertType /= &
                                   IGridVertType2%igridVertType)

      end function ESMF_IGridVertTypeNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridHorzStagNotEq"
!BOPI
! !IROUTINE: ESMF_IGridHorzStagNotEq - inequality of IGrid horizontal staggerings
!
! !INTERFACE:
      function ESMF_IGridHorzStagNotEq(IGridHorzStagger1, IGridHorzStagger2)

! !RETURN VALUE:
      logical :: ESMF_IGridHorzStagNotEq

! !ARGUMENTS:

      type (ESMF_IGridHorzStagger), intent(in) :: &
         IGridHorzStagger1,      &! Two igrid horizontal staggerings to compare for
         IGridHorzStagger2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid horizontal staggerings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridHorzStagger1, IGridHorzStagger2]
!          Two igrid horizontal staggerings to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_IGridHorzStagNotEq = (IGridHorzStagger1%stagger /= &
                                      IGridHorzStagger2%stagger)

      end function ESMF_IGridHorzStagNotEq

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridVertStagNotEq"
!BOPI
! !IROUTINE: ESMF_IGridVertStagNotEq - inequality of IGrid vertical staggerings
!
! !INTERFACE:
      function ESMF_IGridVertStagNotEq(IGridVertStagger1, IGridVertStagger2)

! !RETURN VALUE:
      logical :: ESMF_IGridVertStagNotEq

! !ARGUMENTS:

      type (ESMF_IGridVertStagger), intent(in) :: &
         IGridVertStagger1,      &! Two igrid vertical staggerings to compare for
         IGridVertStagger2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid vertical staggerings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridVertStagger1, IGridVertStagger2]
!          Two igrid vertical staggerings to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_IGridVertStagNotEq = (IGridVertStagger1%stagger /= &
                                      IGridVertStagger2%stagger)

      end function ESMF_IGridVertStagNotEq

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridStorageNotEqual"
!BOPI
! !IROUTINE: ESMF_IGridStorageNotEqual - inequality of IGrid storage schemes
!
! !INTERFACE:
      function ESMF_IGridStorageNotEqual(IGridStorage1, IGridStorage2)

! !RETURN VALUE:
      logical :: ESMF_IGridStorageNotEqual

! !ARGUMENTS:

      type (ESMF_IGridStorage), intent(in) :: &
         IGridStorage1,      &! Two igrid storage schemes to compare for
         IGridStorage2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid storage schemes to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[IGridStorage1, IGridStorage2]
!          Two igrid storage schemes to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_IGridStorageNotEqual = (IGridStorage1%storage /= &
                                  IGridStorage2%storage)

      end function ESMF_IGridStorageNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordOrderNotEqual"
!BOPI
! !IROUTINE: ESMF_CoordOrderNotEqual - inequality of IGrid coordinate orders
!
! !INTERFACE:
      function ESMF_CoordOrderNotEqual(CoordOrder1, CoordOrder2)

! !RETURN VALUE:
      logical :: ESMF_CoordOrderNotEqual

! !ARGUMENTS:

      type (ESMF_CoordOrder), intent(in) :: &
         CoordOrder1,      &! Two igrid coordinate orders to compare for
         CoordOrder2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid coordinate orderings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordOrder1, CoordOrder2]
!          Two coordinate orders to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_CoordOrderNotEqual = (CoordOrder1%order /= &
                                 CoordOrder2%order)

      end function ESMF_CoordOrderNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordIndexNotEqual"
!BOPI
! !IROUTINE: ESMF_CoordIndexNotEqual - inequality of IGrid coordinate indexing
!
! !INTERFACE:
      function ESMF_CoordIndexNotEqual(CoordIndex1, CoordIndex2)

! !RETURN VALUE:
      logical :: ESMF_CoordIndexNotEqual

! !ARGUMENTS:

      type (ESMF_CoordIndex), intent(in) :: &
         CoordIndex1,      &! Two igrid coordinate indexings to compare
         CoordIndex2        ! for inequality

! !DESCRIPTION:
!     This routine compares two ESMF IGrid coordinate indexings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[CoordIndex1, CoordIndex2]
!          Two coordinate indexings to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_CoordIndexNotEqual = (CoordIndex1%index /= &
                                 CoordIndex2%index)

      end function ESMF_CoordIndexNotEqual

!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRectIGridValidate()"
!BOPI
! !IROUTINE: ESMF_LogRectIGridValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_LogRectIGridValidate(lrg, rc)
!
! !ARGUMENTS:
    type(ESMF_LogRectIGrid), intent(inout)              :: lrg
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
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_LogRectIGridGetInit, ESMF_LogRectIGridInit,lrg)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_LogRectIGridValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRectIGridInit()"
!BOPI
! !IROUTINE: ESMF_LogRectIGridInit - Init DataHolder internals

! !INTERFACE:
  subroutine ESMF_LogRectIGridInit(lrg)
!
! !ARGUMENTS:
    type(ESMF_LogRectIGrid), intent(inout)              :: lrg
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
!------------------------------------------------------------------------------
    ESMF_INIT_SET_DEFINED(lrg)
  end subroutine ESMF_LogRectIGridInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRectIGridGetInit"
!BOPI
! !IROUTINE: ESMF_LogRectIGridGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_LogRectIGridGetInit(lrg) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_LogRectIGridGetInit   
!
! !ARGUMENTS:
      type(ESMF_LogRectIGrid), intent(in), optional :: lrg
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
      ESMF_LogRectIGridGetInit = ESMF_INIT_GET(lrg)
    else
      ESMF_LogRectIGridGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_LogRectIGridGetInit
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSpecificValidate()"
!BOPI
! !IROUTINE: ESMF_IGridSpecificValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_IGridSpecificValidate(gs, rc)
!
! !ARGUMENTS:
    type(ESMF_IGridSpecific), intent(inout)              :: gs
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
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_IGridSpecificGetInit, ESMF_IGridSpecificInit,gs)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_IGridSpecificValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSpecificInit()"
!BOPI
! !IROUTINE: ESMF_IGridSpecificInit - Init DataHolder internals

! !INTERFACE:
  subroutine ESMF_IGridSpecificInit(gs)
!
! !ARGUMENTS:
    type(ESMF_IGridSpecific), intent(inout)              :: gs
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
!------------------------------------------------------------------------------
    ESMF_INIT_SET_DEFINED(gs)
  end subroutine ESMF_IGridSpecificInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSpecificGetInit"
!BOPI
! !IROUTINE: ESMF_IGridSpecificGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_IGridSpecificGetInit(gs) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_IGridSpecificGetInit   
!
! !ARGUMENTS:
      type(ESMF_IGridSpecific), intent(in), optional :: gs
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
      ESMF_IGridSpecificGetInit = ESMF_INIT_GET(gs)
    else
      ESMF_IGridSpecificGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_IGridSpecificGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridClassValidate()"
!BOPI
! !IROUTINE: ESMF_IGridClassValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_IGridClassValidate(gc, rc)
!
! !ARGUMENTS:
    type(ESMF_IGridClass), intent(in)              :: gc
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
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_IGridClGetInit,gc,rc)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_IGridClassValidate
!------------------------------------------------------------------------------

! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridClGetInit"
!BOPI
! !IROUTINE: ESMF_IGridClGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_IGridClGetInit(gc) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_IGridClGetInit   
!
! !ARGUMENTS:
      type(ESMF_IGridClass), intent(in), optional :: gc
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
      ESMF_IGridClGetInit = ESMF_INIT_GET(gc)
    else
      ESMF_IGridClGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_IGridClGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_IGridSetInitCreated - Set IGrid init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_IGridSetInitCreated(igrid, rc)
!
! !ARGUMENTS:
    type(ESMF_IGrid), intent(inout)           :: igrid
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in IGrid object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[igrid] 
!          Specified {\tt ESMF\_IGrid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(igrid)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_IGridSetInitCreated



! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IGridGetInit"
!BOPI
! !IROUTINE: ESMF_IGridGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_IGridGetInit(g) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_IGridGetInit   
!
! !ARGUMENTS:
      type(ESMF_IGrid), intent(in), optional :: g
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
      ESMF_IGridGetInit = ESMF_INIT_GET(g)
    else
      ESMF_IGridGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_IGridGetInit
!------------------------------------------------------------------------------



      end module ESMF_IGridTypesMod
