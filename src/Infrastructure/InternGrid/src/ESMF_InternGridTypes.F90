! $Id: ESMF_InternGridTypes.F90,v 1.1 2007/06/22 23:21:37 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_InternGridTypes.F90"
!
!     ESMF InternGrid Types Module
      module ESMF_InternGridTypesMod
!
!==============================================================================
!
! This file contains the InternGrid class definition and basic InternGrid class
! methods.  These are used by the main InternGrid module interfaces which
! provide the user interface for interngrid functions. 
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_InternGridTypesMod - InternGrid class data types
!
! !DESCRIPTION:
!
! The code in this file contains data types and basic functions for the 
! {\tt ESMF\_InternGrid} class.  This class provides utilities for the InternGrid
! class that are used by the main {\tt ESMF\_InternGrid} module.
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
      use ESMF_InternDGMod      ! ESMF distributed interngrid class
      use ESMF_PhysCoordMod     ! ESMF physical coord class
      use ESMF_PhysGridMod      ! ESMF physical interngrid class
      use ESMF_InitMacrosMod  ! init macros stuff
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!  For now, add derived types for specific interngrid structures here:
!  These derived types contain all the necessary information beyond the
!  InternGridClass derived type.
!------------------------------------------------------------------------------
!     ! ESMF_LogRectInternGrid
!
!     ! Type to contain extra information for Logically rectangular interngrids.

      type ESMF_LogRectInternGrid
      sequence
        real(ESMF_KIND_R8), dimension(ESMF_MAXIGRIDDIM) :: deltaPerDim
        type(ESMF_LocalArray), dimension(:), pointer :: coords
        integer, dimension(ESMF_MAXIGRIDDIM) :: countPerDim

         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridSpecific
!
!     ! Type to hold pointers to all available specific interngrid derived types

      type ESMF_InternGridSpecific
      sequence
        type (ESMF_LogRectInternGrid), pointer :: logRectInternGrid

        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridStatus
!
!     ! Type to specify overall status of interngrid.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_InternGridStatus
      sequence
        integer :: interngridStatus
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridStructure
!
!     ! Type to specify overall structure of interngrid for supported ESMF InternGrids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_InternGridStructure
      sequence
!      private
        integer :: interngridStructure
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridType
!
!     ! Type to specify kind of interngrid for supported ESMF InternGrids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_InternGridType
      sequence
!      private
        integer :: interngridType
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridVertType
!
!     ! Type to specify kind of vertical interngrid for supported ESMF InternGrids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_InternGridVertType
      sequence
!      private
        integer :: interngridVertType
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridHorzStagger
!
!     ! Type to specify type of horizontal interngrid staggering for supported
!     !  ESMF InternGrids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_InternGridHorzStagger
      sequence
!      private
        integer :: stagger
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridVertStagger
!
!     ! Type to specify type of vertical interngrid staggering for supported
!     !  ESMF InternGrids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_InternGridVertStagger
      sequence
!      private
        integer :: stagger
      end type

!------------------------------------------------------------------------------
!     ! ESMF_InternGridStorage
!
!     ! Type to specify type of interngrid storage schemes for supported
!     !  ESMF InternGrids.  See the public parameters declared below for the possible
!     !  valid values for this.

      type ESMF_InternGridStorage
      sequence
!      private
        integer :: storage
      end type

!------------------------------------------------------------------------------
!     ! ESMF_CoordOrder
!
!     ! Type to specify logical ordering of coordinate in ESMF InternGrids.
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
!     ! Type to specify global or local indexing of ESMF InternGrids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_CoordIndex
      sequence
!      private
        integer :: index
      end type

!------------------------------------------------------------------------------
!     !  ESMF_InternGridClass
!
!     ! Definition for the InternGrid class.

      type ESMF_InternGridClass
      sequence
!      private

        type (ESMF_Base) :: base              ! base class object
        type (ESMF_InternGridStatus) :: interngridStatus  ! uninitialized, init ok, etc

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
        type (ESMF_InternGridStructure) :: interngridStructure
                                              ! enum for structure of interngrid
                                              ! i.e. logically rectangular, etc
        type (ESMF_InternGridType) :: horzInternGridType  ! enum for type of horizontal interngrid
        type (ESMF_InternGridVertType) :: vertInternGridType
                                              ! enum for type of vertical interngrid
        type (ESMF_InternGridHorzStagger) :: horzStagger
                                              ! enum for horizontal interngrid staggering
        type (ESMF_InternGridVertStagger) :: vertStagger
                                              ! enum for vertical interngrid staggering
        type (ESMF_InternGridStorage) :: interngridStorage
                                              ! enum for interngrid storage scheme
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
        integer :: numPhysGrids               ! number of interngrid descriptors
                                              ! necessary to support
                                              ! staggering, vertical
                                              ! interngrids, background interngrids


        integer :: numPhysGridsAlloc          ! number of physgrids allocated

        type (ESMF_PhysGrid), dimension(:), pointer :: physgrids
                                              ! info for all interngrid descriptions
                                              ! necessary to define horizontal,
                                              ! staggered and vertical interngrids
        integer, dimension(:), pointer :: internDGIndex
                                              ! for each physgrid, the index of
                                              ! the corresponding InternDG


        type (ESMF_InternDG), dimension(:), pointer :: internDGs       
                                              ! decomposition and other
                                              ! logical space info for interngrid

        character(len=ESMF_MAXSTR), dimension(ESMF_MAXIGRIDDIM) :: dimNames
        character(len=ESMF_MAXSTR), dimension(ESMF_MAXIGRIDDIM) :: dimUnits

        type (ESMF_LocalArray) :: boundingBoxes
                                            ! array of bounding boxes on each DE
                                            ! used for search routines
        type (ESMF_InternGridSpecific) :: interngridSpecific

        integer :: numInternDGsAlloc          ! number of InternDGs allocated

        integer :: numInternDGs               ! number of interngrid descriptors
                                              ! necessary to support
                                              ! staggering, vertical
                                              ! interngrids, background interngrids
!       type (???) :: searchStructure

         ESMF_INIT_DECLARE

      end type

!------------------------------------------------------------------------------
!     !  ESMF_InternGrid
!
!     ! The InternGrid data structure that is passed between languages.

      type ESMF_InternGrid
      sequence
!      private
#ifndef ESMF_NO_INITIALIZERS
        type (ESMF_InternGridClass), pointer :: ptr => NULL()
#else
        type (ESMF_InternGridClass), pointer :: ptr
#endif
       
        ESMF_INIT_DECLARE

      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

      public ESMF_LogRectInternGrid,   ESMF_InternGridSpecific,    ESMF_InternGridStatus
      public ESMF_InternGridStructure, ESMF_InternGridHorzStagger, ESMF_InternGridVertStagger
      public ESMF_InternGridClass,     ESMF_InternGridType,        ESMF_InternGridVertType
      public ESMF_CoordOrder,    ESMF_CoordIndex,      ESMF_InternGridStorage
      public ESMF_InternGrid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    ! These functions are generally accessed only by the main InternGrid module
    ! and are not meant to be accessed by the user.  Well...except for
    ! the overloaded operators.

    public ESMF_InternGridGetInit 
    public ESMF_InternGridSetInitCreated
    public ESMF_InternGridClassValidate   
    public ESMF_InternGridClassGetInit 
    public ESMF_LogRectInternGridValidate   
    public ESMF_LogRectInternGridInit
    public ESMF_LogRectInternGridGetInit 
    public ESMF_InternGridSpecificValidate   
    public ESMF_InternGridSpecificInit
    public ESMF_InternGridSpecificGetInit 
    public ESMF_InternGridConstructNew
    public ESMF_InternGridGetDELayout
    public ESMF_InternGridAddInternDG
    public ESMF_InternGridMakeDGSpace
    public ESMF_InternGridAddPhysGrid
    public ESMF_InternGridMakePGSpace
    public ESMF_InternGridGetPhysGrid
    public ESMF_InternGridGetPhysGridID
    public ESMF_InternGridGetInternDG
    public ESMF_InternGridGetBoundingBoxes

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

   type (ESMF_InternGridStatus), parameter, public ::              &
      ESMF_IGRID_STATUS_UNKNOWN      =  ESMF_InternGridStatus(0), &
      ESMF_IGRID_STATUS_UNINIT       =  ESMF_InternGridStatus(1), &
      ESMF_IGRID_STATUS_READY        =  ESMF_InternGridStatus(2), &
      ESMF_IGRID_STATUS_UNALLOCATED  =  ESMF_InternGridStatus(3), &
      ESMF_IGRID_STATUS_ALLOCATED    =  ESMF_InternGridStatus(4), &
      ESMF_IGRID_STATUS_INIT         =  ESMF_InternGridStatus(5), &
      ESMF_IGRID_STATUS_INVALID      =  ESMF_InternGridStatus(6)

  ! Supported ESMF structure kinds:
  !   ESMF_IGRID_STRUCTURE_UNKNOWN         ! unknown or undefined interngrid
  !   ESMF_IGRID_STRUCTURE_LOGRECT         ! logically rectangular interngrid
  !   ESMF_IGRID_STRUCTURE_LOGRECT_BLK     ! logically rectangular blocked interngrid
  !   ESMF_IGRID_STRUCTURE_UNSTRUCT        ! unstructured interngrid
  !   ESMF_IGRID_STRUCTURE_USER            ! user-defined interngrid

   type (ESMF_InternGridStructure), parameter, public ::              &
      ESMF_IGRID_STRUCTURE_UNKNOWN     =  ESMF_InternGridStructure(0), &
      ESMF_IGRID_STRUCTURE_LOGRECT     =  ESMF_InternGridStructure(1), &
      ESMF_IGRID_STRUCTURE_LOGRECT_BLK =  ESMF_InternGridStructure(2), &
      ESMF_IGRID_STRUCTURE_UNSTRUCT    =  ESMF_InternGridStructure(3), &
      ESMF_IGRID_STRUCTURE_USER        =  ESMF_InternGridStructure(4)

  ! Supported ESMF interngrid kinds:
  !   ESMF_IGRID_TYPE_UNKNOWN           ! unknown or undefined interngrid
  !   ESMF_IGRID_TYPE_LATLON            ! aligned with longitude,latitude
  !   ESMF_IGRID_TYPE_LATLON_UNI        ! LatLon interngrid with uniform spacing
  !   ESMF_IGRID_TYPE_LATLON_GAUSS      ! LatLon with gaussian-spaced latitudes
  !   ESMF_IGRID_TYPE_LATLON_MERC       ! LatLon with Mercator-spaced latitudes
  !   ESMF_IGRID_TYPE_REDUCED           ! LatLon with num lon pts a fcn of lat
  !   ESMF_IGRID_TYPE_DIPOLE            ! Displaced-pole dipole interngrid
  !   ESMF_IGRID_TYPE_TRIPOLE           ! Tripolar interngrids
  !   ESMF_IGRID_TYPE_XY                ! aligned with Cartesian x-y coords
  !   ESMF_IGRID_TYPE_XY_UNI            ! XY interngrid with uniform spacing
  !   ESMF_IGRID_TYPE_DATASTREAM        ! Data stream - set of locations
  !   ESMF_IGRID_TYPE_PHYSFOURIER       ! Mixed Fourier/Phys Space interngrid
  !   ESMF_IGRID_TYPE_SPHER_SPECT       ! spectral space:spherical harmonics
  !   ESMF_IGRID_TYPE_CART_SPECT        ! spectral space:Cartesian coords
  !   ESMF_IGRID_TYPE_GEODESIC          ! spherical geodesic interngrid
  !   ESMF_IGRID_TYPE_CUBEDSPHERE       ! cubed sphere interngrid
  !   ESMF_IGRID_TYPE_EXCHANGE          ! intersection of two interngrids

   type (ESMF_InternGridType), parameter, public ::              &
      ESMF_IGRID_TYPE_UNKNOWN           = ESMF_InternGridType( 0), &
      ESMF_IGRID_TYPE_LATLON            = ESMF_InternGridType( 1), &
      ESMF_IGRID_TYPE_LATLON_UNI        = ESMF_InternGridType( 2), &
      ESMF_IGRID_TYPE_LATLON_GAUSS      = ESMF_InternGridType( 3), &
      ESMF_IGRID_TYPE_LATLON_MERC       = ESMF_InternGridType( 4), &
      ESMF_IGRID_TYPE_REDUCED           = ESMF_InternGridType( 5), &
      ESMF_IGRID_TYPE_DIPOLE            = ESMF_InternGridType( 6), &
      ESMF_IGRID_TYPE_TRIPOLE           = ESMF_InternGridType( 7), &
      ESMF_IGRID_TYPE_XY                = ESMF_InternGridType( 8), &
      ESMF_IGRID_TYPE_XY_UNI            = ESMF_InternGridType( 9), &
      ESMF_IGRID_TYPE_DATASTREAM        = ESMF_InternGridType(10), &
      ESMF_IGRID_TYPE_PHYSFOURIER       = ESMF_InternGridType(11), &
      ESMF_IGRID_TYPE_SPHER_SPECT       = ESMF_InternGridType(12), &
      ESMF_IGRID_TYPE_CART_SPECT        = ESMF_InternGridType(13), &
      ESMF_IGRID_TYPE_GEODESIC          = ESMF_InternGridType(14), &
      ESMF_IGRID_TYPE_CUBEDSPHERE       = ESMF_InternGridType(15), &
      ESMF_IGRID_TYPE_EXCHANGE          = ESMF_InternGridType(16)

  ! Supported ESMF vertical interngrid kinds:
  !   ESMF_IGRID_TYPE_VERT_UNKNOWN        ! unknown or undefined vertical interngrid
  !   ESMF_IGRID_TYPE_VERT_USER           ! user-defined vertical interngrid
  !   ESMF_IGRID_TYPE_VERT_DEPTH          ! vertical interngrid with 0 at top surface
  !   ESMF_IGRID_TYPE_VERT_HEIGHT         ! vertical interngrid with 0 at bottom
  !   ESMF_IGRID_TYPE_VERT_PRESSURE       ! vertical interngrid with pressure coordinates
  !   ESMF_IGRID_TYPE_VERT_SIGMA          ! vertical interngrid with sigma coordinates
  !   ESMF_IGRID_TYPE_VERT_THETA          ! vertical interngrid with theta coordinates
  !   ESMF_IGRID_TYPE_VERT_ETA            ! vertical interngrid with eta coordinates
  !   ESMF_IGRID_TYPE_VERT_ISOPYCNAL      ! vertical interngrid with density coordinates
  !   ESMF_IGRID_TYPE_VERT_HYBRID         ! vertical interngrid with hybrid coordinates
  !   ESMF_IGRID_TYPE_VERT_LAGRANGIAN     ! vertical interngrid with lagrangian coordinates

   type (ESMF_InternGridVertType), parameter, public ::              &
      ESMF_IGRID_VERT_TYPE_UNKNOWN      = ESMF_InternGridVertType( 0), &
      ESMF_IGRID_VERT_TYPE_USER         = ESMF_InternGridVertType( 1), &
      ESMF_IGRID_VERT_TYPE_DEPTH        = ESMF_InternGridVertType( 2), &
      ESMF_IGRID_VERT_TYPE_HEIGHT       = ESMF_InternGridVertType( 3), &
      ESMF_IGRID_VERT_TYPE_PRESSURE     = ESMF_InternGridVertType( 4), &
      ESMF_IGRID_VERT_TYPE_SIGMA        = ESMF_InternGridVertType( 5), &
      ESMF_IGRID_VERT_TYPE_THETA        = ESMF_InternGridVertType( 6), &
      ESMF_IGRID_VERT_TYPE_ETA          = ESMF_InternGridVertType( 7), &
      ESMF_IGRID_VERT_TYPE_ISOPYCNAL    = ESMF_InternGridVertType( 8), &
      ESMF_IGRID_VERT_TYPE_HYBRID       = ESMF_InternGridVertType( 9), &
      ESMF_IGRID_VERT_TYPE_LAGRANGIAN   = ESMF_InternGridVertType(10)

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
  !   ESMF_IGRID_HORZ_STAGGER_Z        ! C interngrid equiv for geodesic interngrid

   type (ESMF_InternGridHorzStagger), parameter, public ::              &
      ESMF_IGRID_HORZ_STAGGER_UNKNOWN  = ESMF_InternGridHorzStagger( 0), &
      ESMF_IGRID_HORZ_STAGGER_A        = ESMF_InternGridHorzStagger( 1), &
      ESMF_IGRID_HORZ_STAGGER_B_NE     = ESMF_InternGridHorzStagger( 2), &
      ESMF_IGRID_HORZ_STAGGER_B_SW     = ESMF_InternGridHorzStagger( 3), &
      ESMF_IGRID_HORZ_STAGGER_B_SE     = ESMF_InternGridHorzStagger( 4), &
      ESMF_IGRID_HORZ_STAGGER_B_NW     = ESMF_InternGridHorzStagger( 5), &
      ESMF_IGRID_HORZ_STAGGER_C_NE     = ESMF_InternGridHorzStagger( 6), &
      ESMF_IGRID_HORZ_STAGGER_C_SW     = ESMF_InternGridHorzStagger( 7), &
      ESMF_IGRID_HORZ_STAGGER_C_SE     = ESMF_InternGridHorzStagger( 8), &
      ESMF_IGRID_HORZ_STAGGER_C_NW     = ESMF_InternGridHorzStagger( 9), &
      ESMF_IGRID_HORZ_STAGGER_D_NE     = ESMF_InternGridHorzStagger(10), &
      ESMF_IGRID_HORZ_STAGGER_D_SW     = ESMF_InternGridHorzStagger(11), &
      ESMF_IGRID_HORZ_STAGGER_D_SE     = ESMF_InternGridHorzStagger(12), &
      ESMF_IGRID_HORZ_STAGGER_D_NW     = ESMF_InternGridHorzStagger(13), &
      ESMF_IGRID_HORZ_STAGGER_E        = ESMF_InternGridHorzStagger(14), &
      ESMF_IGRID_HORZ_STAGGER_Z        = ESMF_InternGridHorzStagger(15)

  ! Recognized ESMF vertical staggering types
  !   ESMF_IGRID_VERT_STAGGER_UNKNOWN  ! unknown or undefined staggering
  !   ESMF_IGRID_VERT_STAGGER_CENTER   ! vertical midpoints
  !   ESMF_IGRID_VERT_STAGGER_TOP      ! at top    face of vertical interngrid
  !   ESMF_IGRID_VERT_STAGGER_BOTTOM   ! at bottom face of vertical interngrid

   type (ESMF_InternGridVertStagger), parameter, public ::              &
      ESMF_IGRID_VERT_STAGGER_UNKNOWN  = ESMF_InternGridVertStagger( 0), &
      ESMF_IGRID_VERT_STAGGER_CENTER   = ESMF_InternGridVertStagger( 1), &
      ESMF_IGRID_VERT_STAGGER_TOP      = ESMF_InternGridVertStagger( 2), &
      ESMF_IGRID_VERT_STAGGER_BOTTOM   = ESMF_InternGridVertStagger( 3)

  ! Recognized interngrid storage schemes
  !   ESMF_IGRID_STORAGE_UNKNOWN   ! unknown or undefined interngrid storage
  !   ESMF_IGRID_STORAGE_LOGRECT   ! uses logically rectangular storage, one
  !                               ! block per DE
  !   ESMF_IGRID_STORAGE_BLOCK     ! uses logically rectangular storage, multiple
  !                               ! blocks per DE
  !   ESMF_IGRID_STORAGE_ARBITRARY ! uses arbitrary storage, which infers a
  !                               ! scattering of interngrid cell locations and limits
  !                               ! available communication and query functions

   type (ESMF_InternGridStorage), parameter, public ::         &
      ESMF_IGRID_STORAGE_UNKNOWN   = ESMF_InternGridStorage(0), &
      ESMF_IGRID_STORAGE_LOGRECT   = ESMF_InternGridStorage(1), &
      ESMF_IGRID_STORAGE_BLOCK     = ESMF_InternGridStorage(2), &
      ESMF_IGRID_STORAGE_ARBITRARY = ESMF_InternGridStorage(3)

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
      interngridOrder = reshape((/ 1, 1, 1, &
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
      '$Id: ESMF_InternGridTypes.F90,v 1.1 2007/06/22 23:21:37 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_InternGridPointerEqual
         module procedure ESMF_InternGridStatusEqual
         module procedure ESMF_InternGridStructureEqual
         module procedure ESMF_InternGridTypeEqual
         module procedure ESMF_InternGridVertTypeEqual
         module procedure ESMF_InternGridHorzStaggerEqual
         module procedure ESMF_InternGridVertStaggerEqual
         module procedure ESMF_InternGridStorageEqual
         module procedure ESMF_CoordOrderEqual
         module procedure ESMF_CoordIndexEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF InternGrid ids (enums).  It is provided for easy comparisons of 
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
         module procedure ESMF_InternGridPointerNotEqual
         module procedure ESMF_InternGridStatusNotEqual
         module procedure ESMF_InternGridStructureNotEq
         module procedure ESMF_InternGridTypeNotEqual
         module procedure ESMF_InternGridVertTypeNotEqual
         module procedure ESMF_InternGridHorzStagNotEq
         module procedure ESMF_InternGridVertStagNotEq
         module procedure ESMF_InternGridStorageNotEqual
         module procedure ESMF_CoordOrderNotEqual
         module procedure ESMF_CoordIndexNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF InternGrid ids (enums).  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOPI
      end interface
!
!==============================================================================

      contains

!==============================================================================
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridConstructNew"
!BOPI
! !IROUTINE: ESMF_InternGridConstructNew - Construct the internals of an allocated InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridConstructNew(interngrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGridClass) :: interngrid
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_InternGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_InternGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_InternGridCreate}, which calls
!     {\tt ESMF\_InternGridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_InternGrid}
!     \item[{[name]}]
!          {\tt ESMF\_InternGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: localrc                          ! Error status
      integer :: i
      !character (len = ESMF_MAXSTR) :: defaultname ! default interngrid name

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! Set the InternGrid name if present, otherwise construct a default one
      call ESMF_BaseCreate(interngrid%base, "InternGrid", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Initialize interngrid contents
      interngrid%interngridStatus      = ESMF_IGRID_STATUS_READY
      interngrid%interngridStructure   = ESMF_IGRID_STRUCTURE_UNKNOWN
      interngrid%horzInternGridType    = ESMF_IGRID_TYPE_UNKNOWN
      interngrid%vertInternGridType    = ESMF_IGRID_VERT_TYPE_UNKNOWN
      interngrid%horzStagger     = ESMF_IGRID_HORZ_STAGGER_UNKNOWN
      interngrid%vertStagger     = ESMF_IGRID_VERT_STAGGER_UNKNOWN
      interngrid%interngridStorage     = ESMF_IGRID_STORAGE_UNKNOWN
      interngrid%horzCoordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      interngrid%vertCoordSystem = ESMF_COORD_SYSTEM_UNKNOWN
      interngrid%coordOrder      = ESMF_COORD_ORDER_XYZ
      do i=1,ESMF_MAXIGRIDDIM
        interngrid%periodic(i)     = ESMF_FALSE
      !   interngrid%coversDomain(i) = ESMF_TRUE
      enddo
      interngrid%numInternDGs = 0
      interngrid%numInternDGsAlloc = 0
      nullify(interngrid%internDGs)
      interngrid%numPhysGrids = 0
      interngrid%numPhysGridsAlloc = 0
      nullify(interngrid%physgrids)
      nullify(interngrid%internDGIndex)
      ! nullify(interngrid%interngridSpecific)

      ESMF_INIT_SET_CREATED(interngrid)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridConstructNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetDELayout"
!BOP
! !IROUTINE: ESMF_InternGridGetDELayout - Get pointer to a DELayout from a InternGrid

! !INTERFACE:
      subroutine ESMF_InternGridGetDELayout(interngrid, delayout, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGrid) :: interngrid
      type(ESMF_DELayout),intent(out) :: delayout
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_InternDG} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item[delayout]
!          Pointer to the {\tt ESMF\_DELayout} corresponding to the
!          {\tt ESMF\_InternGrid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridGetInit,interngrid,rc)

      ! call InternDG method to retrieve information otherwise not available
      ! to the application level -- does not matter which one since they all share
      ! the same layout    !TODO: move layout to InternGrid class?
      call ESMF_InternDGGetDELayout(interngrid%ptr%internDGs(1)%ptr, delayout, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetDELayout

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridAddInternDG"
!BOPI
! !IROUTINE: ESMF_InternGridAddInternDG - adds a complete InternDG to InternGrid type

! !INTERFACE:
      subroutine ESMF_InternGridAddInternDG(interngrid, interndg, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGridClass), intent(inout) :: interngrid
      type(ESMF_InternDG), intent(in)    :: interndg
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine attaches an {\tt ESMF\_InternDG} object to an
!     {\tt ESMF\_InternGrid} object.  It is only meant to be called by
!     interngrid creation routines in the processes of building a interngrid.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          InternGrid structure to which new InternDG is to be added.
!     \item[interndg]
!          Complete {\tt ESMF\_InternDG} to be added.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,interngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_InternDGGetInit,interndg,rc)

      ! Update the InternDGAlloc counter and check to see if InternDG
      ! array needs to be resized to add the new interndg
      call ESMF_InternGridMakeDGSpace(interngrid, interngrid%numInternDGs+1, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      interngrid%numInternDGs = interngrid%numInternDGs + 1

      ! Add the InternDG
      interngrid%internDGs(interngrid%numInternDGs) = interndg

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridAddInternDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridMakeDGSpace"
!BOPI
! !IROUTINE: ESMF_InternGridMakeDGSpace - Allocate or extend InternDG array

! !INTERFACE:
      subroutine ESMF_InternGridMakeDGSpace(interngridp, newcount, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGridClass) :: interngridp
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
!     \item[interngridp]
!          Pointer to an {\tt ESMF\_InternGridClass}, the internal structure
!          which holds the {\tt InternGrid} information.
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
      ! in the interngrid structure, but that might not be obvious to the compiler.
      type(ESMF_InternDG), dimension(:), pointer, save :: temp_dinterngrids
      integer :: localrc                          ! Error status
      integer :: i, oldcount, alloccount

      ! Initialize return code; assume routine not implemented
      rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,interngridp,rc)

      ! number of currently used and available entries
      oldcount = interngridp%numInternDGs
      alloccount = interngridp%numInternDGsAlloc

      ! if there are already enough, we are done.
      if (alloccount .ge. newcount) then
         rc = ESMF_SUCCESS
         return
      endif

#define CHUNK 4

      ! if none are allocated yet, allocate and return.
      ! the chunksize is 4 because it is a round number in base 2.
      if (alloccount .eq. 0) then
        allocate(interngridp%internDGs(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial InternDGs", &
                                       ESMF_CONTEXT, rc)) return
        interngridp%numInternDGsAlloc = CHUNK
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
     allocate(temp_dinterngrids(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Extending internal InternGrid list", &
                                    ESMF_CONTEXT, rc)) return

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_dinterngrids(i) = interngridp%internDGs(i)
     enddo

     ! deallocate old array
     deallocate(interngridp%internDGs, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating InternDGs", &
                                    ESMF_CONTEXT, rc)) return

     ! and set original pointer to the new space
     interngridp%internDGs => temp_dinterngrids

     ! update count of how many items are currently allocated
     interngridp%numInternDGsAlloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_InternGridMakeDGSpace

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridAddPhysGrid"
!BOPI
! !IROUTINE: ESMF_InternGridAddPhysGrid - adds a complete PhysGrid to InternGrid type

! !INTERFACE:
      subroutine ESMF_InternGridAddPhysGrid(interngrid, physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGridClass), intent(inout) :: interngrid
      type(ESMF_PhysGrid), intent(in)    :: physgrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine attaches an {\tt ESMF\_PhysGrid} object to an
!     {\tt ESMF\_InternGrid} object.  It is only meant to be called by
!     interngrid creation routines in the processes of building a interngrid.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          InternGrid structure to which new PhysGrid is to be added.
!     \item[physgrid]
!          Complete {\tt ESMF\_PhysGrid} to be added.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,interngrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_PhysGridGetInit,physgrid,rc)

      ! Update the PhysGridAlloc counter and check to see if PhysGrid
      ! array needs to be resized to add the new physgrid
      call ESMF_InternGridMakePGSpace(interngrid, interngrid%numPhysGrids+1, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      interngrid%numPhysGrids = interngrid%numPhysGrids + 1

      ! Add the PhysGrid
      interngrid%physgrids(interngrid%numPhysGrids) = physgrid

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridAddPhysGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridMakePGSpace"
!BOPI
! !IROUTINE: ESMF_InternGridMakePGSpace - Allocate or extend PhysGrid array

! !INTERFACE:
      subroutine ESMF_InternGridMakePGSpace(interngridp, newcount, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGridClass) :: interngridp
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
!     \item[interngridp]
!          Pointer to an {\tt ESMF\_InternGridClass}, the internal structure
!          which holds the {\tt InternGrid} information.
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
      ! in the interngrid structure, but that might not be obvious to the compiler.
      type(ESMF_PhysGrid), dimension(:), pointer, save :: temp_pinterngrids
      integer, dimension(:), pointer, save :: temp_dgIndex
      integer :: localrc                          ! Error status
      integer :: i, oldcount, alloccount

      ! Initialize return code; assume routine not implemented
      rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,interngridp,rc)

      ! number of currently used and available entries
      oldcount = interngridp%numPhysGrids
      alloccount = interngridp%numPhysGridsAlloc

      ! if there are already enough, we are done.
      if (alloccount .ge. newcount) then
         rc = ESMF_SUCCESS
         return
      endif

#define CHUNK 4

      ! if none are allocated yet, allocate and return.
      ! the chunksize is 4 because it is a round number in base 2.
      if (alloccount .eq. 0) then
        allocate(interngridp%physgrids(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial physgrids", &
                                       ESMF_CONTEXT, rc)) return

        allocate(interngridp%internDGIndex(CHUNK), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "Allocating initial InternDGIndex", &
                                       ESMF_CONTEXT, rc)) return

        interngridp%numPhysGridsAlloc = CHUNK
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
     allocate(temp_pinterngrids(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Allocating temp_pinterngrids", &
                                    ESMF_CONTEXT, rc)) return

     allocate(temp_dgIndex(alloccount), stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Allocating temp_dgIndex", &
                                    ESMF_CONTEXT, rc)) return

     ! copy old contents over (note use of = and not => )
     do i = 1, oldcount
       temp_pinterngrids(i)  = interngridp%physgrids(i)
       temp_dgIndex(i) = interngridp%internDGIndex(i)
     enddo

     ! deallocate old arrays
     deallocate(interngridp%physgrids, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating physgrids", &
                                    ESMF_CONTEXT, rc)) return

     deallocate(interngridp%internDGIndex, stat=localrc)
     if (ESMF_LogMsgFoundAllocError(localrc, &
                                    "Deallocating InternDGIndex", &
                                    ESMF_CONTEXT, rc)) return

     ! and set original pointers to the new space
     interngridp%physgrids => temp_pinterngrids
     interngridp%internDGIndex => temp_dgIndex

     ! update count of how many items are currently allocated
     interngridp%numPhysGridsAlloc = alloccount

     rc = ESMF_SUCCESS

     end subroutine ESMF_InternGridMakePGSpace

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetPhysGrid"
!BOPI
! !IROUTINE: ESMF_InternGridGetPhysGrid - retrieves complete PhysGrid from InternGrid type

! !INTERFACE:
      subroutine ESMF_InternGridGetPhysGrid(physgrid, interngrid, relloc, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(out) :: physgrid
      type(ESMF_InternGridClass), intent(inout)  :: interngrid
      type(ESMF_RelLoc), intent(in), optional :: relloc
      character(*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine retrieves an {\tt ESMF\_PhysGrid} object from an
!     {\tt ESMF\_InternGrid} object.  A PhysGrid can be retrieved either
!     by name or by relative location.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} to be retrieved.
!     \item[interngrid]
!          InternGrid structure from which PhysGrid is to be extracted.
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

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,interngrid,rc)

      ! If name supplied, search by name and return selected PhysGrid
      if (present(name)) then
        found = .false.
        name_search: do n = 1,interngrid%numPhysGridsAlloc
          call ESMF_PhysGridGet(interngrid%physgrids(n), name=nameTmp, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (name == nameTmp) then
            physgrid = interngrid%physgrids(n)
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
        relloc_search: do n = 1,interngrid%numPhysGridsAlloc
          call ESMF_PhysGridGet(interngrid%physgrids(n), relloc=rellocTmp, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
          if (relloc == rellocTmp) then
            physgrid = interngrid%physgrids(n)
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

      end subroutine ESMF_InternGridGetPhysGrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetPhysGridID"
!BOPI
! !IROUTINE: ESMF_InternGridGetPhysGridID - Get PhysGrid Id for a given relative location

! !INTERFACE:
      subroutine ESMF_InternGridGetPhysGridID(interngrid, relloc, physInternGridId, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGridClass), intent(inout) :: interngrid
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(out) :: physInternGridId
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Return the {\tt ESMF\_PhysGridId} associated with the given relative
!     location.  Return error if the interngrid contains no PhysGrid at the
!     specified location.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Class to be queried.
!     \item[relloc]
!          Relative location to query
!     \item[physInternGridId]
!          Returned physInternGrid identifier.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: localrc                          ! Error status
      type(ESMF_RelLoc) :: thisRelloc
      integer :: i

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,interngrid,rc)

      physInternGridId = -1

      ! Loop through physgrids comparing rellocs  TODO: make part of the InternGrid obj?
      do i = 1,interngrid%numPhysGrids
        call ESMF_PhysGridGet(interngrid%physgrids(i), relloc=thisRelloc, &
                              rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
        if (relloc.eq.thisRelloc) then
          physInternGridId = i
          localrc = ESMF_SUCCESS
          exit
        endif
      enddo

      ! print error if the relloc is not found
      if (physInternGridId.eq.-1) then
         if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                   "relloc not valid", &
                                   ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = localrc

      end subroutine ESMF_InternGridGetPhysGridID

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetInternDG"
!BOPI
! !IROUTINE: ESMF_InternGridGetInternDG - retrieves complete InternDG from InternGrid type

! !INTERFACE:
      subroutine ESMF_InternGridGetInternDG(interndg, interngrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_InternDG), intent(out) :: interndg
      type(ESMF_InternGridClass), intent(in)  :: interngrid
      character(*), intent(in) :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This routine retrieves an {\tt ESMF\_InternDG} object from an
!     {\tt ESMF\_InternGrid} object, given the name of the interndg.
!
!     The arguments are:
!     \begin{description}
!     \item[interndg]
!          {\tt ESMF\_InternDG} to be retrieved.
!     \item[interngrid]
!          InternGrid structure from which interndg is to be extracted.
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

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,interngrid,rc)

      ! Search by name and return selected InternDG
      found = .false.
      name_search: do n=1,interngrid%numInternDGsAlloc
         call ESMF_InternDGGet(interngrid%internDGs(n), name=nameTmp, rc=localrc)
         if (ESMF_LogMsgFoundError(localrc, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rc)) return
         if (name == nameTmp) then
            interndg = interngrid%internDGs(n)
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

      end subroutine ESMF_InternGridGetInternDG

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetBoundingBoxes"
!BOPI
! !IROUTINE: ESMF_InternGridGetBoundingBoxes - Get the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_InternGridGetBoundingBoxes(interngrid, array, rc)
!
! !ARGUMENTS:
      type(ESMF_InternGridClass), intent(in) :: interngrid
      type(ESMF_LocalArray), intent(out) :: array !BOB changed to just out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the region identifier data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid]
!          Pointer to a {\tt ESMF\_InternGrid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      !integer :: localrc                          ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,interngrid,rc)

      array = interngrid%boundingBoxes

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_InternGridGetBoundingBoxes

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridPointerEqual"
!BOPI
! !IROUTINE: ESMF_InternGridPointerEqual - equality of InternGrids
!
! !INTERFACE:
      function ESMF_InternGridPointerEqual(InternGrid1, InternGrid2)

! !RETURN VALUE:
      logical :: ESMF_InternGridPointerEqual

! !ARGUMENTS:

      type (ESMF_InternGrid), intent(in) :: &
         InternGrid1,      &! Two interngrids to compare for
         InternGrid2        ! equality (identity)

! !DESCRIPTION:
!     This routine compares two ESMF InternGrids to see if
!     they have equivalent pointers to the same internal ESMF_InternGridClass.
!     This will return false if the pointers are different, even if
!     the interngrids describe exactly the same physical dimensions.  It is a
!     quick and dirty check.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGrid1, InternGrid2]
!          Two interngrids to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridPointerEqual = Associated(InternGrid1%ptr, InternGrid2%ptr)

      end function ESMF_InternGridPointerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridStatusEqual"
!BOPI
! !IROUTINE: ESMF_InternGridStatusEqual - equality of InternGrid statuses
!
! !INTERFACE:
      function ESMF_InternGridStatusEqual(InternGridStatus1, InternGridStatus2)

! !RETURN VALUE:
      logical :: ESMF_InternGridStatusEqual

! !ARGUMENTS:

      type (ESMF_InternGridStatus), intent(in) :: &
         InternGridStatus1,      &! Two interngrid statuses to compare for
         InternGridStatus2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid statuses to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridStatus1, InternGridStatus2]
!          Two interngrid statuses to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridStatusEqual = (InternGridStatus1%interngridStatus == &
                              InternGridStatus2%interngridStatus)

      end function ESMF_InternGridStatusEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridStructureEqual"
!BOPI
! !IROUTINE: ESMF_InternGridStructureEqual - equality of InternGrid structures
!
! !INTERFACE:
      function ESMF_InternGridStructureEqual(InternGridStructure1, InternGridStructure2)

! !RETURN VALUE:
      logical :: ESMF_InternGridStructureEqual

! !ARGUMENTS:

      type (ESMF_InternGridStructure), intent(in) :: &
         InternGridStructure1,      &! Two interngrid structures to compare for
         InternGridStructure2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid structures to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridStructure1, InternGridStructure2]
!          Two region types to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridStructureEqual = (InternGridStructure1%interngridStructure == &
                                 InternGridStructure2%interngridStructure)

      end function ESMF_InternGridStructureEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridTypeEqual"
!BOPI
! !IROUTINE: ESMF_InternGridTypeEqual - equality of InternGrid types
!
! !INTERFACE:
      function ESMF_InternGridTypeEqual(InternGridType1, InternGridType2)

! !RETURN VALUE:
      logical :: ESMF_InternGridTypeEqual

! !ARGUMENTS:

      type (ESMF_InternGridType), intent(in) :: &
         InternGridType1,      &! Two interngrid kinds to compare for
         InternGridType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid kinds to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridType1, InternGridType2]
!          Two region types to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridTypeEqual = (InternGridType1%interngridType == &
                            InternGridType2%interngridType)

      end function ESMF_InternGridTypeEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridVertTypeEqual"
!BOPI
! !IROUTINE: ESMF_InternGridVertTypeEqual - equality of vertical InternGrid types
!
! !INTERFACE:
      function ESMF_InternGridVertTypeEqual(InternGridVertType1, InternGridVertType2)

! !RETURN VALUE:
      logical :: ESMF_InternGridVertTypeEqual

! !ARGUMENTS:

      type (ESMF_InternGridVertType), intent(in) :: &
         InternGridVertType1,      &! Two vertical interngrid kinds to compare for
         InternGridVertType2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF vertical InternGrid kinds to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridVertType1, InternGridVertType2]
!          Two vertical interngrid types to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridVertTypeEqual = (InternGridVertType1%interngridVertType == &
                            InternGridVertType2%interngridVertType)

      end function ESMF_InternGridVertTypeEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridHorzStaggerEqual"
!BOPI
! !IROUTINE: ESMF_InternGridHorzStaggerEqual - equality of InternGrid horizontal staggerings
!
! !INTERFACE:
      function ESMF_InternGridHorzStaggerEqual(InternGridHorzStagger1, InternGridHorzStagger2)

! !RETURN VALUE:
      logical :: ESMF_InternGridHorzStaggerEqual

! !ARGUMENTS:

      type (ESMF_InternGridHorzStagger), intent(in) :: &
         InternGridHorzStagger1,      &! Two interngrid horz staggerings to compare for
         InternGridHorzStagger2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid horizontal staggerings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridHorzStagger1, InternGridHorzStagger2]
!          Two interngrid horizontal staggerings to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridHorzStaggerEqual = (InternGridHorzStagger1%stagger == &
                                   InternGridHorzStagger2%stagger)

      end function ESMF_InternGridHorzStaggerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridVertStaggerEqual"
!BOPI
! !IROUTINE: ESMF_InternGridVertStaggerEqual - equality of InternGrid vertical staggerings
!
! !INTERFACE:
      function ESMF_InternGridVertStaggerEqual(InternGridVertStagger1, InternGridVertStagger2)

! !RETURN VALUE:
      logical :: ESMF_InternGridVertStaggerEqual

! !ARGUMENTS:

      type (ESMF_InternGridVertStagger), intent(in) :: &
         InternGridVertStagger1,      &! Two interngrid vert staggerings to compare for
         InternGridVertStagger2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid vertical staggerings to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridVertStagger1, InternGridVertStagger2]
!          Two interngrid vertical staggerings to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridVertStaggerEqual = (InternGridVertStagger1%stagger == &
                                   InternGridVertStagger2%stagger)

      end function ESMF_InternGridVertStaggerEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridStorageEqual"
!BOPI
! !IROUTINE: ESMF_InternGridStorageEqual - equality of InternGrid storage schemes
!
! !INTERFACE:
      function ESMF_InternGridStorageEqual(InternGridStorage1, InternGridStorage2)

! !RETURN VALUE:
      logical :: ESMF_InternGridStorageEqual

! !ARGUMENTS:

      type (ESMF_InternGridStorage), intent(in) :: &
         InternGridStorage1,      &! Two interngrid storage schemes to compare for
         InternGridStorage2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid storage schemes to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridStorage1, InternGridStorage2]
!          Two interngrid storage schemes to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridStorageEqual = (InternGridStorage1%storage == &
                               InternGridStorage2%storage)

      end function ESMF_InternGridStorageEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordOrderEqual"
!BOPI
! !IROUTINE: ESMF_CoordOrderEqual - equality of InternGrid coordinate orders
!
! !INTERFACE:
      function ESMF_CoordOrderEqual(CoordOrder1, CoordOrder2)

! !RETURN VALUE:
      logical :: ESMF_CoordOrderEqual

! !ARGUMENTS:

      type (ESMF_CoordOrder), intent(in) :: &
         CoordOrder1,      &! Two interngrid coordinate orders to compare for
         CoordOrder2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid coordinate orderings to see if
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
! !IROUTINE: ESMF_CoordIndexEqual - equality of InternGrid coordinate indexing
!
! !INTERFACE:
      function ESMF_CoordIndexEqual(CoordIndex1, CoordIndex2)

! !RETURN VALUE:
      logical :: ESMF_CoordIndexEqual

! !ARGUMENTS:

      type (ESMF_CoordIndex), intent(in) :: &
         CoordIndex1,      &! Two interngrid coordinate indexings to compare
         CoordIndex2        ! for equality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid coordinate indexings to see if
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
#define ESMF_METHOD "ESMF_InternGridPointerNotEqual"
!BOPI
! !IROUTINE: ESMF_InternGridPointerNotEqual - equality of InternGrids
!
! !INTERFACE:
      function ESMF_InternGridPointerNotEqual(InternGrid1, InternGrid2)

! !RETURN VALUE:
      logical :: ESMF_InternGridPointerNotEqual

! !ARGUMENTS:

      type (ESMF_InternGrid), intent(in) :: &
         InternGrid1,      &! Two interngrids to compare for
         InternGrid2        ! inequality (not identical)

! !DESCRIPTION:
!     This routine compares two ESMF InternGrids to see if
!     they have pointers to different internal ESMF_InternGridClasses.
!     This will return true if the pointers are different, even if
!     the interngrids describe exactly the same physical dimensions.  It is a
!     quick and dirty check.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGrid1, InternGrid2]
!          Two interngrids to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridPointerNotEqual = .not.Associated(InternGrid1%ptr, InternGrid2%ptr)

      end function ESMF_InternGridPointerNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridStatusNotEqual"
!BOPI
! !IROUTINE: ESMF_InternGridStatusNotEqual - non-equality of InternGrid statuses
!
! !INTERFACE:
      function ESMF_InternGridStatusNotEqual(InternGridStatus1, InternGridStatus2)

! !RETURN VALUE:
      logical :: ESMF_InternGridStatusNotEqual

! !ARGUMENTS:

      type (ESMF_InternGridStatus), intent(in) :: &
         InternGridStatus1,      &! Two InternGrid Statuses to compare for
         InternGridStatus2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid statuses to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridStatus1, InternGridStatus2]
!          Two statuses of InternGrids to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridStatusNotEqual = (InternGridStatus1%interngridStatus /= &
                                 InternGridStatus2%interngridStatus)

      end function ESMF_InternGridStatusNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridStructureNotEq"
!BOPI
! !IROUTINE: ESMF_InternGridStructureNotEq - non-equality of InternGrid structures
!
! !INTERFACE:
      function ESMF_InternGridStructureNotEq(InternGridStructure1, InternGridStructure2)

! !RETURN VALUE:
      logical :: ESMF_InternGridStructureNotEq

! !ARGUMENTS:

      type (ESMF_InternGridStructure), intent(in) :: &
         InternGridStructure1,      &! Two InternGrid Structures to compare for
         InternGridStructure2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid structures to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridStructure1, InternGridStructure2]
!          Two structures of InternGrids to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridStructureNotEq = (InternGridStructure1%interngridStructure /= &
                                    InternGridStructure2%interngridStructure)

      end function ESMF_InternGridStructureNotEq

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_InternGridTypeNotEqual - non-equality of InternGrid kinds
!
! !INTERFACE:
      function ESMF_InternGridTypeNotEqual(InternGridType1, InternGridType2)

! !RETURN VALUE:
      logical :: ESMF_InternGridTypeNotEqual

! !ARGUMENTS:

      type (ESMF_InternGridType), intent(in) :: &
         InternGridType1,      &! Two InternGrid kinds to compare for
         InternGridType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid kinds to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridType1, InternGridType2]
!          Two kinds of InternGrids to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_InternGridTypeNotEqual = (InternGridType1%interngridType /= &
                               InternGridType2%interngridType)

      end function ESMF_InternGridTypeNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridVertTypeNotEqual"
!BOPI
! !IROUTINE: ESMF_InternGridVertTypeNotEqual - non-equality of vertical InternGrid kinds
!
! !INTERFACE:
      function ESMF_InternGridVertTypeNotEqual(InternGridVertType1, InternGridVertType2)

! !RETURN VALUE:
      logical :: ESMF_InternGridVertTypeNotEqual

! !ARGUMENTS:

      type (ESMF_InternGridVertType), intent(in) :: &
         InternGridVertType1,      &! Two vertical InternGrid kinds to compare for
         InternGridVertType2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF vertical InternGrid kinds to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridVertType1, InternGridVertType2]
!          Two kinds of vertical InternGrids to compare for inequality
!     \end{description}
!
!EOPI

      ESMF_InternGridVertTypeNotEqual = (InternGridVertType1%interngridVertType /= &
                                   InternGridVertType2%interngridVertType)

      end function ESMF_InternGridVertTypeNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridHorzStagNotEq"
!BOPI
! !IROUTINE: ESMF_InternGridHorzStagNotEq - inequality of InternGrid horizontal staggerings
!
! !INTERFACE:
      function ESMF_InternGridHorzStagNotEq(InternGridHorzStagger1, InternGridHorzStagger2)

! !RETURN VALUE:
      logical :: ESMF_InternGridHorzStagNotEq

! !ARGUMENTS:

      type (ESMF_InternGridHorzStagger), intent(in) :: &
         InternGridHorzStagger1,      &! Two interngrid horizontal staggerings to compare for
         InternGridHorzStagger2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid horizontal staggerings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridHorzStagger1, InternGridHorzStagger2]
!          Two interngrid horizontal staggerings to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridHorzStagNotEq = (InternGridHorzStagger1%stagger /= &
                                      InternGridHorzStagger2%stagger)

      end function ESMF_InternGridHorzStagNotEq

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridVertStagNotEq"
!BOPI
! !IROUTINE: ESMF_InternGridVertStagNotEq - inequality of InternGrid vertical staggerings
!
! !INTERFACE:
      function ESMF_InternGridVertStagNotEq(InternGridVertStagger1, InternGridVertStagger2)

! !RETURN VALUE:
      logical :: ESMF_InternGridVertStagNotEq

! !ARGUMENTS:

      type (ESMF_InternGridVertStagger), intent(in) :: &
         InternGridVertStagger1,      &! Two interngrid vertical staggerings to compare for
         InternGridVertStagger2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid vertical staggerings to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridVertStagger1, InternGridVertStagger2]
!          Two interngrid vertical staggerings to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridVertStagNotEq = (InternGridVertStagger1%stagger /= &
                                      InternGridVertStagger2%stagger)

      end function ESMF_InternGridVertStagNotEq

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridStorageNotEqual"
!BOPI
! !IROUTINE: ESMF_InternGridStorageNotEqual - inequality of InternGrid storage schemes
!
! !INTERFACE:
      function ESMF_InternGridStorageNotEqual(InternGridStorage1, InternGridStorage2)

! !RETURN VALUE:
      logical :: ESMF_InternGridStorageNotEqual

! !ARGUMENTS:

      type (ESMF_InternGridStorage), intent(in) :: &
         InternGridStorage1,      &! Two interngrid storage schemes to compare for
         InternGridStorage2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid storage schemes to see if
!     they are not equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[InternGridStorage1, InternGridStorage2]
!          Two interngrid storage schemes to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_InternGridStorageNotEqual = (InternGridStorage1%storage /= &
                                  InternGridStorage2%storage)

      end function ESMF_InternGridStorageNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_CoordOrderNotEqual"
!BOPI
! !IROUTINE: ESMF_CoordOrderNotEqual - inequality of InternGrid coordinate orders
!
! !INTERFACE:
      function ESMF_CoordOrderNotEqual(CoordOrder1, CoordOrder2)

! !RETURN VALUE:
      logical :: ESMF_CoordOrderNotEqual

! !ARGUMENTS:

      type (ESMF_CoordOrder), intent(in) :: &
         CoordOrder1,      &! Two interngrid coordinate orders to compare for
         CoordOrder2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid coordinate orderings to see if
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
! !IROUTINE: ESMF_CoordIndexNotEqual - inequality of InternGrid coordinate indexing
!
! !INTERFACE:
      function ESMF_CoordIndexNotEqual(CoordIndex1, CoordIndex2)

! !RETURN VALUE:
      logical :: ESMF_CoordIndexNotEqual

! !ARGUMENTS:

      type (ESMF_CoordIndex), intent(in) :: &
         CoordIndex1,      &! Two interngrid coordinate indexings to compare
         CoordIndex2        ! for inequality

! !DESCRIPTION:
!     This routine compares two ESMF InternGrid coordinate indexings to see if
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
#define ESMF_METHOD "ESMF_LogRectInternGridValidate()"
!BOP
! !IROUTINE: ESMF_LogRectInternGridValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_LogRectInternGridValidate(lrg, rc)
!
! !ARGUMENTS:
    type(ESMF_LogRectInternGrid), intent(inout)              :: lrg
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
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_LogRectInternGridGetInit, ESMF_LogRectInternGridInit,lrg)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_LogRectInternGridValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRectInternGridInit()"
!BOPI
! !IROUTINE: ESMF_LogRectInternGridInit - Init DataHolder internals

! !INTERFACE:
  subroutine ESMF_LogRectInternGridInit(lrg)
!
! !ARGUMENTS:
    type(ESMF_LogRectInternGrid), intent(inout)              :: lrg
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
  end subroutine ESMF_LogRectInternGridInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LogRectInternGridGetInit"
!BOPI
! !IROUTINE: ESMF_LogRectInternGridGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_LogRectInternGridGetInit(lrg) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_LogRectInternGridGetInit   
!
! !ARGUMENTS:
      type(ESMF_LogRectInternGrid), intent(in), optional :: lrg
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
      ESMF_LogRectInternGridGetInit = ESMF_INIT_GET(lrg)
    else
      ESMF_LogRectInternGridGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_LogRectInternGridGetInit
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSpecificValidate()"
!BOPI
! !IROUTINE: ESMF_InternGridSpecificValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_InternGridSpecificValidate(gs, rc)
!
! !ARGUMENTS:
    type(ESMF_InternGridSpecific), intent(inout)              :: gs
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_InternGridSpecificGetInit, ESMF_InternGridSpecificInit,gs)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_InternGridSpecificValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSpecificInit()"
!BOPI
! !IROUTINE: ESMF_InternGridSpecificInit - Init DataHolder internals

! !INTERFACE:
  subroutine ESMF_InternGridSpecificInit(gs)
!
! !ARGUMENTS:
    type(ESMF_InternGridSpecific), intent(inout)              :: gs
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
  end subroutine ESMF_InternGridSpecificInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSpecificGetInit"
!BOPI
! !IROUTINE: ESMF_InternGridSpecificGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_InternGridSpecificGetInit(gs) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_InternGridSpecificGetInit   
!
! !ARGUMENTS:
      type(ESMF_InternGridSpecific), intent(in), optional :: gs
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
      ESMF_InternGridSpecificGetInit = ESMF_INIT_GET(gs)
    else
      ESMF_InternGridSpecificGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_InternGridSpecificGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridClassValidate()"
!BOPI
! !IROUTINE: ESMF_InternGridClassValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_InternGridClassValidate(gc, rc)
!
! !ARGUMENTS:
    type(ESMF_InternGridClass), intent(in)              :: gc
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_InternGridClassGetInit,gc,rc)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_InternGridClassValidate
!------------------------------------------------------------------------------

! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridClassGetInit"
!BOPI
! !IROUTINE: ESMF_InternGridClassGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_InternGridClassGetInit(gc) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_InternGridClassGetInit   
!
! !ARGUMENTS:
      type(ESMF_InternGridClass), intent(in), optional :: gc
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
      ESMF_InternGridClassGetInit = ESMF_INIT_GET(gc)
    else
      ESMF_InternGridClassGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_InternGridClassGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_InternGridSetInitCreated - Set InternGrid init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_InternGridSetInitCreated(interngrid, rc)
!
! !ARGUMENTS:
    type(ESMF_InternGrid), intent(inout)           :: interngrid
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in InternGrid object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[interngrid] 
!          Specified {\tt ESMF\_InternGrid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(interngrid)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_InternGridSetInitCreated



! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InternGridGetInit"
!BOPI
! !IROUTINE: ESMF_InternGridGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_InternGridGetInit(g) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_InternGridGetInit   
!
! !ARGUMENTS:
      type(ESMF_InternGrid), intent(in), optional :: g
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
      ESMF_InternGridGetInit = ESMF_INIT_GET(g)
    else
      ESMF_InternGridGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_InternGridGetInit
!------------------------------------------------------------------------------



      end module ESMF_InternGridTypesMod
