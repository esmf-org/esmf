! $Id: ESMF_GridTypes.F90,v 1.3 2004/01/07 23:01:12 jwolfe Exp $
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
#include "ESMF_Grid.h"
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
!     ! ESMF_GridKind
!
!     ! Type to specify kind of grid for supported ESMF Grids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_GridKind
      sequence
      private
        integer :: gridkind
      end type

!------------------------------------------------------------------------------
!     ! ESMF_GridStagger
!
!     ! Type to specify type of grid staggering for supported ESMF Grids.
!     !  See the public parameters declared below for the possible valid
!     !  values for this.

      type ESMF_GridStagger
      sequence
      private
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
      private
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
      private
        integer :: index
      end type

!------------------------------------------------------------------------------
!     !  ESMF_GridType
!
!     ! Definition for the Grid class.

      type ESMF_GridType
      sequence
      private

        type (ESMF_Base) :: base               ! base class object
        type (ESMF_Status) :: gridstatus       ! uninitialized, init ok, etc
        type (ESMF_GridKind) :: horzGridKind   ! enum for type of horizontal grid
        type (ESMF_GridKind) :: vertGridKind   ! enum for type of vertical grid
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
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: globalMinCoord
        real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: globalMaxCoord
        type (ESMF_LocalArray) :: boundingBoxes
                                               ! array of bounding boxes on each
                                               ! DE - used for search routines
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

      public ESMF_GridType, ESMF_GridKind, ESMF_GridStagger
      public ESMF_CoordOrder, ESMF_CoordIndex, ESMF_Grid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    ! These functions are generally accessed only by the main Grid module
    ! and are not meant to be accessed by the user.  Well...except for
    ! the overloaded operators.
    
    public ESMF_GridConstructNew
    public ESMF_GridDestruct
    public ESMF_GridSetAttributes
    public ESMF_GridGetAttributes
    public ESMF_GridAddPhysGrid
    public ESMF_GridGetPhysGrid
    public ESMF_GridAddDistGrid
    public ESMF_GridGetDistGrid
    public ESMF_GridPrint

    public operator(==), operator(/=) ! for overloading 
                                      ! comparison functions

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

  ! Supported ESMF grid kinds:
  !   ESMF_GridKind_Unknown           ! unknown or undefined grid
  !   ESMF_GridKind_LatLon            ! aligned with longitude,latitude
  !   ESMF_GridKind_Dipole            ! Displaced-pole dipole grid
  !   ESMF_GridKind_Tripole           ! Tripolar grids
  !   ESMF_GridKind_XY                ! aligned with Cartesian x-y coords
  !   ESMF_GridKind_DataStream        ! Data stream - set of locations
  !   ESMF_GridKind_PhysFourier       ! Mixed Fourier/Phys Space grid
  !   ESMF_GridKind_Reduced           ! reduced grid
  !   ESMF_GridKind_SphericalSpectral ! spectral space:spherical harmonics
  !   ESMF_GridKind_CartSpectral      ! spectral space:Cartesian coords
  !   ESMF_GridKind_Geodesic          ! spherical geodesic grid
  !   ESMF_GridKind_CubedSphere       ! cubed sphere grid
  !   ESMF_GridKind_Exchange          ! intersection of two grids

   type (ESMF_GridKind), parameter, public ::              &
      ESMF_GridKind_Unknown           = ESMF_GridKind( 0), &
      ESMF_GridKind_LatLon            = ESMF_GridKind( 1), &
      ESMF_GridKind_Dipole            = ESMF_GridKind( 3), &
      ESMF_GridKind_Tripole           = ESMF_GridKind( 4), &
      ESMF_GridKind_XY                = ESMF_GridKind( 5), &
      ESMF_GridKind_DataStream        = ESMF_GridKind( 6), &
      ESMF_GridKind_PhysFourier       = ESMF_GridKind( 7), &
      ESMF_GridKind_Reduced           = ESMF_GridKind( 9), &
      ESMF_GridKind_SphericalSpectral = ESMF_GridKind(10), &
      ESMF_GridKind_CartSpectral      = ESMF_GridKind(11), &
      ESMF_GridKind_Geodesic          = ESMF_GridKind(12), &
      ESMF_GridKind_CubedSphere       = ESMF_GridKind(13), &
      ESMF_GridKind_Exchange          = ESMF_GridKind(14)

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
   !   ESMF_GridStagger_D_NE       ! Arakawa C (V at E face, U at N face)
   !   ESMF_GridStagger_D_SW       ! Arakawa C (V at W face, U at S face)
   !   ESMF_GridStagger_D_SE       ! Arakawa C (V at E face, U at S face)
   !   ESMF_GridStagger_D_NW       ! Arakawa C (V at W face, U at N face)
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

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_GridTypes.F90,v 1.3 2004/01/07 23:01:12 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridKindEqual
         module procedure ESMF_GridStaggerEqual
         module procedure ESMF_CoordOrderEqual
         module procedure ESMF_CoordIndexEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF Grid ids (enums).  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridKindNotEqual
         module procedure ESMF_GridStaggerNotEqual
         module procedure ESMF_CoordOrderNotEqual
         module procedure ESMF_CoordIndexNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF Grid ids (enums).  It is provided for easy comparisons of 
!     these types with defined values.
!
!EOP
      end interface
!
!==============================================================================

      contains

!==============================================================================
!BOP
! !IROUTINE: ESMF_GridConstructNew - Create new Grid and initialize attributes

! !INTERFACE:
      function ESMF_GridConstructNew(name,                      &
                              horzGridKind,    vertGridKind,    &
                              horzStagger,     vertStagger,     &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder,      coordIndex,      &
                              numPhysGrids,    numDistGrids,    &
                              periodic,                         &
                              globalMinCoord,  globalMaxCoord,  rc)
!
! !RETURN VALUE:
      type(ESMF_GridType) :: ESMF_GridConstructNew
!
! !ARGUMENTS:

      character (len=*),       intent(in), optional :: name
      type (ESMF_GridKind),    intent(in), optional :: horzGridKind
      type (ESMF_GridKind),    intent(in), optional :: vertGridKind
      type (ESMF_GridStagger), intent(in), optional :: horzStagger
      type (ESMF_GridStagger), intent(in), optional :: vertStagger
      type (ESMF_CoordSystem), intent(in), optional :: horzCoordSystem  
      type (ESMF_CoordSystem), intent(in), optional :: vertCoordSystem  
      type (ESMF_CoordOrder),  intent(in), optional :: coordOrder
      type (ESMF_CoordIndex),  intent(in), optional :: coordIndex
      integer,                 intent(in), optional :: numPhysGrids
      integer,                 intent(in), optional :: numDistGrids

      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: globalMinCoord
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: globalMaxCoord
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic

      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!
!     Creates a new {\tt ESMF\_GridType} object and constructs its
!     internals.  It will also optionally initialize several general 
!     attributes of the grid.  The new GridType structure is returned with 
!     the selected attributes initialized.  Any attributes not initialized 
!     may be set later using GridSet routines.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          Character name to be assigned to this grid.
!     \item[{[horzGridKind]}]
!          {\tt ESMF\_GridKind} describing the horizontal grid.
!     \item[{[vertGridKind]}]
!          {\tt ESMF\_GridKind} describing the vertical   grid.
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
!     \item[{[globalMinCoord]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[globalMaxCoord]}]
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

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Set the Grid name if present, otherwise construct a default one
      if (present(name)) then
         call ESMF_SetName(ESMF_GridConstructNew%base, name, "Grid", status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructNew: Setname"
            return
         endif
      endif

!     if present, set information filling in grid derived type
      if(present(horzGridKind   )) ESMF_GridConstructNew%horzGridKind    = horzGridKind 
      if(present(vertGridKind   )) ESMF_GridConstructNew%vertGridKind    = vertGridKind 
      if(present(horzStagger    )) ESMF_GridConstructNew%horzStagger     = horzStagger 
      if(present(vertStagger    )) ESMF_GridConstructNew%vertStagger     = vertStagger 
      if(present(horzCoordSystem)) ESMF_GridConstructNew%horzCoordSystem = horzCoordSystem  
      if(present(vertCoordSystem)) ESMF_GridConstructNew%vertCoordSystem = vertCoordSystem  
      if(present(coordOrder     )) ESMF_GridConstructNew%coordOrder      = coordOrder 
      if(present(coordIndex     )) ESMF_GridConstructNew%coordIndex      = coordIndex 
      if(present(numPhysGrids   )) ESMF_GridConstructNew%numPhysGrids    = numPhysGrids
      if(present(numDistGrids   )) ESMF_GridConstructNew%numDistGrids    = numDistGrids
      
!     Set periodic flags for each dimension
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            ESMF_GridConstructNew%periodic(i) = periodic(i)
         enddo
      endif

!     Set global domain limits
      if (present(globalMinCoord)) then
         if (size(globalMinCoord) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridConstructNew: Error - globalMinCoord too big'
            return
         endif
         do i=1,size(globalMinCoord)
            ESMF_GridConstructNew%globalMinCoord(i) = globalMinCoord(i)
         enddo
      endif
      if (present(globalMaxCoord)) then
         if (size(globalMaxCoord) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridConstructNew: Error - globalMaxCoord too big'
            return
         endif
         do i=1,size(globalMaxCoord)
            ESMF_GridConstructNew%globalMaxCoord(i) = globalMaxCoord(i)
         enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS


!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetAttributes - sets some attributes of Grid

! !INTERFACE:
      subroutine ESMF_GridSetAttributes(grid,  name,            &
                              horzGridKind,    vertGridKind,    &
                              horzStagger,     vertStagger,     &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder,      coordIndex,      &
                              numPhysGrids,    numDistGrids,    &
                              periodic,                         &
                              globalMinCoord,  globalMaxCoord,  rc)

!
! !ARGUMENTS:

      type(ESMF_Grid), intent(inout) :: grid

      character (len=*),       intent(in), optional :: name
      type (ESMF_GridKind),    intent(in), optional :: horzGridKind
      type (ESMF_GridKind),    intent(in), optional :: vertGridKind
      type (ESMF_GridStagger), intent(in), optional :: horzStagger
      type (ESMF_GridStagger), intent(in), optional :: vertStagger
      type (ESMF_CoordSystem), intent(in), optional :: horzCoordSystem  
      type (ESMF_CoordSystem), intent(in), optional :: vertCoordSystem  
      type (ESMF_CoordOrder),  intent(in), optional :: coordOrder
      type (ESMF_CoordIndex),  intent(in), optional :: coordIndex
      integer,                 intent(in), optional :: numPhysGrids
      integer,                 intent(in), optional :: numDistGrids

      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: globalMinCoord
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: globalMaxCoord
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
!     \item[{[horzGridKind]}]
!          {\tt ESMF\_GridKind} describing the horizontal grid.
!     \item[{[vertGridKind]}]
!          {\tt ESMF\_GridKind} describing the vertical   grid.
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
!     \item[{[globalMinCoord]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[globalMaxCoord]}]
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

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     if present, set information filling in grid derived type
      if(present(horzGridKind   )) grid%ptr%horzGridKind    = horzGridKind 
      if(present(vertGridKind   )) grid%ptr%vertGridKind    = vertGridKind 
      if(present(horzStagger    )) grid%ptr%horzStagger     = horzStagger 
      if(present(vertStagger    )) grid%ptr%vertStagger     = vertStagger 
      if(present(horzCoordSystem)) grid%ptr%horzCoordSystem = horzCoordSystem  
      if(present(vertCoordSystem)) grid%ptr%vertCoordSystem = vertCoordSystem  
      if(present(coordOrder     )) grid%ptr%coordOrder      = coordOrder 
      if(present(coordIndex     )) grid%ptr%coordIndex      = coordIndex 

!     Set the Grid name if present, otherwise construct a default one
      if (present(name)) then
         call ESMF_SetName(grid%ptr%base, name, "Grid", status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridSetAttributes: Setname"
            return
         endif
      endif

!     Set number of physical grids - check to make sure number of
!     allocated grids does not already exceed this value.
      if (present(numPhysGrids)) then
         if (numPhysGrids > grid%ptr%numPhysGridsAlloc) &
            grid%ptr%numPhysGrids = numPhysGrids
      endif
      
!     Set number of dist grids - check to make sure number of
!     allocated grids does not already exceed this value.
      if (present(numDistGrids)) then
         if (numDistGrids > grid%ptr%numDistGridsAlloc) &
            grid%ptr%numDistGrids = numDistGrids
      endif
      
!     Set periodic flags for each dimension
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            grid%ptr%periodic(i) = periodic(i)
         enddo
      endif

!     Set global domain limits
      if (present(globalMinCoord)) then
         if (size(globalMinCoord) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridSetAttributes: Error - globalMinCoord too big'
            return
         endif
         do i=1,size(globalMinCoord)
            grid%ptr%globalMinCoord(i) = globalMinCoord(i)
         enddo
      endif
      if (present(globalMaxCoord)) then
         if (size(globalMaxCoord) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridSetAttributes: Error - globalMaxCoord too big'
            return
         endif
         do i=1,size(globalMaxCoord)
            grid%ptr%globalMaxCoord(i) = globalMaxCoord(i)
         enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSetAttributes

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetAttributes - retrieves some attributes of Grid

! !INTERFACE:
      subroutine ESMF_GridGetAttributes(grid,  name,            &
                              horzGridKind,    vertGridKind,    &
                              horzStagger,     vertStagger,     &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder,      coordIndex,      &
                              numPhysGrids,    numDistGrids,    &
                              periodic,                         &
                              globalMinCoord,  globalMaxCoord,  rc)

!
! !ARGUMENTS:

      type(ESMF_Grid), intent(in) :: grid

      character(len=ESMF_MAXSTR), intent(out), optional :: name
      type (ESMF_GridKind),       intent(out), optional :: horzGridKind
      type (ESMF_GridKind),       intent(out), optional :: vertGridKind
      type (ESMF_GridStagger),    intent(out), optional :: horzStagger
      type (ESMF_GridStagger),    intent(out), optional :: vertStagger
      type (ESMF_CoordSystem),    intent(out), optional :: horzCoordSystem  
      type (ESMF_CoordSystem),    intent(out), optional :: vertCoordSystem  
      type (ESMF_CoordOrder),     intent(out), optional :: coordOrder
      type (ESMF_CoordIndex),     intent(out), optional :: coordIndex
      integer,                    intent(out), optional :: numPhysGrids
      integer,                    intent(out), optional :: numDistGrids

      real(ESMF_KIND_R8), dimension(:), intent(out), optional :: globalMinCoord
      real(ESMF_KIND_R8), dimension(:), intent(out), optional :: globalMaxCoord
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
!     \item[{[horzGridKind]}]
!          {\tt ESMF\_GridKind} describing the horizontal grid.
!     \item[{[vertGridKind]}]
!          {\tt ESMF\_GridKind} describing the vertical   grid.
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
!     \item[{[globalMinCoord]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[globalMaxCoord]}]
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

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Get the Grid name if present
      if (present(name)) then
         call ESMF_GetName(grid%ptr%base, name, status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridGetAttributes: GetName"
            return
         endif
      endif

!     if present, get information from grid derived type
      if(present(horzGridKind   )) horzGridKind    = grid%ptr%horzGridKind
      if(present(vertGridKind   )) vertGridKind    = grid%ptr%vertGridKind
      if(present(horzStagger    )) horzStagger     = grid%ptr%horzStagger
      if(present(vertStagger    )) vertStagger     = grid%ptr%vertStagger
      if(present(horzCoordSystem)) horzCoordSystem = grid%ptr%horzCoordSystem
      if(present(vertCoordSystem)) vertCoordSystem = grid%ptr%vertCoordSystem
      if(present(coordOrder     )) coordOrder      = grid%ptr%coordOrder
      if(present(coordIndex     )) coordIndex      = grid%ptr%coordIndex
      if(present(numPhysGrids   )) numPhysGrids    = grid%ptr%numPhysGrids
      if(present(numDistGrids   )) numDistGrids    = grid%ptr%numDistGrids

!     Get periodic flags for each dimension
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            periodic(i) = grid%ptr%periodic(i)
         enddo
      endif

!     Get global domain limits
      if (present(globalMinCoord)) then
         if (size(globalMinCoord) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridGetAttributes: Error - globalMinCoord too big'
            return
         endif
         do i=1,size(globalMinCoord)
            globalMinCoord(i) = grid%ptr%globalMinCoord(i)
         enddo
      endif
      if (present(globalMaxCoord)) then
         if (size(globalMaxCoord) > ESMF_MAXGRIDDIM) then
            print *,'ESMF_GridGetAttributes: Error - globalMaxCoord too big'
            return
         endif
         do i=1,size(globalMaxCoord)
            globalMaxCoord(i) = grid%ptr%globalMaxCoord(i)
         enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetAttributes

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridAddPhysGrid - adds a complete PhysGrid to Grid type

! !INTERFACE:
      subroutine ESMF_GridAddPhysGrid(grid, physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), intent(inout) :: grid
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
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Update the PhysGridAlloc counter and check to see if PhysGrid
!     array needs to be resized to add the new physgrid

      grid%numPhysGridsAlloc = grid%numPhysGridsAlloc + 1
      if (grid%numPhysGridsAlloc > grid%numPhysGrids) then
         grid%numPhysGrids = grid%numPhysGrids + 1
         !TODO: resize physgrid array
         print *,'ERROR in GridAddPhysGrid: resize not yet implemented'
         return
      endif

!     Add the PhysGrid
      grid%physgrids(grid%numPhysGridsAlloc) = physgrid

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddPhysGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetPhysGrid - retrieves complete PhysGrid from Grid type

! !INTERFACE:
      subroutine ESMF_GridGetPhysGrid(physgrid, grid, name, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(out) :: physgrid
      type(ESMF_GridType), intent(in)  :: grid
      character(*), intent(in), optional :: name
      type(ESMF_RelLoc), intent(in), optional :: relloc
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
!     \item[{[name]}]
!          Optional name to identify which PhysGrid to retrieve.
!     \item[{[relloc]}]
!          Relative location ({\tt ESMF_RelLoc}) to identify which
!          PhysGrid to retrieve.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: n                    ! search loop index
      integer :: status               ! Error status
      logical :: rcpresent            ! Return code present
      logical :: found                ! found flag for searches
      character (len=ESMF_MAXSTR) :: nameTmp    ! temporary name variable
      type(ESMF_RelLoc) :: rellocTmp

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     If name supplied, search by name and return selected PhysGrid
      if (present(name)) then
         found = .false.
         name_search: do n=1,grid%numPhysGridsAlloc
            call ESMF_PhysGridGet(grid%physgrids(n), name=nameTmp, rc=status)
            if(status /= ESMF_SUCCESS) then
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

!     If relloc supplied, search by relloc and return selected PhysGrid
      else if (present(relloc)) then
         found = .false.
         relloc_search: do n=1,grid%numPhysGridsAlloc
            call ESMF_PhysGridGet(grid%physgrids(n), relloc=rellocTmp, rc=status)
            if(status /= ESMF_SUCCESS) then
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
      
!     If neither supplied, return error.
      else
         print *,'ERROR in GridGetPhysGrid: must supply name or relloc'
         return
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetPhysGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridAddDistGrid - adds a complete DistGrid to Grid type

! !INTERFACE:
      subroutine ESMF_GridAddDistGrid(grid, distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), intent(inout) :: grid
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
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Update the DistGridAlloc counter and check to see if DistGrids
!     array needs to be resized to add the new distgrid

      grid%numDistGridsAlloc = grid%numDistGridsAlloc + 1
      if (grid%numDistGridsAlloc > grid%numDistGrids) then
         grid%numDistGrids = grid%numDistGrids + 1
         !TODO: resize physgrid array
         print *,'ERROR in GridAddDistGrid: resize not yet implemented'
         return
      endif

!     Add the DistGrid
      grid%distgrids(grid%numDistGridsAlloc) = distgrid

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridAddDistGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetDistGrid - retrieves complete DistGrid from Grid type

! !INTERFACE:
      subroutine ESMF_GridGetDistGrid(distgrid, grid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(out) :: distgrid
      type(ESMF_GridType), intent(in)  :: grid
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
!EOP

      integer :: n                    ! search loop index
      integer :: status               ! Error status
      logical :: rcpresent            ! Return code present
      logical :: found                ! found flag for searches
      character (len=ESMF_MAXSTR) :: nameTmp    ! temporary name variable

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Search by name and return selected DistGrid
      found = .false.
      name_search: do n=1,grid%numDistGridsAlloc
         call ESMF_DistGridGet(grid%distgrids(n), name=nameTmp, rc=status)
         if(status /= ESMF_SUCCESS) then
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

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDistGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridDestruct - Free all resources associated with a Grid 

! !INTERFACE:
      subroutine ESMF_GridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys a {\tt ESMF\_GridType} object previously allocated
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
! !REQUIREMENTS:
!EOP
      integer :: n                            ! dummy loop index
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      !TODO: destruct these
      !  type (ESMF_Base) :: base
      !  type (ESMF_Status) :: gridstatus 
      grid%horzGridKind    = ESMF_GridKind_Unknown
      grid%vertGridKind    = ESMF_GridKind_Unknown
      grid%horzStagger     = ESMF_GridStagger_Unknown
      grid%vertStagger     = ESMF_GridStagger_Unknown
      grid%horzCoordSystem = ESMF_CoordSystem_Unknown
      grid%vertCoordSystem = ESMF_CoordSystem_Unknown
      grid%coordOrder      = ESMF_CoordOrder_Unknown
      grid%coordIndex      = ESMF_CoordIndex_Unknown
      grid%periodic        = ESMF_FALSE
      grid%numPhysGrids    = 0
      grid%numDistGrids    = 0

      do n=1,grid%numPhysGridsAlloc
         call ESMF_PhysGridDestroy(grid%physgrids(n), rc=status)
         if (status /= ESMF_SUCCESS) then
            print *,'ERROR in ESMF_GridDestruct: error destroying physgrids'
            return
         endif
      end do      
      grid%numPhysGridsAlloc = 0
      !TODO: add status to deallocate calls and trap errors
      deallocate(grid%physgrids)
      
      deallocate(grid%distGridIndex)
      
      do n=1,grid%numDistGridsAlloc
         call ESMF_DistGridDestroy(grid%distgrids(n), rc=status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridDestruct: distgrid destroy"
            return
         endif
      end do
      grid%numDistGridsAlloc = 0
      deallocate(grid%distgrids)
    
      grid%globalMinCoord(:) = 0
      grid%globalMaxCoord(:) = 0

      call ESMF_LocalArrayDestroy(grid%boundingBoxes, rc=status)
      if (status /= ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridDestruct: error destroying local array"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDestruct

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
!     \item[opt]
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

      !TODO: complete prints
       
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
      do i=1, gp%numPhysGrids 
        call ESMF_PhysGridPrint(gp%physgrids(i), 'no-opt')
      enddo

      ! Print the DistGrid
      print *, 'DistGrids associated with this Grid:'
      do i=1, gp%numDistGrids 
        call ESMF_DistGridPrint(gp%distgrids(i), 'no-opt')
      enddo

      print *, "*********End Grid Print"

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridKindEqual - equality of Grid kinds
!
! !INTERFACE:
      function ESMF_GridKindEqual(GridKind1, GridKind2)

! !RETURN VALUE:
      logical :: ESMF_GridKindEqual

! !ARGUMENTS:

      type (ESMF_GridKind), intent(in) :: &
         GridKind1,      &! Two grid kinds to compare for
         GridKind2        ! equality

! !DESCRIPTION:
!     This routine compares two ESMF Grid kinds to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[GridKind1, GridKind2]
!          Two region types to compare for equality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridKindEqual = (GridKind1%gridkind == &
                            GridKind2%gridkind)

      end function ESMF_GridKindEqual

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
! !IROUTINE: ESMF_GridKindNotEqual - non-equality of Grid kinds
!
! !INTERFACE:
      function ESMF_GridKindNotEqual(GridKind1, GridKind2)

! !RETURN VALUE:
      logical :: ESMF_GridKindNotEqual

! !ARGUMENTS:

      type (ESMF_GridKind), intent(in) :: &
         GridKind1,      &! Two Grid kinds to compare for
         GridKind2        ! inequality

! !DESCRIPTION:
!     This routine compares two ESMF Grid kinds to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[GridKind1, GridKind2]
!          Two kinds of Grids to compare for inequality
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_GridKindNotEqual = (GridKind1%gridkind /= &
                               GridKind2%gridkind)

      end function ESMF_GridKindNotEqual

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
