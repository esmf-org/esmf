! $Id: ESMF_PhysGrid.F90,v 1.33 2003/07/31 23:01:54 jwolfe Exp $
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
#include "ESMF_PhysGrid.h"
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_PhysGridMod - Physical properties of Grid
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_PhysGrid} class and is responsible
! for computing or initializing physical properties of grids.   Such
! properties include coordinate information necessary for describing grids,
! metric information for grid distances, grid masks and assignment of
! region identifiers to grids.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LocalArrayMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     !  ESMF_PhysGridType
!
!     !  Description of ESMF_PhysGrid.

      type ESMF_PhysGridType
      sequence
      private

        type (ESMF_Base) :: base

        integer :: dim_num         ! number of dimensions

        character (len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: &
           dim_names,             &! dimension names
           dim_units               ! dimension units

        real, dimension(ESMF_MAXGRIDDIM) :: &   ! TODO: originally real*8
           global_min,            &! global coordinate minimums
           global_max,            &! global coordinate maximums
           local_min,             &! local  coordinate minimums
           local_max               ! local  coordinate maximums

        integer :: num_corners     ! number of corners for each
                                   ! grid cell (can be degenerate)
        integer :: num_faces       ! likely assume same as num_corners
                                   ! but might specify storage of only
                                   ! 2 of 4 faces, for example
        type (ESMF_LocalArray), pointer ::      &
           center_coord,          &! coordinates of centers each cell
           corner_coord,          &! coordinates of corners each cell
           face_coord              ! coords of face centers each cell

        integer :: num_metrics     ! counter for number of metrics
        character (len=ESMF_MAXSTR), dimension(:), pointer :: &
           metric_names            ! array of names for each metric
        type (ESMF_LocalArray), dimension(:), pointer ::      &
           metrics                 ! an array of defined grid metrics

        integer :: num_lmasks      ! counter for number of logical masks
        character (len=ESMF_MAXSTR), dimension(:), pointer :: &
           lmask_names             ! names for each defined logical mask
        type (ESMF_LocalArray), dimension(:), pointer ::      & 
           lmask                   ! array of defined logical masks 

        integer :: num_mmasks      ! counter for number of multiplicative masks
        character (len=ESMF_MAXSTR), dimension(:), pointer :: &
           mmask_names             ! names for each multiplicative mask
        type (ESMF_LocalArray), dimension(:), pointer ::      &
           mmask                   ! array of defined multiplicative masks

        integer :: num_region_ids  ! counter for the number of region 
                                   ! identifiers
        character (len=ESMF_MAXSTR), dimension(:), pointer :: &
           region_id_names         ! names associated with each region id
        type (ESMF_LocalArray), pointer ::      &
           region_id               ! array assigning region identifier
                                   ! to each cell

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
   public ESMF_PhysGrid, ESMF_PhysGridType
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
   public ESMF_PhysGridGetMetric
   public ESMF_PhysGridSetMetric
   public ESMF_PhysGridGetLMask
   public ESMF_PhysGridSetLMask
   public ESMF_PhysGridGetMMask
   public ESMF_PhysGridSetMMask
   public ESMF_PhysGridGetRegionID
   public ESMF_PhysGridSetRegionID

   public ESMF_PhysGridValidate
   public ESMF_PhysGridPrint

!   public ESMF_PhysGridSearchBboxSpherical
!   public ESMF_PhysGridSearchGeneralSpherical
!   public ESMF_PhysGridSearchBboxCartesian

   public ESMF_PhysGridPointInCell

   public ESMF_PhysGridCompDistSpherical
   public ESMF_PhysGridCompDistCartesian
 
!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

   integer, parameter, public ::        &! internally-recognized grid metrics
      ESMF_GridMetric_Unknown     =  0, &! unknown or undefined metric
      ESMF_GridMetric_Area        =  1, &! area of grid cell
      ESMF_GridMetric_Volume      =  2, &! volume of 3-d grid cell
      ESMF_GridMetric_FaceLength  =  3, &! length of 2d grid cell along face
      ESMF_GridMetric_NbrDist     =  4, &! cell center to neighbor center dist
      ESMF_GridMetric_FaceDist    =  5, &! cell center to cell face distance
      ESMF_GridMetric_CornDist    =  6   ! cell center to cell corner distance

   integer, parameter, public ::        &! internally-recognized cell locations
      ESMF_CellLoc_Unknown        =  0, &! unknown or undefined metric
      ESMF_CellLoc_Center_X       =  1, &! cell center, x-coordinate  
      ESMF_CellLoc_Center_Y       =  2, &! cell center, y-coordinate  
      ESMF_CellLoc_Corner_X       =  3, &! cell vertex, x-coordinate  
      ESMF_CellLoc_Corner_Y       =  4, &! cell vertex, y-coordinate  
      ESMF_CellLoc_Face_X         =  5, &! cell face center, x-coordinate  
      ESMF_CellLoc_Face_Y         =  6   ! cell face center, y-coordinate  

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_PhysGrid.F90,v 1.33 2003/07/31 23:01:54 jwolfe Exp $'

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
!        module procedure ESMF_PhysGridCreateEmpty
         module procedure ESMF_PhysGridCreateInternal
         module procedure ESMF_PhysGridCreateSpecd
!        module procedure ESMF_PhysGridCreateStagger
!        module procedure ESMF_PhysGridCreateRead
!        module procedure ESMF_PhysGridCreateCopy
!        module procedure ESMF_PhysGridCreateCutout
!        module procedure ESMF_PhysGridCreateChangeResolution
!        module procedure ESMF_PhysGridCreateExchange

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_PhysGrid} create
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_PhysGridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_PhysGridSetCoordFromArray
         module procedure ESMF_PhysGridSetCoordInternal
!        module procedure ESMF_PhysGridSetCoordStagger
!        module procedure ESMF_PhysGridSetCoordRead

! !DESCRIPTION:
!     This interface provides a single function for various methods of
!     computing grid coordinates.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_PhysGridSetMetric

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_PhysGridSetMetricFromArray
!        module procedure ESMF_PhysGridSetMetricInternal
!        module procedure ESMF_PhysGridSetMetricStagger
!        module procedure ESMF_PhysGridSetMetricRead

! !DESCRIPTION:
!     This interface provides a single function for computing or initializing
!     grid metric information.
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_PhysGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_PhysGridConstructNew
         module procedure ESMF_PhysGridConstructInternal
         module procedure ESMF_PhysGridConstructSpecd

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct
!     a complete {\tt ESMF\_PhysGrid}.
!
!EOP
      end interface 
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
      function ESMF_PhysGridCreateNew(name, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridCreateNew
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_PhysGrid} object and constructs its
!     internals.  Returns a pointer to a new {\tt ESMF\_PhysGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt ESMF\_PhysGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_PhysGridType), pointer :: physgrid   ! Pointer to new physgrid
      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize pointers
      nullify(physgrid)
      nullify(ESMF_PhysGridCreateNew%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(physgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateNew: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_PhysGridConstruct(physgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateNew: PhysGrid construct"
        return
      endif

!     Set return values.
      ESMF_PhysGridCreateNew%ptr => physgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridCreateInternal - Create a new PhysGrid internally

! !INTERFACE:
      function ESMF_PhysGridCreateInternal(dim_num,    &
                                        local_min , local_max , local_nmax , &
                                        global_min, global_max, global_nmax, &
                                        name, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridCreateInternal
!
! !ARGUMENTS:

      integer, intent(in) :: dim_num  ! number of dimensions for grid

      integer, dimension(dim_num), intent(in) :: &
         local_nmax,    &! local number of grid increments in each direction
         global_nmax     ! global number of grid increments in each direction

      real, dimension(dim_num), intent(in) :: &
         local_min,     &! local coordinate minimum in each direction
         local_max,     &! local coordinate maximum in each direction
         global_min,    &! global coordinate minimum in each direction
         global_max      ! global coordinate maximum in each direction

      character (len = *), intent(in), optional :: name  ! name for grid

      integer, intent(out), optional :: rc  ! return code               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_PhysGrid} object, constructs its
!     internals, and internally generates the {\tt ESMF\_PhysGrid}.  Returns a
!     pointer to the new {\tt ESMF\_PhysGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[dim\_num]
!          Number of physical dimensions for this grid.
!     \item[local\_min]
!          Minimum local physical coordinate in each coordinate direction.
!     \item[local\_max]
!          Maximum local physical coordinate in each coordinate direction.
!     \item[local\_nmax]
!          Number of local grid increments in each coordinate direction.
!     \item[global\_min]
!          Minimum global physical coordinate in each coordinate direction.
!     \item[global\_max]
!          Maximum global physical coordinate in each coordinate direction.
!     \item[global\_nmax]
!          Number of global grid increments in each coordinate direction.
!     \item[[name]]
!          {\tt ESMF\_PhysGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_PhysGridType), pointer :: physgrid   ! Pointer to new physgrid
      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize pointers
      nullify(physgrid)
      nullify(ESMF_PhysGridCreateInternal%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(physgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateInternal: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      if (present(name)) then
         call ESMF_PhysGridConstructInternal(physgrid, dim_num,             &
                                       local_min , local_max , local_nmax , &
                                       global_min, global_max, global_nmax, &
                                       name, status)
      else
         call ESMF_PhysGridConstructInternal(physgrid, dim_num,             &
                                       local_min , local_max , local_nmax , &
                                       global_min, global_max, global_nmax, &
                                       rc=status)
      endif
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateInternal: PhysGrid construct"
        return
      endif

!     Set return values.
      ESMF_PhysGridCreateInternal%ptr => physgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCreateInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridCreateSpecd - Create a new PhysGrid from specifications

! !INTERFACE:
      function ESMF_PhysGridCreateSpecd(dim_num, myDE, dx, dy, global_min, &
                                        countsPerDE1, countsPerDE2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridCreateSpecd
!
! !ARGUMENTS:
      integer, intent(in) :: dim_num
      integer, dimension(dim_num), intent(in) :: myDE
      real, dimension(:), intent(in) :: dx
      real, dimension(:), intent(in) :: dy
      real, dimension(dim_num), intent(in) :: global_min
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_PhysGrid} object, constructs its
!     internals, and internally generates the {\tt ESMF\_PhysGrid}.  Returns a
!     pointer to the new {\tt ESMF\_PhysGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[dim\_num]
!          Number of physical dimensions for this grid.
!     \item[local\_min]
!          Minimum local physical coordinate in each coordinate direction.
!     \item[local\_max]
!          Maximum local physical coordinate in each coordinate direction.
!     \item[local\_nmax]
!          Number of local grid increments in each coordinate direction.
!     \item[global\_min]
!          Minimum global physical coordinate in each coordinate direction.
!     \item[global\_max]
!          Maximum global physical coordinate in each coordinate direction.
!     \item[global\_nmax]
!          Number of global grid increments in each coordinate direction.
!     \item[[name]]
!          {\tt ESMF\_PhysGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_PhysGridType), pointer :: physgrid   ! Pointer to new physgrid
      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize pointers
      nullify(physgrid)
      nullify(ESMF_PhysGridCreateSpecd%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(physgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateSpecd: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_PhysGridConstruct(physgrid, dim_num, myDE, dx, dy, &
                                  global_min, countsPerDE1, countsPerDE2, &
                                  name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateSpecd: PhysGrid construct"
        return
      endif

!     Set return values.
      ESMF_PhysGridCreateSpecd%ptr => physgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCreateSpecd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridDestroy - Free all resources associated with a PhysGrid 

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
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

      if (present(rc)) rcpresent = .TRUE.

      call ESMF_PhysGridDestruct(physgrid%ptr, status)

      nullify(physgrid%ptr)
      if (rcpresent) rc = status

      end subroutine ESMF_PhysGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridConstructNew - Construct new empty PhysGrid 

! !INTERFACE:
      subroutine ESMF_PhysGridConstructNew(physgrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType) :: physgrid  
      character (len = *), intent(in), optional :: name  ! name
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_PhysGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_PhysGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_PhysGridCreate}, which calls
!     {\tt ESMF\_PhysGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Pointer to a {\tt ESMF\_PhysGrid}.
!     \item[[name]] 
!          {\tt ESMF\_PhysGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_SUCCESS               ! Error status
      logical :: rcpresent=.FALSE.                 ! Return code present
      integer :: i

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Set the PhysGrid name if present, otherwise construct a default one
      if (present(name)) then
         call ESMF_SetName(physgrid%base, name, "PhysGrid", status)
      else
         call ESMF_SetName(physgrid%base, "PhysGridNoName", "PhysGrid", status)
      endif
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridConstructNew: Setname"
        return
      endif

!     Initialize physgrid contents  TODO: an integer "flag" for uninit, like -9999
      physgrid%dim_num = 0
      physgrid%num_corners = 0
      physgrid%num_faces = 0
      physgrid%num_metrics = 0
      physgrid%num_lmasks = 0
      physgrid%num_mmasks = 0
      physgrid%num_region_ids = 0

      do i = 1,ESMF_MAXGRIDDIM
        physgrid%dim_names(i) = ' '
        physgrid%dim_units(i) = ' '
        physgrid%global_min(i) = 0.0
        physgrid%global_max(i) = 0.0
        physgrid%local_min(i) = 0.0
        physgrid%local_max(i) = 0.0
      enddo

      nullify(physgrid%center_coord)
      nullify(physgrid%corner_coord)
      nullify(physgrid%face_coord)
      nullify(physgrid%metrics)
      nullify(physgrid%lmask)
      nullify(physgrid%mmask)
      nullify(physgrid%region_id)

      nullify(physgrid%metric_names)
      nullify(physgrid%lmask_names)
      nullify(physgrid%mmask_names)
      nullify(physgrid%region_id_names)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridConstructInternal - Construct the internals of an allocated PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridConstructInternal(physgrid, dim_num,            &
                                         local_min , local_max , local_nmax , &
                                         global_min, global_max, global_nmax, &
                                         name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType) :: physgrid  

      integer, intent(in) :: dim_num  ! number of dimensions for grid

      integer, dimension(dim_num), intent(in) :: &
         local_nmax,    &! local number of grid increments in each direction
         global_nmax     ! global number of grid increments in each direction

      real, dimension(dim_num), intent(in) :: &
         local_min,     &! local coordinate minimum in each direction
         local_max,     &! local coordinate maximum in each direction
         global_min,    &! global coordinate minimum in each direction
         global_max      ! global coordinate maximum in each direction

      character (len = *), intent(in), optional :: name  ! name for grid

      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_PhysGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_PhysGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_PhysGridCreate}, which calls
!     {\tt ESMF\_PhysGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[dim\_num]
!          Number of physical dimensions for this grid.
!     \item[local\_min]
!          Minimum local physical coordinate in each coordinate direction.
!     \item[local\_max]
!          Maximum local physical coordinate in each coordinate direction.
!     \item[local\_nmax]
!          Number of local grid increments in each coordinate direction.
!     \item[global\_min]
!          Minimum global physical coordinate in each coordinate direction.
!     \item[global\_max]
!          Maximum global physical coordinate in each coordinate direction.
!     \item[global\_nmax]
!          Number of global grid increments in each coordinate direction.
!     \item[[name]]
!          {\tt ESMF\_PhysGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      if (present(name)) then
         call ESMF_PhysGridConstructNew(physgrid, name=name, rc=status)
      else
         call ESMF_PhysGridConstructNew(physgrid, rc=status)
      endif
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridConstructInternal: PhysGrid construct"
        return
      endif

!TODO: compute all grid coordinates and stuff
!      Fill in physgrid derived type with function arguments
       call ESMF_PhysGridSet(physgrid, dim_num=dim_num, &
                             local_min=local_min, local_max=local_max, &
                             global_min=global_min, global_max=global_max, &
                             rc=status)

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridConstructInternal: PhysGrid set"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridConstructInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridConstructSpecd - Construct the internals of an allocated PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridConstructSpecd(physgrid, dim_num, myDE, dx, dy, &
                                             global_min, countsPerDE1, &
                                             countsPerDE2, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType) :: physgrid  
      integer, intent(in) :: dim_num
      integer, dimension(dim_num), intent(in) :: myDE
      real, dimension(:), intent(in) :: dx
      real, dimension(:), intent(in) :: dy
      real, dimension(dim_num), intent(in) :: global_min
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_PhysGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_PhysGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_PhysGridCreate}, which calls
!     {\tt ESMF\_PhysGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[dim\_num]
!          Number of physical dimensions for this grid.
!     \item[local\_min]
!          Minimum local physical coordinate in each coordinate direction.
!     \item[local\_max]
!          Maximum local physical coordinate in each coordinate direction.
!     \item[local\_nmax]
!          Number of local grid increments in each coordinate direction.
!     \item[global\_min]
!          Minimum global physical coordinate in each coordinate direction.
!     \item[global\_max]
!          Maximum global physical coordinate in each coordinate direction.
!     \item[global\_nmax]
!          Number of global grid increments in each coordinate direction.
!     \item[[name]]
!          {\tt ESMF\_PhysGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: i
      integer, dimension(dim_num) :: start_local, stop_local
      real, dimension(dim_num) :: local_min, local_max, global_max

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      call ESMF_PhysGridConstructNew(physgrid, name=name, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridConstructSpecd: PhysGrid construct"
        return
      endif

!     Calculate local mins and maxs
      start_local(1) = 1
      if (myDE(1).ge.2) then
        do i = 1,myDE(1)-1
          start_local(1) = start_local(1) + countsPerDE1(i)
        enddo
      endif
      stop_local(1) = start_local(1) + countsPerDE1(myDE(1))

      local_min(1) = global_min(1)
      if (start_local(1).gt.1) then
        do i = 1,start_local(1)-1
          local_min(1) = local_min(1) + dx(i)
        enddo
      endif
      local_max(1) = local_min(1)
      do i = start_local(1), stop_local(1)
        local_max(1) = local_max(1) + dx(i)
      enddo

      start_local(2) = 1
      if (myDE(2).ge.2) then
        do i = 1,myDE(2)-1
          start_local(2) = start_local(2) + countsPerDE1(2)
        enddo
      endif
      stop_local(2) = start_local(2) + countsPerDE1(myDE(2))

      local_min(2) = global_min(1)
      if (start_local(2).ge.2) then
        do i = 1,start_local(2)-1
          local_min(2) = local_min(2) + dy(i)
        enddo
      endif
      local_max(2) = local_min(2)
      do i = start_local(2), stop_local(2)
        local_max(2) = local_max(2) + dy(i)
      enddo

!     Calculate global maxs
      global_max(1) = global_min(1)
      do i = 1,size(dx)
        global_max(1) = global_max(1) + dx(i)
      enddo
      global_max(2) = global_min(2)
      do i = 1,size(dy)
        global_max(2) = global_max(2) + dy(i)
      enddo

!     Fill in physgrid derived type with function arguments
      call ESMF_PhysGridSet(physgrid, dim_num=dim_num, &
                            local_min=local_min, local_max=local_max, &
                            global_min=global_min, global_max=global_max, &
                            rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridConstructSpecd: PhysGrid set"
        return
      endif
 !TODO: add set coord call

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridConstructSpecd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridDestruct - Free any PhysGrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_PhysGridDestruct(physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType), intent(inout) :: physgrid    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt ESMF\_PhysGridConstruct}, does any additional cleanup before the
!     original {\tt ESMF\_PhysGrid} object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_PhysGridDestroy}, which calls
!     {\tt ESMF\_PhysGridDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          The class to be destructed.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: i

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     nullify(physgrid%base)   TODO: ?

      physgrid%dim_num = 0
      physgrid%num_corners = 0
      physgrid%num_faces = 0
      physgrid%num_metrics = 0
      physgrid%num_lmasks = 0
      physgrid%num_mmasks = 0
      physgrid%num_region_ids = 0

      do i = 1,ESMF_MAXGRIDDIM
        physgrid%dim_names(i) = ' '
        physgrid%dim_units(i) = ' '
        physgrid%global_min(i) = 0.0
        physgrid%global_max(i) = 0.0
        physgrid%local_min(i) = 0.0
        physgrid%local_max(i) = 0.0
      enddo

      !TODO: deallocate these instead of nullify
      nullify(physgrid%center_coord)
      nullify(physgrid%corner_coord)
      nullify(physgrid%face_coord)
      nullify(physgrid%metrics)
      nullify(physgrid%lmask)
      nullify(physgrid%mmask)
      nullify(physgrid%region_id)

      !TODO: deallocate these instead of nullify
      nullify(physgrid%metric_names)
      nullify(physgrid%lmask_names)
      nullify(physgrid%mmask_names)
      nullify(physgrid%region_id_names)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGet - Get information from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGet(physgrid,                &
                                  name       , dim_num   , &
                                  num_corners, num_faces , &
                                  dim_names  , dim_units , &
                                  global_min , global_max, &
                                  local_min  , local_max , &
                                  rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType), intent(in) :: physgrid

      character (*), intent(inout), optional :: name ! name of grid

      integer, intent(out), optional :: &
         dim_num,                &! number of physical dimensions
         num_corners,            &! number of corners in each cell
         num_faces                ! number of faces for each cell

      character (*), dimension(ESMF_MAXGRIDDIM), intent(inout), optional :: &
         dim_names,              &! names for each dimension
         dim_units                ! units for each dimension

      real, dimension(ESMF_MAXGRIDDIM), &
         intent(inout), optional :: &
         local_min,              &! local minimum in each coord direction
         local_max,              &! local maximum in each coord direction
         global_min,             &! global minimum in each coord direction
         global_max               ! global maximum in each coord direction

      integer, intent(out), optional :: rc ! return code              
!
! !DESCRIPTION:
!     This version gets a variety of information about a {\tt ESMF\_PhysGrid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Pointer to a {\tt ESMF\_PhysGrid}.
!     \item[[name]]
!          {\tt ESMF\_PhysGrid} name.
!     \item[[dim\_num]] 
!          Number of physical dimensions for this grid.
!     \item[[dim\_names]] 
!          Names for each physical dimension of this grid.
!     \item[[dim\_units]] 
!          Units for each physical dimension of this grid.
!     \item[[num\_corners]] 
!          Number of corners for each grid cell (can be degenerate).
!     \item[[num\_faces]] 
!          Number of faces for each grid cell.
!     \item[[local\_min]]
!          Minimum local physical coordinate in each coordinate direction.
!     \item[[local\_max]]
!          Maximum local physical coordinate in each coordinate direction.
!     \item[[global\_min]]
!          Minimum global physical coordinates in each coordinate direction.
!     \item[[global\_max]]
!          Maximum global physical coordinates in each coordinate direction.
!     \item[[rc]] 
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

!     if present, get information from physgrid derived type

      if (present(name)) then
         call ESMF_GetName(physgrid%base, name, status)
      endif

      if (present(dim_num)) then
         dim_num = physgrid%dim_num
      endif
      if (present(num_corners)) then
         num_corners = physgrid%num_corners
      endif
      if (present(num_faces)) then
         num_faces = physgrid%num_faces
      endif
      if (present(dim_names)) then
         dim_names = physgrid%dim_names
      endif
      if (present(dim_units)) then
         dim_units = physgrid%dim_units
      endif
      if (present(local_min)) then
         local_min = physgrid%local_min
      endif
      if (present(local_max)) then
         local_max = physgrid%local_max
      endif
      if (present(global_min)) then
         global_min = physgrid%global_min
      endif
      if (present(global_max)) then
         global_max = physgrid%global_max
      endif

      if(rcpresent) rc = status

      end subroutine ESMF_PhysGridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSet - Set information for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSet(physgrid,                &
                                  name       , dim_num   , &
                                  num_corners, num_faces , &
                                  dim_names  , dim_units , &
                                  global_min , global_max, &
                                  local_min  , local_max , &
                                  rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType), intent(inout) :: physgrid

      character (*), intent(in), optional :: name ! name of grid

      integer, intent(in), optional :: &
         dim_num,                &! number of physical dimensions
         num_corners,            &! number of corners in each cell
         num_faces                ! number of faces for each cell

      character (*), dimension(ESMF_MAXGRIDDIM), intent(in), optional :: &
         dim_names,              &! names for each dimension
         dim_units                ! units for each dimension

      real, dimension(ESMF_MAXGRIDDIM), &
         intent(in), optional :: &
         local_min,              &! local minimum in each coord direction
         local_max,              &! local maximum in each coord direction
         global_min,             &! global minimum in each coord direction
         global_max               ! global maximum in each coord direction

      integer, intent(out), optional :: rc ! return code              
!
! !DESCRIPTION:
!     This version sets a variety of information about a {\tt ESMF\_PhysGrid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Pointer to a {\tt ESMF\_PhysGrid}.
!     \item[[name]]
!          {\tt ESMF\_PhysGrid} name.
!     \item[[dim\_num]] 
!          Number of physical dimensions for this grid.
!     \item[[dim\_names]] 
!          Names for each physical dimension of this grid.
!     \item[[dim\_units]] 
!          Units for each physical dimension of this grid.
!     \item[[num\_corners]] 
!          Number of corners for each grid cell (can be degenerate).
!     \item[[num\_faces]] 
!          Number of faces for each grid cell.
!     \item[[local\_min]]
!          Minimum local physical coordinate in each coordinate direction.
!     \item[[local\_max]]
!          Maximum local physical coordinate in each coordinate direction.
!     \item[[global\_min]]
!          Minimum global physical coordinates in each coordinate direction.
!     \item[[global\_max]]
!          Maximum global physical coordinates in each coordinate direction.
!     \item[[rc]] 
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

!     if present, set information filling in physgrid derived type

      if (present(name)) then
         call ESMF_SetName(physgrid%base, name, "PhysGrids", status)
      endif

      if (present(dim_num)) then
         physgrid%dim_num = dim_num
      endif
      if (present(num_corners)) then
         physgrid%num_corners = num_corners
      endif
      if (present(num_faces)) then
         physgrid%num_faces = num_faces
      endif
      if (present(dim_names)) then
         physgrid%dim_names = dim_names
      endif
      if (present(dim_units)) then
         physgrid%dim_units = dim_units
      endif
      if (present(local_min)) then
         physgrid%local_min = local_min
      endif
      if (present(local_max)) then
         physgrid%local_max = local_max
      endif
      if (present(global_min)) then
         physgrid%global_min = global_min
      endif
      if (present(global_max)) then
         physgrid%global_max = global_max
      endif

      if(rcpresent) rc = status

      end subroutine ESMF_PhysGridSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetConfig - Get configuration information from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetConfig(physgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the {\tt ESMF\_PhysGrid} object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
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
      end subroutine ESMF_PhysGridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetConfig - Set configuration information for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSetConfig(physgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the {\tt ESMF\_PhysGrid} object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
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
      end subroutine ESMF_PhysGridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetCoord - Gets PhysGrid coordinate arrays

! !INTERFACE:
      subroutine ESMF_PhysGridGetCoord(physgrid, center_coord,   &
                                       corner_coord, face_coord, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(in) :: physgrid

      type(ESMF_LocalArray), pointer, optional :: &
         center_coord,       &! coordinates for each cell center
         corner_coord,       &! coordinates for corners of each cell
         face_coord           ! coordinates for face centers of each cell

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Extracts coordinate information from a {\tt ESMF\_PhysGrid} structure.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which coordinates are to be defined.
!     \item[[center\_coord]]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[[corner\_coord]]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout grid.
!     \item[[face\_coord]]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the face index.  Faces should 
!          be numbered consistently with corners.  For example, face 1 should
!          correspond to the face between corners 1,2.
!     \item[[rc]]
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

      if (present(center_coord)) then
         center_coord => physgrid%center_coord
      endif

      if (present(corner_coord)) then
         corner_coord => physgrid%corner_coord
      endif

      if (present(face_coord)) then
         face_coord => physgrid%face_coord
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridGetCoord

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetCoordFromArray - Sets PhysGrid coords from input array

! !INTERFACE:
      subroutine ESMF_PhysGridSetCoordFromArray(physgrid, center_coord,   &
                                                corner_coord, face_coord, &
                                                rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(inout) :: physgrid

      type(ESMF_LocalArray), intent(in), target, optional :: &
         center_coord,       &! coordinates for each cell center
         corner_coord,       &! coordinates for corners of each cell
         face_coord           ! coordinates for face centers of each cell

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Assigns a {\tt ESMF\_PhysGrid}'s coordinates from user-defined coordinates
!     sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which coordinates are to be defined.
!     \item[[center\_coord]]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[[corner\_coord]]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout grid.
!     \item[[face\_coord]]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the face index.  Faces should 
!          be numbered consistently with corners.  For example, face 1 should
!          correspond to the face between corners 1,2.
!     \item[[rc]]
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

      if (present(center_coord)) then
         physgrid%center_coord => center_coord
      endif

      if (present(corner_coord)) then
         physgrid%corner_coord => corner_coord
      endif

      if (present(face_coord)) then
         physgrid%face_coord => face_coord
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetCoordFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetCoordInternal - Set Coords for a PhysGrid internally

! !INTERFACE:
      subroutine ESMF_PhysGridSetCoordInternal(physgrid, ncoord_locs, &
                                               coord_loc, &
                                               global_nmin1, global_nmax1, &
                                               global_nmin2, global_nmax2, &
                                               delta1, delta2, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType) :: physgrid
      integer, intent(in) :: ncoord_locs
      integer, dimension(:), intent(in) :: coord_loc
      integer, intent(in) :: global_nmin1
      integer, intent(in) :: global_nmax1
      integer, intent(in) :: global_nmin2
      integer, intent(in) :: global_nmax2
      real, intent(in) :: delta1
      real, intent(in) :: delta2
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Compute a {\tt ESMF\_PhysGrid}'s coordinates from a given gridtype and set of
!     physical parameters.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          Class to be modified.
!     \item[[ncoord\_locs]]
!          Number of coordinate location specifiers
!     \item[[coord\_loc]]
!          Array of integer specifiers to denote coordinate location relative
!          to the cell (center, corner, face)
!     \item[[global\_nmin1]]
!          Global minimum counter in the 1st direction
!     \item[[global\_nmax1]]
!          Global maximum counter in the 1st direction
!     \item[[global\_nmin2]]
!          Global minimum counter in the 2nd direction
!     \item[[global\_nmax2]]
!          Global maximum counter in the 2nd direction
!     \item[[delta1]]
!          Grid cell size in the 1st direction (assumed constant for now)
!     \item[[delta2]]
!          Grid cell size in the 2nd direction (assumed constant for now)
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_SUCCESS              ! Error status
      integer :: i                                ! local counter
      integer :: global_n1, global_n2             ! counters
      integer :: local_n1, local_n2               ! counters
      integer :: l1, l2
      logical :: rcpresent=.FALSE.                ! Return code present
      real(selected_real_kind(6,45)), dimension(:,:,:), pointer :: temp
      type(ESMF_LocalArray), target :: array_temp
      

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     If no coordinate location specifiers, return with error
      if(ncoord_locs.le.0) then
        print *, "ERROR in ESMF_PhysGridSetCoordInternal: bad ncoord_locs"
        return
      endif

!     Loop over number of coordinate location specifiers
      do i = 1,ncoord_locs

!     For now, a case construct for the different coordinate locations
        select case (coord_loc(i))
        case (ESMF_CellLoc_Unknown)
          status = ESMF_FAILURE
        case (ESMF_CellLoc_Center_X)
          l1 = global_nmax1 - global_nmin1 + 1
          l2 = global_nmax2 - global_nmin2 + 1
          allocate(temp(l1,l2,2))  ! TODO local sizing
          do global_n1 = global_nmin1,global_nmax1
            local_n1 = global_n1 - global_nmin1 + 1
            do global_n2 = global_nmin2,global_nmax2
              local_n2 = global_n2 - global_nmin2 + 1
              temp(local_n1,local_n2,1) = &
                           delta1*0.5*real(global_n1+global_n1-1)
            enddo
          enddo
          array_temp = ESMF_LocalArrayCreate(temp, ESMF_DATA_REF, rc)
          physgrid%center_coord => array_temp 
!         nullify(temp)
!         deallocate(temp)    ! TODO: figure out how to load one array
        case (ESMF_CellLoc_Center_Y)
          do global_n2 = global_nmin2,global_nmax2
            local_n2 = global_n2 - global_nmin2 + 1
            do global_n1 = global_nmin1,global_nmax1
              local_n1 = global_n1 - global_nmin1 + 1
              temp(local_n1,local_n2,2) = &
                           delta2*0.5*real(global_n2+global_n2-1)
            enddo
          enddo
          array_temp = ESMF_LocalArrayCreate(temp, ESMF_DATA_REF, rc)
          physgrid%center_coord => array_temp 
          nullify(temp)
!         deallocate(temp)
        case (ESMF_CellLoc_Corner_X)
!          corner_coord1()=
        case (ESMF_CellLoc_Corner_Y)
!          corner_coord2()=
        case (ESMF_CellLoc_Face_X)
!          face_coord1()=
        case (ESMF_CellLoc_Face_Y)
!          face_coord2()=
        end select

      enddo

!     call ESMF_LocalArrayPrint(physgrid%center_coord1, "foo", rc)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridSetCoordInternal: TODO"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetCoordInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetMetric - Gets a defined metric from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetMetric(physgrid, metric_array, name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(in) :: physgrid

      type(ESMF_LocalArray), pointer :: metric_array

      character(*), intent(in), optional :: name

      integer, intent(in), optional :: id

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Extracts metric information from a {\tt ESMF\_PhysGrid} structure.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} containing metrics.
!     \item[metric\_array]
!          Pointer to array in which metric is stored.
!     \item[[name]]
!          Name assigned to this metric when it was defined.
!     \item[[id]]
!          Integer id assigned to the metric when it was defined.
!          Either the name or id must be supplied.  The id permits
!          more direct access of metric.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i
      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      if (present(id)) then
         ! get metric directly

         if (id > 0 .and. id <= physgrid%num_metrics) then
            metric_array => physgrid%metrics(id)
         else
            print *,'Invalid metric id'
            status = ESMF_FAILURE
         endif

      else if (present(name)) then
         srch_loop: do i=1,physgrid%num_metrics
            if (name == trim(physgrid%metric_names(i))) exit srch_loop
         end do srch_loop

         if (i <= physgrid%num_metrics) then
            metric_array => physgrid%metrics(id)
         else
            print *,'No metric of that name defined'
            status = ESMF_FAILURE
         endif
      else
         print *,'Must supply either name or id'
         status = ESMF_FAILURE
      endif

      if(rcpresent) rc = status

      end subroutine ESMF_PhysGridGetMetric

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetMetricFromArray - Sets grid metrics from input array

! !INTERFACE:
      subroutine ESMF_PhysGridSetMetricFromArray(physgrid, metric_array,   &
                                                 name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(inout) :: physgrid

      type(ESMF_LocalArray), intent(in), target :: &
         metric_array         ! array containing metric value for each cell

      character(*), intent(in) :: name ! name to assign to metric

      integer, intent(out) :: id  ! id assigned to metric

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Assigns a metric to a {\tt ESMF\_PhysGrid} using user-defined data 
!     sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which coordinates are to be defined.
!     \item[metric\_array]
!          Array containing metric values at for each grid cell.
!     \item[name]
!          Name to assign to metric.
!     \item[id]
!          Integer id assigned to metric which allows direct reference
!          into metric array.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      type(ESMF_LocalArray), dimension(:), allocatable, target :: temp_metrics
                                             ! temporary array of metrics

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     increment metric counter and allocate memory
!
      physgrid%num_metrics = physgrid%num_metrics + 1
      n = physgrid%num_metrics
      if(n .eq. 1) then
        allocate(physgrid%metrics(1), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetricFromArray: metrics allocate"
          return
        endif
      else
        allocate(temp_metrics(n), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetricFromArray: temp_metrics allocate"
          return
        endif
        do i = 1, n - 1
          temp_metrics(i) = physgrid%metrics(i)
        enddo
        deallocate(physgrid%metrics, stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetricFromArray: metrics deallocate"
          return
        endif
        allocate(physgrid%metrics(n), stat=status)  ! TODO: is this necessary?
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMetricFromArray: metrics allocate"
          return
        endif
      endif
      temp_metrics(n) = metric_array
      physgrid%metric_names(n) = name
      physgrid%metrics => temp_metrics

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetMetricFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetLMask - Gets a defined logical mask from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetLMask(physgrid, lmask_array, name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(in) :: physgrid

      type(ESMF_LocalArray), pointer :: lmask_array

      character(*), intent(in), optional :: name

      integer, intent(in), optional :: id

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Extracts logical mask information from a {\tt ESMF\_PhysGrid} structure.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} containing metrics.
!     \item[lmask\_array]
!          Pointer to the requested logical mask.
!     \item[[name]]
!          Name assigned to this lmask when it was defined.
!     \item[[id]]
!          Integer id assigned to the lmask when it was defined.
!          Either the name or id must be supplied.  The id permits
!          more direct access to the lmask.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i
      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      if (present(id)) then
         ! get lmask directly

         if (id > 0 .and. id <= physgrid%num_lmasks) then
            lmask_array => physgrid%lmask(id)
         else
            print *,'Invalid lmask id'
            status = ESMF_FAILURE
         endif

      else if (present(name)) then
         srch_loop: do i=1,physgrid%num_lmasks
            if (name == trim(physgrid%lmask_names(i))) exit srch_loop
         end do srch_loop

         if (i <= physgrid%num_lmasks) then
            lmask_array => physgrid%lmask(id)
         else
            print *,'No lmask of that name defined'
            status = ESMF_FAILURE
         endif
      else
         print *,'Must supply either name or id'
         status = ESMF_FAILURE
      endif

      if(rcpresent) rc = status

      end subroutine ESMF_PhysGridGetLMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetLMask - Sets grid logical mask from input array

! !INTERFACE:
      subroutine ESMF_PhysGridSetLMask(physgrid, lmask_array, name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(inout) :: physgrid

      type(ESMF_LocalArray), intent(in), target :: &
         lmask_array         ! array containing logical mask value for each cell

      character(*), intent(in) :: name ! name to assign to logical mask

      integer, intent(out) :: id  ! id assigned to logical mask

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Assigns a logical mask to a {\tt ESMF\_PhysGrid} using user-defined data 
!     sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which mask is to be added.
!     \item[lmask\_array]
!          Array containing logical mask values at for each grid cell.
!     \item[name]
!          Name to assign to logical mask.
!     \item[id]
!          Integer id assigned to logical mask which allows direct reference
!          into logical mask array.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      type(ESMF_LocalArray), dimension(:), allocatable, target :: temp_lmask
                                             ! temporary array of lmasks

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     increment logical mask counter and allocate memory
!
      physgrid%num_lmasks = physgrid%num_lmasks + 1
      n = physgrid%num_lmasks
      if(n .eq. 1) then
        allocate(physgrid%lmask(1), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetLMaskFromArray: lmask allocate"
          return
        endif
      else
        allocate(temp_lmask(n), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetLMaskFromArray: temp_lmask allocate"
          return
        endif
        do i = 1, n - 1
          temp_lmask(i) = physgrid%lmask(i)
        enddo
        deallocate(physgrid%lmask, stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetLMaskFromArray: lmask deallocate"
          return
        endif
        allocate(physgrid%lmask(n), stat=status)  ! TODO: is this necessary?
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetLMaskFromArray: lmask allocate"
          return
        endif
      endif
      temp_lmask(n) = lmask_array
      physgrid%lmask_names(n) = name
      physgrid%lmask => temp_lmask

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetLMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetMMask - Gets a defined multiplicative mask from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetMMask(physgrid, mmask_array, name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(in) :: physgrid

      type(ESMF_LocalArray), pointer :: mmask_array

      character(*), intent(in), optional :: name

      integer, intent(in), optional :: id

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Extracts multiplicative mask information from a {\tt ESMF\_PhysGrid} structure.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} containing requested mask.
!     \item[mmask\_array]
!          Pointer to the requested multiplicative mask.
!     \item[[name]]
!          Name assigned to this mmask when it was defined.
!     \item[[id]]
!          Integer id assigned to the mmask when it was defined.
!          Either the name or id must be supplied.  The id permits
!          more direct access to the mmask.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i
      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      if (present(id)) then
         ! get mmask directly

         if (id > 0 .and. id <= physgrid%num_mmasks) then
            mmask_array => physgrid%mmask(id)
         else
            print *,'Invalid mmask id'
            status = ESMF_FAILURE
         endif

      else if (present(name)) then
         srch_loop: do i=1,physgrid%num_mmasks
            if (name == trim(physgrid%mmask_names(i))) exit srch_loop
         end do srch_loop

         if (i <= physgrid%num_mmasks) then
            mmask_array => physgrid%mmask(id)
         else
            print *,'No mmask of that name defined'
            status = ESMF_FAILURE
         endif
      else
         print *,'Must supply either name or id'
         status = ESMF_FAILURE
      endif

      if(rcpresent) rc = status

      end subroutine ESMF_PhysGridGetMMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetMMask - Sets multiplicative mask from input array

! !INTERFACE:
      subroutine ESMF_PhysGridSetMMask(physgrid, mmask_array, name, id, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(inout) :: physgrid

      type(ESMF_LocalArray), intent(in), target :: &
         mmask_array         ! array containing multiplicative mask value for each cell

      character(*), intent(in) :: name ! name to assign to multiplicative mask

      integer, intent(out) :: id  ! id assigned to multiplicative mask

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Assigns a multiplicative mask to a {\tt ESMF\_PhysGrid} using user-defined data 
!     sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which mask is to be added.
!     \item[lmask\_array]
!          Array containing multiplicative mask values at for each grid cell.
!     \item[name]
!          Name to assign to multiplicative mask.
!     \item[id]
!          Integer id assigned to multiplicative mask which allows direct 
!          reference into multiplicative mask array.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i, n
      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      type(ESMF_LocalArray), dimension(:), allocatable, target :: temp_mmask
                                             ! temporary array of mmasks

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     increment multiplicative mask counter and allocate memory
!
      physgrid%num_mmasks = physgrid%num_mmasks + 1
      n = physgrid%num_mmasks
      if(n .eq. 1) then
        allocate(physgrid%mmask(1), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMMaskFromArray: mmask allocate"
          return
        endif
      else
        allocate(temp_mmask(n), stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMMaskFromArray: temp_mmasks allocate"
          return
        endif
        do i = 1, n - 1
          temp_mmask(i) = physgrid%mmask(i)
        enddo
        deallocate(physgrid%mmask, stat=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMMaskFromArray: mmask deallocate"
          return
        endif
        allocate(physgrid%mmask(n), stat=status)  ! TODO: is this necessary?
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_PhysGridSetMMaskFromArray: mmask allocate"
          return
        endif
      endif
      temp_mmask(n) = mmask_array
      physgrid%mmask_names(n) = name
      physgrid%mmask => temp_mmask

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetMMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridGetRegionID - Gets a defined region id from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetRegionID(physgrid, &
                                          region_array, region_names, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(in) :: physgrid

      type(ESMF_LocalArray), pointer :: region_array

      character(*), dimension(:), intent(out) :: region_names

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Extracts region id information from a {\tt ESMF\_PhysGrid} structure.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} containing requested region id.
!     \item[region\_array]
!          Pointer to the array assigning a region ID to each grid point.
!     \item[region\_names]
!          Name assigned to each region number.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: i
      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      region_array => physgrid%region_id
      do i=1,physgrid%num_region_ids
         region_names(i) = physgrid%region_id_names(i)
      end do

      if(rcpresent) rc = status

      end subroutine ESMF_PhysGridGetRegionID

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridSetRegionID - Sets grid region id from input array

! !INTERFACE:
      subroutine ESMF_PhysGridSetRegionID(physgrid, &
                                          region_array, region_names, rc)
!
! !ARGUMENTS:

      type(ESMF_PhysGridType), intent(inout) :: physgrid

      type(ESMF_LocalArray), intent(in), target :: &
         region_array         ! array containing region ids for each cell

      character(*), dimension(:), intent(in), target :: &
         region_names         ! name to assign to each region id

      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Assigns region ids to a {\tt ESMF\_PhysGrid} using user-defined data 
!     sent in an input array.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          {\tt ESMF\_PhysGrid} for which region ids are to be defined.
!     \item[region\_array]
!          Array containing region id for each grid cell.
!     \item[region\_names]
!          Name to assign to each region.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: n
      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
!
!     increment logical mask counter
!
      physgrid%num_region_ids = size(region_names)
      !TODO: check for consistency with region ids in array
!
!     set pointers to region id
!
      physgrid%region_id_names => region_names
      physgrid%region_id => region_array

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetRegionID

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
!     \item[[opt]]
!          Print ptions that control the type of information and level of 
!          detail.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
      integer :: i

! This code will surely change, but for development purposes it
! is necessary to have some information available currently.

      print *, 'Physgrid:'
      print *, ' Dimensions:', physgrid%ptr%dim_num

      print *, ' Coordinate Extents:'

! The global and local coordinate extents
      print *, '  Global Extents:'
      do i=1, physgrid%ptr%dim_num
         print *, '   ', physgrid%ptr%global_min(i), &
         ',', physgrid%ptr%global_max(i)
      if (i .ne. physgrid%ptr%dim_num) print *, '      x'
      enddo

      print *, '  Local Extents:'
      do i=1, physgrid%ptr%dim_num
         print *, '   ', physgrid%ptr%local_min(i), &
         ',', physgrid%ptr%local_max(i)
      if (i .ne. physgrid%ptr%dim_num) print *, '      x'
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
!      real(kind=ESMF_IKIND_R8), intent(in) :: &
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
!!     \item[[dis\_grid]
!!          {\tt ESMF\_DistGrid} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[[rc]]
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
!      real(kind=ESMF_IKIND_R8), intent(in) :: &
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
!!     \item[[dis\_grid]
!!          {\tt ESMF\_DistGrid} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[[rc]]
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
!      real(kind=ESMF_IKIND_R8), intent(in) :: &
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
!!     \item[[dis\_grid]
!!          {\tt ESMF\_DistGrid} describing distribution of {\tt ESMF\_PhysGrid} above.
!!     \item[[rc]]
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
      function ESMF_PhysGridPointInCell(point, corner_x, corner_y, rc)

!
! !RETURN VALUE:
      logical :: ESMF_PhysGridPointInCell ! true if point located in cell
!
! !ARGUMENTS:
      real(kind=ESMF_IKIND_R8), dimension(2), intent(in) :: &
         point          ! x,y coordinates of search point 

      real(kind=ESMF_IKIND_R8), dimension(:), intent(in) :: &
         corner_x,     & ! x coordinates of cell corners
         corner_y        ! y coordinates of cell corners 

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
!     \item[point]
!          Coordinates of search point.
!     \item[corner\_x]
!          x-coordinate of grid cell corners.
!     \item[corner\_y]
!          y-coordinate of grid cell corners.
!     \item[ESMF\_PhysGridPointInCell]
!          return value = 1 if cell contains point.
!     \item[[rc]]
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

   real(kind=ESMF_IKIND_R8) :: &
      vec1_x, vec1_y,  &! components of the cell side vector
      vec2_x, vec2_y,  &! components of the vector from vertex to point
      cross_product,   &! cross product of two vectors
      test_product,    &! 
      ref_product,     &! the cross product for first non-zero value
      sign_test,       &! test to see if cross products are same sign
      zero, one

!
!     set default return value
!

      ESMF_PhysGridPointInCell = .false.
!
!     set constants
!
      zero = 0.d0
!
!     perform the cross product for each cell side
!

      num_corners = size(corner_x)

      corner_loop: do ncorn=1,num_corners
         next_n = MOD(ncorn,num_corners) + 1

!
!        here we take the cross product of the vector making
!        up each cell side with the vector formed by the vertex
!        and search point.  if all the cross products are
!        the same sign, the point is contained in the cell.
!

         vec1_x = corner_x(next_n) - corner_x(ncorn)
         vec1_y = corner_y(next_n) - corner_y(ncorn)
         vec2_x = point(1) - corner_x(ncorn)
         vec2_y = point(2) - corner_y(ncorn)

!
!        if search point coincident with vertex
!        then cell contains the point
!

         if (vec2_x == 0 .and. vec2_y == 0) then
            ESMF_PhysGridPointInCell = .true.
            exit corner_loop
         endif

!
!        if cell side has zero length (degenerate vertices)
!         then skip the side and move on to the next
!

         if (vec1_x == 0 .and. vec1_y == 0) cycle corner_loop

!        compute cross product

         cross_product = vec1_x*vec2_y - vec2_x*vec1_y

!
!        if the cross product is zero, the point
!        lies exactly on the side and is contained in the cell
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
! !IROUTINE: ESMF_PhysGridCompDistSpherical - Distance in spherical coords
!
! !INTERFACE:
      function ESMF_PhysGridCompDistSpherical(x1, y1, x2, y2, rc)

!
! !RETURN VALUE:
      real(kind=ESMF_IKIND_R8) :: ESMF_PhysGridCompDistSpherical
!
! !ARGUMENTS:
      real(kind=ESMF_IKIND_R8), intent(in) :: &
         x1, y1,        & ! x,y coordinates of points between which to 
         x2, y2           !   compute distance: x=longitude(deg), y=lat (deg)

      integer, intent(out) :: rc  ! return code
!
! !DESCRIPTION:
!     This routine computes the distance between two points defined by
!     the input coordinates.  This version computes the angular distance
!     in spherical coordinates given longitude, latitude in degrees.
!
!     The arguments are:
!     \begin{description}
!     \item[x1,y1,x2,y2]
!          Coordinates of points between which distance is computed.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!     local variables
!

      real(kind=ESMF_IKIND_R8) :: &
         lon1, lat1, lon2, lat2  ! lon/lat in radians
      real(kind=ESMF_IKIND_R8) :: pi

!
!     initialize return code
!

      rc = ESMF_FAILURE
!
!     set constants
!
      pi = 3.1416d0   ! TODO really set pi, just a bug fix for now
!
!     convert input coordinates to radians
!

      lon1 = x1*pi/180.d0
      lat1 = y1*pi/180.d0
      lon2 = x2*pi/180.d0
      lat2 = y2*pi/180.d0

!
!     compute angular distance
!

      ESMF_PhysGridCompDistSpherical = &
         acos( cos(lat1)*cos(lat2)*cos(lon1)*cos(lon2) + &
               cos(lat1)*cos(lat2)*sin(lon1)*sin(lon2) + &
               sin(lat1)*sin(lat2) )

!
!     set return code
!

      rc = ESMF_SUCCESS

      end function ESMF_PhysGridCompDistSpherical

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_PhysGridCompDistCartesian - Distance in Cartesian coords
!
! !INTERFACE:
      function ESMF_PhysGridCompDistCartesian(x1, y1, x2, y2, rc)

!
! !RETURN VALUE:
      real(kind=ESMF_IKIND_R8) :: ESMF_PhysGridCompDistCartesian
!
! !ARGUMENTS:
      real(kind=ESMF_IKIND_R8), intent(in) :: &
         x1, y1,        & ! x,y coordinates of points between which to 
         x2, y2           !   compute distance

      integer, intent(out) :: rc  ! return code
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
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

!
!     initialize return code
!

      rc = ESMF_FAILURE

!
!     compute distance using the usual Cartesian formula
!

      ESMF_PhysGridCompDistCartesian = sqrt( (x2-x1)**2 + (y2-y1)**2 )

!
!     set return code
!

      rc = ESMF_SUCCESS

      end function ESMF_PhysGridCompDistCartesian

!------------------------------------------------------------------------------

      end module ESMF_PhysGridMod
