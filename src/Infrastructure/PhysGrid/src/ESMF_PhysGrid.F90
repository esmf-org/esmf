! $Id: ESMF_PhysGrid.F90,v 1.10 2002/12/13 18:52:27 jwolfe Exp $
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
#include "ESMF_Macros.inc"
!==============================================================================
!BOP
! !MODULE: ESMF_PhysGridMod - Physical properties of Grid
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt PhysGrid} class and is responsible
! for computing or initializing physical properties of grids.   Such
! properties include coordinate information necessary for describing grids,
! metric information for grid distances, grid masks and assignment of
! region identifiers to grids.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod    ! ESMF base class
      use ESMF_ArrayMod
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

        integer :: num_corners           ! number of corners for
                                         ! each grid cell
        integer :: num_faces             ! likely assume same as num_corners
                                         ! but might specify storage of only
                                         ! 2 of 4 faces, for example
        integer :: num_metrics           ! a counter for the number of
                                         ! metrics for this subgrid
        integer :: num_lmasks            ! a counter for the number of
                                         ! logical masks for this subgrid
        integer :: num_mmasks            ! a counter for the number of
                                         ! multiplicative masks for this
                                         ! subgrid
        integer :: num_region_ids        ! a counter for the number of
                                         ! region identifiers for this
                                         ! subgrid
        integer :: global_max_i          ! cell count extents in 1st coord
                                         ! direction
        integer :: global_max_j          ! cell count extents in 2nd coord
                                         ! direction
        real :: global_min_coord1        ! coordinate extents in 1st coord
        real :: global_max_coord1        !    direction
        real :: global_min_coord2        ! coordinate extents in 2nd coord
        real :: global_max_coord2        !    direction
        type (ESMF_Array) :: center_coord1   ! coord of center of
                                             ! each cell in 1st direction
        type (ESMF_Array) :: center_coord2   ! coord of center of
                                             ! each cell in 2nd direction
        type (ESMF_Array) :: corner_coord1   ! coord of corner of
                                             ! each cell in 1st direction
        type (ESMF_Array) :: corner_coord2   ! coord of corner of
                                             ! each cell in 2nd direction
        type (ESMF_Array) :: face_coord1     ! coord of face center of
                                             ! each cell in 1st direction
        type (ESMF_Array) :: face_coord2     ! coord of face center of
                                             ! each cell in 2nd direction
        type (ESMF_Array) :: metrics         ! an array of defined metrics
                                             ! for each cell
        character (len=ESMF_MAXSTR), dimension(:), pointer :: metric_names
        type (ESMF_Array) :: lmask     ! an array of defined logical
                                       ! masks for each cell
        type (ESMF_Array) :: mmask     ! an array of defined
                                       ! multiplicative masks for each cell
        type (ESMF_Array) :: region_id ! an array of defined region identifiers
                                       ! for each cell

      end type

!------------------------------------------------------------------------------
!     !  ESMF_PhysGrid
!
!     !  The PhysGrid data structure that is passed between languages.

      type ESMF_PhysGrid
      sequence
      private
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
!   public ESMF_PhysGridGetCoord
   public ESMF_PhysGridSetCoord
!   public ESMF_PhysGridGetMetric
!   public ESMF_PhysGridSetMetric
!   public ESMF_PhysGridGetLMask
!   public ESMF_PhysGridSetLMask
!   public ESMF_PhysGridGetMMask
!   public ESMF_PhysGridSetMMask
!   public ESMF_PhysGridGetRegionID
!   public ESMF_PhysGridSetRegionID
   public ESMF_PhysGridValidate
   public ESMF_PhysGridPrint
 
!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

   integer, parameter ::                     &! internally-recognized grid metrics
      ESMF_GridMetric_Unknown          =  0, &! unknown or undefined metric
      ESMF_GridMetric_Area             =  1, &! area of grid cell
      ESMF_GridMetric_Volume           =  2, &! volume of 3-d grid cell
      ESMF_GridMetric_NorthFaceLength  =  3, &! length of horiz grid cell along north face
      ESMF_GridMetric_EastFaceLength   =  4, &! length of horiz grid cell along east  face
      ESMF_GridMetric_WestFaceLength   =  5, &! length of horiz grid cell along west  face
      ESMF_GridMetric_SouthFaceLength  =  6, &! length of horiz grid cell along south face
      ESMF_GridMetric_NNbrDist         =  7, &! cell center to north nbr center dist
      ESMF_GridMetric_ENbrDist         =  8, &! cell center to east  nbr center dist
      ESMF_GridMetric_WNbrDist         =  9, &! cell center to west  nbr center dist
      ESMF_GridMetric_SNbrDist         = 10, &! cell center to south nbr center dist
      ESMF_GridMetric_NFaceDist        = 12, &! cell center to north face distance
      ESMF_GridMetric_EFaceDist        = 13, &! cell center to east  face distance
      ESMF_GridMetric_WFaceDist        = 14, &! cell center to west  face distance
      ESMF_GridMetric_SFaceDist        = 15   ! cell center to south face distance

!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_PhysGrid.F90,v 1.10 2002/12/13 18:52:27 jwolfe Exp $'

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
!        module procedure ESMF_PhysGridCreateStagger
!        module procedure ESMF_PhysGridCreateRead
!        module procedure ESMF_PhysGridCreateCopy
!        module procedure ESMF_PhysGridCreateCutout
!        module procedure ESMF_PhysGridCreateChangeResolution
!        module procedure ESMF_PhysGridCreateExchange

! !DESCRIPTION:
!     This interface provides a single entry point for PhysGrid create
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
!        module procedure ESMF_PhysGridSetCoordFromArray
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
!        module procedure ESMF_PhysGridSetMetricFromArray
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

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct
!     a complete {\tt PhysGrid}.
!
!EOP
      end interface 
!
!==============================================================================

      contains

!==============================================================================
!
! This section includes the PhysGrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridCreateNew - Create a new PhysGrid

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
!     Allocates memory for a new {\tt PhysGrid} object and constructs its
!     internals.  Returns a pointer to a new {\tt PhysGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt PhysGrid} name.
!     \item[arg2]
!          Argument 2.         
!     \item[[arg3]] 
!          Argument 3.
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
      call ESMF_PhysGridConstructNew(physgrid, name, status)
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
! !IROUTINE: 
!     ESMF_PhysGridCreateInternal - Create a new PhysGrid internally

! !INTERFACE:
      function ESMF_PhysGridCreateInternal(name, global_max_i, global_max_j, &
                                           global_min_coord1, &
                                           global_max_coord1, &
                                           global_min_coord2, &
                                           global_max_coord2, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridCreateInternal
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      integer, intent(in) :: global_max_i
      integer, intent(in) :: global_max_j
      real, intent(in) :: global_min_coord1
      real, intent(in) :: global_max_coord1
      real, intent(in) :: global_min_coord2
      real, intent(in) :: global_max_coord2
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt PhysGrid} object, constructs its
!     internals, and internally generates the {\tt PhysGrid}.  Returns a
!     pointer to the new {\tt PhysGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]]
!          {\tt PhysGrid} name.
!     \item[[global\_max\_i]]
!          Number of grid increments in the 1st coordinate direction.
!     \item[[global\_max\_j]]
!          Number of grid increments in the 2nd coordinate direction.
!     \item[[global\_min\_coord1]]
!          Minimum physical coordinate in the 1st coordinate direction.
!     \item[[global\_max\_coord1]]
!          Maximum physical coordinate in the 1st coordinate direction.
!     \item[[global\_min\_coord2]]
!          Minimum physical coordinate in the 2nd coordinate direction.
!     \item[[global\_max\_coord2]]
!          Maximum physical coordinate in the 2nd coordinate direction.
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
      call ESMF_PhysGridConstructNew(physgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridCreateInternal: PhysGrid construct"
        return
      endif

!     Set physgrid attributes from function arguments
      physgrid%global_max_i = global_max_i
      physgrid%global_max_j = global_max_j
      physgrid%global_min_coord1 = global_min_coord1
      physgrid%global_max_coord1 = global_max_coord1
      physgrid%global_min_coord2 = global_min_coord2
      physgrid%global_max_coord2 = global_max_coord2

!     Set return values.
      ESMF_PhysGridCreateInternal%ptr => physgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_PhysGridCreateInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridDestroy - Free all resources associated with a PhysGrid 

! !INTERFACE:
      subroutine ESMF_PhysGridDestroy(physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt PhysGrid} object previously allocated
!     via an {\tt ESMF_PhysGridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
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
      end subroutine ESMF_PhysGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridConstructNew - Construct the internals of an allocated
!                                 PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridConstructNew(physgrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGridType), intent(in) :: physgrid  
      character (len = *), intent(in), optional :: name  ! name
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt PhysGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF_PhysGridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_PhysGridCreate}, which calls
!     {\tt ESMF\_PhysGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Pointer to a {\tt PhysGrid}.
!     \item[arg1]
!          Argument 1.
!     \item[arg2]
!          Argument 2.         
!     \item[[name]] 
!          Argument 3.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
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

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridConstructNew: PhysGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridDestruct - Free any PhysGrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_PhysGridDestruct(physgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_PhysGridConstruct}, does any additional cleanup before the
!     original PhysGrid object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_PhysGridDestroy}, which calls
!     {\tt ESMF_PhysGridDestruct}.  
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

!
!  code goes here
!
      end subroutine ESMF_PhysGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridGetConfig - Get configuration information from a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetConfig(physgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the PhysGrid object was configured with.
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
! !IROUTINE: 
!     ESMF_PhysGridSetConfig - Set configuration information for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSetConfig(physgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the PhysGrid object with set of resources given.
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
! !IROUTINE: 
!     ESMF_PhysGridGetValue - Get <Value> for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridGetValue(physgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of PhysGrid attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
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
      end subroutine ESMF_PhysGridGetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridSetValue - Set <Value> for a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridSetValue(PhysGrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a PhysGrid attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid] 
!          Class to be modified.
!     \item[value]
!          Value to be set.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_PhysGridSetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridSetCoordInternal - Set Coords for a PhysGrid internally

! !INTERFACE:
      subroutine ESMF_PhysGridSetCoordInternal(physgrid, coord_id, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid) :: physgrid
      integer, intent(in) :: coord_id
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Compute a PhysGrid's coordinates from a given gridtype and set of
!     physical parameters.
!
!     The arguments are:
!     \begin{description}
!     \item[physgrid]
!          Class to be modified.
!     \item[[coord_id]]
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

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     For now, assume just an x-y grid on one processor.  TODO: decide
!     whether each gridtype requires a separate subroutine or put it all here
!     call LayoutGetDEPosition(PE_id)
!     PE_id = 1


      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_PhysGridConstructNew: PhysGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_PhysGridSetCoordInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_PhysGridValidate - Check internal consistency of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridValidate(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a PhysGrid is internally consistent.
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
! !IROUTINE: 
!     ESMF_PhysGridPrint - Print the contents of a PhysGrid

! !INTERFACE:
      subroutine ESMF_PhysGridPrint(physgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: physgrid      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a PhysGrid.  
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

!
!  code goes here
!
      end subroutine ESMF_PhysGridPrint

!------------------------------------------------------------------------------

      end module ESMF_PhysGridMod
