! $Id: ESMF_Grid.F90,v 1.7 2002/11/20 22:26:39 jwolfe Exp $
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
#include <ESMF_Grid.h>
#include <ESMF_Macros.inc>
!==============================================================================
!BOP
! !MODULE: ESMF_GridMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt Class> class ...
!
! < Insert a paragraph or two explaining the function of this class. >
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_PhysGridMod    ! ESMF physical grid class
!jw   use ESMF_VertGridMod    ! ESMF base class
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
!     !  ESMF_PhysGridSpec
!
!     ! Definition for a PhysGridSpec class.

      type ESMF_PhysGridSpec
      sequence
      private
        
        type (ESMF_Base) :: base         ! base class object
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
        real, dimension(:), pointer :: center_x   ! x-coord of center of
                                                  ! each cell
        real, dimension(:), pointer :: center_y   ! y-coord of center of
                                                  ! each cell
        real, dimension(:,:), pointer :: corner_x ! x-coord of each corner
                                                  ! of each cell
        real, dimension(:,:), pointer :: corner_y ! y-coord of each corner
                                                  ! of each cell
        real, dimension(:,:), pointer :: face_x   ! x-coord of each face
                                                  ! center of each cell
        real, dimension(:,:), pointer :: face_y   ! y-coord of each face
                                                  ! center of each cell
        real, dimension(:,:), pointer :: metrics  ! an array of defined metrics
                                                  ! for each cell
        character (len=ESMF_MAXSTR), dimension(:), pointer :: metric_names
        logical, dimension(:,:), pointer :: lmask ! an array of defined logical
                                                  ! masks for each cell
        real, dimension(:,:), pointer :: mmask    ! an array of defined 
                                                  ! multiplicative masks for
                                                  ! each cell
        integer, dimension(:,:), pointer :: region_id ! an array of defined
                                                      ! region identifiers
                                                      ! for each cell

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
        type (ESMF_Status) :: gridstatus             ! uninitialized, init ok, etc
        character (len=ESMF_MAXSTR) :: gridtype      ! identifier for type of grid
        character (len=ESMF_MAXSTR) :: stagger       ! grid staggering
        character (len=ESMF_MAXSTR) :: coord_system  ! physical coordinate system
        character (len=ESMF_MAXSTR) :: coord_order   ! mapping of xyz to ijk
!jw     integer :: gridtype                          ! TODO better as an enum?
!jw     integer :: stagger                           ! TODO better as an enum?
!jw     integer :: coord_system                      ! TODO better as an enum?
!jw     integer :: coord_order                       ! TODO better as an enum?
        integer :: num_subgrids                      ! number of grid descriptors
                                                     ! necessary to support
                                                     ! staggering
        real :: global_min_x                         ! global extents
        real :: global_max_x                         ! global extents
        real :: global_min_y                         ! global extents
        real :: global_max_y                         ! global extents
        type (ESMF_PhysGridSpec), dimension(:), pointer :: subgrid  !
        type (ESMF_PhysGrid), pointer :: physgrid  !
!jw     type (ESMF_VertGrid), pointer :: vertgrid    !
        type (ESMF_DistGrid), pointer :: distgrid    !

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
! !PUBLIC TYPES:
      public ESMF_GridConfig
      public ESMF_Grid
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
!  Pick one or the other of the init/create sections depending on
!  whether this is a deep class (the class/derived type has pointers to
!  other memory which must be allocated/deallocated) or a shallow class
!  (the class/derived type is self-contained) and needs no destroy methods
!  other than deleting the memory for the object/derived type itself.

! the following routines apply to deep classes only
    public ESMF_GridCreate                 ! interface only, deep class
    public ESMF_GridDestroy                ! interface only, deep class

!jw public ESMF_GridGetConfig
!jw public ESMF_GridSetConfig
!jw public ESMF_GridGetValue               ! Get<Value>
!jw public ESMF_GridSetValue               ! Set<Value>
 
!jw public ESMF_GridValidate
!jw public ESMF_GridPrint
 
! < list the rest of the public interfaces here >
!
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.7 2002/11/20 22:26:39 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_GridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridCreateNew

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

!    < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Grid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_GridCreateNew - Create a new Grid

! !INTERFACE:
      function ESMF_GridCreateNew(name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateNew
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc               

!     integer, intent(in) :: arg1                        
!     integer, intent(in) :: arg2                        
!     character (len = *), intent(in), optional :: arg3  
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt Grid} object and constructs its
!     internals.  Return a pointer to a new {\tt Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]] 
!          {\tt Grid} name.
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

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateNew%ptr)

!     Initialize return code  
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateNew: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateNew: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateNew%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_GridDestroy - Free all resources associated with a Grid 

! !INTERFACE:
      subroutine ESMF_GridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt Grid} object previously allocated
!     via an {\tt ESMF_GridCreate routine}.
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
! !IROUTINE: 
!     ESMF_GridConstructNew - Construct the internals of an allocated Grid

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
!     as needed.  Must call the corresponding ESMF_GridDestruct
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
! !IROUTINE: 
!     ESMF_GridDestruct - Free any Grid memory allocated internally

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
!     {\tt ESMF_GridDestruct}.  
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
! !IROUTINE: 
!     ESMF_GridGetConfig - Get configuration information from a Grid

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
! !IROUTINE: 
!     ESMF_GridSetConfig - Set configuration information for a Grid

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
! !IROUTINE: 
!     ESMF_GridGetValue - Get <Value> for a Grid

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
! !IROUTINE: 
!     ESMF_GridSetValue - Set <Value> for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetValue(Grid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a Grid attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[grid] 
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
      end subroutine ESMF_GridSetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_GridValidate - Check internal consistency of a Grid

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
! !IROUTINE: 
!     ESMF_GridPrint - Print the contents of a Grid

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
      end subroutine ESMF_GridPrint

!------------------------------------------------------------------------------

      end module ESMF_GridMod
