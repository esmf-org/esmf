! $Id: ESMF_DistGrid.F90,v 1.12 2002/12/31 17:08:34 jwolfe Exp $
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
!     ESMF DistGrid Module
      module ESMF_DistGridMod
!
!==============================================================================
!
! This file contains the DistGrid class definition and all DistGrid class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_DistGrid.h"
#include "ESMF_Macros.inc"
!==============================================================================
!BOP
! !MODULE: ESMF_DistGridMod - contains Grid decompostion methods
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt DistGrid} class, which contains a
! collection of subgrids which constitute a single logical {\tt Grid}. The
! subgrids can be operated on in parallel on a multiprocessor machine. The
! {\tt DistGrid} class contains the mapping between the local grid 
! decompositions and the global logical {\tt Grid}. It contains methods to
! synchronize data values between the boundaries of subsets, and to collect
! and communicate global data values. It interacts closely with the
! {\tt PhysGrid} object.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_ArrayMod
      use ESMF_BaseMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_DistGridConfig
!
!     ! Description of ESMF_DistGridConfig

      type ESMF_DistGridConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     ! ESMF_DecompAxis
!
!     ! Description of ESMF_DecompAxis

      type ESMF_DecompAxis
      sequence
!     private
        integer :: start
        integer :: end
        integer :: size
      end type

!------------------------------------------------------------------------------
!     ! ESMF_Decomp
!
!     ! Description of ESMF_Decomp

      type ESMF_Decomp
      sequence
      private
        type (ESMF_DecompAxis) :: DEdir1   ! axis decomposition in 1st dir
        type (ESMF_DecompAxis) :: DEdir2   ! axis decomposition in 2nd dir
        logical ::  periodic_dir1          ! periodic boundary in 1st dir
        logical ::  periodic_dir2          ! periodic boundary in 2nd dir
        integer :: num_masks               ! number of decomposition masks
        type (ESMF_Array) :: mask          ! decomposition masks
      end type

!------------------------------------------------------------------------------
!     ! ESMF_MyDE
!
!     ! Description of ESMF_MyDE

      type ESMF_MyDE
      sequence
!     private
        integer :: MyDE      ! identifier for this DE
        integer :: DE_East   ! identifier for DE to the east
        integer :: DE_West   ! identifier for DE to the west
        integer :: DE_North  ! identifier for DE to the north
        integer :: DE_South  ! identifier for DE to the south
        integer :: DE_NE     ! identifier for DE to the northeast
        integer :: DE_NW     ! identifier for DE to the northwest
        integer :: DE_SE     ! identifier for DE to the southeast
        integer :: DE_SW     ! identifier for DE to the southwest
        integer :: lsize     ! local (on this DE) number of cells
        type (ESMF_DecompAxis) :: n_dir1  ! global cell count in 1st dir
        type (ESMF_DecompAxis) :: n_dir2  ! global cell count in 2nd dir
      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGridType
!
!     !  Description of ESMF_DistGrid. 
!     !  WARNING!  this is not complete -- there is no halo functionality

      type ESMF_DistGridType
      sequence
!     private
        type (ESMF_Base) :: base
!       type (ESMF_Layout), dimension(:), pointer :: layouts
        type (ESMF_Decomp) :: decomp       ! DE decomposition object
        type (ESMF_MyDE) :: MyDE           ! local DE identifiers
        integer :: gsize                   ! global number of cells
        integer :: gsize_dir1              ! global number of cells in 1st dir
        integer :: gsize_dir2              ! global number of cells in 2nd dir
        type (ESMF_Array) :: DEids         ! array of all DE identifiers
        type (ESMF_DecompAxis), dimension(:), pointer :: local_dir1
        type (ESMF_DecompAxis), dimension(:), pointer :: local_dir2
        type (ESMF_DecompAxis), dimension(:), pointer :: global_dir1
        type (ESMF_DecompAxis), dimension(:), pointer :: global_dir2
        type (ESMF_DecompAxis), dimension(:), pointer :: memory_dir1
        type (ESMF_DecompAxis), dimension(:), pointer :: memory_dir2
        integer :: maxsize_dir1            ! maximum DE cell count in 1st dir
        integer :: maxsize_dir2            ! maximum DE cell count in 2nd dir
        logical :: covers_domain_dir1      ! identifiers if distgrid covers
        logical :: covers_domain_dir2      ! the entire physical domain
      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGrid
!
!     !  The DistGrid data structure that is passed between languages.

      type ESMF_DistGrid
      sequence
!     private
        type (ESMF_DistGridType), pointer :: ptr     ! pointer to a distgrid
                                                     ! type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_DistGridConfig
      public ESMF_MyDE
      public ESMF_DecompAxis
      public ESMF_Decomp
      public ESMF_DistGrid
      public ESMF_DistGridType
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
    public ESMF_DistGridCreate
    public ESMF_DistGridDestroy
    public ESMF_DistGridGetConfig
    public ESMF_DistGridSetConfig
    public ESMF_DistGridGetInfo
    public ESMF_DistGridSetInfo
    public ESMF_DistGridGetCounts
    public ESMF_DistGridSetCounts
    public ESMF_DistGridSetDecomp
    public ESMF_DistGridGetValue
    public ESMF_DistGridSetValue
    public ESMF_DistGridValidate
    public ESMF_DistGridPrint
 
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DistGrid.F90,v 1.12 2002/12/31 17:08:34 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_DistGridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridCreateEmpty
         module procedure ESMF_DistGridCreateInternal
!        module procedure ESMF_DistGridCreateCopy

! !DESCRIPTION:
!     This interface provides a single entry point for DistGrid create
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_DistGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridConstructNew
         module procedure ESMF_DistGridConstructInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct
!     a complete {\tt DistGrid}.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_DistGridSetCounts

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridSetCountsInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     extent counts in a {\tt DistGrid}.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_DistGridSetDecomp

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridSetDecompInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     extent counts in a {\tt DistGrid}.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!==============================================================================

      contains

!==============================================================================
!
! This section includes the DistGrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridCreateEmpty - Create a new DistGrid with no data

! !INTERFACE:
      function ESMF_DistGridCreateEmpty(name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateEmpty
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates memory for a new {\tt DistGrid} object and constructs its
!     internals.  Returns a pointer to a new {\tt DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]] 
!          {\tt DistGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_DistGridType), pointer :: distgrid  ! Pointer to new distgrid
      integer :: status=ESMF_FAILURE                ! Error status
      logical :: rcpresent=.FALSE.                  ! Return code present

!     Initialize pointers
      nullify(distgrid)
      nullify(ESMF_DistGridCreateEmpty%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(distgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateEmpty: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstructNew(distgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateEmpty: DistGrid construct"
        return
      endif

!     Set return values.
      ESMF_DistGridCreateEmpty%ptr => distgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateEmpty

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridCreateInternal - Create a new DistGrid internally

! !INTERFACE:
      function ESMF_DistGridCreateInternal(nDE_i, nDE_j, i_max, j_max, &
                                           name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateInternal
!
! !ARGUMENTS:
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates memory for a new {\tt DistGrid} object, constructs its
!     internals, and internally sets necessary attributes and values.
!     Returns a pointer to a new {\tt DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[nDE\_i]] 
!          Number of distributed elements (DE's) in the 1st direction.
!     \item[[nDE\_j]] 
!          Number of distributed elements (DE's) in the 2nd direction.
!     \item[[i\_max]] 
!          Global number of computation cells in the 1st direction.
!     \item[[j\_max]] 
!          Global number of computation cells in the 2nd direction.
!     \item[[name]] 
!          {\tt DistGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_DistGridType), pointer :: distgrid  ! Pointer to new distgrid
      integer :: status=ESMF_FAILURE                ! Error status
      logical :: rcpresent=.FALSE.                  ! Return code present

!     Initialize pointers
      nullify(distgrid)
      nullify(ESMF_DistGridCreateInternal%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(distgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateInternal: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstructInternal(distgrid, name, nDE_i, nDE_j, i_max, &
                                          j_max, rc)

!     Set return values.
      ESMF_DistGridCreateInternal%ptr => distgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridDestroy - Free all resources associated with a DistGrid 

! !INTERFACE:
      subroutine ESMF_DistGridDestroy(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt DistGrid} object previously allocated
!     via an {\tt ESMF_DistGridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridConstructNew - Construct the internals of an allocated DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridConstructNew(distgrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid 
      character (len = *), intent(in), optional :: name  ! name
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF_DistGridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Pointer to a {\tt DistGrid}.
!     \item[[name]] 
!          Name of the {\tt DistGrid}.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_SUCCESS               ! Error status
      logical :: rcpresent=.FALSE.                 ! Return code present
      character (len = ESMF_MAXSTR) :: defaultname ! default distgrid name

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Set the DistGrid name if present, otherwise construct a default one
      if(present(name)) then
        call ESMF_SetName(distgrid%base, name, status)
      else
        defaultname = "default_distgrid_name"
        call ESMF_SetName(distgrid%base, defaultname, status)
      endif
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructNew: Setname"
        return
      endif

!     Initialize distgrid contents
      distgrid%decomp%DEdir1%start = 0
      distgrid%decomp%DEdir1%end = 0
      distgrid%decomp%DEdir1%size = 0
      distgrid%decomp%DEdir2%start = 0
      distgrid%decomp%DEdir2%end = 0
      distgrid%decomp%DEdir2%size = 0
      distgrid%decomp%periodic_dir1 = .false.
      distgrid%decomp%periodic_dir2 = .false.
      distgrid%decomp%num_masks = 0
      distgrid%MyDE%MyDE = 0
      distgrid%MyDE%DE_East = 0
      distgrid%MyDE%DE_West = 0
      distgrid%MyDE%DE_North = 0
      distgrid%MyDE%DE_South = 0
      distgrid%MyDE%DE_NE = 0
      distgrid%MyDE%DE_NW = 0
      distgrid%MyDE%DE_SE = 0
      distgrid%MyDE%DE_SW = 0
      distgrid%MyDE%lsize = 0
      distgrid%MyDE%n_dir1%start = 0
      distgrid%MyDE%n_dir1%end = 0
      distgrid%MyDE%n_dir1%size = 0
      distgrid%MyDE%n_dir2%start = 0
      distgrid%MyDE%n_dir2%end = 0
      distgrid%MyDE%n_dir2%size = 0
      distgrid%gsize = 0
      distgrid%gsize_dir1 = 0
      distgrid%gsize_dir2 = 0
      distgrid%local_dir1%start = 0
      distgrid%local_dir1%end = 0
      distgrid%local_dir1%size = 0
      distgrid%local_dir2%start = 0
      distgrid%local_dir2%end = 0
      distgrid%local_dir2%size = 0
      distgrid%global_dir1%start = 0
      distgrid%global_dir1%end = 0
      distgrid%global_dir1%size = 0
      distgrid%global_dir2%start = 0
      distgrid%global_dir2%end = 0
      distgrid%global_dir2%size = 0
      distgrid%memory_dir1%start = 0
      distgrid%memory_dir1%end = 0
      distgrid%memory_dir1%size = 0
      distgrid%memory_dir2%start = 0
      distgrid%memory_dir2%end = 0
      distgrid%memory_dir2%size = 0
      distgrid%maxsize_dir1 = 0
      distgrid%maxsize_dir2 = 0
      distgrid%covers_domain_dir1 = .false.
      distgrid%covers_domain_dir2 = .false.

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridConstructInternal - Construct the internals of an allocated DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridConstructInternal(distgrid, name, nDE_i, nDE_j, &
                                                i_max, j_max, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid 
      character (len = *), intent(in), optional :: name  
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF_DistGridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Pointer to a {\tt DistGrid}.
!     \item[[name]] 
!          {\tt DistGrid} name.
!     \item[[nDE\_i]] 
!          Number of distributed elements (DE's) in the 1st direction.
!     \item[[nDE\_j]] 
!          Number of distributed elements (DE's) in the 2nd direction.
!     \item[[i\_max]] 
!          Global number of computation cells in the 1st direction.
!     \item[[j\_max]] 
!          Global number of computation cells in the 2nd direction.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: gsize_dir1
      integer :: gsize_dir2
      integer :: gsize
      logical :: cover_domain_dir1
      logical :: cover_domain_dir2

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      call ESMF_DistGridConstructNew(distgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: DistGrid construct"
        return
      endif

!     Fill in distgrid derived type with input or default
!     TODO:  temporary fix, also add defaults to objects
      call ESMF_DistGridSetInfo(distgrid, gsize_dir1=i_max, &
                                gsize_dir2=j_max, &
                                gsize=i_max*j_max, &
                                covers_domain_dir1=.true., &
                                covers_domain_dir2=.true., rc=status)

!     Create layout with specified decomposition (assume 1 in 3rd direction)
!     call ESMF_LayoutCreate(nDE_i, nDE_j, 1, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: Layout create"
        return
      endif

!     Set decomposition based on input numbers of processors
      call ESMF_DistGridSetDecomp(distgrid, nDE_i, nDE_j, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: Layout create"
        return
      endif

!     Parse problem size
      call ESMF_DistGridSetCounts(distgrid, nDE_i, nDE_j, i_max, j_max, &
                                  status)
        
!     Fill in DE derived type
!     call ESMF_DistGridSetDE(distgrid, status)    TODO
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: Set de"
        return
      endif

!     Calculate other distgrid values from DE information
!     distgrid%maxsize_dir1 = GlobalCommMax()
!     distgrid%maxsize_dir2 = GlobalCommMax()

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: DistGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridDestruct - Free any DistGrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_DistGridDestruct(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_DistGridConstruct}, does any additional cleanup before the
!     original DistGrid object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_DistGridDestroy}, which calls
!     {\tt ESMF_DistGridDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridGetInfo - Get information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetInfo(distgrid, &
                                      gsize_dir1, gsize_dir2, gsize, &
                                      covers_domain_dir1, &
                                      covers_domain_dir2, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(inout), optional :: gsize_dir1
      integer, intent(inout), optional :: gsize_dir2
      integer, intent(inout), optional :: gsize
      logical, intent(inout), optional :: covers_domain_dir1
      logical, intent(inout), optional :: covers_domain_dir2
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns information from the DistGrid object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[[gsize\_dir1]]
!          Global number of cells in 1st direction.
!     \item[[gsize\_dir2]]
!          Global number of cells in 2nd direction.
!     \item[[gsize]]
!          Global total number of cells.
!     \item[[covers\_domain\_dir1]]
!          Logical identifier if distgrid covers the entire physical domain
!          in the 1st direction.
!     \item[[covers\_domain\_dir2]]
!          Logical identifier if distgrid covers the entire physical domain
!          in the 2nd direction.
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

!     if present, get information from distgrid derived type
      if(present(gsize_dir1)) gsize_dir1 = distgrid%gsize_dir1
      if(present(gsize_dir2)) gsize_dir2 = distgrid%gsize_dir2
      if(present(gsize)) gsize = distgrid%gsize
      if(present(covers_domain_dir1)) &
                 covers_domain_dir1 = distgrid%covers_domain_dir1
      if(present(covers_domain_dir2)) &
                 covers_domain_dir2 = distgrid%covers_domain_dir2

      if(rcpresent) rc = ESMF_SUCCESS
!
      end subroutine ESMF_DistGridGetInfo

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridSetInfo - Set information about a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetInfo(distgrid, gsize_dir1, gsize_dir2, &
                                      gsize, covers_domain_dir1, &
                                      covers_domain_dir2, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in), optional :: gsize_dir1
      integer, intent(in), optional :: gsize_dir2
      integer, intent(in), optional :: gsize
      logical, intent(in), optional :: covers_domain_dir1
      logical, intent(in), optional :: covers_domain_dir2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Sets the DistGrid object with information given.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be set.
!     \item[[gsize\_dir1]]
!          Global number of cells in 1st direction.
!     \item[[gsize\_dir2]]
!          Global number of cells in 2nd direction.
!     \item[[gsize]]
!          Global total number of cells.
!     \item[[covers\_domain\_dir1]]
!          Logical identifier if distgrid covers the entire physical domain
!          in the 1st direction.
!     \item[[covers\_domain\_dir2]]
!          Logical identifier if distgrid covers the entire physical domain
!          in the 2nd direction.
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

!     if present, set information filling in distgrid derived type
      if(present(gsize_dir1)) distgrid%gsize_dir1 = gsize_dir1
      if(present(gsize_dir2)) distgrid%gsize_dir2 = gsize_dir2
      if(present(gsize)) distgrid%gsize = gsize
      if(present(covers_domain_dir1)) &
                 distgrid%covers_domain_dir1 = covers_domain_dir1
      if(present(covers_domain_dir2)) &
                 distgrid%covers_domain_dir2 = covers_domain_dir2

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetInfo

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridGetConfig - Get configuration information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetConfig(distgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the DistGrid object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridSetConfig - Set configuration information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetConfig(distgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the DistGrid object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridGetValue - Get <Value> for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetValue(distgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of DistGrid attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridGetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridSetValue - Set <Value> for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetValue(DistGrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a DistGrid attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridSetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridGetCounts - Get extent counts for a DistGrid for a given DE

! !INTERFACE:
      subroutine ESMF_DistGridGetCounts(distgrid, DE_id, &
                                        size_dir1, &
                                        global_start_dir1, &
                                        global_end_dir1, &
                                        size_dir2, &
                                        global_start_dir2, &
                                        global_end_dir2, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in) :: DE_id
      integer, intent(inout), optional :: size_dir1
      integer, intent(inout), optional :: global_start_dir1
      integer, intent(inout), optional :: global_end_dir1
      integer, intent(inout), optional :: size_dir2
      integer, intent(inout), optional :: global_start_dir2
      integer, intent(inout), optional :: global_end_dir2
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get DistGrid extent counts for a given DE
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[DE\_id]]
!          Given DE's identifier.
!     \item[[size\_dir1]]
!          Extent count in the 1st direction.
!     \item[[global\_start\_dir1]]
!          Starting extent count in the 1st direction, in global index.
!     \item[[global\_end\_dir1]]
!          Ending extent count in the 1st direction, in global index.
!     \item[[size\_dir2]]
!          Extent count in the 2nd direction.
!     \item[[global\_start\_dir2]]
!          Starting extent count in the 2nd direction, in global index.
!     \item[[global\_end\_dir2]]
!          Ending extent count in the 2nd direction, in global index.
!     \item[[rc]] 
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

!     Retrieve extent counts for each axis from the de identifier
!     TODO:  check validity of DE_id
      if(present(size_dir1)) size_dir1 = distgrid%global_dir1(DE_id)%size
      if(present(size_dir2)) size_dir2 = distgrid%global_dir2(DE_id)%size
      if(present(global_start_dir1)) &
                 global_start_dir1 = distgrid%global_dir1(DE_id)%start
      if(present(global_end_dir1)) &
                 global_end_dir1 = distgrid%global_dir1(DE_id)%end
      if(present(global_start_dir2)) &
                 global_start_dir2 = distgrid%global_dir2(DE_id)%start
      if(present(global_end_dir2)) &
                 global_end_dir2 = distgrid%global_dir2(DE_id)%end

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetCounts

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridSetCountsInternal - Set extent counts for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetCountsInternal(distgrid, nDE_i, nDE_j, &
                                                i_max, j_max, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set DistGrid extent counts
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[nDE\_i]]
!          Number of DE's in the 1st direction.
!     \item[[nDE\_j]]
!          Number of DE's in the 2nd direction.
!     \item[[i\_max]]
!          Number of cells in the 1st direction.
!     \item[[j\_max]]
!          Number of cells in the 2nd direction.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      integer :: i, j                                !
      integer :: ni, nj                              ! increment counters
      integer :: global_s, global_e                  ! global counters
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Set extent counts for each axis from the number of de's and number
!     of cells
!     TODO:  this is not quite the correct algorithm to distribute over a
!            large number of DE's along an axis -- it loads up the residual
!            to the last one instead of spreading it around.  But OK for now.
      ni = i_max/nDE_i
      global_s = 1
      global_e = 0
      do i = 1,nDE_i
        global_e = min(global_e + ni, i_max)
        distgrid%global_dir1(i)%start = global_s
        distgrid%global_dir1(i)%end = global_e
        distgrid%local_dir1(i)%start = 1
        distgrid%local_dir1(i)%end = global_e - global_s + 1
        global_s = global_e + 1
      enddo
      nj = j_max/nDE_j
      global_s = 1
      global_e = 0
      do j = 1,nDE_j
        global_e = min(global_e + nj, j_max)
        distgrid%global_dir2(j)%start = global_s
        distgrid%global_dir2(j)%end = global_e
        distgrid%local_dir2(j)%start = 1
        distgrid%local_dir2(j)%end = global_e - global_s + 1
        global_s = global_e + 1
      enddo

      end subroutine ESMF_DistGridSetCountsInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridSetDEInternal - Set DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetDEInternal(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a DistGrid attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[rc]] 
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

      end subroutine ESMF_DistGridSetDEInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridSetDecompInternal - Set decomposition for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetDecompInternal(DistGrid, nDE_i, nDE_j, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a DistGrid decomposition object corresponding to the given number
!     of DE's per decomposition axis.  The number of DE's for either axis
!     may be one to set a 1-D decomposition.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[nDE\_i]]
!          Number of DE's in the 1st direction.
!     \item[[nDE\_j]]
!          Number of DE's in the 2nd direction.
!     \item[[rc]] 
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

      distgrid%decomp%DEdir1%start = 1
      distgrid%decomp%DEdir1%end = nDE_i
      distgrid%decomp%DEdir1%size = nDE_i
      distgrid%decomp%DEdir2%start = 1
      distgrid%decomp%DEdir2%end = nDE_j
      distgrid%decomp%DEdir2%size = nDE_j
      distgrid%decomp%periodic_dir1 = .false.
      distgrid%decomp%periodic_dir2 = .false.
      distgrid%decomp%num_masks = 0

      end subroutine ESMF_DistGridSetDecompInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridValidate - Check internal consistency of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridValidate(distgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a DistGrid is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_DistGridPrint - Print the contents of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridPrint(distgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a DistGrid.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      end subroutine ESMF_DistGridPrint

!------------------------------------------------------------------------------

      end module ESMF_DistGridMod
