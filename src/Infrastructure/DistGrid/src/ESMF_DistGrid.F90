! $Id: ESMF_DistGrid.F90,v 1.144.2.9 2007/10/18 02:42:36 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_DistGrid.F90"
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
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_DistGridMod - contains Grid decompostion methods
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_DistGrid} class, which 
! contains a collection of subgrids which constitute a single logical
! {\tt ESMF\_Grid}. The subgrids can be operated on in parallel on a
! multiprocessor machine. The {\tt ESMF\_DistGrid} class contains the mapping
! between the local grid decompositions and the global logical {\tt ESMF\_Grid}.
! It contains methods to synchronize data values between the boundaries of
! subsets, and to collect and communicate global data values. It interacts closely
! with the {\tt ESMF\_PhysGrid} object.
!
!------------------------------------------------------------------------------
! !USES: 
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_DELayoutMod
      use ESMF_VMMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_DistGridLocal
!
!     ! The DistGridLocal type contains detailed subgrid information for
!     ! the data located on this PE.  When we implement multiple DEs
!     ! per PE then we will have a list of these instead of a single one
!     ! in the DistGridType type.

      type ESMF_DistGridLocal
      sequence
      !private

        ! single values for this DE:

        integer :: MyDE                     ! identifier for this DE
        integer :: localCellCount           ! total number of cells for this DE

        ! one value per axis/dimension of the Grid:

        integer, dimension(:), pointer :: localCellCountPerDim
                                            ! number of cells per dim
        integer, dimension(:), pointer :: globalStartPerDim
                                            ! offset, per dim, for this DE from
                                            ! the global grid origin
        type (ESMF_AxisIndex), dimension(:), pointer :: globalAIPerDim
                                            ! (min/max/stride) per axis relative
                                            ! to the global grid origin
        integer, dimension(:,:), pointer :: localIndices
      end type

!------------------------------------------------------------------------------
!     ! ESMF_DistGridGlobal
!
!     ! The DistGridGlobal type contains general information about each of 
!     ! the subgrids that the entire grid has been decomposed into. This
!     ! includes information about how each part relates to the whole, how
!     ! many points/cells there are per decomposition, etc.  This information
!     ! allows DistGrid to compute information about other decompositions on
!     ! other PEs without having to do communication first.

      type ESMF_DistGridGlobal
      sequence
      !private
 
      ! values where there are only a single one per Grid:

        integer :: globalCellCount      ! total number of cells in the entire grid
        integer :: maxLocalCellCount    ! maximum number of cells in the largest
                                        ! single DE for this grid

      ! values where there are 1 per dimension of the Grid:
  
        integer, dimension(:), pointer :: globalCellCountPerDim
                                        ! number of cells in the entire grid,
                                        ! per axis/dimension
        integer, dimension(:), pointer :: maxLocalCellCountPerDim
                                        ! the largest number of cells per axis
                                        ! in any single DE for this grid 

      ! values where there are 1 per DE in this Grid:  (For the 2d values
      ! below, one dim is per DE, the other dim is per Grid dimension.)

        integer, dimension(:), pointer :: cellCountPerDE
                                        ! total number of cells per each DE
        integer, dimension(:,:), pointer :: cellCountPerDEPerDim
                                        ! number of cells per axis, one for each DE
        integer, dimension(:,:), pointer :: globalStartPerDEPerDim
                                        ! offset from the origin of the entire
                                        ! grid, per axis, per DE
        type (ESMF_AxisIndex), dimension(:,:), pointer :: AIPerDEPerDim
                                        ! (min/max/stride) per axis, per DE 
                                        ! relative to the entire grid

      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGridType
!
!     !  There is one of these types per Grid.  It contains both detailed 
!     !  information about the local decomposition on this PE as well as
!     !  general information about the rest of the other decompositions on
!     !  other PEs which avoids additional communication overhead.
!     !  For each of the local and global types there are two versions:
!     !  one for the computational area, which is the unique set of
!     !  cells in the grid where each cell belongs to one and only one DE.
!     !  The other is the total area, which includes the computational cells
!     !  as well as a layer of boundary cells around the edges.  
!     !  These are not the same as a data halo; this information is used by
!     !  the Grid code in conjunction with PhysGrid information to compute 
!     !  Regridding exterior boundary conditions and relative weightings for
!     !  contributions on the interior edges of decompositions without 
!     !  requiring additional interprocessor communication.

      type ESMF_DistGridType
      sequence
      !private

        type (ESMF_Base) :: base          ! standard ESMF base object
        type(ESMF_DELayout) :: delayout    ! the delayout for this grid

      ! 1 per dimension of the Grid
        integer, dimension(:), pointer :: decompIDs
                                      ! decomposition identifiers
        logical, dimension(:), pointer :: coversDomain
                                      ! distgrid covers entire physical domain?

      ! local and global information, for both the total number of cells 
      ! including the boundary regions, and the computational cells 
      ! (where each cell belongs to one and only one DE).
        type (ESMF_DistGridLocal) :: myDETotal 
        type (ESMF_DistGridLocal) :: myDEComp 
        type (ESMF_DistGridGlobal) :: globalTotal
        type (ESMF_DistGridGlobal) :: globalComp

        integer :: dimCount               ! Number of dimensions
        integer :: arbitrary              ! identifier for arbitrary storage
        integer :: gridBoundaryWidth      ! # of exterior cells/edge

      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGrid
!
!     !  The DistGrid data structure that is passed between languages.

      type ESMF_DistGrid
      sequence
      !private
#ifndef ESMF_NO_INITIALIZERS
        type (ESMF_DistGridType), pointer :: ptr => NULL()
#else
        type (ESMF_DistGridType), pointer :: ptr 
#endif

      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_DistGrid
      public ESMF_DistGridType   ! TODO: this is really internal to Grid
      public ESMF_DistGridLocal, ESMF_DistGridGlobal  ! ditto
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
    public ESMF_DistGridCreate
    public ESMF_DistGridDestroy
    public ESMF_DistGridGet
    public ESMF_DistGridSet
    public ESMF_DistGridSetCounts
    public ESMF_DistGridGetDE
    public ESMF_DistGridSetDE
    public ESMF_DistGridGetAllAxisIndex
    public ESMF_DistGridGetAIsAllDEs
    public ESMF_DistGridGetAllCounts
    public ESMF_DistGridGetDELayout
    ! TODO:  combine all the get subroutines into one
    public ESMF_DistGridLocalToGlobalIndex
    public ESMF_DistGridGlobalToLocalIndex
    public ESMF_DistGridValidate
    public ESMF_DistGridPrint
    public ESMF_DistGridSerialize
    public ESMF_DistGridDeserialize
 
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DistGrid.F90,v 1.144.2.9 2007/10/18 02:42:36 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_DistGridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridCreateEmpty
         module procedure ESMF_DistGridCreateBlock
         module procedure ESMF_DistGridCreateArb
!        module procedure ESMF_DistGridCreateCopy

! !DESCRIPTION:
!     This interface provides a single entry point for
!     {\tt ESMF\_DistGrid} create methods.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_DistGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridConstructNew
         module procedure ESMF_DistGridConstructBlock
         module procedure ESMF_DistGridConstructArb

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct
!     a complete {\tt ESMF\_DistGrid}.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_DistGridGetAllAxisIndex

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridGetAllAIBlock
         module procedure ESMF_DistGridGetAllAIArb

! !DESCRIPTION:
!     This interface provides a single entry point for methods that get
!     the complete set of AxisIndices from a {\tt ESMF\_DistGrid}.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_DistGridSetCounts

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridSetCountsBlock

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     extent counts in a {\tt ESMF\_DistGrid}.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_DistGridSetDE

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridSetDEBlock
         module procedure ESMF_DistGridSetDEArb

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     extent counts in a {\tt ESMF\_DistGrid}.
!
!EOPI
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateEmpty"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create a new DistGrid with no data

! !INTERFACE:
      ! Private name; call using ESMF_DistGridCreate()
      function ESMF_DistGridCreateEmpty(name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateEmpty
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_DistGrid} object and constructs its
!     internals.  Returns a pointer to a new {\tt ESMF\_DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}] 
!          {\tt DistGrid} name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      type(ESMF_DistGridType), pointer :: dgtype  ! Pointer to new distgrid
      integer :: localrc                          ! Error status

!     Initialize pointers
      nullify(dgtype)
      nullify(ESMF_DistGridCreateEmpty%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      allocate(dgtype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Distgrid type", &
                                     ESMF_CONTEXT, rc)) return

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstructNew(dgtype, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

!     Set return values.
      ESMF_DistGridCreateEmpty%ptr => dgtype
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateBlock"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create a new DistGrid internally

! !INTERFACE:
      ! Private name; call using ESMF_DistGridCreate()
      function ESMF_DistGridCreateBlock(dimCount, counts, delayout, decompIDs, &
                                        countsPerDEDim1, countsPerDEDim2, &
                                        periodic, coversDomain, name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateBlock
!
! !ARGUMENTS:
      integer, intent(in) :: dimCount
      integer, dimension(dimCount), intent(in) :: counts
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(dimCount), intent(in) :: decompIDs
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      type(ESMF_Logical), dimension(dimCount), intent(in), optional :: periodic
      type(ESMF_Logical), dimension(dimCount), intent(in), optional :: coversDomain
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc       

! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_DistGrid} object, constructs its
!     internals, and internally sets necessary attributes and values.
!     Returns a pointer to a new {\tt ESMF\_DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[dimCount]
!          Number of dimensions described by this DistGrid.
!     \item[counts]
!          Array of number of computational cells in each direction.
!     \item[delayout]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[decompIDs]
!          Identifier for which Grid axes are decomposed.
!     \item[countsPerDEDim1]
!          Array of number of grid increments per DE in the first decomposed 
!          direction.
!     \item[{[countsPerDEDim2]}]
!          Array of number of grid increments per DE in the second decomposed
!          direction.
!     \item[{[periodic]}] 
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[coversDomain]}] 
!          Logical specifier (array) to denote if the DistGrid covers the entire
!          physical domain in each direction.
!     \item[{[name]}] 
!          {\tt ESMF\_DistGrid} name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      type(ESMF_DistGridType), pointer :: dgtype  ! Pointer to new distgrid
      integer :: localrc                          ! Error status

!     Initialize pointers
      nullify(dgtype)
      nullify(ESMF_DistGridCreateBlock%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      allocate(dgtype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Distgrid type", &
                                     ESMF_CONTEXT, rc)) return

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstruct(dgtype, dimCount, delayout, decompIDs, &
                                  counts, countsPerDEDim1, countsPerDEDim2, &
                                  periodic=periodic, &
                                  coversDomain=coversDomain, &
                                  name=name, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

!     Set return values.
      ESMF_DistGridCreateBlock%ptr => dgtype
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateArb"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create a new DistGrid internally

! !INTERFACE:
      ! Private name; call using ESMF_DistGridCreate()
      function ESMF_DistGridCreateArb(dimCount, myCount, myIndices, counts, &
                                      delayout, decompIDs, name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateArb
!
! !ARGUMENTS:
      integer, intent(in) :: dimCount
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, dimension(:), intent(in) :: counts
      type(ESMF_DELayout), intent(in), optional :: delayout
      integer, dimension(dimCount), intent(in) :: decompIDs
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc       

! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_DistGrid} object, constructs its
!     internals, and internally sets necessary attributes and values.
!     Returns a pointer to a new {\tt ESMF\_DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[dimCount]
!          Number of dimensions described by this DistGrid.
!     \item[myCount]
!          Number of computational cells on this DE.
!     \item[myIndices]
!          Array of grid indices to be distributed to this DE, as (i,j) pairs.
!          The size of this array must be at least [myCount] in the first
!          dimension and 2 in the second.
!     \item[counts]
!          Array of global numbers of computational cells per dimension for
!          the Grid.
!     \item[delayout]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[decompIDs]
!          Identifier for which Grid axes are decomposed.
!     \item[{[name]}] 
!          {\tt ESMF\_DistGrid} name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      type(ESMF_DistGridType), pointer :: dgtype  ! Pointer to new distgrid
      integer :: localrc                          ! Error status

!     Initialize pointers
      nullify(dgtype)
      nullify(ESMF_DistGridCreateArb%ptr)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      allocate(dgtype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Distgrid type", &
                                     ESMF_CONTEXT, rc)) return

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstructArb(dgtype, dimCount, delayout, decompIDs, &
                                      myCount, myIndices, counts, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

!     Set return values.
      ESMF_DistGridCreateArb%ptr => dgtype
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridDestroy"
!BOPI
! !IROUTINE: ESMF_DistGridDestroy - Free all resources associated with a DistGrid 

! !INTERFACE:
      subroutine ESMF_DistGridDestroy(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid) :: distgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt ESMF\_DistGrid} object previously allocated
!     via an {\tt ESMF\_DistGridCreate} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          The class to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

!      ! If already destroyed or never created, return ok
!      if (.not. associated(distgrid%ptr)) then
!        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
!                              "DistGrid uninitialized or already destroyed", &
!                               ESMF_CONTEXT, rc)
!        return
!      endif

      ! Destruct all distgrid internals and then free field memory.
      if (associated(distgrid%ptr)) then
        call ESMF_DistGridDestruct(distgrid, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      nullify(distgrid%ptr)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConstructNew"
!BOPI
! !IROUTINE: ESMF_DistGridConstruct - Construct the internals of an allocated DistGrid

! !INTERFACE:
      ! Private name; call using ESMF_DistGridConstruct()
      subroutine ESMF_DistGridConstructNew(dgtype, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype 
      character (len = *), intent(in), optional :: name  ! name
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_DistGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!          Pointer to a {\tt ESMF\_DistGrid}.
!     \item[{[name]}] 
!          Name of the {\tt ESMF\_DistGrid}.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: localrc                          ! Error status
      type(ESMF_DistGridLocal),  pointer :: me
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Construct a default name if one is not given
      call ESMF_BaseCreate(dgtype%base, "DistGrid", name, 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

!     Initialize distgrid contents  TODO:  move this into the derived type
!                                          as defaults
!     DistGridType contents here:
      dgtype%dimCount          = 0
      dgtype%arbitrary         = 0
      dgtype%gridBoundaryWidth = 1   ! TODO: this must be settable
      nullify(dgtype%decompIDs)
      nullify(dgtype%coversDomain)

!     myDETotal contents here:
      me => dgtype%myDETotal
      me%myDE           = 0
      me%localCellCount = 0
      nullify(me%localCellCountPerDim)
      nullify(me%globalStartPerDim)
      nullify(me%globalAIPerDim)
      nullify(me%localIndices)

!     myDEComp contents here:
      me => dgtype%myDEComp
      me%myDE           = 0
      me%localCellCount = 0
      nullify(me%localCellCountPerDim)
      nullify(me%globalStartPerDim)
      nullify(me%globalAIPerDim)
      nullify(me%localIndices)

!     globalTotal contents here:
      glob => dgtype%globalTotal
      glob%globalCellCount   = 0
      glob%maxLocalCellCount = 0
      nullify(glob%globalCellCountPerDim)
      nullify(glob%maxLocalCellCountPerDim)
      nullify(glob%cellCountPerDE)
      nullify(glob%cellCountPerDEPerDim)
      nullify(glob%globalStartPerDEPerDim)
      nullify(glob%AIPerDEPerDim)

!     globalComp contents here:
      glob => dgtype%globalComp
      glob%globalCellCount   = 0
      glob%maxLocalCellCount = 0
      nullify(glob%globalCellCountPerDim)
      nullify(glob%maxLocalCellCountPerDim)
      nullify(glob%cellCountPerDE)
      nullify(glob%cellCountPerDEPerDim)
      nullify(glob%globalStartPerDEPerDim)
      nullify(glob%AIPerDEPerDim)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConstructBlock"
!BOPI
! !IROUTINE: ESMF_DistGridConstruct - Construct the internals of an allocated DistGrid

! !INTERFACE:
      ! Private name; call using ESMF_DistGridConstruct()
      subroutine ESMF_DistGridConstructBlock(dgtype, dimCount, delayout, &
                                             decompIDs, counts, &
                                             countsPerDEDim1, countsPerDEDim2, &
                                             gridBoundaryWidth, periodic, &
                                             coversDomain, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype 
      integer, intent(in) :: dimCount
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(dimCount), intent(in) :: decompIDs
      integer, dimension(dimCount), intent(in) :: counts
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      integer, intent(in), optional :: gridBoundaryWidth
      type(ESMF_Logical), dimension(dimCount), intent(in), optional :: periodic
      type(ESMF_Logical), dimension(dimCount), intent(in), optional :: coversDomain
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_DistGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype]
!          Pointer to a {\tt ESMF\_DistGrid}.
!     \item[dimCount]
!          Number of dimensions described by this DistGrid.
!     \item[delayout]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[decompIDs]
!          Identifier for which Grid axes are decomposed by the layout.
!     \item[counts]
!          Array of number of computational cells along each axis.
!     \item[countsPerDEDim1]
!          Array of number of computational cells per DE in first axis.
!     \item[{[countsPerDEDim2]}]
!          Array of number of computational cells per DE in second axis.
!     \item[{[periodic]}] 
!          Logical value for whether the axes are periodic, one value per axis.
!          Default is non-periodic.
!     \item[{[coversDomain]}]
!          Logical specifier (array) to denote if the DistGrid covers the entire
!          physical domain in each direction.  Default is true.
!     \item[{[name]}] 
!          {\tt ESMF\_DistGrid} name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: localrc                          ! Error status
      integer :: globalCellCount
      integer :: i, j, nDE, nDEs(0:2), bnd
      integer :: ndim
      integer, dimension(ESMF_MAXDECOMPDIM) :: globalCellCountPerDim, &
                                               nDEsUse
      type(ESMF_DistGridLocal),  pointer :: me
      type(ESMF_DistGridGlobal), pointer :: glob
      type(ESMF_Logical):: otoFlag, lrFlag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

!     Error checking for required input   TODO: complete
!     call ESMF_DELayoutValidate(delayout, rc=localrc)
!     if (ESMF_LogMsgFoundError(localrc, &
!                               ESMF_ERR_PASSTHRU, &
!                               ESMF_CONTEXT, rc)) return
 
      ! Initialize the derived type contents, including setting name
      call ESMF_DistGridConstructNew(dgtype, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Allocate resources based on number of DE's
      call ESMF_DELayoutGet(delayout, dimCount=ndim, oneToOneFlag=otoFlag, &
                            logRectFlag=lrFlag, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Check DELayout attributes
      ! TODO: we only care about DEs with non-zero counts.  1-to-1 might
      ! not be true and not an error.
      !if (otoFlag .ne. ESMF_TRUE) then    ! ensure this is 1-to-1 layout
      !  call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, "not a 1-to-1 layout", &
      !                              ESMF_CONTEXT, rc)
      !  return
      !endif
      ! if (ndim .ne. 2) then               ! ensure this is 2D Layout
      !   call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, "not a 2D layout", &
      !                               ESMF_CONTEXT, rc)
      !   return
      ! endif
      ! if (lrFlag .ne. ESMF_TRUE) then     ! ensure this is logical rect layout
      !   call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, &
      !                              "not a logically rectangular layout", &
      !                              ESMF_CONTEXT, rc)
      !   return
      ! endif
      call ESMF_DELayoutGet(delayout, deCountPerDim=nDEs(1:2), rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      nDEs(0) = 1
      nDE = nDEs(1) * nDEs(2)
      if (nDE .le. 0) then
        call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, &
                                   "number of DEs less than or equal to zero", &
                                   ESMF_CONTEXT, rc)
        return
      endif
      call ESMF_DistGridAllocateBlock(dgtype, nDE, dimCount, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Fill in distgrid derived type with input
      dgtype%dimCount = dimCount
      dgtype%delayout = delayout
      do i = 1,dimCount
        dgtype%decompIDs(i) = decompIDs(i)
      enddo
      if (present(gridBoundaryWidth)) &
        dgtype%gridBoundaryWidth = gridBoundaryWidth
      if (present(coversDomain)) then
        do i = 1,dimCount
          dgtype%coversDomain(i) = coversDomain(i)
        enddo
      endif

      ! Calculate values for computational cells
      glob => dgtype%globalComp
      me   => dgtype%myDEComp
      globalCellCount = 1
      do i = 1,dimCount
        globalCellCountPerDim(i) = counts(i)   ! TODO: fix for reordering
        if (globalCellCountPerDim(i).ne.0) &
            globalCellCount = globalCellCount * globalCellCountPerDim(i)
      enddo
      if (globalCellCount.le.0) then
        call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, &
                                   "globalCellCount le 0", &
                                    ESMF_CONTEXT, rc)
        return
      endif

      ! indirect addressing for nDEs due to decompIds -- nDEs should match
      ! the ordering of countsPerDE arrays, which have beed changed due to
      ! the decompositions
      do i = 1,2
        nDEsUse(i) = nDEs(i)
      enddo
      do i = 1,size(decompIds)
        if (decompIds(i).ne.0) nDEsUse(i) = nDEs(decompIds(i))
      enddo

      ! call internal routine to set counts per DE
      call ESMF_DistGridSetCounts(dgtype, dimCount, nDEsUse, &
                                  countsPerDEDim1, countsPerDEDim2, &
                                  periodic, total=.FALSE., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      glob%globalCellCount   = globalCellCount 
      glob%maxLocalCellCount = maxval(glob%cellCountPerDE)
      do j = 1,dimCount
        glob%globalCellCountPerDim(j)   = globalCellCountPerDim(j)
        glob%maxLocalCellCountPerDim(j) = maxval(glob%cellCountPerDEPerDim(:,j))
      enddo

      ! now repeat the process, accounting for boundary cells
      glob => dgtype%globalTotal
      me   => dgtype%myDETotal
      bnd = dgtype%gridBoundaryWidth
      globalCellCount = 1
      do i = 1,dimCount
        globalCellCountPerDim(i) = counts(i) + 2*bnd  ! TODO: fix for reordering
        if (globalCellCountPerDim(i).ne.0) &
            globalCellCount = globalCellCount * globalCellCountPerDim(i)
      enddo
      if (globalCellCount.le.0) then
        call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, &
                                   "globalCellCount le 0", &
                                    ESMF_CONTEXT, rc)
        return
      endif
      ! call internal routine to set counts per DE
      call ESMF_DistGridSetCounts(dgtype, dimCount, nDEsUse, &
                                  countsPerDEDim1, countsPerDEDim2, &
                                  periodic, total=.TRUE., rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      glob%globalCellCount   = globalCellCount 
      glob%maxLocalCellCount = maxval(glob%cellCountPerDE)
      do j = 1,dimCount
        glob%globalCellCountPerDim(j)   = globalCellCountPerDim(j)
        glob%maxLocalCellCountPerDim(j) = maxval(glob%cellCountPerDEPerDim(:,j))
      enddo

      ! Fill in DE derived type
      call ESMF_DistGridSetDE(dgtype, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConstructArb"
!BOPI
! !IROUTINE: ESMF_DistGridConstruct - Construct the internals of an allocated DistGrid

! !INTERFACE:
      ! Private name; call using ESMF_DistGridConstruct()
      subroutine ESMF_DistGridConstructArb(dgtype, dimCount, delayout, &
                                           decompIDs, myCount, myIndices, &
                                           counts, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype 
      integer, intent(in) :: dimCount
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in) :: decompIDs
      integer, intent(in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, dimension(:), intent(in) :: counts
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_DistGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype]
!          Pointer to a {\tt ESMF\_DistGrid}.
!     \item[dimCount]
!          Number of dimensions described by this DistGrid.
!     \item[delayout]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[decompIDs]
!          Identifier for which Grid axes are decomposed by the layout.
!     \item[myCount]
!          Number of computational cells on this DE.
!     \item[myIndices]
!          Array of grid indices to be distributed to this DE, as (i,j) pairs.
!          The size of this array must be at least [myCount] in the first
!          dimension and 2 in the second.
!     \item[counts]
!          Array of global numbers of computational cells per dimension for
!          the Grid.
!     \item[{[name]}] 
!          {\tt ESMF\_DistGrid} name.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: localrc                          ! Error status
      integer :: i, ndim, nDEs
      integer :: globalCellCount, globalCellCountPerDim(2)
      type(ESMF_DistGridLocal),  pointer :: me
      type(ESMF_DistGridGlobal), pointer :: glob
      type(ESMF_Logical):: otoFlag, lrFlag

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

!     Error checking for required input   TODO: complete
!     call ESMF_DELayoutValidate(delayout, rc=localrc)
!     if (ESMF_LogMsgFoundError(localrc, &
!                               ESMF_ERR_PASSTHRU, &
!                               ESMF_CONTEXT, rc)) return
 
      ! Initialize the derived type contents, including setting name
      call ESMF_DistGridConstructNew(dgtype, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Allocate resources based on number of DE's
      call ESMF_DELayoutGet(delayout, dimCount=ndim, oneToOneFlag=otoFlag, &
                            logRectFlag=lrFlag, deCount=nDEs, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

    !  ! Check DELayout attributes
    !  if (otoFlag .ne. ESMF_TRUE) then    ! ensure this is 1-to-1 layoutu
    !    call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, &
    !                               "not a 1-to-1 layout", &
    !                               ESMF_CONTEXT, rc)
    !    return
    !  endif

      ! Allocate necessary arrays
      call ESMF_DistGridAllocateArb(dgtype, nDEs, size(counts), myCount, &
                                    localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Fill in distgrid derived type with input
      dgtype%dimCount = dimCount
      dgtype%arbitrary= 1
      dgtype%delayout = delayout
      do i = 1,dimCount
        dgtype%decompIDs(i) = decompIDs(i)
      enddo

      ! Calculate values for computational domain -- for arbitrary storage
      ! there is no difference
      glob => dgtype%globalComp
      me   => dgtype%myDEComp
      globalCellCount = 1
      do i = 1,2                               ! TODO: fix this
        globalCellCountPerDim(i)      = counts(i)   ! TODO: fix for reordering
        glob%globalCellCountPerDim(i) = counts(i)
        if (globalCellCountPerDim(i).ne.0) &
            globalCellCount = globalCellCount * globalCellCountPerDim(i)
      enddo
      if (globalCellCount.le.0) then
        call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, &
                                 "globalCellCount le 0", &
                                 ESMF_CONTEXT, rc)
        return
      endif
      glob%globalCellCount       = globalCellCount

      ! call internal routine to set counts per DE
      call ESMF_DistGridSetCountsArb(dgtype, dimCount, delayout, counts, &
                                      myCount, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      glob%maxLocalCellCount = maxval(glob%cellCountPerDE)

      ! Fill in DE derived type
      call ESMF_DistGridSetDE(dgtype, myCount, myIndices, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridDestruct"
!BOPI
! !IROUTINE: ESMF_DistGridDestruct - Free any DistGrid memory allocated internally

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
!     {\tt ESMF\_DistGridDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          The class to be destructed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status
      logical :: dummy
      type(ESMF_DistGridType), pointer :: dgtype
      type(ESMF_DistGridLocal), pointer :: me     
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      dgtype => distgrid%ptr

      ! deallocate DistGrid arrays and nullify pointers -- check for "associated" first

      ! globalComp contents here:
      glob => dgtype%globalComp
      if (associated(glob%globalCellCountPerDim)) then
        deallocate(glob%globalCellCountPerDim, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating globalComp contents", &
                                       ESMF_CONTEXT, rc)) return
        nullify(glob%globalCellCountPerDim)
      endif
      if (associated(glob%maxLocalCellCountPerDim)) then
        deallocate(glob%maxLocalCellCountPerDim, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating globalComp contents", &
                                       ESMF_CONTEXT, rc)) return
        nullify(glob%maxLocalCellCountPerDim)
      endif
      if (associated(glob%cellCountPerDE)) then
        deallocate(glob%cellCountPerDE, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating globalComp contents", &
                                       ESMF_CONTEXT, rc)) return
        nullify(glob%cellCountPerDE)
      endif
      if (associated(glob%cellCountPerDEPerDim)) then
        deallocate(glob%cellCountPerDEPerDim, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating globalComp contents", &
                                       ESMF_CONTEXT, rc)) return
        nullify(glob%cellCountPerDEPerDim)
      endif
      if (associated(glob%globalStartPerDEPerDim)) then
        deallocate(glob%globalStartPerDEPerDim, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating globalComp contents", &
                                       ESMF_CONTEXT, rc)) return
        nullify(glob%globalStartPerDEPerDim)
      endif
      if (associated(glob%AIPerDEPerDim)) then
        deallocate(glob%AIPerDEPerDim, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating globalComp contents", &
                                       ESMF_CONTEXT, rc)) return
        nullify(glob%AIPerDEPerDim)
      endif

      ! myDEComp contents here:
      me => dgtype%myDEComp
      if (associated(me%localCellCountPerDim)) then
        deallocate(me%localCellCountPerDim, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating myDEComp", &
                                       ESMF_CONTEXT, rc)) return
        nullify(me%localCellCountPerDim)
      endif
      if (associated(me%globalStartPerDim)) then
        deallocate(me%globalStartPerDim, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating myDEComp", &
                                       ESMF_CONTEXT, rc)) return
        nullify(me%globalStartPerDim)
      endif
      if (associated(me%globalAIPerDim)) then
        deallocate(me%globalAIPerDim, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocating myDEComp", &
                                       ESMF_CONTEXT, rc)) return
        nullify(me%globalAIPerDim)
      endif

      select case(dgtype%arbitrary)

      !-------------
      !  block structure
      case(0)

        ! globalTotal contents here:
        glob => dgtype%globalTotal
        if (associated(glob%globalCellCountPerDim)) then
          deallocate(glob%globalCellCountPerDim, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, &
                                         "deallocating globalTotal contents", &
                                         ESMF_CONTEXT, rc)) return
          nullify(glob%globalCellCountPerDim)
        endif
        if (associated(glob%maxLocalCellCountPerDim)) then
          deallocate(glob%maxLocalCellCountPerDim, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, &
                                         "deallocating globalTotal contents", &
                                         ESMF_CONTEXT, rc)) return
          nullify(glob%maxLocalCellCountPerDim)
        endif
        if (associated(glob%cellCountPerDE)) then
          deallocate(glob%cellCountPerDE, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, &
                                         "deallocating globalTotal contents", &
                                         ESMF_CONTEXT, rc)) return
          nullify(glob%cellCountPerDE)
        endif
        if (associated(glob%cellCountPerDEPerDim)) then
          deallocate(glob%cellCountPerDEPerDim, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, &
                                         "deallocating globalTotal contents", &
                                         ESMF_CONTEXT, rc)) return
          nullify(glob%cellCountPerDEPerDim)
        endif
        if (associated(glob%globalStartPerDEPerDim)) then
          deallocate(glob%globalStartPerDEPerDim, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, &
                                         "deallocating globalTotal contents", &
                                         ESMF_CONTEXT, rc)) return
          nullify(glob%globalStartPerDEPerDim)
        endif
        if (associated(glob%AIPerDEPerDim)) then
          deallocate(glob%AIPerDEPerDim, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, &
                                         "deallocating globalTotal contents", &
                                         ESMF_CONTEXT, rc)) return
          nullify(glob%AIPerDEPerDim)
        endif

        ! myDETotal contents here:
        me => dgtype%myDETotal
        if (associated(me%localCellCountPerDim)) then
          deallocate(me%localCellCountPerDim, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocating myDETotal", &
                                         ESMF_CONTEXT, rc)) return
          nullify(me%localCellCountPerDim)
        endif
        if (associated(me%globalStartPerDim)) then
          deallocate(me%globalStartPerDim, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocating myDETotal", &
                                         ESMF_CONTEXT, rc)) return
          nullify(me%globalStartPerDim)
        endif
        if (associated(me%globalAIPerDim)) then
          deallocate(me%globalAIPerDim, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocating myDETotal", &
                                         ESMF_CONTEXT, rc)) return
          nullify(me%globalAIPerDim)
        endif

      !-------------
      !  vector (arbitrary) structure
      case(1)

        ! myDEComp contents here:
        me => dgtype%myDEComp
        if (associated(me%localIndices)) then
          deallocate(me%localIndices, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocating myDEComp", &
                                         ESMF_CONTEXT, rc)) return
          nullify(me%localIndices)
        endif

      case default
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                      "Invalid distgrid structure", &
                                      ESMF_CONTEXT, rc)
        return
      end select

      ! DistGridType contents here:
      if (associated(dgtype%decompIDs)) then
        deallocate(dgtype%decompIDs, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating distgrid contents", &
                                       ESMF_CONTEXT, rc)) return
        nullify(dgtype%decompIDs)
      endif
      if (associated(dgtype%coversDomain)) then
        deallocate(dgtype%coversDomain, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, &
                                       "deallocating distgrid contents", &
                                       ESMF_CONTEXT, rc)) return
        nullify(dgtype%coversDomain)
      endif

      ! destroy associated classes

      ! delete the base class
      call ESMF_BaseDestroy(dgtype%base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! TODO: Agree that this is correct.  The Grid is passed in a layout
      !  created outside this grid (and perhaps shared amongst many grids)
      !  so it seems that it should not be destroyed here.)
      !call ESMF_DELayoutDestroy(dgtype%delayout, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                          ESMF_ERR_PASSTHRU, &
      !                          ESMF_CONTEXT, rc)) return

      deallocate(dgtype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocating distgrid type", &
                                     ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridDestruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGet"
!BOPI
! !IROUTINE: ESMF_DistGridGet - Get information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGet(distgrid, dimCount, coversDomain, &
                                  globalCellCount, globalCellCountPerDim, &
                                  globalStartPerDEPerDim, maxLocalCellCount, &
                                  maxLocalCellCountPerDim, delayout, &
                                  name, total, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), target, intent(in) :: distgrid
      integer, intent(inout), optional :: dimCount
      logical, dimension(:), intent(inout), optional :: coversDomain
      integer, intent(inout), optional :: globalCellCount
      integer, dimension(:), intent(inout), optional :: globalCellCountPerDim
      integer, dimension(:,:), intent(inout), optional :: globalStartPerDEPerDim
      integer, intent(inout), optional :: maxLocalCellCount
      integer, dimension(:), intent(inout), optional :: maxLocalCellCountPerDim
      type(ESMF_DELayout), intent(out), optional :: delayout
      character (len = *), intent(out), optional :: name
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns information from the {\tt ESMF\_DistGrid} object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[{[dimCount]}]
!          Number of dimensions in the {\tt ESMF\_DistGrid}.
!     \item[{[coversDomain]}]
!          Array of logical identifiers if distgrid covers the entire physical domain.
!     \item[{[globalCellCount]}]
!          Global total number of cells.
!     \item[{[globalCellCountPerDim]}]
!          Array of the global number of cells in each dimension.
!     \item[{[globalStartPerDEPerDim]}]
!          Array of the global starting count on each DE in each dimension,
!          dimensioned (nDEs, dimCount)
!     \item[{[maxLocalCellCount]}]
!          Maximum number of cells on any {\tt ESMF\_DE}.
!     \item[{[maxLocalCellCountPerDim]}]
!          Array of the maximum number of cells in each dimension on
!          any {\tt ESMF\_DE}.
!     \item[{[name]}]
!          {\tt ESMF\_DistGrid} name.
!     \item[{[total]}] 
!          Logical; if TRUE return these values including boundary cells.  
!          Default is FALSE, returning only computational cells.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status
      integer :: i, j, nndes, count
      type(ESMF_DistGridGlobal), pointer :: glob
      type(ESMF_DistGridType), pointer :: dgtype

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If total is true, get info for the total cells including
      ! boundary areas; otherwise get only computational areas.
      dgtype => distgrid%ptr
      glob   => distgrid%ptr%globalComp
      if (present(total)) then
        if (total) glob => dgtype%globalTotal
      endif

      ! set count -- helps set the correct size for storage distribution
      count = dgtype%dimCount + dgtype%arbitrary

      ! If present, get information from distgrid derived type
      if (present(dimCount))     dimCount     = dgtype%dimCount
      if (present(coversDomain)) coversDomain = dgtype%coversDomain

      if (present(globalCellCount)) &
                  globalCellCount = glob%globalCellCount

      if (present(globalCellCountPerDim)) then
                 ! TODO: add check that globalCellCountPerDim is large enough
                 !       or use the size of the array for the loop limit
        do i = 1,count
          globalCellCountPerDim(i) = glob%globalCellCountPerDim(i)
        enddo
      endif

      if (present(globalStartPerDEPerDim)) then
                 ! TODO: add check that globalStartPerDEPerDim is large enough
                 !       or use the size of the array for the loop limit
        call ESMF_DELayoutGet(dgtype%delayout, deCount=nndes, rc=rc)
        do i = 1, nndes
          do j = 1,count
            globalStartPerDEPerDim(i,j) = glob%globalStartPerDEPerDim(i,j)
          enddo
        enddo
      endif

      if (present(maxLocalCellCount)) maxLocalCellCount = glob%maxLocalCellCount

      if (present(maxLocalCellCountPerDim)) then
                 ! TODO: add check that maxLocalCellCountPerDim is large enough
                 !       or use the size of the array for the loop limit
        do i = 1,count
          maxLocalCellCountPerDim(i) = glob%maxLocalCellCountPerDim(i)
        enddo
      endif

      if (present(name)) then
        call ESMF_GetName(dgtype%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(delayout)) delayout = dgtype%delayout

      if (present(rc)) rc = ESMF_SUCCESS
 
      end subroutine ESMF_DistGridGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSet"
!BOPI
! !IROUTINE: ESMF_DistGridSet - Set information about a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSet(dgtype, coversDomain, decompIDs, &
                                  globalCellCount, globalCellCountPerDim, &
                                  maxLocalCellCount, maxLocalCellCountPerDim, &
                                  total, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      logical, dimension(:), intent(in), optional :: coversDomain
      integer, dimension(:), intent(in), optional :: decompIDs
      integer, intent(in), optional :: globalCellCount
      integer, dimension(:), intent(in), optional :: globalCellCountPerDim
      integer, intent(in), optional :: maxLocalCellCount
      integer, dimension(:), intent(in), optional :: maxLocalCellCountPerDim
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Sets the {\tt ESMF\_DistGrid} object with information given.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!        Class to be set.
!     \item[{[coversDomain]}]
!        Array of logical identifiers if distgrid covers the entire physical domain.
!     \item[{[decompIDs]}]
!          Identifier for which Grid axes are decomposed.
!     \item[{[globalCellCount]}]
!        Global total number of cells.
!     \item[{[globalCellCountPerDim]}]
!        Array of the global number of cells in each dimension.
!     \item[{[maxLocalCellCount]}]
!        Maximum number of cells on any {\tt ESMF\_DE}.
!     \item[{[maxLocalCellCountPerDim]}]
!        Array of the maximum number of cells in each dimension on
!        any {\tt ESMF\_DE}.
!     \item[{[total]}]
!        Logical; if TRUE, sets counts for total cells including boundary cells.
!        Default is FALSE, sets only computational cells.
!     \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      !integer :: localrc                          ! Error status
      type(ESMF_DistGridGlobal), pointer :: glob
      integer :: i

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If total is true, set info for the total cells including
      ! boundary areas; otherwise set only computational areas.
      glob => dgtype%globalComp
      if (present(total)) then
        if (total) glob => dgtype%globalTotal
      endif

      ! if present, set information filling in distgrid derived type
      if (present(coversDomain)) then
        do i = 1,dgtype%dimCount
          dgtype%coversDomain(i) = coversDomain(i)
        enddo
      endif

      if (present(decompIDs)) then
        ! TODO: add check that decompIDs is large enough
        !       or use the size of the array for the loop limit
        do i = 1,dgtype%dimCount
          dgtype%decompIDs(i) = decompIDs(i)
        enddo
      endif

      if (present(globalCellCount)) glob%globalCellCount = globalCellCount

      if (present(globalCellCountPerDim)) then
        ! TODO: add check that globalCellCountPerDim is large enough
        !       or use the size of the array for the loop limit
        do i = 1,dgtype%dimCount
          glob%globalCellCountPerDim(i) = globalCellCountPerDim(i)
        enddo
      endif

      if (present(maxLocalCellCount)) glob%maxLocalCellCount = maxLocalCellCount

      if (present(maxLocalCellCountPerDim)) then
        ! TODO: add check that maxLocalCellCountPerDim is large enough
        !       or use the size of the array for the loop limit
        do i = 1,dgtype%dimCount
          glob%maxLocalCellCountPerDim(i) = maxLocalCellCountPerDim(i)
        enddo
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSetCountsBlock"
!BOPI
! !IROUTINE: ESMF_DistGridSetCountsBlock - Set extent counts for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetCountsBlock(dgtype, dimCount, nDE, &
                                             countsPerDEDim1, countsPerDEDim2, &
                                             periodic, total, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      integer, intent(in) :: dimCount
      integer, dimension(:), intent(in) :: nDE
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set {\tt ESMF\_DistGrid} extent counts
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!          Class to be modified.
!     \item[dimCount] 
!          Number of dimensions.
!     \item[nDE]
!          Array of number of {\tt ESMF\_DE}'s in the each direction.
!     \item[countsPerDEDim1]
!          Array of number of computational cells per DE in first direction.
!     \item[countsPerDEDim2]
!          Array of number of computational cells per DE in second direction.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[total]}] 
!          Logical; if TRUE set the total counts.  Defaults to setting the
!          computational region only.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      !integer :: localrc                          ! Error status
      integer :: i, j, de, bnd, localCount
      integer :: globalStart, globalEnd           ! global counters
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If total is true, set info for the total cells including
      ! boundary areas; otherwise set only computational areas.
      glob => dgtype%globalComp
      bnd = 0
      if (present(total)) then
        if (total) then
          glob => dgtype%globalTotal
          bnd  = dgtype%gridBoundaryWidth * 2
        endif
      endif

      glob%cellCountPerDE(:) = 1

      ! Set extent counts per axis from the number of de's and number of cells
      ! First in the 1 direction

      ! break out here by decompId value
      select case(dgtype%decompIds(1))

      !-------------
      case(0) ! not a decomposed axis

        globalStart = 1
        globalEnd   = bnd + countsPerDEDim1(1)
        localCount  = countsPerDEDim1(1)
        if (localCount.gt.0) localCount = localCount + bnd
 
        do i   = 1,nDE(1)
          do j = 1,nDE(2)
            de = (j-1)*nDE(1) + i
            glob%cellCountPerDE(de)           = glob%cellCountPerDE(de) &
                                              * localCount
            glob%cellCountPerDEPerDim(de,1)   = localCount
            glob%globalStartPerDEPerDim(de,1) = globalStart - 1
            glob%AIPerDEPerDim(de,1)%min      = globalStart
            glob%AIPerDEPerDim(de,1)%max      = globalEnd
            glob%AIPerDEPerDim(de,1)%stride   = globalEnd
            if (present(periodic)) then
              if (periodic(1).eq.ESMF_TRUE) &
              glob%AIPerDEPerDim(de,1)%stride = globalEnd &
                                              + countsPerDEDim1(1) &
                                              + countsPerDEDim1(1)
             ! TODO: check this
            endif
          enddo
        enddo

      !-------------
      case(1,2) ! a decomposed axis

        globalStart = 1
        globalEnd   = bnd
      
        do i = 1,nDE(1)
          globalEnd  = globalEnd + countsPerDEDim1(i)
          localCount = countsPerDEDim1(i)
          if (localCount.gt.0) localCount = localCount + bnd
          do j = 1,nDE(2)
            de = (j-1)*nDE(1) + i
            glob%cellCountPerDE(de)           = glob%cellCountPerDE(de) &
                                              * localCount
            glob%cellCountPerDEPerDim(de,1)   = localCount
            glob%globalStartPerDEPerDim(de,1) = globalStart - 1
            glob%AIPerDEPerDim(de,1)%min      = globalStart
            glob%AIPerDEPerDim(de,1)%max      = globalEnd
          enddo
          globalStart = globalEnd - bnd + 1
        enddo
        do i = 1,nDE(1)
          do j = 1,nDE(2)
            de = (j-1)*nDE(1) + i
            glob%AIPerDEPerDim(de,1)%stride = globalEnd
            if (present(periodic)) then
              if (periodic(1).eq.ESMF_TRUE) &
              glob%AIPerDEPerDim(de,1)%stride = globalEnd &
                                              + countsPerDEDim1(1) &
                                              + countsPerDEDim1(nDE(1))
            endif
          enddo
        enddo

      !-------------
      case default
        call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, &
                                  "Invalid decompIds(1)", &
                                   ESMF_CONTEXT, rc)
        return
      end select


      ! Then the 2 decomposition if applicable

      if (dimCount.eq.2) then

        ! break out here by decompId value
        select case(dgtype%decompIds(2))

        !-------------
        case(0) ! not a decomposed axis

          globalStart = 1
          globalEnd   = bnd + countsPerDEDim2(1)
          localCount  = countsPerDEDim2(1)
          if (localCount.gt.0) localCount = localCount + bnd
 
          do i = 1,nDE(1)
            do j = 1,nDE(2)
              de = (j-1)*nDE(1) + i
              glob%cellCountPerDE(de)           = glob%cellCountPerDE(de) &
                                                * localCount
              glob%cellCountPerDEPerDim(de,2)   = localCount
              glob%globalStartPerDEPerDim(de,2) = globalStart - 1
              glob%AIPerDEPerDim(de,2)%min      = globalStart
              glob%AIPerDEPerDim(de,2)%max      = globalEnd
              glob%AIPerDEPerDim(de,2)%stride   = globalEnd
              if (present(periodic)) then
                if (periodic(2).eq.ESMF_TRUE) &
                glob%AIPerDEPerDim(de,2)%stride = globalEnd &
                                                + countsPerDEDim2(1) &
                                                + countsPerDEDim2(1)
               ! TODO: check this
              endif
            enddo
          enddo

        !-------------
        case(1,2) ! a decomposed axis
  
          globalStart = 1
          globalEnd   = bnd
          ! TODO: this code was removed from the 1 decomp case above.  should it
          !       be removed from here as well?
          if (present(periodic)) then
            if (periodic(2).eq.ESMF_TRUE) then
              globalStart = countsPerDEDim2(nDE(2)) + 1
              globalEnd   = countsPerDEDim2(nDE(2))
            endif
          endif
      
          do j = 1,nDE(2)
            globalEnd  = globalEnd + countsPerDEDim2(j)
            localCount = countsPerDEDim2(j)
            if (localCount.gt.0) localCount = localCount + bnd
            do i = 1,nDE(1)
              de = (j-1)*nDE(1) + i
              glob%cellCountPerDE(de)           = glob%cellCountPerDE(de) &
                                                * localCount
              glob%cellCountPerDEPerDim(de,2)   = localCount
              glob%globalStartPerDEPerDim(de,2) = globalStart - 1
              glob%AIPerDEPerDim(de,2)%min      = globalStart
              glob%AIPerDEPerDim(de,2)%max      = globalEnd
            enddo
            globalStart = globalEnd - bnd + 1
          enddo
          do i   = 1,nDE(1)
            do j = 1,nDE(2)
              de = (j-1)*nDE(1) + i
              glob%AIPerDEPerDim(de,2)%stride = globalEnd
              if (present(periodic)) then
                if (periodic(2).eq.ESMF_TRUE) &
                glob%AIPerDEPerDim(de,2)%stride = globalEnd &
                                                + countsPerDEDim2(1) &
                                                + countsPerDEDim2(nDE(2))
              endif
            enddo
          enddo

        !-------------
        case default
          call ESMF_LogMsgSetError(ESMF_RC_NOT_FOUND, &
                                     "Invalid decompIds(2)", &
                                     ESMF_CONTEXT, rc)
          return
        end select

      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetCountsBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSetCountsArb"
!BOPI
! !IROUTINE: ESMF_DistGridSetCountsArb - Set extent counts for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetCountsArb(dgtype, dimCount, delayout, &
                                            counts, myCount, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      integer, intent(in) :: dimCount
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: myCount
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set {\tt ESMF\_DistGrid} extent counts
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!          Class to be modified.
!     \item[dimCount] 
!          Number of dimensions.
!     \item[delayout]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[counts]
!          Array of global numbers of computational cells per dimension for
!          the Grid.
!     \item[myCount]
!          Number of computational cells on this DE.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status
      integer :: j, thisCount(1), nDEs, myDE, thisDE
      type(ESMF_DistGridGlobal), pointer :: glob
      type(ESMF_VM) :: vm
      integer, dimension(:), allocatable :: allCounts

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! pointer to data
      glob => dgtype%globalComp

      ! set defaults
      glob%cellCountPerDE(:)           = 1
      glob%cellCountPerDEPerDim(:,:)   = 1
      glob%globalStartPerDEPerDim(:,:) = 0
      glob%AIPerDEPerDim(:,:)%min      = 1
      glob%AIPerDEPerDim(:,1)%max      = counts(1)
      glob%AIPerDEPerDim(:,1)%stride   = counts(1)
      glob%AIPerDEPerDim(:,2)%max      = counts(2)
      glob%AIPerDEPerDim(:,2)%stride   = counts(2)

      ! Allocate resources based on number of DE's

      ! first, query the delayout for information
      call ESMF_DELayoutGet(delayout, deCount=nDEs, localDe=myDE, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_DELayoutGetVM(delayout, vm, localrc)
 
#if 1
      ! collective call to gather count from all DEs
      ! New implementation (P. Li, 7/2006) -- This implementation calls
      ! ESMF_VMAllGather instead of ESMF_VMBroadcast() to collect count 
      ! We have to allocate a temperary array allCount to store the values
      allocate(allCounts(nDEs), stat=localrc)
      thisCount(1) = myCount
      call ESMF_VMAllGather(vm, thisCount, allCounts, 1, rc=localrc)
      do j = 1,nDEs
        glob%cellCountPerDE(j)         = allCounts(j)
        glob%cellCountPerDEPerDim(j,1) = allCounts(j)
      enddo
      deallocate(allCounts, stat=localrc)
#else
      ! collective call to gather count from all DEs
      ! Old implementation -- This implementation calls n ESMF_VMBroadcast()
      ! (n is total number of DEs) in order to collect the count from all DEs.
      ! It performs very poorly on Cray X1 system.  
      do j = 1,nDEs
        thisDE = j - 1
        if (myDE.eq.thisDE) then
          thisCount(1) = myCount
        endif
        call ESMF_VMBroadcast(vm, thisCount, 1, thisDE, rc=localrc)
        glob%cellCountPerDE(j)         = thisCount(1)
        glob%cellCountPerDEPerDim(j,1) = thisCount(1)
      enddo
#endif
      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_DistGridSetCountsArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetDE"
!BOPI
! !IROUTINE: ESMF_DistGridGetDE - Get DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetDE(dgtype, localCellCount, &
                                    localCellCountPerDim, globalStartPerDim, &
                                    globalAIPerDim, total, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      integer, intent(inout), optional :: localCellCount
      integer, dimension(:), intent(inout), optional :: localCellCountPerDim
      integer, dimension(:), intent(inout), optional :: globalStartPerDim
      type(ESMF_AxisIndex), dimension(:), intent(inout), &
                            optional :: globalAIPerDim
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype]
!          Class to be modified.
!     \item[{[localCellCount]}]
!          Local (on this {\tt ESMF\_DE}) number of cells.
!     \item[{[localCellCountPerDim]}]
!          Local (on this {\tt ESMF\_DE}) number of cells along each axis.
!     \item[{[globalStartPerDim]}]
!          Global index of starting count for cells.
!     \item[{[globalAIPerDim]}]
!          Axis indices for cells on this DE.
!     \item[{[total]}]
!          Logical flag; if TRUE, return the information for the total cells
!          including the boundary widths, not just the computational cells.
!          The default is FALSE; return computational cells only.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      !integer :: localrc                          ! Error status
      integer :: i, i2
      type(ESMF_DistGridLocal), pointer :: me

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If total is true, return info for the total cells including
      ! boundary areas; otherwise return computational areas.
      me => dgtype%myDEComp
      if (present(total)) then
        if (total) me => dgtype%myDETotal
      endif

      ! If present, get information from distgrid derived type
      if (present(localCellCount)) &
                 localCellCount = me%localCellCount

      if (present(localCellCountPerDim)) then
        i2 = size(localCellCountPerDim)
        if (size(localCellCountPerDim).gt.dgtype%dimCount) then
          i2 = dgtype%dimCount
          call ESMF_LogWrite("size of array gt dimCount", ESMF_LOG_WARNING)
        endif
        do i = 1,i2
          localCellCountPerDim(i) = me%localCellCountPerDim(i)
        enddo
      endif

      if (present(globalStartPerDim)) then
        i2 = size(globalStartPerDim)
        if (size(globalStartPerDim).gt.dgtype%dimCount) then
          i2 = dgtype%dimCount
          call ESMF_LogWrite("size of array gt dimCount", ESMF_LOG_WARNING)
        endif
        do i = 1,i2
          globalStartPerDim(i) = me%globalStartPerDim(i)
        enddo
      endif

      if (present(globalAIPerDim)) then
        i2 = size(globalAIPerDim)
        if (size(globalAIPerDim).gt.dgtype%dimCount) then
          i2 = dgtype%dimCount
          call ESMF_LogWrite("size of array gt dimCount", ESMF_LOG_WARNING)
        endif
        do i = 1,i2
          globalAIPerDim(i) = me%globalAIPerDim(i)
        enddo
      endif
! TODO:  how to query for parts of an Index type

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetDE

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSetDEBlock"
!BOPI
! !IROUTINE: ESMF_DistGridSetDE - Set DE information for a DistGrid

! !INTERFACE:
      ! Private name; call using ESMF_DistGridSetDE()
      subroutine ESMF_DistGridSetDEBlock(dgtype, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Set a {\tt ESMF\_DistGrid} attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!          Class to be modified.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status
      integer :: DEID, localCellCount
      integer :: compCellCount
      integer :: i, myDEx, myDEy, nDEx, nDEy
      integer :: localDE, deCountPerDim(2), coord(2)
      type(ESMF_DistGridGlobal), pointer :: globC,  globT
      type(ESMF_DistGridLocal),  pointer :: localC, localT

      ! Initialize return code; assume failure until success is certain
      rc = ESMF_FAILURE

      call ESMF_DELayoutGet(dgtype%delayout, localDe=localDE, &
                            deCountPerDim=deCountPerDim, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_DELayoutGetDELocalInfo(dgtype%delayout, de=localDE, &
                                       coord=coord, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! bring things into the form that the local code expects them in
      nDEx = deCountPerDim(1)
      nDEy = deCountPerDim(2)
      myDEx = coord(1)
      myDEy = coord(2)

      DEID = (myDEy-1)*nDEx + myDEx

      globC  => dgtype%globalComp
      globT  => dgtype%globalTotal
      localC => dgtype%myDEComp
      localT => dgtype%myDETotal

      localC%MyDE  = DEID
      localT%MyDE  = DEID

!     TODO: need to create the following with ESMFArrayCreate before doing this
!     dgtype%DEids(DEID) = DEID        ! need to add capability for this
!                                          ! not to be true

      localCellCount = 1
      compCellCount  = 1
      do i = 1,dgtype%dimCount
        localC%globalAIPerDim(i)       = globC%AIPerDEPerDim(DEID,i)
        localC%globalStartPerDim(i)    = globC%globalStartPerDEPerDim(DEID,i)
        localC%localCellCountPerDim(i) = globC%cellCountPerDEPerDim(DEID,i)
        compCellCount                  = compCellCount &
                                       * globC%cellCountPerDEPerDim(DEID,i)

        localT%globalAIPerDim(i)       = globT%AIPerDEPerDim(DEID,i)
        localT%globalStartPerDim(i)    = globT%globalStartPerDEPerDim(DEID,i)
        localT%localCellCountPerDim(i) = globT%cellCountPerDEPerDim(DEID,i)
        localCellCount                 = localCellCount &
                                       * globT%cellCountPerDEPerDim(DEID,i)
      enddo
      localC%localCellCount = compCellCount
      localT%localCellCount = localCellCount

      rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetDEBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSetDEArb"
!BOPI
! !IROUTINE: ESMF_DistGridSetDE - Set DE information for a DistGrid

! !INTERFACE:
      ! Private name; call using ESMF_DistGridSetDE()
      subroutine ESMF_DistGridSetDEArb(dgtype, myCount, myIndices, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      integer, intent( in) :: myCount
      integer, dimension(:,:), intent(in) :: myIndices
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Set a {\tt ESMF\_DistGrid} attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!          Class to be modified.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status
      integer :: i, n, myDE
      type(ESMF_DistGridGlobal), pointer :: glob
      type(ESMF_DistGridLocal),  pointer :: local

      ! Initialize return code; assume failure until success is certain
      rc = ESMF_FAILURE

      call ESMF_DELayoutGet(dgtype%delayout, localDe=myDE, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! change myDE to F90 style, 1-based
      myDE = myDE + 1

      ! set pointers for ease of use
      glob  => dgtype%globalComp
      local => dgtype%myDEComp

      ! set local derived type information
      local%MyDE           = myDE
      local%localCellCount = myCount
      do i = 1,2
        local%globalAIPerDim(i)       = glob%AIPerDEPerDim(myDE,i)
        local%globalStartPerDim(i)    = glob%globalStartPerDEPerDim(myDE,i)
        local%localCellCountPerDim(i) = glob%cellCountPerDEPerDim(myDE,i)
        do n = 1,myCount
          local%localIndices(n,i)     = myIndices(n,i)
        enddo
      enddo

      rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetDEArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetAIsAllDEs"
!BOPI
! !IROUTINE: ESMF_DistGridGetAIsAllDEs - Get array of AxisIndices for DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetAIsAllDEs(dgtype, AIList, localGlobalFlag, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AIList
      type(ESMF_LocalGlobalFlag), intent(in) :: localGlobalFlag
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype]
!          Class to be modified.
!     \item[AI]
!          Array of {\tt AxisIndices} corresponding to the {\tt DistGrid}.
!     \item[localGlobalFlag]
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: localrc
      integer :: i, j, nDEs, rank
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! For now, assume the return AIs are from the computational domain
      ! TODO: add a flag for toal?
      glob => dgtype%globalComp

      ! Get information from distgrid derived type
      call ESMF_DELayoutGet(dgtype%delayout, deCount=nDEs, rc=localrc)
      rank = dgtype%dimCount

      ! check on AIList sizes
      if (size(AIList,1).lt.nDEs .OR. size(AIList,2).lt.rank) then
        call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, &
                                 "AIList array lengths not sufficient", &
                                 ESMF_CONTEXT, rc)
        return
      endif

      ! load return array
      do j   = 1, rank
        do i = 1, nDEs
          AIList(i,j)%min    = glob%AIPerDEPerDim(i,j)%min
          AIList(i,j)%max    = glob%AIPerDEPerDim(i,j)%max
          AIList(i,j)%stride = glob%AIPerDEPerDim(i,j)%stride
        enddo
      enddo

      ! if local indexing is requested, then modify the AIList array
      ! if it's global, we're done
      if (localGlobalFlag.eq.ESMF_LOCAL) then
        do j   = 1, rank
          do i = 1, nDEs
            AIList(i,j)%stride = AIList(i,j)%max - AIList(i,j)%min +1
            AIList(i,j)%max    = AIList(i,j)%stride
            AIList(i,j)%min    = 1
          enddo
        enddo
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetAIsAllDEs

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetAllAIBlock"
!BOPI
! !IROUTINE: ESMF_DistGridGetAllAIBlock - Get array of AxisIndices for DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetAllAIBlock(dgtype, AI, total, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype]
!          Class to be modified.
!     \item[AI]
!          Array of {\tt AxisIndices} corresponding to the {\tt DistGrid}.
!     \item[{[total]}]
!          Logical; if TRUE return AIs for all cells including boundary cells.
!          Default is FALSE, returning AIs which describe only computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      !integer :: localrc                          ! Error status
      integer :: i, i2, j, j2
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If total is true, return info for the AIs including
      ! boundary areas; otherwise return computational AIs.
      glob => dgtype%globalComp
      if (present(total)) then
        if (total) glob => dgtype%globalTotal
      endif

      ! Get information from distgrid derived type
      i2 = size(AI, 1)
      j2 = size(AI, 2)
      ! TODO: add size checking for i2, j2
      do j   = 1,j2
        do i = 1,i2
          AI(i,j)%min    = glob%AIPerDEPerDim(i,j)%min
          AI(i,j)%max    = glob%AIPerDEPerDim(i,j)%max
          AI(i,j)%stride = glob%AIPerDEPerDim(i,j)%stride
        enddo
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetAllAIBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetAllAIArb"
!BOPI
! !IROUTINE: ESMF_DistGridGetAllAIArb - Get array of AxisIndices for DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetAllAIArb(dgtype, AI, AIcountPerDE, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI
      integer, dimension(:), pointer :: AIcountPerDE
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype]
!          Class to be modified.
!     \item[AI]
!          Array of {\tt AxisIndices} corresponding to the {\tt DistGrid}.
!     \item[{[total]}]
!          Logical; if TRUE return AIs for all cells including boundary cells.
!          Default is FALSE, returning AIs which describe only computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      integer :: localrc                          ! Error status
      integer :: i, i1, i2, i3, j, badcount
      integer :: nDEs, myDE, thisDE
      integer, dimension(:), allocatable :: indices, globalIndices
      integer, dimension(:), allocatable :: disp, bufsize
      type(ESMF_DistGridGlobal), pointer :: glob
      type(ESMF_DistGridLocal), pointer :: me
      type(ESMF_VM) :: vm

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! set pointers to data
      glob => dgtype%globalComp
      me   => dgtype%myDEComp

      ! allocate local array
      allocate(indices(2*glob%maxLocalCellCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "indices", &
                                     ESMF_CONTEXT, rc)) return

      ! first get the appropriate VM
      call ESMF_DELayoutGetVM(dgtype%delayout, vm, localrc)
      call ESMF_DELayoutGet(dgtype%delayout, deCount=nDEs, rc=localrc)
      call ESMF_VMGet(vm, localPet=myDE, rc=localrc)   ! fix this

#if 0
      ! This is the old implementation that uses a loop of ESMF_VMBroadcast to
      ! collect the localIndices from all the DEs and put them into the AI
      ! array.  It performs very poorly on Cray X1.
       i1    = 0
      do j  = 1,nDEs
        thisDE = j - 1
        if (myDE.eq.thisDE) then
          do i = 1,me%localCellCount
            i2 = i + me%localCellCount
            indices(i)  = me%localIndices(i,1)
            indices(i2) = me%localIndices(i,2)
          enddo
        endif
        call ESMF_VMBroadcast(vm, indices, 2*glob%cellCountPerDE(j), &
                              thisDE, rc=localrc)
        ! unload into the big AI array
        AICountPerDE(j) = glob%cellCountPerDE(j)
        do i = 1,glob%cellCountPerDE(j)
          i1 = i1 + 1
          i2 = i + glob%cellCountPerDE(j)
          AI(i1,1)%min    = indices(i)
          AI(i1,1)%max    = indices(i)
          AI(i1,1)%stride = glob%globalCellCountPerDim(1)
          AI(i1,2)%min    = indices(i2)
          AI(i1,2)%max    = indices(i2)
          AI(i1,2)%stride = glob%globalCellCountPerDim(2)
        enddo
      enddo
#else
      ! globalIndices is a global indices array used by MPI_AllGatherV
      ! New implementation (P.Li - 7/2006):  Use ESMF_VMAllGatherV() to replace
      ! a loop of ESMF_VMBroadcast().  This implementation requires more
      ! memory space to hold the global Indices.  The performance of this
      ! implementation on IBM cluster and Cray X1 is better than the old
      ! implementation. However, it is memory inefficient when a large number
      ! of processors is used.  Therefore, we might want to consider using
      ! a loop of asynchronized send and recv calls instead of a global
      ! call. 
      allocate(globalIndices(2*glob%globalCellCount), stat=localrc)
      allocate(bufsize(nDEs), stat=localrc)
      allocate(disp(nDEs), stat=localrc)
	  do i = 1,me%localCellCount
            i2 = i + me%localCellCount
            indices(i)  = me%localIndices(i,1)
            indices(i2) = me%localIndices(i,2)
          enddo

      ! construct displacement array for MPI_AllGatherV()
      disp(1) = 0
      bufsize(1) = glob%cellCountPerDE(1)*2
      do j = 2,nDEs
	disp(j) = disp(j-1)+glob%cellCountPerDE(j-1)*2      
        bufsize(j) = glob%cellCountPerDE(j)*2
      enddo
  
      call ESMF_VMAllGatherV(vm, indices, 2*me%localCellCount, &
	 	globalIndices, bufsize, disp, rc=localrc)

      i1 = 0
      do j  = 1,nDEs
          AICountPerDE(j) = glob%cellCountPerDE(j)
          i2 = i1*2
        do i = 1,glob%cellCountPerDE(j)
          i1 = i1 + 1
          i2 = i2 + 1
	  i3 = i2 + glob%cellCountPerDE(j)
          AI(i1,1)%min    = globalIndices(i2)
          AI(i1,1)%max    = globalIndices(i2)
          AI(i1,1)%stride = glob%globalCellCountPerDim(1)
          AI(i1,2)%min    = globalIndices(i3)
          AI(i1,2)%max    = globalIndices(i3)
          AI(i1,2)%stride = glob%globalCellCountPerDim(2)
        enddo
      enddo

      ! clean up
      deallocate(globalIndices, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(disp, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(bufsize, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return
#endif
      deallocate(indices, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return
                                     
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetAllAIArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetAllCounts"
!BOPI
! !IROUTINE: ESMF_DistGridGetAllCounts - Get array of counts for DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetAllCounts(dgtype, cellCountPerDEPerDim, &
                                           total, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      integer, dimension(:,:) :: cellCountPerDEPerDim
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype]
!          Class to be modified.
!     \item[cellCountPerDEPerDim]
!          Array of the numbers of cells along each axis on each DE.
!     \item[{[total]}]
!          Logical; if TRUE return counts including boundary cells.  Default is FALSE,
!          returning only computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      !integer :: localrc                          ! Error status
      type(ESMF_DistGridGlobal), pointer :: glob
      integer :: i, j, nndes

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If total is true, return info for the total cells including
      ! boundary areas; otherwise return computational areas.
      glob => dgtype%globalComp
      if (present(total)) then
        if (total) glob => dgtype%globalTotal
      endif

      ! Get information from distgrid derived type
      ! TODO:  add check for array size or use size for loop limit
      call ESMF_DELayoutGet(dgtype%delayout, deCount=nndes, rc=rc)
      do i = 1,nndes
        do j = 1,dgtype%dimCount
          cellCountPerDEPerDim(i,j) = glob%cellCountPerDEPerDim(i,j)
        enddo
      enddo

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetAllCounts

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetDELayout"
!BOPI
! !IROUTINE: ESMF_DistGridGetDELayout - Get pointer to a DELayout for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetDELayout(dgtype, delayout, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      type(ESMF_DELayout) :: delayout
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a DistGrid attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype]
!          Class to be modified.
!     \item[delayout]
!          The {\tt ESMF\_DELayout} corresponding to the {\tt ESMF\_DistGrid}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 

      !integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Get information from distgrid derived type
      !  Note this needs to use = and not => to return the actual layout
      !  and not a pointer.
      delayout = dgtype%delayout

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetDELayout

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGlobalToLocalIndex"
!BOPI
! !IROUTINE: ESMF_DistGridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_DistGridGlobalToLocalIndex(dgtype, global1D, local1D, &
                                                 global2D, local2D, &
                                                 globalAI1D, localAI1D, &
                                                 globalAI2D, localAI2D, &
                                                 dimOrder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) :: global1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: local1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) :: global2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: local2D
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) ::  globalAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) :: localAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) ::  globalAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) :: localAI2D
      integer, dimension(:), optional, intent(in) :: dimOrder
      logical, optional, intent(in) :: total
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Translates global indexing to local indexing for a {\tt ESMF\_DistGrid}
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!          Class to be used.
!     \item[{[global1D]}]
!        One-dimensional Array of global identifiers to be translated.
!        Infers translating between positions in memory.
!     \item[{[local1D]}]
!        One-dimensional Array of local identifiers corresponding to
!        global identifiers.
!     \item[{[global2D]}]
!        Two-dimensional Array of global identifiers to be translated.
!        Infers translating between indices in ij space.
!     \item[{[local2D]}]
!        Two-dimensional Array of local identifiers corresponding to
!        global identifiers.
!     \item[{[globalAI1D]}]
!        One-dimensional array of global AxisIndices to be translated.
!     \item[{[localAI1D]}]
!        One-dimensional array of local AxisIndices corresponding to global AIs.
!     \item[{[globalAI2D]}]
!        Two-dimensional array of global AxisIndices to be translated.
!     \item[{[localAI2D]}]
!        Two-dimensional array of local AxisIndices corresponding to global AIs.
!     \item[{[total]}] 
!          Logical; if TRUE return values for total cells including boundary
!          cells.  If FALSE, return computational cells (which is the default).
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  XXXn.n, YYYn.n
! TODO: figure out the right way to translate AI's - should 1D refer to a number of AIs or rank?


      integer :: localrc                          ! Error status
      integer :: i, j, base, l1, r1, l2, r2
      integer, dimension(:), allocatable :: dimOrderUse
      type(ESMF_DistGridLocal), pointer :: me
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If total is true, return info for the total cells including
      ! boundary areas; otherwise return computational areas.
      me => dgtype%myDEComp
      glob => dgtype%globalComp
      if (present(total)) then
        if (total) then
          me => dgtype%myDETotal
          glob => dgtype%globalTotal
        endif
      endif

      !memory translation here
      if (present(global1D)) then

      !make sure local array is present as well
        if (.not. present(local1D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "local array not present", &
                                   ESMF_CONTEXT, rc)
          return
        endif
      !make sure array lengths are the same
        if (size(global1D) .NE. size(local1D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                   "array lengths not equal", &
                                   ESMF_CONTEXT, rc)
          return
        endif

!     the following code works only for grid where the global data is
!     organized (indexed) by DE  !TODO add coding for other cases
!     TODO: decide where enumerator for grid organization should be
!     TODO: this assumes exclusive indexing for local cells - total too?
!jw        base = dgtype%MyDE%globalStartPerDim()
! TODO: check this!!
        base = 0
        if (me%MyDE.ne.1) then
          do i = 1,me%MyDE-1
            base = base + glob%cellCountPerDE(i)
          enddo
        endif
        do i = 1, size(global1D)
          local1D(i) = global1D(i) - base
        enddo
  
      endif

      !index translation here
      if (present(global2D)) then

      !make sure local array is present as well
        if (.not. present(local2D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "global array not present", &
                                     ESMF_CONTEXT, rc)
          return
        endif
      !make sure array lengths are the same
        if (size(global2D) .NE. size(local2D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "array lengths not equal", &
                                     ESMF_CONTEXT, rc)
          return
        endif

        l1 = me%globalAIPerDim(1)%min
        r1 = me%globalAIPerDim(1)%max
        l2 = me%globalAIPerDim(2)%min
        r2 = me%globalAIPerDim(2)%max
        do i = 1, size(global2D,1)
          if (global2D(i,1).ge.l1 .and. global2D(i,1).le.r1 .and. &
             global2D(i,2).ge.l2 .and. global2D(i,2).le.r2 ) then
            local2D(i,1) = global2D(i,1) - l1 + 1
            local2D(i,2) = global2D(i,2) - l2 + 1
          else
            local2D(i,1) = -1 ! TODO: make an ESMF_NOTFOUND to use instead of -1
            local2D(i,2) = -1
          endif
        enddo
  
      endif

      !1-D AxisIndex translation here
      if (present(globalAI1D)) then
        !make sure local AI array is present as well
        if (.not. present(localAI1D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "local array not present", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        !make sure array lengths are the same
        if (size(globalAI1D) .NE. size(localAI1D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "array lengths not equal", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        ! calculate default if dimOrder is not present
        allocate(dimOrderUse(size(globalAI1D)), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "dimOrderUse", &
                                       ESMF_CONTEXT, rc)) return

        if (present(dimOrder)) then
          dimOrderUse(:) = dimOrder(:)
        else
          do i = 1,size(dimOrderUse)
            dimOrderUse(i) = i
          enddo
        endif

        do i = 1, size(globalAI1D)
          if     (dimOrderUse(i).eq.0) then
            localAI1D(i)%min = globalAI1D(i)%min
            localAI1D(i)%max = globalAI1D(i)%max
          elseif (dimOrderUse(i).eq.dgtype%decompIds(1)) then
            localAI1D(i)%min = globalAI1D(i)%min - me%globalStartPerDim(1)
            localAI1D(i)%max = globalAI1D(i)%max - me%globalStartPerDim(1)
          elseif (dimOrderUse(i).eq.dgtype%decompIds(2)) then
            localAI1D(i)%min = globalAI1D(i)%min - me%globalStartPerDim(2)
            localAI1D(i)%max = globalAI1D(i)%max - me%globalStartPerDim(2)
          endif
          localAI1D(i)%stride = localAI1D(i)%max - localAI1D(i)%min + 1
        enddo
        deallocate(dimOrderUse, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate dimOrderUse", &
                                       ESMF_CONTEXT, rc)) return
      endif

      !2-D AxisIndex translation here
      if (present(globalAI2D)) then
        !make sure local ai array is present as well
        if (.not. present(localAI2D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "local array not present", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        !make sure array lengths are the same
        if (size(globalAI2D) .NE. size(localAI2D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "array lengths not equal", &
                                     ESMF_CONTEXT, rc)
          return
        endif

        ! calculate default if dimOrder is not present
        allocate(dimOrderUse(size(globalAI2D,2)), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "dimOrderuse", &
                                       ESMF_CONTEXT, rc)) return

        if (present(dimOrder)) then
          dimOrderUse(:) = dimOrder(:)
        else
          do i = 1,size(dimOrderUse)
            dimOrderUse(i) = i
          enddo
        endif

        do j = 1, size(globalAI2D,2)
          do i = 1, size(globalAI2D,1)
            if     (dimOrderUse(j).eq.0) then
              localAI2D(i,j)%min    = globalAI2D(i,j)%min
              localAI2D(i,j)%max    = globalAI2D(i,j)%max
            elseif (dimOrderUse(j).eq.dgtype%decompIds(1)) then
              localAI2D(i,j)%min    = globalAI2D(i,j)%min &
                                    - glob%globalStartPerDEPerDim(i,1)
              localAI2D(i,j)%max    = globalAI2D(i,j)%max &
                                    - glob%globalStartPerDEPerDim(i,1)
            elseif (dimOrderUse(j).eq.dgtype%decompIds(2)) then
              localAI2D(i,j)%min    = globalAI2D(i,j)%min &
                                    - glob%globalStartPerDEPerDim(i,2)
              localAI2D(i,j)%max    = globalAI2D(i,j)%max &
                                    - glob%globalStartPerDEPerDim(i,2)
            endif
            localAI2D(i,j)%stride = localAI2D(i,j)%max - localAI2D(i,j)%min + 1
          enddo
        enddo
        deallocate(dimOrderUse, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate dimOrderUse", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGlobalToLocalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridLocalToGlobalIndex"
!BOPI
! !IROUTINE: ESMF_DistGridLocalToGlobalIndex - translate local indexing to global

! !INTERFACE:
      subroutine ESMF_DistGridLocalToGlobalIndex(dgtype, local1D, global1D, &
                                                 local2D, global2D, &
                                                 localAI1D, globalAI1D, &
                                                 localAI2D, globalAI2D, &
                                                 total, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) :: local1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: global1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) :: local2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: global2D
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) ::  localAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) :: globalAI2D
      logical, optional, intent(in) :: total
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Translates local indexing to global indexing for a {\tt ESMF\_DistGrid}
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!        Class to be used.
!     \item[{[local1D]}]
!        One-dimensional Array of local identifiers to be translated.
!        Infers translating between positions in memory.
!     \item[{[global1D]}]
!        One-dimensional Array of global identifiers corresponding to
!        local identifiers.
!     \item[{[local2D]}]
!        Two-dimensional Array of local identifiers to be translated.
!        Infers translating between indices in ij space.
!     \item[{[global2D]}]
!        Two-dimensional Array of global identifiers corresponding to
!        local identifiers.
!     \item[{[localAI1D]}]
!        One-dimensional array of local AxisIndices to be translated.
!     \item[{[globalAI1D]}]
!        One-dimensional array of global AxisIndices corresponding to local AIs.
!     \item[{[localAI2D]}]
!        Two-dimensional array of local AxisIndices to be translated.
!     \item[{[globalAI2D]}]
!        Two-dimensional array of global AxisIndices corresponding to local AIs.
!     \item[{[total]}] 
!        Logical; if TRUE then all values returned are in total cells including
!        boundary cells.  The default is FALSE; all values returned are in
!        terms of computational cells.
!     \item[{[rc]}] 
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  XXXn.n, YYYn.n

      !integer :: localrc                          ! Error status
      integer :: i, j, l1, l2, base, localCount
      type(ESMF_DistGridLocal), pointer :: me
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! If total is true, return info for the total cells including
      ! boundary areas; otherwise return computational areas.
      me => dgtype%myDEComp
      glob => dgtype%globalComp
      if (present(total)) then
        if (total) then
          me => dgtype%myDETotal
          glob => dgtype%globalTotal
        endif
      endif

      ! 1-D memory translation here
      if (present(local1D)) then
        ! make sure global array is present as well
        if (.not. present(global1D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "1D global array not present", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        ! make sure array lengths are the same
        if (size(global1D) .NE. size(local1D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "1D array lengths not equal", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        ! the following code works only for grid where the global data is
        ! organized (indexed) by DE  !TODO add coding for other cases
        ! TODO: decide where enumerator for grid organization should be
        ! TODO: this assumes exclusive indexing for local cells - total too?
        base = 0
        if (me%MyDE.ne.1) then
          do i = 1,me%MyDE-1
            base = base + glob%cellCountPerDE(i)
          enddo
        endif
        do i = 1, size(local1D)
          global1D(i) = local1D(i) + base
        enddo
      endif

      ! 2-D index translation here
      if (present(local2D)) then
        ! make sure global array is present as well
        if (.not. present(global2D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "2D global array not present", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        ! make sure array lengths are the same
        if (size(global2D) .NE. size(local2D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "2D array lengths not equal", &
                                     ESMF_CONTEXT, rc)
          return
        endif

        l1 = me%globalAIPerDim(1)%min
        l2 = me%globalAIPerDim(2)%min
        do i = 1, size(local2D,1)
          global2D(i,1) = local2D(i,1) + l1 - 1
          global2D(i,2) = local2D(i,2) + l2 - 1
        enddo
      endif

      ! 1-D AxisIndex translation here
      if (present(localAI1D)) then
        ! make sure global AI array is present as well
        if (.not. present(globalAI1D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "1D AI global array not present", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        ! make sure array lengths are the same
        if (size(globalAI1D) .NE. size(localAI1D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "1D AI array lengths not equal", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        localCount = me%globalAIPerDim(1)%max &
                   - me%globalAIPerDim(1)%min + 1
        do i = 1, size(localAI1D)
          globalAI1D(i)%min    = localAI1D(i)%min + me%globalStartPerDim(1)
          globalAI1D(i)%max    = localAI1D(i)%max + me%globalStartPerDim(1)
          globalAI1D(i)%stride = localAI1D(i)%stride - localCount &
                               + me%globalAIPerDim(1)%stride
        enddo
      endif

      ! 2-D AxisIndex translation here
      if (present(localAI2D)) then
        ! make sure global ai array is present as well
        if (.not. present(globalAI2D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "2D AI global array not present", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        ! make sure array lengths are the same
        if (size(globalAI2D) .NE. size(localAI2D)) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_INCOMP, &
                                     "2D AI array lengths not equal", &
                                     ESMF_CONTEXT, rc)
          return
        endif
        do j = 1, size(localAI2D,2)
          do i = 1, size(localAI2D,1)
            localCount = glob%AIPerDEPerDim(i,j)%max &
                       - glob%AIPerDEPerDim(i,j)%min + 1
            globalAI2D(i,j)%min    = localAI2D(i,j)%min &
                                   + glob%globalStartPerDEPerDim(i,j)
            globalAI2D(i,j)%max    = localAI2D(i,j)%max &
                                   + glob%globalStartPerDEPerDim(i,j)
            globalAI2D(i,j)%stride = localAI2D(i,j)%stride - localCount &
                                   + glob%AIPerDEPerDim(i,j)%stride
          enddo
        enddo
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridLocalToGlobalIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridAllocateBlock"
!BOPI
! !IROUTINE: ESMF_DistGridAllocateBlock - Allocate arrays in a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridAllocateBlock(dgtype, nDEs, dimCount, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype 
      integer, intent(in) :: nDEs
      integer, intent(in) :: dimCount
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     Allocates {\tt ESMF\_DistGrid} arrays to the specified sizes.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!          Class to be allocated.
!     \item[nDEs]
!          Number of DE's in the layout.
!     \item[dimCount]
!          Number of dimensions described by this {\tt ESMF\_DistGrid}.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: localrc                          ! Error status
      type(ESMF_DistGridLocal), pointer :: me
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      rc = ESMF_FAILURE

      ! DistGridType contents here:
      allocate(dgtype%decompIDs   (dimCount), &
               dgtype%coversDomain(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "distgrid contents", &
                                     ESMF_CONTEXT, rc)) return

      ! myDETotal contents here:
      me => dgtype%myDETotal
      allocate(me%localCellCountPerDim(dimCount), &
               me%globalStartPerDim   (dimCount), &
               me%globalAIPerDim      (dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "myDETotal contents", &
                                     ESMF_CONTEXT, rc)) return

      ! myDEComp contents here:
      me => dgtype%myDEComp
      allocate(me%localCellCountPerDim(dimCount), &
               me%globalStartPerDim   (dimCount), &
               me%globalAIPerDim      (dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "myDEComp", &
                                     ESMF_CONTEXT, rc)) return

      ! globalTotal contents here:
      glob => dgtype%globalTotal
      allocate(glob%globalCellCountPerDim  (     dimCount), &
               glob%maxLocalCellCountPerDim(     dimCount), &
               glob%cellCountPerDE         (nDEs         ), &
               glob%cellCountPerDEPerDim   (nDEs,dimCount), &
               glob%globalStartPerDEPerDim (nDEs,dimCount), &
               glob%AIPerDEPerDim          (nDEs,dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "globalTotal contents", &
                                     ESMF_CONTEXT, rc)) return

      ! globalComp contents here:
      glob => dgtype%globalComp
      allocate(glob%globalCellCountPerDim  (     dimCount), &
               glob%maxLocalCellCountPerDim(     dimCount), &
               glob%cellCountPerDE         (nDEs         ), &
               glob%cellCountPerDEPerDim   (nDEs,dimCount), &
               glob%globalStartPerDEPerDim (nDEs,dimCount), &
               glob%AIPerDEPerDim          (nDEs,dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "globalComp contents", &
                                     ESMF_CONTEXT, rc)) return

      rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridAllocateBlock

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridAllocateArb"
!BOPI
! !IROUTINE: ESMF_DistGridAllocateArb - Allocate arrays in a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridAllocateArb(dgtype, nDEs, dimCount, myCount, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), pointer :: dgtype 
      integer, intent(in) :: nDEs
      integer, intent(in) :: dimCount
      integer, intent(in) :: myCount
      integer, intent(out) :: rc            
!
! !DESCRIPTION:
!     Allocates {\tt ESMF\_DistGrid} arrays to the specified sizes.
!
!     The arguments are:
!     \begin{description}
!     \item[dgtype] 
!          Class to be allocated.
!     \item[dimCount]
!          Number of dimensions described by this {\tt ESMF\_DistGrid}.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: localrc                          ! Error status
      type(ESMF_DistGridLocal), pointer :: me
      type(ESMF_DistGridGlobal), pointer :: glob

      ! Initialize return code; assume failure until success is certain
      rc = ESMF_FAILURE

      ! DistGridType contents here:
      allocate(dgtype%decompIDs   (dimCount), &
               dgtype%coversDomain(dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "distgrid contents", &
                                     ESMF_CONTEXT, rc)) return

      ! myDEComp contents here:
      me => dgtype%myDEComp
      allocate(me%localCellCountPerDim(dimCount), &
               me%globalStartPerDim   (dimCount), &
               me%globalAIPerDim      (dimCount), &
               me%localIndices(myCount,dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "myDETotal contents", &
                                     ESMF_CONTEXT, rc)) return

      ! globalComp contents here:
      glob => dgtype%globalComp
      allocate(glob%globalCellCountPerDim  (     dimCount), &
               glob%globalStartPerDEPerDim (nDEs,dimCount), &
               glob%cellCountPerDE         (nDEs         ), &
               glob%cellCountPerDEPerDim   (nDEs,dimCount), &
               glob%AIPerDEPerDim          (nDEs,dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "globalTotal contents", &
                                     ESMF_CONTEXT, rc)) return

      rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridAllocateArb

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridValidate"
!BOPI
! !IROUTINE: ESMF_DistGridValidate - Check internal consistency of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridValidate(distgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a {\tt ESMF\_DistGrid} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridPrint"
!BOPI
! !IROUTINE: ESMF_DistGridPrint - Print the contents of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridPrint(distgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a {\tt ESMF\_DistGrid}.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
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
 
      integer :: i, j
      type(ESMF_DistGridType) , pointer :: dg

      print *, 'DistGrid Print:'

      dg => distgrid%ptr

      !Print the global axis indicies per DE
      if (associated(dg%globalComp%AIPerDEPerDim)) then
        do i = 1, size(dg%globalComp%AIPerDEPerDim, 1)
          print *, '   DE:', i
          do j = 1, size(dg%globalComp%AIPerDEPerDim, 2)
            print *, 'min:', dg%globalComp%AIPerDEPerDim(i,j)%min, 'max:', &
            dg%globalComp%AIPerDEPerDim(i,j)%max
          enddo
        enddo
      else
        print *, 'AIPerDEPerDim array not associated'
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridPrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSerialize"

!BOPI
! !IROUTINE: ESMF_DistGridSerialize - Serialize distgrid info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_DistGridSerialize(distgrid, buffer, length, offset, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), target, intent(in) :: distgrid
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_DistGrid} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_GridWrite()} and {\tt ESMF\_GridRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [distgrid]
!           {\tt ESMF\_DistGrid} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                     ! Error status
      integer :: nDEs
      type(ESMF_DistGridGlobal), pointer :: glob

      ! shortcut to internals
      glob => distgrid%ptr%globalComp

      ! get the number of DEs from the layout to pass through
      call ESMF_DELayoutGet(distgrid%ptr%delayout, deCount=nDEs, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! serialize the grid derived type
      call c_ESMC_DistGridSerialize(distgrid%ptr%dimCount, nDEs, &
                                    distgrid%ptr%decompIDs, &
                                    glob%cellCountPerDEPerDim, &
                                    buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_GridDeserialize"

!BOPI
! !IROUTINE: ESMF_GridDeserialize - Deserialize a byte stream into a Grid
!
! !INTERFACE:
      subroutine ESMF_DistGridDeserialize(buffer, offset, decompIDs, &
                                          countPerDEDecomp1, &
                                          countPerDEDecomp2, rc)
!
! !ARGUMENTS:
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, dimension(2), intent(inout) :: decompIDs
      integer, dimension(:), intent(inout) :: countPerDEDecomp1
      integer, dimension(:), intent(inout), optional :: countPerDEDecomp2
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a Grid object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_GridWrite()} and {\tt ESMF\_GridRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc, status             ! Error status, allocation status
      integer :: dimCount, i, i1, nDE1, nDE2
      integer, dimension(:,:), allocatable :: cellCountPerDEPerDim

      ! figure out sizes
      nDE2       = 1
      dimCount   = 1
      nDE1       = size(countPerDEDecomp1)
      if (present(countPerDEDecomp2)) then
        nDE2     = size(countPerDEDecomp2)
        dimCount = 2
      endif

      ! allocate array for distgrid deserialization
      allocate(cellCountPerDEPerDim(nDE1*nDE2, dimCount), stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "cellCountPerDEPerDim", &
                                     ESMF_CONTEXT, rc)) return

      ! deserialize the grid derived type
      call c_ESMC_DistGridDeserialize(decompIDs, &
                                      cellCountPerDEPerDim, &
                                      buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! load decomposition information into arrays
      do i = 1,nDE1
        countPerDEDecomp1(i) = cellCountPerDEPerDim(i,1)
      enddo
      if (present(countPerDEDecomp2)) then
        do i = 1,nDE2
          i1 = (i-1)*nDE1 + 1
          countPerDEDecomp2(i) = cellCountPerDEPerDim(i1,2)
        enddo
      endif

      ! clean up
      deallocate(cellCountPerDEPerDim, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "deallocate", &
                                     ESMF_CONTEXT, rc)) return

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridDeserialize

!------------------------------------------------------------------------------

      end module ESMF_DistGridMod
