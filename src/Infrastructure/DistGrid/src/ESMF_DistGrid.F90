! $Id: ESMF_DistGrid.F90,v 1.29 2003/02/21 21:13:58 jwolfe Exp $
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
      use ESMF_LayoutMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_MyDE
!
!     ! Description of ESMF_MyDE

      type ESMF_MyDE
      sequence
!     private
        integer :: MyDE            ! identifier for this DE
        integer :: lcelltot_count  ! local (on this DE) number of total cells
        integer :: lcellexc_count  ! local (on this DE) number of exclusive cells
        integer :: gcelltot_start  ! global index of starting count
        integer :: gcellexc_start  ! global index of starting count
        type (ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: lcelltot_index
                                ! local cell index in each direction using
                                ! global indexing, covering the total domain
                                ! (exclusive plus communicated).
        type (ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: lcellexc_index
                                ! local cell index in each direction using 
                                ! global indexing, covering the exclusive
                                ! domain only
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
        type (ESMF_Layout) :: layout
        type (ESMF_MyDE) :: MyDE       ! local DE identifiers
        logical :: covers_domain       ! identifier if distgrid covers
                                       ! the entire physical domain
        integer :: gcell_count         ! global number of cells
        integer :: halo_width          ! number of cells in each direction
                                       ! for the halo
        integer, dimension(ESMF_MAXGRIDDIM) :: decompids
        integer, dimension(ESMF_MAXGRIDDIM) :: gcell_dim
                                       ! global number of cells in each
                                       ! dimension
        integer :: lcelltot_max        ! maximum number of total cells
                                       ! on any DE
        integer :: lcellexc_max        ! maximum number of exclusive cells
                                       ! on any DE
        integer, dimension(ESMF_MAXGRIDDIM) :: lcelltot_max_dim
                                       ! maximum DE cell counts in each
                                       ! grid dimension
        integer, dimension(ESMF_MAXGRIDDIM) :: lcellexc_max_dim
                                       ! maximum DE cell counts in each
                                       ! grid dimension
        integer, dimension(:), pointer :: gcelltot_start
        integer, dimension(:), pointer :: gcellexc_start
        type (ESMF_AxisIndex), dimension(:,:), pointer :: lcelltot_index
        type (ESMF_AxisIndex), dimension(:,:), pointer :: lcellexc_index
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
      public ESMF_MyDE
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
    public ESMF_DistGridGet
    public ESMF_DistGridSet
    public ESMF_DistGridGetCounts
    public ESMF_DistGridSetCounts
    public ESMF_DistGridGetDE
    public ESMF_DistGridSetDE
    public ESMF_DistGridGetValue
    public ESMF_DistGridSetValue
    public ESMF_DistGridLocalToGlobalIndex
    public ESMF_DistGridGlobalToLocalIndex
    public ESMF_DistGridHalo
    public ESMF_DistGridValidate
    public ESMF_DistGridPrint
 
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DistGrid.F90,v 1.29 2003/02/21 21:13:58 jwolfe Exp $'

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
      interface ESMF_DistGridSetDE

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridSetDEInternal

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
! !IROUTINE: ESMF_DistGridCreateEmpty - Create a new DistGrid with no data

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
! !IROUTINE: ESMF_DistGridCreateInternal - Create a new DistGrid internally

! !INTERFACE:
      function ESMF_DistGridCreateInternal(nDE_i, nDE_j, i_max, j_max, &
                                           halo_width, layout, name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateInternal
!
! !ARGUMENTS:
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      integer, intent(in), optional :: halo_width
      type (ESMF_Layout), intent(in), optional :: layout
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
!     \item[[halo\_width]] 
!          Constant number of computation cells added to the decomposed
!          axes for haloing (added to the total cells but not exclusive
!          cells).
!     \item[[layout]]
!          Layout of DE's.
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
      call ESMF_DistGridConstructInternal(distgrid, nDE_i, nDE_j, i_max, &
                                          j_max, halo_width, layout, name, rc)

!     Set return values.
      ESMF_DistGridCreateInternal%ptr => distgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridDestroy - Free all resources associated with a DistGrid 

! !INTERFACE:
      subroutine ESMF_DistGridDestroy(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt DistGrid} object previously allocated
!     via an {\tt ESMF\_DistGridCreate routine}.
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

      integer :: status=ESMF_SUCCESS               ! Error status
      logical :: rcpresent=.FALSE.                 ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_LayoutDestroy(distgrid%ptr%layout, status)

      end subroutine ESMF_DistGridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridConstructNew - Construct the internals of an allocated DistGrid

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
!     as needed.  Must call the corresponding ESMF\_DistGridDestruct
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
      integer :: i                                 ! loop counter

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Set the DistGrid name if present, otherwise construct a default one
      call ESMF_SetName(distgrid%base, name, "DistGrid", status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructNew: Setname"
        return
      endif

!     Initialize distgrid contents
      distgrid%MyDE%MyDE = 0
      distgrid%MyDE%lcelltot_count = 0
      distgrid%MyDE%lcellexc_count = 0
      distgrid%MyDE%gcelltot_start = 0
      distgrid%MyDE%gcellexc_start = 0
      do i = 1,ESMF_MAXGRIDDIM
        distgrid%MyDE%lcelltot_index(i)%l = 0
        distgrid%MyDE%lcelltot_index(i)%r = 0
        distgrid%MyDE%lcelltot_index(i)%max = 0
        distgrid%MyDE%lcelltot_index(i)%decomp = 0
        distgrid%MyDE%lcellexc_index(i)%l = 0
        distgrid%MyDE%lcellexc_index(i)%r = 0
        distgrid%MyDE%lcellexc_index(i)%max = 1
        distgrid%MyDE%lcellexc_index(i)%decomp = 0
      enddo
      distgrid%covers_domain = .false.
      distgrid%gcell_count = 0
      distgrid%halo_width = 0
      do i = 1,ESMF_MAXGRIDDIM
        distgrid%gcell_dim(i) = 0
        distgrid%lcelltot_max_dim(i) = 0
        distgrid%lcellexc_max_dim(i) = 0
      enddo
      nullify(distgrid%gcelltot_start)
      nullify(distgrid%gcellexc_start)
      nullify(distgrid%lcelltot_index)
      nullify(distgrid%lcellexc_index)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridConstructInternal - Construct the internals of an allocated DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridConstructInternal(distgrid, nDE_i, nDE_j, &
                                                i_max, j_max, halo_width, &
                                                layout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid 
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      integer, intent(in), optional :: halo_width
      type (ESMF_Layout), intent(in), optional :: layout
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF\_DistGridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Pointer to a {\tt DistGrid}.
!     \item[[nDE\_i]] 
!          Number of distributed elements (DE's) in the 1st direction.
!     \item[[nDE\_j]] 
!          Number of distributed elements (DE's) in the 2nd direction.
!     \item[[i\_max]] 
!          Global number of computation cells in the 1st direction.
!     \item[[j\_max]] 
!          Global number of computation cells in the 2nd direction.
!     \item[[halo\_width]] 
!          Constant number of computation cells added to the decomposed
!          axes for haloing (added to the total cells but not exclusive
!          cells).
!     \item[[name]] 
!          {\tt DistGrid} name.
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
      integer, dimension(:), allocatable :: PEList
      logical :: cover_domain_dir1
      logical :: cover_domain_dir2
      integer :: i, lDE_i, lDE_j
      integer, dimension(ESMF_MAXGRIDDIM) :: gcell_dim

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
      gcell_dim(1) = i_max
      gcell_dim(2) = j_max
      call ESMF_DistGridSet(distgrid, &
                            covers_domain=.true., &
                            gcell_count=i_max*j_max, &
                            gcell_dim=gcell_dim, &
                            halo_width=halo_width, &
                            rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: distgrid set"
        return
      endif

!     If a layout is passed in, set the distgrid layout to it.
!     Otherwise create layout with specified decomposition.
      if(present(layout)) then
        call ESMF_LayoutGetSize(layout, lDE_i, lDE_j, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_DistGridConstructInternal: Layout get size"
          return
        endif
        distgrid%layout = layout
      else
        if(nDE_i.eq.0 .or. nDE_j.eq.0) then
          print *, "ERROR in ESMF_DistGridConstructInternal: nDE_i or nDE_j"
          return
        endif
        allocate(PEList(nDE_i*nDE_j))
        do i = 1,nDE_i*nDE_j
          PEList(i) = i - 1  ! TODO:  short-term fix to go to C++
        enddo
        distgrid%layout = ESMF_LayoutCreate(nDE_i, nDE_j, PEList, &
                                            ESMF_XFAST, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_DistGridConstructInternal: Layout create"
          return
        endif
        lDE_i = nDE_i
        lDE_j = nDE_j
      endif

!     Allocate resources based on number of DE's
      allocate(distgrid%gcelltot_start(lDE_i*lDE_j), stat=status)
      allocate(distgrid%gcellexc_start(lDE_i*lDE_j), stat=status)
      allocate(distgrid%lcelltot_index(lDE_i*lDE_j,ESMF_MAXGRIDDIM), &
               stat=status)
      allocate(distgrid%lcellexc_index(lDE_i*lDE_j,ESMF_MAXGRIDDIM), &
               stat=status)
      if(status .NE. 0) then
        print *, "ERROR in ESMF_DistGridConstructInternal: allocate"
        return
      endif

!     Parse problem size
      call ESMF_DistGridSetCounts(distgrid, lDE_i, lDE_j, i_max, j_max, &
                                  status)
        
!     Fill in DE derived type
      call ESMF_DistGridSetDE(distgrid, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: Set de"
        return
      endif

!     Calculate other distgrid values from DE information
!     distgrid%lcelltot_max = GlobalCommMax()
!     distgrid%lcellexc_max = GlobalCommMax()
!     do i = 1,ESMF_MAXGRIDDIM
!       distgrid%lcelltot_max_dim(i) = GlobalCommMax()
!     enddo

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: DistGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructInternal

!------------------------------------------------------------------------------
!BOP
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
! !IROUTINE: ESMF_DistGridGet - Get information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGet(distgrid, &
                                  covers_domain, gcell_count, gcell_dim, &
                                  lcelltot_max, lcellexc_max, &
                                  lcelltot_max_dim, lcellexc_max_dim, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      logical, intent(inout), optional :: covers_domain
      integer, intent(inout), optional :: gcell_count
      integer, dimension(:), intent(inout), optional :: gcell_dim
      integer, intent(inout), optional :: lcelltot_max
      integer, intent(inout), optional :: lcellexc_max
      integer, dimension(:), intent(inout), optional :: lcelltot_max_dim
      integer, dimension(:), intent(inout), optional :: lcellexc_max_dim
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns information from the DistGrid object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[[covers\_domain]]
!          Logical identifier if distgrid covers the entire physical domain.
!     \item[[gcell_count]]
!          Global total number of cells.
!     \item[[gcell\_dim]]
!          Array of the global number of cells in each dimension.
!     \item[[lcelltot\_max]]
!          Maximum number of total cells on any DE.
!     \item[[lcelltot\_max]]
!          Maximum number of exclusive cells on any DE.
!     \item[[lcelltot\_max\_dim]]
!          Array of the maximum number of total cells in each dimension on
!          any DE.
!     \item[[lcellexc\_max\_dim]]
!          Array of the maximum number of exclusive cells in each dimension
!          on any DE.
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

!     if present, get information from distgrid derived type
      if(present(covers_domain)) &
                 covers_domain = distgrid%covers_domain
      if(present(gcell_count)) gcell_count = distgrid%gcell_count
      if(present(gcell_dim)) then
                 ! TODO: add check that gcell_dim is large enough
                 !       or use the size of the array for the loop
                 !       limit
        do i = 1,ESMF_MAXGRIDDIM
          gcell_dim(i) = distgrid%gcell_dim(i)
        enddo
      endif
      if(present(lcelltot_max)) lcelltot_max = distgrid%lcelltot_max
      if(present(lcellexc_max)) lcellexc_max = distgrid%lcellexc_max
      if(present(lcelltot_max_dim)) then
                 ! TODO: add check that lcelltot_max_dim is large enough
                 !       or use the size of the array for the loop
                 !       limit
        do i = 1,ESMF_MAXGRIDDIM
          lcelltot_max_dim(i) = distgrid%lcelltot_max_dim(i)
        enddo
      endif
      if(present(lcellexc_max_dim)) then
                 ! TODO: add check that lcellexc_max_dim is large enough
                 !       or use the size of the array for the loop
                 !       limit
        do i = 1,ESMF_MAXGRIDDIM  
          lcellexc_max_dim(i) = distgrid%lcellexc_max_dim(i)
        enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS
!
      end subroutine ESMF_DistGridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSet - Set information about a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSet(distgrid, &
                                  covers_domain, gcell_count, gcell_dim, &
                                  halo_width, lcelltot_max, lcellexc_max, &
                                  lcelltot_max_dim, lcellexc_max_dim, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      logical, intent(in), optional :: covers_domain
      integer, intent(in), optional :: gcell_count
      integer, dimension(:), intent(in), optional :: gcell_dim
      integer, intent(in), optional :: halo_width
      integer, intent(in), optional :: lcelltot_max
      integer, intent(in), optional :: lcellexc_max
      integer, dimension(:), intent(in), optional :: lcelltot_max_dim
      integer, dimension(:), intent(in), optional :: lcellexc_max_dim
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Sets the DistGrid object with information given.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be set.
!     \item[[covers\_domain]]
!          Logical identifier if distgrid covers the entire physical domain.
!     \item[[gcell_count]]
!          Global total number of cells.
!     \item[[gcell\_dim]]
!          Array of the global number of cells in each dimension.
!     \item[[lcelltot\_max]]
!          Maximum number of total cells on any DE.
!     \item[[lcelltot\_max]]
!          Maximum number of exclusive cells on any DE.
!     \item[[lcelltot\_max\_dim]]
!          Array of the maximum number of total cells in each dimension on
!          any DE.
!     \item[[lcellexc\_max\_dim]]
!          Array of the maximum number of exclusive cells in each dimension
!          on any DE.
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

!     if present, set information filling in distgrid derived type
      if(present(covers_domain)) &
                 distgrid%covers_domain = covers_domain
      if(present(gcell_count)) distgrid%gcell_count = gcell_count
      if(present(gcell_dim)) then
                 ! TODO: add check that gcell_dim is large enough
                 !       or use the size of the array for the loop
                 !       limit
        do i = 1,ESMF_MAXGRIDDIM
          distgrid%gcell_dim(i) = gcell_dim(i)
        enddo
      endif
      if(present(halo_width)) distgrid%halo_width = halo_width
      if(present(lcelltot_max)) distgrid%lcelltot_max = lcelltot_max
      if(present(lcellexc_max)) distgrid%lcellexc_max = lcellexc_max
      if(present(lcelltot_max_dim)) then
                 ! TODO: add check that lcelltot_max_dim is large enough
                 !       or use the size of the array for the loop
                 !       limit
        do i = 1,ESMF_MAXGRIDDIM
          distgrid%lcelltot_max_dim(i) = lcelltot_max_dim(i)
        enddo
      endif
      if(present(lcellexc_max_dim)) then
                 ! TODO: add check that lcellexc_max_dim is large enough
                 !       or use the size of the array for the loop
                 !       limit
        do i = 1,ESMF_MAXGRIDDIM  
          distgrid%lcellexc_max_dim(i) = lcellexc_max_dim(i)
        enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetConfig - Get configuration information from a DistGrid

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
! !IROUTINE: ESMF_DistGridSetConfig - Set configuration information for a DistGrid

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
! !IROUTINE: ESMF_DistGridGetValue - Get <Value> for a DistGrid

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
! !IROUTINE: ESMF_DistGridSetValue - Set <Value> for a DistGrid

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
! !IROUTINE: ESMF_DistGridGetCounts - Get extent counts for a DistGrid for a given DE

! !INTERFACE:
      subroutine ESMF_DistGridGetCounts(distgrid, DE_id, &
                                        lcelltot_count, lcellexc_count, &
                                        lcelltot_start, lcellexc_start, &
                                        lcelltot_end, lcellexc_end, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in) :: DE_id
      integer, dimension(:), intent(inout), optional :: lcelltot_count
      integer, dimension(:), intent(inout), optional :: lcellexc_count
      integer, dimension(:), intent(inout), optional :: lcelltot_start
      integer, dimension(:), intent(inout), optional :: lcellexc_start
      integer, dimension(:), intent(inout), optional :: lcelltot_end
      integer, dimension(:), intent(inout), optional :: lcellexc_end
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
!     \item[[lcelltot\_count]]
!          Array of the number of total cells per dimension for this DE.
!     \item[[lcellexc\_count]]
!          Array of the number of exclusive cells per dimension for this DE.
!     \item[[lcelltot\_start]]
!          Array of the starting position, in the global decompostion, for
!          the total cells per dimension for this DE.
!     \item[[lcellexc\_start]]
!          Array of the starting position, in the global decompostion, for
!          the exclusive cells per dimension for this DE.
!     \item[[lcelltot\_end]]
!          Array of the ending position, in the global decompostion, for
!          the total cells per dimension for this DE.
!     \item[[lcellexc\_end]]
!          Array of the ending position, in the global decompostion, for
!          the exclusive cells per dimension for this DE.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present
      integer :: i

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Retrieve extent counts for each axis from the de identifier
!     TODO:  check validity of DE_id
      if(present(lcelltot_count)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          lcelltot_count(i) = distgrid%lcelltot_index(DE_id,i)%r &
                            - distgrid%lcelltot_index(DE_id,i)%l + 1
        enddo
      endif
      if(present(lcellexc_count)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          lcellexc_count(i) = distgrid%lcellexc_index(DE_id,i)%r &
                            - distgrid%lcellexc_index(DE_id,i)%l + 1
        enddo
      endif
      if(present(lcelltot_start)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          lcelltot_start(i) = distgrid%lcelltot_index(DE_id,i)%l
        enddo
      endif
      if(present(lcellexc_start)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          lcellexc_start(i) = distgrid%lcellexc_index(DE_id,i)%l
        enddo
      endif
      if(present(lcelltot_end)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          lcelltot_end(i) = distgrid%lcelltot_index(DE_id,i)%r
        enddo
      endif
      if(present(lcellexc_end)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          lcellexc_end(i) = distgrid%lcellexc_index(DE_id,i)%r
        enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetCounts

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetCountsInternal - Set extent counts for a DistGrid

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
      integer :: i, j, de                            !
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
!     TODO:  make a layout method to calculate this so it's consistent
      ni = i_max/nDE_i
      global_s = 1
      global_e = 0
      do i = 1,nDE_i
        global_e = global_e + ni
        if(i.eq.nDE_i) global_e = i_max
        do j = 1,nDE_j
          de = (j-1)*nDE_j + i
          distgrid%lcellexc_index(de,1)%l = global_s
          distgrid%lcellexc_index(de,1)%r = global_e
          distgrid%lcellexc_index(de,1)%max = i_max
          distgrid%lcellexc_index(de,1)%decomp = 1
          distgrid%lcelltot_index(de,1)%l = global_s
          distgrid%lcelltot_index(de,1)%r = global_e
          distgrid%lcelltot_index(de,1)%max = i_max
          distgrid%lcelltot_index(de,1)%decomp = 1
        enddo
        global_s = global_e + 1
      enddo
      nj = j_max/nDE_j
      global_s = 1
      global_e = 0
      do j = 1,nDE_j
        global_e = global_e + nj
        if(j.eq.nDE_j) global_e = j_max
        do i = 1,nDE_i
          de = (j-1)*nDE_j + i
          distgrid%lcellexc_index(de,2)%l = global_s
          distgrid%lcellexc_index(de,2)%r = global_e
          distgrid%lcellexc_index(de,2)%max = j_max
          distgrid%lcellexc_index(de,2)%decomp = 2
          distgrid%lcelltot_index(de,2)%l = global_s
          distgrid%lcelltot_index(de,2)%r = global_e
          distgrid%lcelltot_index(de,2)%max = j_max
          distgrid%lcelltot_index(de,2)%decomp = 2
        enddo
        global_s = global_e + 1
      enddo
      global_s = 0
      do j = 1,nDE_j
        do i = 1,nDE_i
          de = (j-1)*nDE_j + i
          distgrid%gcellexc_start(de) = global_s
          distgrid%gcelltot_start(de) = global_s
          global_s = global_s + (distgrid%lcellexc_index(de,1)%r - &
                                 distgrid%lcellexc_index(de,1)%l + 1)* &
                                (distgrid%lcellexc_index(de,2)%r - &
                                 distgrid%lcellexc_index(de,2)%l + 1)
                                ! TODO: add third dimension?
                                ! TODO: add total cells
        enddo
      enddo

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetCountsInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetDE - Get DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetDE(distgrid, MyDE, &
                                    lcelltot_count, lcellexc_count, &
                                    gcelltot_start, gcellexc_start, &
                                    lcelltot_index, lcellexc_index, &
                                    rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(inout), optional :: MyDE
      integer, intent(inout), optional :: lcelltot_count
      integer, intent(inout), optional :: lcellexc_count
      integer, intent(inout), optional :: gcelltot_start
      integer, intent(inout), optional :: gcellexc_start
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM), intent(inout), &
                        optional :: lcelltot_index
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM), intent(inout), &
                        optional :: lcellexc_index
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a DistGrid attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Class to be modified.
!     \item[[MyDE]]
!          Identifier for this DE.
!     \item[[lcelltot\_count]]
!          Local (on this DE) number of total cells.
!     \item[[lcellexc\_count]]
!          Local (on this DE) number of exclusive cells.
!     \item[[gcelltot\_start]]
!          Global index of starting count for total cells.
!     \item[[gcellexc\_start]]
!          Global index of starting count for exclusive cells.
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

!     if present, get information from physgrid derived type
      if(present(MyDE)) MyDE = distgrid%myDE%MyDE
      if(present(lcelltot_count)) lcelltot_count = distgrid%myDE%lcelltot_count
      if(present(lcellexc_count)) lcellexc_count = distgrid%myDE%lcellexc_count
      if(present(gcelltot_start)) gcelltot_start = distgrid%myDE%gcelltot_start
      if(present(gcellexc_start)) gcellexc_start = distgrid%myDE%gcellexc_start
      if(present(lcelltot_index)) lcelltot_index = distgrid%myDE%lcelltot_index
      if(present(lcellexc_index)) lcellexc_index = distgrid%myDE%lcellexc_index
! TODO:  how to query for parts of an Index type

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetDE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetDEInternal - Set DE information for a DistGrid

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
      integer :: DE_id, lcelltot_count, lcellexc_count
      integer :: i

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_LayoutGetDEid(distgrid%layout, DE_id, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridSetDEInternal: layout get DEid"
        return
      endif
      DE_id = DE_id + 1    ! TODO:  have to add one to go from C

      distgrid%MyDE%MyDE = DE_id
!     TODO: need to create the following with ESMFArrayCreate before doing this
!     distgrid%DEids(DE_id) = DE_id        ! need to add capability for this
!                                          ! not to be true

      lcelltot_count = 1
      lcellexc_count = 1
      do i=1,ESMF_MAXGRIDDIM
        distgrid%MyDE%lcelltot_index(i) = distgrid%lcelltot_index(DE_id,i)
        distgrid%MyDE%lcellexc_index(i) = distgrid%lcellexc_index(DE_id,i)
        lcelltot_count = lcelltot_count * (distgrid%lcelltot_index(DE_id,i)%r - &
                                           distgrid%lcelltot_index(DE_id,i)%l + 1)
        lcellexc_count = lcellexc_count * (distgrid%lcellexc_index(DE_id,i)%r - &
                                           distgrid%lcellexc_index(DE_id,i)%l + 1)
      enddo
      distgrid%MyDE%lcelltot_count = lcelltot_count
      distgrid%MyDE%lcellexc_count = lcellexc_count
      distgrid%MyDE%gcelltot_start = distgrid%gcelltot_start(DE_id)
      distgrid%MyDE%gcellexc_start = distgrid%gcellexc_start(DE_id)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetDEInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_DistGridGlobalToLocalIndex(distgrid, global, local, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), intent(in) :: distgrid
      integer(ESMF_IKIND_I4), dimension(:), intent(in) :: global
      integer(ESMF_IKIND_I4), dimension(:), intent(out) :: local
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Translates global indexing to local indexing for a DistGrid
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be used.
!     \item[[global]]
!          Array of global identifiers to be translated.
!     \item[[local]]
!          Array of local identifiers corresponding to global identifiers.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present
      integer :: i, base

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     make sure array lengths are the same
      if(size(local) .NE. size(global)) then
        print *, "ERROR in ESMF_DistGridGlobalToLocal: array lengths not equal"
        return
      endif

!     the following code works only for grid where the global data is
!     organized (indexed) by DE  !TODO add coding for other cases
!     TODO: decide where enumerator for grid organization should be
!     TODO: this assumes exclusive indexing for local cells - total too?
      base = distgrid%MyDE%gcellexc_start
      do i = 1, size(global)
        local(i) = global(i) - base
      enddo

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGlobalToLocalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridLocalToGlobalIndex - translate local indexing to global

! !INTERFACE:
      subroutine ESMF_DistGridLocalToGlobalIndex(distgrid, local, global, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), intent(in) :: distgrid
      integer(ESMF_IKIND_I4), dimension(:), intent(in) :: local
      integer(ESMF_IKIND_I4), dimension(:), intent(out) :: global
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Translates local indexing to global indexing for a DistGrid
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be used.
!     \item[[local]]
!          Array of local identifiers to be translated.
!     \item[[global]]
!          Array of global identifiers corresponding to local identifiers.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present
      integer :: i, base

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     make sure array lengths are the same
      if(size(global) .NE. size(local)) then
        print *, "ERROR in ESMF_DistGridLocalToGlobal: array lengths not equal"
        return
      endif

!     the following code works only for grid where the global data is
!     organized (indexed) by DE  !TODO add coding for other cases
!     TODO: decide where enumerator for grid organization should be
!     TODO: this assumes exclusive indexing for local cells - total too?
      base = distgrid%MyDE%gcellexc_start
      do i = 1, size(local)
        global(i) = local(i) + base
      enddo

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridLocalToGlobalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridHalo - halo an array

! !INTERFACE:
      subroutine ESMF_DistGridHalo(distgrid, array, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), intent(in) :: distgrid
      type(ESMF_Array), intent(inout) :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Halo an array.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be used.
!     \item[[array]]
!          ESMF\_Array to be haloed.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      integer :: rank, i, j
      integer, dimension(:), allocatable :: decompids
      type(ESMF_AxisIndex), dimension(:), allocatable ::  AI_exc, AI_tot, AI_array

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Get Array rank to determine size of some local arrays
      call ESMF_ArrayGet(array, rank=rank, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridHalo: array get"
        return
      endif

!     Allocate local arrays
      allocate(decompids(rank))
      allocate(AI_exc(rank))
      allocate(AI_tot(rank))
      allocate(AI_array(rank))

!     Get array axis indices
      call ESMF_ArrayGetAxisIndex(array, AI_array, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridHalo: array get"
        return
      endif

!     Set decompids and local axis indices based on mapping of Array to
!     Grid
      do j = 1,ESMF_MAXGRIDDIM
        do i = 1,rank
          if (AI_array(i)%decomp .eq. j) then
            decompids(i) = j
            AI_exc(i) = distgrid%MyDE%lcellexc_index(j)
            AI_tot(i) = distgrid%MyDE%lcelltot_index(j)
          else
            decompids(i) = 0
            AI_exc(i) = AI_array(i)
            AI_tot(i) = AI_array(i)
          endif
        enddo
      enddo

!     Call Array method with DistGrid AxisIndices
      call ESMF_ArrayHalo(array, distgrid%layout, decompids, AI_exc, &
                          AI_tot, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridHalo:"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridHalo

!------------------------------------------------------------------------------
!BOP
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
